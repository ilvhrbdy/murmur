mod lexer;
mod utils;

use lexer::{Func, Item, Phrase, ReturnValue};
use std::collections::HashMap;
use std::path::Path;

// TODO: need to do something with cases like this:
//
//      @as start
//      - start
//          @hide
//
//      @jump start
//      # which leads to inf loop of jumping to itself
//      # same as
//      @as aboba
//      @jump aboba       # which is funny
//
//
// i mean it is a logical error, not a problem of the language, but maybe a debuger mode for murmur cli or some sort of to debug behaviour of the script
// we could warn for some weird behavior like this:
//
//      @as aboba
//      @jump aboba
//
//      which is a valid murmur syntax, but just an infinite loop
//      maybe i need to be strict with this kind of shit?

// TODO: `@def <temlate name> [$args] <block>` macro and expanding it with
//      @ <module>.<template> for expanding macro from other module
//      @ .<tempate> for current module
// same for interpolation @{ .<template> }
// so it expands either as text or code depending on the context of expansion
// @def string Aboba
// or
// @def custom_jump $name @jump $name
// or
// @def block $opt
//      - aboba
//      > $opt
// TODO: @if @elif @else
//      idea: to validate that the function returns a bool we can execute it on a dummy (cloned) state and get the return value
// TODO: write tests, please?
// TODO: normal error messages

#[derive(Debug)]
pub struct Conversation<State> {
    state: State,
    funcs: Vec<Func<State>>,

    vis: Vec<bool>,
    items: Vec<Item>,
    links: Vec<Option<usize>>,

    current: usize,
}

impl<State: Clone> Conversation<State> {
    pub fn load(
        path: impl AsRef<Path>,
        funcs_map: HashMap<&'static str, Func<State>>,
        state: State,
    ) -> Result<Self, ()> {
        let path = path.as_ref();
        let file_name = path.file_name().unwrap().to_str().unwrap();
        let module_name = file_name.strip_suffix(".mur").unwrap_or(file_name);
        let src = utils::read_mur_file(path).map_err(|e| {
            eprintln!("failed to load file: {e}");
        })?;

        Self::from_source(src, module_name, funcs_map, state)
    }

    pub fn from_source(
        src: impl AsRef<str>,
        module_name: &str,
        funcs_map: HashMap<&'static str, Func<State>>,
        mut state: State,
    ) -> Result<Self, ()> {
        let tokens = lexer::tokenize(src.as_ref());
        let (items, links, funcs) =
            lexer::parse_tokens_into_items(module_name, tokens, funcs_map, &mut state)?;

        if items.is_empty() {
            eprintln!("you must define at least one state");
            return Err(());
        }

        let mut it = Self {
            state,
            funcs,
            vis: vec![true; items.len()],
            current: 0,
            items,
            links,
        };

        it.get_to_response();

        Ok(it)
    }

    pub fn response(&self) -> Option<&str> {
        if let Item::Response { phrase, .. } = &self.items[self.current]
            && !phrase.is_empty()
        {
            Some(phrase.as_str())
        } else {
            None
        }
    }

    pub fn options(&self) -> impl Iterator<Item = &str> {
        self.options_indices().map(|opt| {
            let Item::Option { phrase, .. } = &self.items[opt] else {
                unreachable!();
            };

            let (Phrase::Static(buffer) | Phrase::Dynamic { buffer, .. }) = &phrase;

            buffer.as_str()
        })
    }

    pub fn next_state(&mut self) -> Option<()> {
        self.current = self.links[self.current]?;
        self.get_to_response()
    }

    pub fn next_state_from_option(&mut self, option_idx: usize) -> Option<()> {
        let next = self
            .options_indices()
            .nth(option_idx)
            .expect("valid index of an option"); // TODO: error?

        self.current = next;
        self.next_state()?;

        Some(())
    }

    fn options_indices(&self) -> impl Iterator<Item = usize> {
        match &self.items[self.current] {
            Item::Response { options, .. } => options,
            _ => unreachable!(),
        }
        .iter()
        .filter(|opt| self.vis[**opt])
        .copied()
    }

    fn get_to_response(&mut self) -> Option<()> {
        loop {
            match &self.items[self.current] {
                _ if !self.vis[self.current] => self.current = self.links[self.current]?,
                Item::Response { .. } => {
                    self.update_phrases_in_state();
                    // println!("found next response");
                    return Some(());
                }
                Item::Show(items) => {
                    for item in items {
                        //     println!("showing item {item}");
                        self.vis[*item] = true;
                    }
                    self.current = self.links[self.current]?;
                }
                Item::Hide(items) => {
                    for item in items {
                        //     println!("hiding item {item}");
                        self.vis[*item] = false;
                    }
                    self.current = self.links[self.current]?;
                }
                Item::Jump(target) => {
                    // println!("jumping to {target}");
                    self.current = *target;
                }
                Item::FunctionCall { func, args } => {
                    (self.funcs[*func])(&mut self.state, args);
                    self.current = self.links[self.current]?;
                }
                Item::Option { .. } => unreachable!(),
            }
        }
    }

    fn update_phrases_in_state(&mut self) {
        let Item::Response { phrase, .. } = &mut self.items[self.current] else {
            unreachable!();
        };

        phrase.update(&self.funcs, &mut self.state);

        let Some((left, right)) = self.items.split_at_mut_checked(self.current + 1) else {
            // means there is no options
            return;
        };

        let Some(Item::Response { options, .. }) = left.last_mut() else {
            unreachable!();
        };

        let offset = self.current + 1;

        for &option in options.iter() {
            let opt = option - offset;

            if !self.vis[opt] {
                continue;
            }

            let Item::Option { phrase } = &mut right[opt] else {
                unreachable!();
            };

            phrase.update(&self.funcs, &mut self.state);
        }
    }
}

fn run<State: Clone>(mut conv: Conversation<State>) -> std::io::Result<()> {
    use std::io::Write;

    // for i in 0..conv.items.len() {
    //     println!("{:?} -> {:?}", conv.items[i], conv.links[i]);
    // }

    let out = &mut std::io::stdout();
    let mut input = String::new();

    loop {
        // println!("{:#?}", conv.vis);
        let state = conv.current;

        write!(out, "- ")?;
        if let Some(phrase) = conv.response() {
            write!(out, "{phrase:?}")?;
        }

        write!(out, " [{state} -> ")?;
        match &conv.links[state] {
            Some(next) => writeln!(out, "{next}]")?,
            None => writeln!(out, "End]")?,
        }

        let opt_indices = conv.options_indices();

        for (i, (phrase, opt)) in conv.options().zip(opt_indices).enumerate() {
            write!(out, "({i}) > {phrase:?} [{opt} -> ")?;
            match &conv.links[opt] {
                Some(next) => writeln!(out, "{next}]")?,
                None => writeln!(out, "End]")?,
            }
        }

        out.flush()?;
        let next = loop {
            input.clear();
            std::io::stdin().read_line(&mut input)?;
            utils::trim(&mut input);

            if let Ok(opt_idx) = input.parse::<usize>() {
                break conv.next_state_from_option(opt_idx);
            } else if input.is_empty() {
                break conv.next_state();
            }
        };

        if next.is_some() {
            writeln!(out)?;
        } else {
            break;
        }
    }

    Ok(())
}

// TODO: tell when import is unused
const TEST_CONVO: &str = "
# nice

@import test
@import test2

@as suka
    - 23 -> 24, which is a jump to `start`
    @jump test.Test # 24 -> 0

@as start
- lasdkfj @{ aboba suka blyad idi nahuy } 1 -> 16 @{ aboba salupa }
@jump start

- @{kringe} 1 -> 16
> 2 -> 3
    - 3 -> 16
    > 4 -> 5
        - 5 -> 16
    > 6 -> 7
        - 7 -> 10
        > 8 -> 10
        > 9 -> 10

        - 10 -> 16
        > 11 -> 16
> 12 -> 13
    - 13 -> 14
    - 14 -> 16
    > 15 -> 16


- 16 -> 17

- 17 -> 18
    - 18 -> 20
    > 19 -> 20

- 20 -> 21 which is a jump to a test.mur file -> 22
@jump test.Test # 23 -> 24 which is in test.mur file
";

fn test(_: &mut usize, _args: &[String]) -> ReturnValue {
    ().into()
}

fn main() {
    let Ok(convo) =
        Conversation::from_source(TEST_CONVO, "main", [("aboba", test as _)].into(), 0usize)
        // Conversation::load("test2.mur", [("aboba", test as _)].into(), 0usize)
    else {
        return;
    };

    run(convo).unwrap();
}
