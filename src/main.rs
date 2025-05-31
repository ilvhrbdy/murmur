mod lexer;
mod utils;

use lexer::StateItem;
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
// TODO:
// enum Phrase {
//     Static(String),
//     Dynamic {
//         buffer: String,
//         parts: Vec<PhraseParts>
//     }
// }

// enum PhraseParts {
//     Static(String),
//     FromFunction(usize),
// }
// TODO:
//      @ hide [zero or miltiple space separated args]
//      and
//      @ show [zero or miltiple space separated args]
//
//      that will manipulate the vis of the item
//      hide or show without arguments is going to operate on previous item, so something like this is possible:
//          @as lbl
//          - What?
//          > Nothing
//              @hide
//              @jump lbl # jumping back to this state, but option will be excluded
//
// TODO: compile time "once" function execution with `@ !func` or `@{ !func }`
// TODO: @if @elif @else
// TODO: function mapping and calling via `@ func` or @{ func } or comp time version with "!"
// TODO: @{} - text expansion function (either text of labeled item or function)
//      in this case we cannot have labeled item and function with the same name
// TODO: the `main` module is always available even without @import and it kind of counter-intuitive. should i fix that?
// TODO: write tests, please?
// TODO: normal error messages
// TODO: we could warn for some weird behavior like this:
//
//      @as aboba
//      @jump aboba
//
//      which is a valid murmur syntax, but just an infinite loop
//      maybe i need to be strict with this kind of shit?

#[derive(Debug)]
pub struct Conversation {
    vis: Vec<bool>,
    items: Vec<StateItem>,
    links: Vec<Option<usize>>,

    current: usize,
}

impl Conversation {
    pub fn load(path: impl AsRef<Path>) -> Result<Self, ()> {
        let path = path.as_ref();
        let file_name = path.file_name().unwrap().to_str().unwrap();
        let module_name = file_name.strip_suffix(".mur").unwrap_or(file_name);
        let src = utils::read_mur_file(module_name)?;

        Self::from_source(src, module_name)
    }

    pub fn from_source(src: impl AsRef<str>, module_name: &str) -> Result<Self, ()> {
        let tokens = lexer::tokenize(src.as_ref());
        let (items, links) = lexer::parse_tokens_into_items(module_name, tokens)?;

        if items.is_empty() {
            eprintln!("you must define at least one state");
            return Err(());
        }

        let mut it = Self {
            vis: vec![true; items.len()],
            current: 0,
            items,
            links,
        };

        it.get_to_response();

        Ok(it)
    }

    pub fn response(&self) -> Option<&str> {
        if let StateItem::Response { phrase, .. } = &self.items[self.current]
            && !phrase.is_empty()
        {
            Some(phrase.as_str())
        } else {
            None
        }
    }

    pub fn options(&self) -> impl Iterator<Item = &str> {
        self.options_indices().map(|opt| {
            let StateItem::Option { phrase, .. } = &self.items[opt] else {
                unreachable!();
            };

            phrase.as_str()
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
            .expect("valid index of an option"); // TODO: error

        self.current = next;
        self.next_state()?;

        Some(())
    }

    pub fn get_to_response(&mut self) -> Option<()> {
        loop {
            match &self.items[self.current] {
                _ if !self.vis[self.current] => self.current = self.links[self.current]?,
                StateItem::Response { .. } => {
                    // println!("found next response");
                    return Some(());
                }
                StateItem::Show(items) => {
                    for item in items {
                    //     println!("showing item {item}");
                        self.vis[*item] = true;
                    }
                    self.current = self.links[self.current]?;
                }
                StateItem::Hide(items) => {
                    for item in items {
                    //     println!("hiding item {item}");
                        self.vis[*item] = false;
                    }
                    self.current = self.links[self.current]?;
                }
                StateItem::Jump(target) => {
                    // println!("jumping to {target}");
                    self.current = *target;
                }
                StateItem::Option { .. } => unreachable!(),
            }
        }
    }

    fn options_indices(&self) -> impl Iterator<Item = usize> {
        match &self.items[self.current] {
            StateItem::Response { options, .. } => options,
            StateItem::Option { .. } => std::slice::from_ref(&self.current),
            _ => unreachable!(),
        }
        .iter()
        .filter(|opt| self.vis[**opt])
        .copied()
    }
}

fn run(mut conv: Conversation) -> std::io::Result<()> {
    use std::io::Write;

    for i in 0..conv.items.len() {
        println!("{:?} -> {:?}", conv.items[i], conv.links[i]);
    }

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

const TEST_CONVO: &str = "
# nice

@import test

@as suka
    - 23 -> 24, which is a jump to `start`
    @jump start # 24 -> 0

@hide tutu
@jump suka # 0 -> 23

@as start
- 1 -> 16
@as tutu
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

fn main() {
    let Ok(convo) = Conversation::from_source(TEST_CONVO, "main") else {
        return;
    };

    run(convo).unwrap();
}
