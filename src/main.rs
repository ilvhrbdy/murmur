mod lexer;
mod utils;

use lexer::{FuncData, Function, Item, Phrase};
use std::collections::HashMap;
use std::path::Path;

// TODO: when reporting errors use full path
// TODO: checking return type on dummmy is bad idea because the function may return other types in some cases
// TODO: escape `"` for quoted words
// TODO: how to treat this case:
//      @{
//              func "hello
//              world"
//      }
//
//      same rules as with PhraseContinuation? so remove only depth level and keep the rest? but it means i need to collect indentation of the interpolation block
// TODO: need to do something with cases like this:
//
//
//      @as start
//      - start
//          @hide
//
//      @jump start
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
// TODO: @if @elif @else
// TODO: write tests, please?
// TODO: normal error messages

// only to shut the linter, see no point in having error types anyway in our case
pub enum Error {
    ReadingInputFile,
    ParsingSource,
}

pub struct Compiler<ExternalState> {
    funcs_map: HashMap<&'static str, Function<ExternalState>>,
    state: ExternalState,
}

impl<ExternalState> Compiler<ExternalState> {
    pub fn new(state: ExternalState) -> Self {
        Self {
            state,
            funcs_map: HashMap::new(),
        }
    }

    pub fn register_function<T>(
        mut self,
        name: &'static str,
        f: impl lexer::IntoFunction<T, ExternalState>,
    ) -> Self {
        self.funcs_map.insert(name, f.into_function());
        self
    }

    pub fn compile_source_as(
        mut self,
        module_name: impl AsRef<str>,
        source: impl AsRef<str>,
    ) -> Result<Conversation<ExternalState>, Error> {
        let tokens = lexer::tokenize(source.as_ref());
        let Ok((items, links, funcs)) = lexer::parse_tokens_into_items(
            module_name.as_ref(),
            tokens,
            self.funcs_map,
            &mut self.state,
        ) else {
            return Err(Error::ParsingSource);
        };

        let (current, next) = if items.is_empty() {
            (None, None)
        } else {
            (Some(0), Some(0))
        };

        let mut it = Conversation {
            state: self.state,
            funcs,
            vis: vec![true; items.len()],
            current,
            next,
            items,
            links,
        };

        it.next_state();

        Ok(it)
    }

    pub fn compile(self, path: impl AsRef<Path>) -> Result<Conversation<ExternalState>, Error> {
        let path = path.as_ref();
        let file_name = path.file_name().unwrap().to_str().unwrap();
        let module_name = file_name.strip_suffix(".mur").unwrap_or(file_name);
        let src = utils::read_mur_file(path).map_err(|e| {
            eprintln!("failed to load file: {e}");
            Error::ReadingInputFile
        })?;

        self.compile_source_as(src, module_name)
    }
}

pub struct State<'s> {
    items: &'s [Item],
    current: usize,
}

impl<'s> State<'s> {
    pub fn response(&self) -> Option<&str> {
        let Item::Response { phrase, .. } = &self.items[self.current] else {
            unreachable!();
        };

        phrase.as_nonempty_str()
    }

    pub fn options(&self) -> impl Iterator<Item = &str> {
        let Item::Response { options, .. } = &self.items[self.current] else {
            unreachable!();
        };

        options.iter().copied().map(|opt| {
            let Item::Option { phrase, .. } = &self.items[opt] else {
                unreachable!();
            };

            let (Phrase::Static(buffer) | Phrase::Dynamic { buffer, .. }) = &phrase;

            buffer.as_str()
        })
    }

    fn option_indices(&self) -> impl Iterator<Item = usize> {
        let Item::Response { options, .. } = &self.items[self.current] else {
            unreachable!();
        };

        options.iter().copied()
    }
}

pub struct Conversation<ExternalState> {
    state: ExternalState,
    funcs: Vec<Function<ExternalState>>,

    vis: Vec<bool>,
    items: Vec<Item>,
    links: Vec<Option<usize>>,

    current: Option<usize>,
    next: Option<usize>,
}

impl<ExternalState> Conversation<ExternalState> {
    pub fn next_state(&mut self) -> bool {
        self.current = Some(loop {
            let Some(current) = self.next else {
                return false;
            };

            let Some(item) = self.items.get(current) else {
                return false;
            };

            match item {
                _ if !self.vis[current] => self.next = self.links[current],
                Item::End => {
                    self.next = None;
                    return false;
                }
                Item::Response { .. } => {
                    self.update_phrases_in_state(current);
                    self.next = self.links[current];
                    break current;
                }
                Item::Show(items) => {
                    for item in items {
                        self.vis[*item] = true;
                    }
                    self.next = self.links[current];
                }
                Item::Hide(items) => {
                    for item in items {
                        self.vis[*item] = false;
                    }
                    self.next = self.links[current];
                }
                Item::Jump(target) => {
                    self.next = Some(*target);
                }
                Item::FunctionCall { func, func_data } => {
                    self.funcs[*func].call_drop(&mut self.state, func_data);
                    self.next = self.links[current];
                }
                Item::Option { .. } => unreachable!(),
            }
        });

        true
    }

    fn next_state_from_option(&mut self, option_idx: usize) -> bool {
        let Some(current) = self.current else {
            return false;
        };

        let Item::Response { options, .. } = &self.items[current] else {
            unreachable!();
        };

        // TODO: i am trying to avoid any runtime errors from murmur
        //      what should i do here?
        self.next = options
            .iter()
            .filter(|&&opt| self.vis[opt])
            .nth(option_idx)
            .and_then(|&opt| self.links[opt]);


        self.next_state()
    }

    fn current_state<'s>(&'s self) -> Option<State<'s>> {
        self.current.map(|current| State {
            items: &self.items,
            current,
        })
    }

    fn update_phrases_in_state(&mut self, state: usize) {
        let Item::Response { phrase, .. } = &mut self.items[state] else {
            unreachable!();
        };

        phrase.update(&self.funcs, &mut self.state);

        let Some((left, right)) = self.items.split_at_mut_checked(state + 1) else {
            return;
        };

        let Some(Item::Response { options, .. }) = left.last_mut() else {
            unreachable!();
        };

        let offset = state + 1;

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

    let out = &mut std::io::stdout();
    let mut input = String::new();

    while let Some(state) = conv.current_state() {
        let current = state.current;

        write!(out, "- ")?;
        if let Some(phrase) = state.response() {
            write!(out, "{phrase}")?;
        }

        write!(out, " [{current} -> ")?;
        match &conv.links[current] {
            Some(next) => writeln!(out, "{next}]")?,
            None => writeln!(out, "End]")?,
        }

        for (i, (phrase, opt)) in state.options().zip(state.option_indices()).enumerate() {
            write!(out, "({i}) > {phrase:?} [{opt} -> ")?;
            match &conv.links[opt] {
                Some(next) => writeln!(out, "{next}]")?,
                None => writeln!(out, "End]")?,
            }
        }

        out.flush()?;

        let continuing = loop {
            input.clear();
            std::io::stdin().read_line(&mut input)?;
            let input = input.trim();

            if let Ok(opt_idx) = input.parse::<usize>() {
                break conv.next_state_from_option(opt_idx);
            } else if input.is_empty() {
                break conv.next_state();
            };
        };

        if !continuing {
            break;
        }
    }

    Ok(())
}

const TEST_CONVO: &str = "
-       1 -> 16 @{

    chel }

# nice

@import test

@as suka
    - 23 -> 24, which is a jump to `start`
    @jump test.Test # 24 -> 0

@as start
- lasdkfj @{   dura suka } 1 -> 16 @{ boba salupa }

@!as s@u.ka kuka
    -
@as suka
- ....


# @if test
# @jump bubu

-       1 -> 16 @{

    chel }
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
@ jump test.Test # 23 -> 24 which is in test.mur file
";

const TEST_CONVO2: &str = r#"
- I am tired to make something funny
  \        < this shit is not trimmed after '\'
  # Hello I am a comment in the middle of the phrase for some reason!!?!?
  \    #   < look this is escaped and not treated as comment 0_0
  And I am just a casual new line without indentation..
- suka
> one
    @ aboba
> two
- @ "print hello" "#;

fn test(_: &mut usize, func: &FuncData) -> String {
    if func.is_comptime {
        println!("calling function once");
    }

    "hello".into()
}

fn main() {
    let Ok(convo) = Compiler::new(0usize)
        .register_function("test suka", test)
        .register_function("print hello", |_: &mut usize, d: &FuncData| {
            println!("hello {}", d.is_comptime)
        })
        .register_function("aboba", |_: &mut usize, d: &FuncData| {
            println!("aboba {}", d.is_comptime)
        })
        .compile_source_as("main", TEST_CONVO2)
    else {
        return;
    };
    println!("{:#?}", convo.links);
    run(convo).unwrap();
}
