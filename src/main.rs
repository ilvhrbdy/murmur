mod lexer;
mod utils;

use lexer::{FuncData, Function, Item, Phrase};
use std::collections::HashMap;
use std::path::Path;

// TODO: push phrase continuation on tokenization step, so on parsing i can just throw an error if any PhraseContinuation left
// TODO: rework "isolated blocks" make the not isolated, but create Item::Block that can be hidden along with related items
// TODO: when reporting errors use full path
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
        f: impl lexer::IntoFunction<ExternalState, T>,
    ) -> Self {
        self.funcs_map.insert(name, f.into_function());
        self
    }

    pub fn compile_source_as(
        self,
        module_name: impl AsRef<str>,
        source: impl AsRef<str>,
    ) -> Result<Conversation<ExternalState>, Error> {
        let tokens = lexer::tokenize(source.as_ref());
        let Ok((items, links, funcs)) =
            lexer::parse_tokens_into_items(module_name.as_ref(), tokens, self.funcs_map)
        else {
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

        self.compile_source_as(module_name, src)
    }
}

pub struct State<'s> {
    vis: &'s [bool],
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

    pub fn choices(&self) -> impl Iterator<Item = &str> {
        self.choice_indices().map(|ch| {
            let Item::Choice { phrase, .. } = &self.items[ch] else {
                unreachable!()
            };

            let (Phrase::Static(buffer) | Phrase::Dynamic { buffer, .. }) = &phrase;

            buffer.as_str()
        })
    }

    fn choice_indices(&self) -> impl Iterator<Item = usize> {
        let Item::Response { choices, .. } = &self.items[self.current] else {
            unreachable!();
        };

        choices
            .iter()
            .filter(|&&ch| {
                let Item::Choice { display, .. } = &self.items[ch] else {
                    unreachable!()
                };

                self.vis[ch] && *display
            })
            .copied()
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

            let Some(item) = self.items.get_mut(current) else {
                return false;
            };

            match item {
                _ if !self.vis[current] => self.next = self.links[current],
                Item::Nop => self.next = self.links[current],
                Item::Response { .. } => {
                    self.update_phrases_in_state();
                    self.next = self.links[current];
                    break current;
                }
                Item::If {
                    func,
                    func_data,
                    next,
                    ..
                } => {
                    let Function::Bool(f) = &self.funcs[*func] else {
                        unreachable!();
                    };

                    let passed = f(&mut self.state, func_data);

                    let next = if passed { *next } else { self.links[current] };

                    self.next = next;

                    if func_data.once {
                        *item = Item::Nop;
                        if passed {
                            self.links[current] = next;
                        }
                    }
                }
                Item::End => {
                    self.next = None;
                    return false;
                }
                Item::Show { targets, once } => {
                    for item in targets {
                        self.vis[*item] = true;
                    }

                    self.next = self.links[current];

                    if *once {
                        *item = Item::Nop
                    }
                }
                Item::Hide { targets, once } => {
                    for item in targets {
                        self.vis[*item] = false;
                    }

                    self.next = self.links[current];

                    if *once {
                        *item = Item::Nop
                    }
                }
                Item::Jump { target, once } => {
                    self.next = Some(*target);

                    if *once {
                        *item = Item::Nop
                    }
                }
                Item::FunctionCall { func, func_data } => {
                    self.funcs[*func].call_drop(&mut self.state, func_data);
                    self.next = self.links[current];

                    if func_data.once {
                        *item = Item::Nop
                    }
                }
                Item::Choice { .. } => unreachable!(),
            }
        });

        true
    }

    fn next_state_from_choice(&mut self, option_idx: usize) -> Option<bool> {
        let Some(current) = self.current else {
            return Some(false);
        };

        let Item::Response { choices, .. } = &self.items[current] else {
            unreachable!();
        };

        let choice = choices.iter().filter(|&&ch| self.vis[ch]).nth(option_idx)?;

        self.next = self.links[*choice];

        Some(self.next_state())
    }

    fn current_state<'s>(&'s self) -> Option<State<'s>> {
        self.current.map(|current| State {
            vis: &self.vis,
            items: &self.items,
            current,
        })
    }

    fn update_phrases_in_state(&mut self) {
        let state = self.current.expect("caller to be sure about calling this");

        let Some((left, right)) = self.items.split_at_mut_checked(state + 1) else {
            return;
        };

        let Some(Item::Response { choices, phrase }) = left.last_mut() else {
            unreachable!();
        };

        phrase.update(&self.funcs, &mut self.state);

        let offset = state + 1;
        // TODO: maybe conditions buffer inside of the Conversation
        let mut checked = HashMap::<usize, bool>::new();
        for c in choices.iter() {
            let choice = *c - offset;

            if !self.vis[choice] {
                continue;
            }

            let (
                conds,
                [
                    Item::Choice {
                        conditions,
                        phrase,
                        display,
                    },
                    ..,
                ],
            ) = right.split_at_mut_checked(choice).unwrap()
            else {
                unreachable!();
            };

            // reversing conditions because we pushed them on parsing stage in the wrong order
            let all_passed = conditions.iter().rev().all(|cond_item| {
                let cond_with_offset = cond_item - offset;
                let (passed, once, next) = {
                    let Item::If {
                        func,
                        func_data,
                        next,
                        ..
                    } = &mut conds[cond_with_offset]
                    else {
                        unreachable!();
                    };

                    if let Some(check_result) = checked.get(cond_item) {
                        return *check_result;
                    }

                    let Function::Bool(f) = &self.funcs[*func] else {
                        unreachable!();
                    };

                    let passed = f(&mut self.state, func_data);
                    assert!(checked.insert(*cond_item, passed).is_none());

                    (passed, func_data.once, *next)
                };

                if once {
                    conds[cond_with_offset] = Item::Nop;
                    if passed {
                        self.links[*cond_item] = next;
                    }
                }

                passed
            });

            *display = all_passed;
            if all_passed {
                phrase.update(&self.funcs, &mut self.state);
            }
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

        for (i, (phrase, ch)) in state.choices().zip(state.choice_indices()).enumerate() {
            write!(out, "({i}) > {phrase:?} [{ch} -> ")?;
            match &conv.links[ch] {
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
                break conv.next_state_from_choice(opt_idx).unwrap();
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

const _TEST_CONVO: &str = "
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


# if test
# jump bubu

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

// TODO: location of phrases must point on first non-whitespace
const TEST_CONVO2: &str = r#"

#as test
- test1  #{



name # i don't like your face

}

# !if true "checking"
    - test 2

#jump test
"#;

fn main() {
    let Ok(convo) = Compiler::new(0usize)
        .register_function("true", |_: &mut usize, d: &FuncData| {
            println!("true: {msg}", msg = d.args[0]);
            true
        })
        .register_function("false", |_: &mut usize, d: &FuncData| {
            println!("false: {msg}", msg = d.args[0]);
            false
        })
        .register_function("dbg", |_: &mut usize, d: &FuncData| {
            println!("{}", d.args[0]);
        })
        .register_function("name", |state: &mut usize, _: &FuncData| {
            let x = ["one", "two", "three"];
            let r = x[*state].to_string();
            *state += 1;
            r
        })
        .compile_source_as("main", TEST_CONVO2)
    else {
        return;
    };

    // panic!("{:#?}\n{:#?}", convo.items, convo.links);
    run(convo).unwrap();
}
