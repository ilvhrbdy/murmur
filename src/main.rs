mod lexer;
mod utils;

use lexer::{Condition, Context, Function, Item, ItemVIsibility, Phrase};
use std::collections::HashMap;
use std::path::Path;
// TODO: vec of errors (just strings) instead of printing to stdout
// TODO: be able to laod multiple sources from files or memory and compile as different sources, just because it is cool
// TODO: snapshots of the conversation with/without external state
// TODO: when reporting errors use full path
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

// TODO: `#def <temlate name> [$args] <block>` macro and expanding it with
//      # <module>.<template> for expanding macro from other module
//      # .<tempate> for current module
// same for interpolation #{ .<template> }
// so it expands either as text or code depending on the context of expansion
// #def string Aboba
// or
// #def custom_jump $name #jump $name
// or
// #def block $opt
//      - aboba
//      > $opt
// TODO: #if #elif #else
// TODO: write tests, please?
// TODO: normal error messages

pub enum Error {
    ReadingInputFile,
    ParsingSource,
}

pub enum FunctionMapError {
    FunctionExist,
    DiffersReturnType,
}

#[derive(Default)]
pub struct FunctionMap<ExternalState> {
    pub(crate) indices: HashMap<&'static str, usize>,
    pub(crate) funcs: Vec<Function<ExternalState>>,
}

impl<ExternalState> FunctionMap<ExternalState> {
    pub fn replace<T>(
        &mut self,
        name: &'static str,
        f: impl lexer::IntoFunction<ExternalState, T>,
    ) -> Result<(), FunctionMapError> {
        let Some(existing_ftype) = &self.indices.get(name).map(|idx| &self.funcs[*idx]) else {
            return self.register(name, f)
        };

        let ftype = f.into_function();

        if let (Function::Bool(_), Function::Bool(_))
            | (Function::Nothing(_), Function::Nothing(_))
            | (Function::String(_), Function::String(_))  = (&ftype, existing_ftype) {
                self.register::<T>(name, ftype)
        } else {
            Err(FunctionMapError::DiffersReturnType)
        }
    }
    pub fn register<T>(
        &mut self,
        name: &'static str,
        f: impl lexer::IntoFunction<ExternalState, T>,
    ) -> Result<(), FunctionMapError> {
        if lexer::FunctionKind::BUILTINS_STR.contains(&name) || self.indices.contains_key(&name) {
            return Err(FunctionMapError::FunctionExist);
        }

        self.indices.insert(name, self.funcs.len());
        self.funcs.push(f.into_function());

        Ok(())
    }
}

pub struct State<'s> {
    items: &'s [Item],
    choices: &'s [usize],
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
        self.choices.iter().map(|&ch| {
            let Item::Choice { phrase, .. } = &self.items[ch] else {
                unreachable!()
            };

            let (Phrase::Static(buffer) | Phrase::Dynamic { buffer, .. }) = &phrase;

            buffer.as_str()
        })
    }
}

pub struct Conversation<'a, ExternalState> {
    funcs_map: &'a FunctionMap<ExternalState>,

    choices_to_display: Vec<usize>,

    vis: Vec<ItemVIsibility>,
    items: Vec<Item>,
    links: Vec<Option<usize>>,

    current: Option<usize>,
    next: Option<usize>,

    choice: Option<usize>,
}

impl<'a, ExternalState> Conversation<'a, ExternalState> {
    pub fn compile_from_source_as(
        module_name: impl AsRef<str>,
        source: impl AsRef<str>,
        funcs_map: &'a FunctionMap<ExternalState>,
    ) -> Result<Conversation<'a, ExternalState>, Error> {
        let module_name = module_name.as_ref();
        let Ok(tokens) = lexer::tokenize(module_name, source.as_ref()) else {
            return Err(Error::ParsingSource);
        };

        let Ok((items, vis, links)) =
            lexer::parse_tokens_into_items(module_name, tokens, funcs_map)
        else {
            return Err(Error::ParsingSource);
        };

        Ok(Conversation {
            funcs_map,
            choices_to_display: Vec::new(),
            vis,
            current: None,
            choice: None,
            next: (!items.is_empty()).then_some(0),
            items,
            links,
        })
    }

    pub fn compile(
        path: impl AsRef<Path>,
        funcs_map: &'a FunctionMap<ExternalState>,
    ) -> Result<Conversation<'a, ExternalState>, Error> {
        let path = path.as_ref();
        let file_name = path.file_name().unwrap().to_str().unwrap();
        let module_name = file_name.strip_suffix(".mur").unwrap_or(file_name);
        let src = utils::read_mur_file(path).map_err(|e| {
            eprintln!("failed to load file: {e}");
            Error::ReadingInputFile
        })?;

        Self::compile_from_source_as(module_name, src, funcs_map)
    }

    pub fn next_state(&mut self, external_state: &mut ExternalState) -> bool {
        if let Some(choice_idx) = self.choice.take() {
            self.next = self.links[self.choices_to_display[choice_idx]];
        }

        self.current = loop {
            let Some(current) = self.next else {
                break None;
            };

            let visible = {
                let vis = &mut self.vis[current];
                !matches!(vis.condition, Condition::AlwaysHidden) || vis.manual
            };

            if !visible {
                self.next = self.links[current];
                continue;
            } else if let Condition::Check(..) = &self.vis[current].condition {
                self.next = self.next_state_from_condition_chain(current, external_state);
                continue;
            }

            let Some(item) = self.items.get_mut(current) else {
                break None;
            };

            match item {
                Item::Response { .. } => {
                    self.prepare_state(current, external_state);
                    self.next = self.links[current];
                    break Some(current);
                }
                Item::Block {
                    next,
                    guarded_choices,
                    ..
                } => {
                    assert!(guarded_choices.is_none());
                    self.next = *next;
                }
                Item::End => {
                    self.next = None;
                    break None;
                }
                Item::Show { targets, once } => {
                    for item in targets {
                        self.vis[*item].manual = true;
                    }

                    self.next = self.links[current];

                    if *once {
                        self.vis[current].condition = Condition::AlwaysHidden;
                    }
                }
                Item::Hide { targets, once } => {
                    for item in targets {
                        self.vis[*item].manual = false;
                    }

                    self.next = self.links[current];

                    if *once {
                        self.vis[current].condition = Condition::AlwaysHidden;
                    }
                }
                Item::Jump { target, once } => {
                    self.next = Some(*target);

                    if *once {
                        self.vis[current].condition = Condition::AlwaysHidden;
                    }
                }
                Item::FunctionCall { func, func_data } => {
                    self.funcs_map.funcs[*func].call_drop(func_data.as_context(external_state));
                    self.next = self.links[current];

                    if func_data.once {
                        self.vis[current].condition = Condition::AlwaysHidden;
                    }
                }
                Item::Choice { .. } => unreachable!(),
            }
        };

        self.current.is_some()
    }

    pub fn select_choice(&mut self, choice: usize) -> bool {
        if self.choices_to_display.get(choice).is_some() {
            self.choice = Some(choice);
            return true;
        }

        true
    }

    pub fn current_state<'s>(&'s self) -> Option<State<'s>> {
        self.current.map(|current| State {
            items: &self.items,
            choices: &self.choices_to_display,
            current,
        })
    }

    fn next_state_from_condition_chain(
        &mut self,
        chain_start: usize,
        external_state: &mut ExternalState,
    ) -> Option<usize> {
        let mut current_check = Some(chain_start);

        macro_rules! return_next {
            ($next:expr) => {{
                let next_state = $next?;
                return if let Item::Block { next, .. } = &self.items[next_state] {
                    *next
                } else {
                    $next
                };
            }};
        }

        while let Some(check) = current_check.take() {
            let this_vis = &mut self.vis[check];

            let (passed, once, fallback) = if let Condition::Check(
                func,
                func_data,
                fallback_condition,
            ) = &this_vis.condition
            {
                if !this_vis.manual {
                    current_check = *fallback_condition;
                    continue;
                }

                let Function::Bool(check) = &self.funcs_map.funcs[*func] else {
                    unreachable!();
                };

                (
                    check(func_data.as_context(external_state)),
                    func_data.once,
                    *fallback_condition,
                )
            } else if let Condition::AlwaysHidden = &this_vis.condition {
                break;
            } else {
                if this_vis.manual {
                    return_next!(Some(check));
                }

                break;
            };

            if once {
                self.vis[check].condition = if passed {
                    Condition::AlwaysShown
                } else {
                    Condition::AlwaysHidden
                };
            }

            if passed {
                return_next!(Some(check));
            } else {
                current_check = fallback;
            }
        }

        return_next!(self.links[current_check?])
    }

    fn prepare_state(&mut self, state: usize, external_state: &mut ExternalState) {
        let Item::Response { choices, .. } = &self.items[state] else {
            unreachable!();
        };

        self.choices_to_display.clear();
        update_choices_list_from(
            choices,
            external_state,
            &mut self.choices_to_display,
            &mut self.vis,
            &self.items,
            &self.funcs_map.funcs,
        );

        for &item in std::iter::once(&state).chain(self.choices_to_display.iter()) {
            let (Item::Choice { phrase } | Item::Response { phrase, .. }) = &mut self.items[item]
            else {
                unreachable!();
            };

            phrase.update(&self.funcs_map.funcs, external_state);
        }
    }
}

// function is recursive and the borrow checker makes me nervous
fn update_choices_list_from<State>(
    choices: &[usize],
    state: &mut State,
    choices_buff: &mut Vec<usize>,
    vis: &mut [ItemVIsibility],
    items: &[Item],
    funcs: &[Function<State>],
) {
    'choices_loop: for &choice in choices {
        let mut current_check = Some(choice);

        while let Some(check) = current_check.take() {
            let this_vis = &mut vis[check];

            macro_rules! push_choices {
                () => {{
                    match &items[check] {
                        Item::Choice { .. } => choices_buff.push(check),
                        Item::Block {
                            guarded_choices, ..
                        } => update_choices_list_from(
                            guarded_choices.as_ref().unwrap(),
                            state,
                            choices_buff,
                            vis,
                            items,
                            funcs,
                        ),
                        _ => unreachable!(),
                    }
                }};
            }

            let (passed, once, fallback) = if let Condition::Check(
                func,
                func_data,
                fallback_condition,
            ) = &this_vis.condition
            {
                if !this_vis.manual {
                    current_check = *fallback_condition;
                    continue;
                }

                let Function::Bool(f) = &funcs[*func] else {
                    unreachable!();
                };

                (
                    f(func_data.as_context(state)),
                    func_data.once,
                    *fallback_condition,
                )
            } else if let Condition::AlwaysHidden = this_vis.condition {
                continue 'choices_loop;
            } else {
                if this_vis.manual {
                    push_choices!();
                }

                continue 'choices_loop;
            };

            if once {
                this_vis.condition = if passed {
                    Condition::AlwaysShown
                } else {
                    Condition::AlwaysHidden
                };
            }

            if passed {
                push_choices!();
            } else {
                current_check = fallback;
            }
        }
    }
}

fn run<ExternalState>(
    mut conv: Conversation<ExternalState>,
    mut state: ExternalState,
) -> std::io::Result<()> {
    use std::io::Write;

    let out = &mut std::io::stdout();
    let mut input = String::new();

    while conv.next_state(&mut state) {
        let state = conv.current_state().unwrap();
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

        for (i, (phrase, &ch)) in state.choices().zip(state.choices).enumerate() {
            write!(out, "({i}) > {phrase:?} [{ch} -> ")?;
            match &conv.links[ch] {
                Some(next) => writeln!(out, "{next}]")?,
                None => writeln!(out, "End]")?,
            }
        }

        out.flush()?;

        loop {
            input.clear();
            std::io::stdin().read_line(&mut input)?;
            let input = input.trim();

            if input.is_empty() {
                break;
            } else if let Ok(choice_idx) = input.parse::<usize>() {
                if conv.select_choice(choice_idx) {
                    break;
                } else {
                    continue;
                }
            };
        }
    }

    Ok(())
}

// TODO: this shit doesn't work
const TEST_CONVO: &str = r#" 
#hide test aboba test2

- one

#as aboba
#if true 1
> two

#as test
#elif true 2
    > three
    > three2

#as test2
#else
> four
"#;

fn main() {
    let mut fs = FunctionMap::default();

    let _ = fs.register("true", |ctx: Context<usize>| {
        println!("true: {msg}", msg = ctx.args[0]);
        true
    });

    let _ = fs.register("false", |ctx: Context<usize>| {
        println!("false: {msg}", msg = ctx.args[0]);
        false
    });

    let _ = fs.register("print", |ctx: Context<usize>| {
        for arg in ctx.args {
            print!("{arg}");
        }
        println!();
    });

    let _ = fs.register("name", |ctx: Context<usize>| {
        let x = ["one", "two", "three"];
        let r = x[*ctx.state].to_string();
        *ctx.state += 1;
        r
    });

    let Ok(conv) = Conversation::compile_from_source_as("main", TEST_CONVO, &fs) else {
        return;
    };

    // let Ok(_conv2) = Conversation::compile("test.mur", &fs) else {
    //     return;
    // };

    // panic!("{:#?}\n{:#?}\n{:#?}", convo.items, convo.links, convo.vis);
    run(conv, 0usize).unwrap();
}
