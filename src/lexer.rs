use std::collections::HashMap;
use std::fmt;
use std::iter::Peekable;

fn trim_end(s: &mut String) {
    if let Some(end_idx) = s.rfind(|c: char| !c.is_whitespace()) {
        s.truncate(end_idx + 1);
    } else {
        s.clear();
    }

    // trim start
    // if let Some(start_idx) = s.find(|c: char| !c.is_whitespace()) {
    //     let _ = s.drain(..start_idx);
    // }
}

#[derive(Hash, Eq, PartialEq, Debug, Clone, Copy, Default)]
pub struct StateId {
    pub(super) depth: u32,
    sibling_counter: u32,
    pub(super) idx: usize,
}

impl fmt::Display for StateId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}:{}", self.depth, self.sibling_counter, self.idx)
    }
}

#[derive(Default, Debug, Clone)]
pub enum StateRef {
    Label(String),
    Id(StateId),
    #[default]
    EndConvo,
}

#[derive(Default, Debug, Clone)]
pub struct State {
    pub response: Option<String>,
    pub options: Vec<YourOption>,
    pub next: StateRef,
}

#[derive(Default, Debug, Clone)]
pub struct YourOption {
    pub text: String,
    pub next: StateRef,
}

#[derive(Copy, Clone, Debug)]
pub enum Indentation {
    Spaces(u32),
    Tabs(u32),
}

impl Indentation {
    pub fn empty() -> Self {
        Indentation::Spaces(0)
    }

    pub fn is_empty(&self) -> bool {
        self.get_amount() == 0
    }

    pub fn get_amount(&self) -> u32 {
        let (Indentation::Tabs(n) | Indentation::Spaces(n)) = self;
        *n
    }

    fn as_str_name(&self) -> &str {
        match self {
            Indentation::Spaces(_) => "spaces",
            Indentation::Tabs(_) => "tabs",
        }
    }
}

#[derive(Copy, Clone, Debug)]
struct Loc {
    line: u32,
    col: u32,
}

#[derive(Debug)]
enum TokenKind<'s> {
    Intrinsic(Intrinsic<'s>),
    Response(&'s str),           // `-` - npc's response phrase in conversation
    Option(&'s str),             // `>` - player's option in conversation
    PhraseContinuation(&'s str), // phrases on newlines excluding indentation
    SpaceLine,
}

#[derive(Debug)]
pub(super) struct Token<'s> {
    kind: TokenKind<'s>,
    loc: Loc,
    indentation: Indentation,
}

#[derive(Debug)]
pub(super) enum Intrinsic<'s> {
    Label(&'s str),
    Jump(&'s str),
}

fn collect_intrinsic<'s>(
    src: &'s str,
    stream: &mut Peekable<impl Iterator<Item = (usize, char, Loc)>>,
) -> Option<(Loc, Intrinsic<'s>)> {
    let (loc, word) = collect_word(src, stream)?;
    let int = match word {
        "lbl" => collect_word(src, stream).map(|(_, w)| Intrinsic::Label(w))?,
        "jmp" => collect_word(src, stream).map(|(_, w)| Intrinsic::Jump(w))?,
        word => panic!("unknown intrinsic: {word} at {loc:?}"),
    };

    Some((loc, int))
}

fn collect_word<'s>(
    src: &'s str,
    stream: &mut Peekable<impl Iterator<Item = (usize, char, Loc)>>,
) -> Option<(Loc, &'s str)> {
    while let Some((_, ch, loc)) = stream.next_if(|(_, ch, _)| ch.is_whitespace()) {
        if ch == '\n' {
            return Some((loc, ""));
        }
    }

    let (word_start, _, loc) = stream.peek().copied()?;
    let mut word_end: usize = word_start;

    for (idx, ch, _) in stream {
        if ch.is_whitespace() {
            break;
        }
        word_end = idx;
    }

    src.get(word_start..=word_end).map(|s| (loc, s))
}

fn collect_phrase<'s>(
    src: &'s str,
    stream: &mut Peekable<impl Iterator<Item = (usize, char, Loc)>>,
) -> Option<(Loc, &'s str)> {
    while let Some((_, ch, loc)) = stream.next_if(|(_, ch, _)| ch.is_whitespace()) {
        if ch == '\n' {
            return Some((loc, ""));
        }
    }

    let (phrase_start, _, loc) = stream.peek().copied()?;
    let mut phrase_end: usize = phrase_start;

    // NOTE: End is not trimmed, because the phrases may be combined in the future: Response/Option + PhraseContinuation
    //       So the result Option or Response in the state will most likely contains a newline, which will be removed on parsing stage
    for (idx, ch, _) in stream {
        phrase_end = idx;
        if ch == '\n' {
            break;
        }
    }

    src.get(phrase_start..=phrase_end).map(|s| (loc, s))
}

fn collect_indent(
    stream: &mut Peekable<impl Iterator<Item = (usize, char, Loc)>>,
) -> Option<Indentation> {
    let mut amount = 0;
    let (_, ch, _) = stream.peek()?;

    match ch {
        ' ' => {
            while stream.next_if(|t| t.1 == ' ').is_some() {
                amount += 1;
            }

            Some(Indentation::Spaces(amount))
        }
        '\t' => {
            while stream.next_if(|t| t.1 == '\t').is_some() {
                amount += 1;
            }

            Some(Indentation::Tabs(amount))
        }
        _ => Some(Indentation::empty()),
    }
}

fn next_token<'s>(
    src: &'s str,
    stream: &mut Peekable<impl Iterator<Item = (usize, char, Loc)>>,
) -> Option<Token<'s>> {
    let indentation = collect_indent(stream)?;
    let (_, ch, _) = stream.peek()?;

    let (loc, kind) = match ch {
        '@' => {
            let _ = stream.next();
            let (loc, int) = collect_intrinsic(src, stream)?;
            (loc, TokenKind::Intrinsic(int))
        }
        '>' => {
            let _ = stream.next();
            let (loc, phrase) = collect_phrase(src, stream)?;
            (loc, TokenKind::Option(phrase))
        }
        '-' => {
            let _ = stream.next();
            let (loc, phrase) = collect_phrase(src, stream)?;
            (loc, TokenKind::Response(phrase))
        }
        '\n' => {
            let (.., loc) = stream.next()?;
            (loc, TokenKind::SpaceLine)
        }
        _ => {
            let (loc, phrase) = collect_phrase(src, stream)?;
            (loc, TokenKind::PhraseContinuation(phrase))
        }
    };

    Some(Token {
        kind,
        loc,
        indentation,
    })
}

pub(super) fn tokenize<'s>(src: &'s str) -> Vec<Token<'s>> {
    let mut tokens = Vec::new();
    let mut loc = Loc { line: 1, col: 1 };

    let mut stream = src
        .chars()
        .enumerate()
        .map(|(idx, ch)| {
            let it = (idx, ch, loc);

            if ch == '\n' {
                loc.line += 1;
                loc.col = 1;
            } else {
                loc.col += 1;
            }

            it
        })
        .skip_while(|t| t.1 == '\n') // trim start
        .peekable();

    while let Some(t) = next_token(src, &mut stream) {
        tokens.push(t);
    }

    tokens
}

pub(super) fn parse_tokens_into_conversation<'s>(
    tokens: Vec<Token<'s>>,
    module_name: &str, // will be used to generate labels for the current conversation file (module)
) -> crate::Conversation {
    let make_label = |name: &str| module_name.to_owned() + "." + name;

    let mut single_indent: Indentation = Indentation::empty();
    let mut get_depth_from_token = |token: &Token| -> u32 {
        if single_indent.is_empty() {
            if token.indentation.is_empty() {
                return 0;
            }

            single_indent = token.indentation;
        }

        let loc = Loc {
            col: 1,
            ..token.loc
        };

        let (got_amount, single_amount, indent) = match (token.indentation, &single_indent) {
            (Indentation::Spaces(_), Indentation::Tabs(_)) => {
                panic!("got spaces instead of tabs at {loc:?}")
            }
            (Indentation::Tabs(_), Indentation::Spaces(_)) => {
                panic!("got tabs instead of spaces at {loc:?}")
            }
            _ => (
                token.indentation.get_amount(),
                single_indent.get_amount(),
                single_indent.as_str_name(),
            ),
        };

        if got_amount % single_amount != 0 {
            panic!(
                "inconsistent amount of {indent} at {loc:?}: expected {single_amount} as a single_indent, but got {got_amount} total"
            );
        }

        got_amount / single_amount
    };

    let mut state_id = StateId::default();
    let mut int_stack = Vec::new();
    let mut labels_map = HashMap::<String, StateId>::new();
    let mut last_modification: Option<(StateId, TokenKind)> = None;

    let mut ids = Vec::<StateId>::new();
    let mut parents = Vec::<Option<StateId>>::new();
    let mut states = Vec::<State>::new();

    for token in tokens {
        match token.kind {
            TokenKind::Intrinsic(int) => int_stack.push(int),
            TokenKind::Response(phrase) => {
                state_id.depth = get_depth_from_token(&token);
                state_id.sibling_counter = 0;

                let parent = ids
                    .iter()
                    .rev()
                    .find_map(|i| (i.depth < state_id.depth).then_some(*i));

                let mut state = State {
                    response: Some(phrase.to_string()),
                    ..Default::default()
                };

                for int in int_stack.drain(..) {
                    match int {
                        Intrinsic::Label(lbl) => {
                            if labels_map.insert(make_label(lbl), state_id).is_some() {
                                panic!("duplicate label");
                            }
                        }
                        Intrinsic::Jump(lbl) => state.next = StateRef::Label(make_label(lbl)),
                    }
                }

                if let Some((mut last_id, last_state, last_token)) = last_modification
                    .and_then(|(id, tk)| states.get_mut(id.idx).map(|s| (id, s, tk)))
                {
                    // if last state is a sibling
                    if last_id.depth == state_id.depth {
                        state_id.sibling_counter = last_id.sibling_counter + 1;

                    // if last state is a child of some parent state
                    } else if last_id.depth > state_id.depth {
                        // go back along the parents tree until we found a parent with the same depth
                        while last_id.depth != state_id.depth {
                            let Some(pid) = parents[last_id.idx] else {
                                break;
                            };

                            last_id = pid;
                        }
                        state_id.sibling_counter = last_id.sibling_counter + 1;
                    }

                    if let TokenKind::Option(_) = &last_token {
                        last_state.options.last_mut().unwrap().next = StateRef::Id(state_id);
                    } else if matches!(last_state.next, StateRef::EndConvo) {
                        last_state.next = StateRef::Id(state_id);
                    }
                }

                ids.push(state_id);
                parents.push(parent);
                states.push(state);
                last_modification = Some((state_id, token.kind));

                state_id.idx += 1;
            }
            TokenKind::Option(phrase) => {
                let option_depth = get_depth_from_token(&token);
                let Some((last_id, last_state)) = states
                    .iter_mut()
                    .enumerate()
                    .rev()
                    .find_map(|(i, s)| (ids[i].depth == option_depth).then_some((ids[i], s)))
                else {
                    panic!(
                        "could find related response for this option on this depth of indentation {state_id}: {phrase:?}"
                    );
                };

                let mut opt = YourOption {
                    text: phrase.to_string(),
                    next: StateRef::default(),
                };

                for int in int_stack.drain(..) {
                    match int {
                        Intrinsic::Label(_) => panic!("labeling options is not allowed"),
                        Intrinsic::Jump(lbl) => opt.next = StateRef::Label(make_label(lbl)),
                    }
                }

                last_state.options.push(opt);
                last_modification = Some((last_id, token.kind));
            }
            TokenKind::PhraseContinuation(phrase) => match last_modification {
                Some((_, TokenKind::Response(_))) => states
                    .last_mut()
                    .unwrap() // we know that the last state is defined by this token
                    .response
                    .as_mut()
                    .unwrap() // state's response is always defined as Some
                    .push_str(phrase),
                Some((_, TokenKind::Option(_))) => states
                    .last_mut()
                    .unwrap() // if we managed to push Option token, so at least one state exist
                    .options
                    .last_mut()
                    .unwrap()
                    .text
                    .push_str(phrase),
                None => panic!("lonely phrase is not allowed"),
                _ => unreachable!(),
            },
            TokenKind::SpaceLine => {}
        }
    }

    // trim newlines that we kept on tokenization stage
    for state in &mut states {
        if let Some(text) = state.response.as_mut().filter(|t| !t.trim().is_empty()) {
            trim_end(text);
        } else {
            state.response = None;
        }

        for opt in &mut state.options {
            if opt.text.trim().is_empty() {
                panic!("empty option is not allowed");
            }

            trim_end(&mut opt.text);
        }
    }

    // link states that didn't got linked on the stage of parsing tokens
    for (state, id) in states.iter_mut().zip(ids.iter()) {
        if !matches!(state.next, StateRef::EndConvo) {
            continue;
        }

        let find_next_state = || {
            let mut pid = parents[id.idx];
            while let Some(mut parent) = pid {
                parent.sibling_counter += 1;
                if let Some(found) = ids[id.idx + 1..].iter().find(|i| {
                    i.depth == parent.depth && i.sibling_counter == parent.sibling_counter
                }) {
                    return Some(*found);
                }
                pid = parents[parent.idx];
            }

            None
        };

        let Some(ns_id) = find_next_state() else {
            continue;
        };

        state.next = StateRef::Id(ns_id);

        if !state
            .options
            .iter()
            .any(|o| matches!(o.next, StateRef::EndConvo))
        {
            continue;
        }

        for opt in &mut state.options {
            if matches!(opt.next, StateRef::EndConvo) {
                opt.next = StateRef::Id(ns_id);
            }
        }
    }

    crate::Conversation {
        indentation: single_indent,
        states: HashMap::from_iter(ids.into_iter().zip(states)),
        labels_map,
    }
}
