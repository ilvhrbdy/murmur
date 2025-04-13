use std::collections::HashMap;
use std::fmt;
use std::iter::Peekable;

// TODO: how are we gonna apply intrinsics?
//      for items of the state? for state? which state and whcih items?
//      are indentations for intrinsics matter?
const TEST_CONVO: &str = "
- aboba

- suka #00 blyad
    - Yeah. #11

    @lbl Suka
    - wowawaaw #13
    > blyad
        - blyad reply
- Hello #14
    - suka
";

#[derive(Hash, Eq, PartialEq, Debug, Clone, Copy, Default)]
struct StateId {
    idx: u32,
    depth: u32,
}

impl fmt::Display for StateId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.depth, self.idx)
    }
}

#[derive(Default, Debug, Clone)]
enum StateRef {
    Label(String),
    Id(StateId),
    FromOption,
    #[default]
    End,
}

impl fmt::Display for StateRef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            StateRef::Id(id) => write!(f, "{id}"),
            _ => write!(f, "{self:?}"),
        }
    }
}

struct Conversation {
    indentation: Indentation,
    states: HashMap<StateId, State>,
    // labels: HashMap<&'s str, StateId>,
}

impl fmt::Display for Conversation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut sorted = self.states.iter().collect::<Vec<_>>();
        sorted.sort_by(|(id, _), (oid, _)| id.idx.cmp(&oid.idx));

        let single_indent = match self.indentation {
            Indentation::Spaces(n) => ' '.to_string().repeat(n as _),
            Indentation::Tabs(n) => '\t'.to_string().repeat(n as _),
        };

        for (id, state) in sorted {
            let indent = if id.depth == 0 {
                String::new()
            } else {
                single_indent.repeat(id.depth as _)
            };

            writeln!(f, "{indent}({id}) => ({})", state.next_state)?;

            for rp in &state.response_phrases {
                writeln!(f, "{indent}- {rp:?}")?;
            }

            for o in &state.options {
                writeln!(f, "{indent}> {:?} => ({})", o.text, o.next_state)?;
            }

            writeln!(f)?;
        }

        Ok(())
    }
}

// phrases are heap allocated because they may be multi-line and we need to get read of indentation:
// - some multi-line
//      phrase right here
//      will preserve newlines
//      <- but will remove this indentations
#[derive(Default, Debug, Clone)]
struct State {
    response_phrases: Vec<String>,
    options: Vec<YourOption>,
    // if this is None then get next state from option
    // if no options, so the conversation is finished
    next_state: StateRef,
}

#[derive(Default, Clone, Debug)]
struct YourOption {
    text: String,
    next_state: StateRef,
}

impl State {
    fn is_empty(&self) -> bool {
        self.response_phrases.is_empty() && self.options.is_empty()
    }

    fn merge_with(&mut self, mut other: Self) {
        for p in other.response_phrases.drain(..) {
            self.response_phrases.push(p)
        }

        for o in other.options.drain(..) {
            self.options.push(o)
        }
    }
}

#[derive(Copy, Clone, Debug)]
enum Indentation {
    Spaces(u32),
    Tabs(u32),
}

impl Indentation {
    fn empty() -> Self {
        Indentation::Spaces(0)
    }

    fn is_empty(&self) -> bool {
        self.get_amount() == 0
    }

    fn get_amount(&self) -> u32 {
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

#[derive(Debug)]
enum TokenKind<'s> {
    Intrinsic(Intrinsic<'s>),
    Response(&'s str),           // `-` - npc's response phrase in conversation
    Option(&'s str),             // `>` - player's option in conversation
    PhraseContinuation(&'s str), // phrases on newlines excluding indentation
    SpaceLine,
}

#[derive(Debug)]
struct Token<'s> {
    kind: TokenKind<'s>,
    loc: Loc,
    indentation: Indentation,
    indentation_loc: Loc,
}

#[derive(Copy, Clone, Debug)]
struct Loc {
    line: u32,
    col: u32,
}

#[derive(Debug)]
enum Intrinsic<'s> {
    DefineLabel(&'s str),
}

fn collect_intrinsic<'s>(
    src: &'s str,
    stream: &mut Peekable<impl Iterator<Item = (usize, char, Loc)>>,
) -> Option<Intrinsic<'s>> {
    match collect_word(src, stream)? {
        "lbl" => collect_word(src, stream).map(Intrinsic::DefineLabel),
        _ => panic!("unknown intrinsic"),
    }
}

fn tokenize<'s>(src: &'s str) -> Vec<Token<'s>> {
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

fn next_token<'s>(
    src: &'s str,
    stream: &mut Peekable<impl Iterator<Item = (usize, char, Loc)>>,
) -> Option<Token<'s>> {
    let (indentation_loc, indentation) = collect_indent(stream)?;
    let (_, ch, loc) = stream.peek().copied()?;

    let kind = match ch {
        '@' => {
            let _ = stream.next();
            TokenKind::Intrinsic(collect_intrinsic(src, stream)?)
        }
        '>' => {
            let _ = stream.next();
            TokenKind::Option(collect_phrase(src, stream)?)
        }
        '-' => {
            let _ = stream.next();
            TokenKind::Response(collect_phrase(src, stream)?)
        }
        '\n' => {
            let _ = stream.next();
            TokenKind::SpaceLine
        }
        _ => TokenKind::PhraseContinuation(collect_phrase(src, stream)?),
    };

    Some(Token {
        kind,
        loc,
        indentation_loc,
        indentation,
    })
}

fn collect_word<'s>(
    src: &'s str,
    stream: &mut Peekable<impl Iterator<Item = (usize, char, Loc)>>,
) -> Option<&'s str> {
    while stream.next_if(|t| t.1.is_whitespace()).is_some() {}
    let (word_start, _, _) = stream.peek().copied()?;
    let mut word_end: usize = word_start;

    for (idx, ch, _) in stream {
        if ch.is_whitespace() {
            break;
        }
        word_end = idx;
    }

    src.get(word_start..=word_end)
}

fn collect_phrase<'s>(
    src: &'s str,
    stream: &mut Peekable<impl Iterator<Item = (usize, char, Loc)>>,
) -> Option<&'s str> {
    let (phrase_start, _, _) = stream.peek().copied()?;
    let mut phrase_end: usize = phrase_start;

    for (idx, ch, _loc) in stream {
        phrase_end = idx;
        if ch == '\n' {
            break;
        }
    }

    // NOTE: End is not trimmed, because the phrases may be combined in the future: Response/Option + PhraseContinuation
    //       So the result Option or Response in the state will most likely contains a newline
    Some(src[phrase_start..=phrase_end].trim_start())
}

fn collect_indent(
    stream: &mut Peekable<impl Iterator<Item = (usize, char, Loc)>>,
) -> Option<(Loc, Indentation)> {
    let (_, ch, loc) = stream.peek().copied()?;
    let mut amount = 0;

    match ch {
        ' ' => {
            while stream.next_if(|t| t.1 == ' ').is_some() {
                amount += 1;
            }

            Some((loc, Indentation::Spaces(amount)))
        }
        '\t' => {
            while stream.next_if(|t| t.1 == '\t').is_some() {
                amount += 1;
            }

            Some((loc, Indentation::Tabs(amount)))
        }
        _ => Some((loc, Indentation::empty())),
    }
}

fn parse_unlinked_states(tokens: &[Token]) -> (Indentation, Vec<(StateId, State)>) {
    enum Collecting {
        Responses,
        Options,
    }

    fn try_get_indent_depth(input: Indentation, single: Indentation, loc: Loc) -> u32 {
        if input.is_empty() {
            return 0;
        }

        let (got_amount, single_amount, indent) = match (input, single) {
            (Indentation::Spaces(_), Indentation::Tabs(_)) => {
                panic!("got spaces instead of tabs at {loc:?}")
            }
            (Indentation::Tabs(_), Indentation::Spaces(_)) => {
                panic!("got tabs instead of spaces at {loc:?}")
            }
            _ => (
                input.get_amount(),
                single.get_amount(),
                single.as_str_name(),
            ),
        };

        if got_amount % single_amount != 0 {
            panic!(
                "inconsistent amount of {indent} at {loc:?}: expected {single_amount} as a single, but got {got_amount} total"
            );
        }

        got_amount / single_amount
    }

    let mut single_indent: Option<Indentation> = None;
    let mut current_depth = 0;

    let mut token_is_on_other_depth = |token: &Token, state_id: &mut StateId| -> bool {
        if single_indent.is_none() && !token.indentation.is_empty() {
            let _ = single_indent.insert(token.indentation);
        }

        if let Some(single) = single_indent {
            let new_depth = try_get_indent_depth(token.indentation, single, token.indentation_loc);
            state_id.depth = current_depth;
            current_depth = new_depth;
        }

        current_depth != state_id.depth
    };

    let mut collecting = Collecting::Responses;
    let mut token_stream = tokens.iter().peekable();

    // - chunk 1 => state 1
    //     - chunk 2 => state 2
    //     - chunk 2 => state 2
    //     > chunk 2 => state 2
    //
    //     - chunk 3 => state 3
    //     > chunk 3 => state 3
    // > chunk 4 => state 1
    //
    // - chunk 5 => state 4
    // > chunk 5 => state 4

    let mut state_id = StateId::default();
    let mut chunk = State::default();
    let mut chunks = Vec::<(StateId, State)>::new();

    macro_rules! push_chunk {
        ($( $cf:ident )?) => {{
            chunks.push((state_id, chunk.clone()));
            state_id.idx += 1;
            chunk.response_phrases.clear();
            chunk.options.clear();


            $(
                collecting = Collecting::Responses;
                $cf
            )?
        }};
    }

    while let Some(token) = token_stream.peek() {
        match &token.kind {
            TokenKind::Intrinsic(_int) => {
                // "TODO: apply intrinsic or better store it for later stages of parsing"
            }
            TokenKind::SpaceLine => {
                if let Collecting::Responses = collecting {
                    push_chunk!()
                }
            }
            TokenKind::PhraseContinuation(phrase) => match collecting {
                Collecting::Responses => {
                    chunk.response_phrases.last_mut().unwrap().push_str(phrase)
                }
                Collecting::Options => chunk.options.last_mut().unwrap().text.push_str(phrase),
            },
            _ if token_is_on_other_depth(token, &mut state_id) => {
                push_chunk!(continue);
            }
            TokenKind::Response(phrase) => match collecting {
                Collecting::Responses => chunk.response_phrases.push(phrase.to_string()),
                Collecting::Options => push_chunk!(continue),
            },
            TokenKind::Option(phrase) => match collecting {
                Collecting::Responses => {
                    collecting = Collecting::Options;
                    continue;
                }
                Collecting::Options => chunk.options.push(YourOption {
                    text: phrase.to_string(),
                    next_state: StateRef::End,
                }),
            },
        }

        let _ = token_stream.next();
    }

    if !chunk.is_empty() {
        push_chunk!();
    }

    // merge chunks of the same state
    chunks.sort_by(|(id, _), (other_id, _)| id.depth.cmp(&other_id.depth));
    let mut states = Vec::new();
    let mut chunks = chunks.into_iter().peekable();

    while let Some((id, mut chunk)) = chunks.next() {
        while let Some((_, ochunk)) =
            chunks.next_if(|(oi, os)| oi.depth == id.depth && os.response_phrases.is_empty())
        {
            chunk.merge_with(ochunk);
        }

        states.push((id, chunk));
    }

    // returning unordered states
    (single_indent.unwrap_or_else(Indentation::empty), states)
}

// this sets all 'next_state' fields
fn link_states(states: Vec<(StateId, State)>) -> HashMap<StateId, State> {
    // let mut labels = HashMap::<String, StateId>::new();
    let mut linked_states = HashMap::from_iter(states.clone());

    for (id, mut state) in states {
        // if state doesn't have options:
        //      if have child:
        //          next_state = child
        //
        //      while current_depth state_idx + 1 not a valid state:
        //          depth -= 1
        //
        //      next_state = found state on 'depth' or None
        if state.options.is_empty() {
            let mut ns_id = id;
            ns_id.idx += 1;

            // check if have a child state
            ns_id.depth += 1;
            if linked_states.contains_key(&ns_id) {
                linked_states.get_mut(&id).unwrap().next_state = StateRef::Id(ns_id);
                continue;
            }

            // sub depth until a valid next state's id found
            // *first sub is justified by previous addition to the depth
            while let Some(new_depth) = ns_id.depth.checked_sub(1) {
                ns_id.depth = new_depth;

                if linked_states.contains_key(&ns_id) {
                    linked_states.get_mut(&id).unwrap().next_state = StateRef::Id(ns_id);
                    break;
                }
            }

            continue;
        }

        // if state does have options we need to iterate over them and assign for each one a next state
        for op in state.options.iter_mut() {
            let mut ns_id = id;
            ns_id.idx += 1;

            // check if option have a child state
            ns_id.depth += 1;
            if linked_states.contains_key(&ns_id) {
                op.next_state = StateRef::Id(ns_id);
                continue;
            }

            // sub depth until a valid next state's id found
            while let Some(new_depth) = ns_id.depth.checked_sub(1) {
                ns_id.depth = new_depth;

                if linked_states.contains_key(&ns_id) {
                    op.next_state = StateRef::Id(ns_id);
                    break;
                }
            }
        }

        let s = linked_states.get_mut(&id).unwrap();
        s.options = state.options;
        s.next_state = StateRef::FromOption;
        assert!(!s.options.is_empty());
    }

    linked_states
}

fn main() {
    let tokens = tokenize(TEST_CONVO);
    let (indentation, unlinked_states) = parse_unlinked_states(&tokens);
    let states = link_states(unlinked_states);

    let convo = Conversation {
        indentation,
        states,
    };

    println!("{}", convo);
}
