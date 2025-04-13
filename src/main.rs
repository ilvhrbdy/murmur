use std::collections::HashMap;
use std::iter::Peekable;

const TEST_CONVO: &str = "
- suka #00 blyad
    - Yeah. #11
    - wowawaaw #13
    > blyad
        - blyad reply
- Hello #14
";

#[derive(Default, Clone, Debug)]
struct YourOption {
    text: String,
    next_state: StateRef,
}

#[derive(Hash, Eq, PartialEq, Debug, Clone, Default)]
struct StateId {
    idx: u32,
    depth: u32,
}

#[derive(Default, Debug, Clone)]
enum StateRef {
    Label(String),
    Id(StateId),
    #[default]
    None, 
}

#[derive(Default, Debug, Clone)]
struct State {
    response_phrases: Vec<String>,
    options: Vec<YourOption>,
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

    fn as_str(&self) -> &str {
        match self {
            Indentation::Spaces(_) => "spaces",
            Indentation::Tabs(_) => "tabs",
        }
    }

    fn try_get_indent_depth(&self, single: Indentation, loc: Loc) -> u32 {
        if self.is_empty() {
            return 0;
        }

        let (got_amount, single_amount, indent) = match (self, single) {
            (Indentation::Spaces(_), Indentation::Tabs(_)) => {
                panic!("got spaces instead of tabs at {loc:?}")
            }
            (Indentation::Tabs(_), Indentation::Spaces(_)) => {
                panic!("got tabs instead of spaces at {loc:?}")
            }
            _ => (self.get_amount(), single.get_amount(), single.as_str()),
        };

        if got_amount % single_amount != 0 {
            panic!(
                "inconsistent amount of {indent} at {loc:?}: expected {single_amount} as a single, but got {got_amount} total"
            );
        }

        got_amount / single_amount
    }
}

#[derive(Debug)]
enum TokenKind<'s> {
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
    let (idx, ch, loc) = stream.next()?;

    let kind = match ch {
        '>' => TokenKind::Option(collect_phrase(idx + 1, src, stream)?),
        '-' => TokenKind::Response(collect_phrase(idx + 1, src, stream)?),
        '\n' => TokenKind::SpaceLine,
        _ => TokenKind::PhraseContinuation(collect_phrase(idx, src, stream)?),
    };

    Some(Token {
        kind,
        loc,
        indentation_loc,
        indentation,
    })
}

fn collect_phrase<'s>(
    from_idx: usize,
    src: &'s str,
    stream: &mut Peekable<impl Iterator<Item = (usize, char, Loc)>>,
) -> Option<&'s str> {
    let phrase_start: usize = from_idx;
    let mut phrase_end: usize = from_idx;

    for (idx, ch, _loc) in stream {
        phrase_end = idx;
        if ch == '\n' {
            break;
        }
    }

    src.get(phrase_start..=phrase_end)
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
fn parse_states(tokens: &[Token]) -> Vec<(StateId, State)> {
    enum Collecting {
        Responses,
        Options,
    }

    let mut single_indent: Option<Indentation> = None;
    let mut current_depth = 0;

    let mut token_is_on_other_depth = |token: &Token, state_id: &mut StateId| -> bool {
        if single_indent.is_none() && !token.indentation.is_empty() {
            let _ = single_indent.insert(token.indentation);
        }

        if let Some(single) = single_indent {
            let new_depth = token
                .indentation
                .try_get_indent_depth(single, token.indentation_loc);

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

    macro_rules! push_state {
        ($( $cf:ident )?) => {{
            chunks.push((state_id.clone(), chunk.clone()));
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
        match token.kind {
            TokenKind::SpaceLine => {}
            TokenKind::PhraseContinuation(phrase) => match collecting {
                Collecting::Responses => {
                    chunk.response_phrases.last_mut().unwrap().push_str(phrase)
                }
                Collecting::Options => chunk.options.last_mut().unwrap().text.push_str(phrase),
            },
            _ if token_is_on_other_depth(token, &mut state_id) => {
                push_state!(continue);
            }
            TokenKind::Response(phrase) => match collecting {
                Collecting::Responses => chunk.response_phrases.push(phrase.to_owned()),
                Collecting::Options => push_state!(continue),
            },
            TokenKind::Option(phrase) => match collecting {
                Collecting::Responses => {
                    collecting = Collecting::Options;
                    continue;
                }
                Collecting::Options => chunk.options.push(YourOption {
                    text: phrase.to_owned(),
                    next_state: StateRef::None,
                }),
            },
        }

        let _ = token_stream.next();
    }

    if !chunk.is_empty() {
        push_state!();
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

    // rearrange back to previous order and update indices
    states.sort_by(|(id, _), (other_id, _)| id.idx.cmp(&other_id.idx));
    for (i, (id, _)) in states.iter_mut().enumerate() {
        id.idx = i as _;
    }

    states
}

fn link_states(states: Vec<(StateId,State)>) -> HashMap<StateId, State> {
    let mut hsh_states = HashMap::from_iter(states.clone());

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
            let mut ns_id = id.clone();
            ns_id.idx += 1;

            // check if have a child state
            ns_id.depth += 1;
            if hsh_states.contains_key(&ns_id) {
                hsh_states.get_mut(&id).unwrap().next_state = StateRef::Id(ns_id);
                continue;
            }

            ns_id.depth -= 1;
            // sub depth until a valid next state's id found
            loop {
                if hsh_states.contains_key(&ns_id) {
                    hsh_states.get_mut(&id).unwrap().next_state = StateRef::Id(ns_id);
                    break;
                }

                match ns_id.depth.checked_sub(1) {
                    Some(new_depth) => ns_id.depth = new_depth,
                    None => break,
                }
            }

            continue;
        }

        // if state does have options we need to iterate over them and assign for each one a next state
        for op in state.options.iter_mut() {
            let mut ns_id = id.clone();
            ns_id.idx += 1;

            // check if option have a child state
            ns_id.depth += 1;
            if hsh_states.contains_key(&ns_id) {
                op.next_state = StateRef::Id(ns_id);
                continue;
            }

            ns_id.depth -= 1;
            // sub depth until a valid next state's id found
            loop {
                if hsh_states.contains_key(&ns_id) {
                    op.next_state = StateRef::Id(ns_id);
                    break;
                }

                match ns_id.depth.checked_sub(1) {
                    Some(new_depth) => ns_id.depth = new_depth,
                    None => break,
                }
            }
        }

        hsh_states.get_mut(&id).unwrap().options = state.options;
    }

    hsh_states
}

// fn is_last_state(states: HashMap<>)

fn main() {
    let tokens = tokenize(TEST_CONVO);
    let unlinked_states = parse_states(&tokens);
    let states = link_states(unlinked_states);
    let mut states_vec = states.into_iter().collect::<Vec<_>>();
    states_vec.sort_by(|(id, _), (oid, _)| id.idx.cmp(&oid.idx));

    println!("{:#?}", states_vec);
}
