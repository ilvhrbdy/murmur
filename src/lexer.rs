use std::collections::HashMap;
use std::fmt;
use std::iter::Peekable;
use std::path::Path;

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

#[derive(Default, Debug, Clone)]
struct State {
    response_phrases: Vec<String>,
    options: Vec<YourOption>,
    next_state: StateRef,
}

#[derive(Default, Debug, Clone)]
struct YourOption {
    text: String,
    next_state: StateRef,
}

impl State {
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

pub struct Conversation {
    indentation: Indentation,
    states: HashMap<StateId, State>,
    labels_map: HashMap<String, StateId>,
}

impl TryFrom<&Path> for Conversation {
    type Error = std::io::Error;

    fn try_from(p: &Path) -> Result<Self, Self::Error> {
        let src = std::fs::read_to_string(p)?;
        Ok(Self::from(src.as_str()))
    }
}

impl From<&str> for Conversation {
    fn from(src: &str) -> Self {
        let tokens = tokenize(src);
        // println!("{:#?}", tokens);
        // println!("--------------------------");
        let (indentation, ops) = parse_tokens_into_operations(tokens);
        // println!("{:?}\n{:#?}", indentation, ops);
        let (unlinked_states, labels_map) = parse_operations_into_unlinked_states(ops);
        let states = link_conversation_states(unlinked_states);
        // println!("--------------------------");
        // println!("{:#?}", unlinked_states);

        Self {
            indentation,
            states,
            labels_map,
        }
    }
}

impl fmt::Display for Conversation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut sorted = self.states.iter().collect::<Vec<_>>();
        sorted.sort_by(|(id, _), (oid, _)| id.idx.cmp(&oid.idx));

        let make_indent = |depth: u32| {
            let mut it = String::new();
            if depth == 0 {
                return it;
            }

            let (ch, n) = match self.indentation {
                Indentation::Spaces(n) => (' ', n * depth),
                Indentation::Tabs(n) => ('\t', n * depth),
            };

            it.reserve(n as _);
            for _ in 0..n {
                it.push(ch);
            }

            it
        };

        macro_rules! find_lbl_by_id {
            ($id:ident) => {
                self.labels_map
                    .iter()
                    .find_map(|(l, i)| (i == $id).then_some(l.as_str()))
            };
        }

        for (id, state) in sorted {
            let indent = make_indent(id.depth);
            let label = find_lbl_by_id!(id).unwrap_or("");
            writeln!(f, "{indent}({label}) [{id}] => [{}] ", state.next_state)?;

            for rp in &state.response_phrases {
                writeln!(f, "{indent}- {rp:?}")?;
            }

            for o in &state.options {
                let next_state_label = match &o.next_state {
                    StateRef::Label(name) => name,
                    StateRef::Id(ns_id) => find_lbl_by_id!(ns_id).unwrap_or(""),
                    _ => "",
                };
                writeln!(f, "{indent}> {:?} => ({next_state_label})", o.text,)?;
            }

            writeln!(f)?;
        }

        Ok(())
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
struct Token<'s> {
    kind: TokenKind<'s>,
    loc: Loc,
    indentation: Indentation,
}

#[derive(Debug)]
enum Intrinsic<'s> {
    Label(&'s str),
    Jump(&'s str),
}

fn collect_intrinsic<'s>(
    src: &'s str,
    stream: &mut Peekable<impl Iterator<Item = (usize, char, Loc)>>,
) -> Option<Intrinsic<'s>> {
    match collect_word(src, stream)? {
        "lbl" => collect_word(src, stream).map(Intrinsic::Label),
        "jmp" => collect_word(src, stream).map(Intrinsic::Jump),
        word => panic!("unknown intrinsic: {word}"),
    }
}

fn collect_word<'s>(
    src: &'s str,
    stream: &mut Peekable<impl Iterator<Item = (usize, char, Loc)>>,
) -> Option<&'s str> {
    let (word_start, _, _) = stream.peek().copied()?;
    let mut word_end: usize = word_start;

    for (idx, ch, _loc) in stream {
        if ch.is_whitespace() {
            break;
        }
        word_end = idx;
    }

    src.get(word_start..=word_end).map(str::trim_start)
}

fn collect_phrase<'s>(
    src: &'s str,
    stream: &mut Peekable<impl Iterator<Item = (usize, char, Loc)>>,
) -> Option<&'s str> {
    let (phrase_start, _, _) = stream.peek().copied()?;
    let mut phrase_end: usize = phrase_start;

    // NOTE: End is not trimmed, because the phrases may be combined in the future: Response/Option + PhraseContinuation
    //       So the result Option or Response in the state will most likely contains a newline, which will be removed on parsing stage
    for (idx, ch, _loc) in stream {
        phrase_end = idx;
        if ch == '\n' {
            break;
        }
    }

    src.get(phrase_start..=phrase_end).map(str::trim_start)
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
        indentation,
    })
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

#[derive(Debug)]
enum ParseOp<'s> {
    DefState {
        // strong is defined by two newlines
        // strong state that contains only options will not be merged with previous on the same depth:
        // - state 1
        //      - state 2
        //
        //
        // > state 3, defined by two newlines
        //
        // but:
        //
        // - state 1
        //      - state 2
        // > opt of state 1
        strong: bool,
        id: StateId,
    },
    PushResponse(String),
    PushOption(YourOption),
    ApplyIntrinsic(Intrinsic<'s>),
}

fn parse_tokens_into_operations<'s>(tokens: Vec<Token<'s>>) -> (Indentation, Vec<ParseOp<'s>>) {
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
    let mut state_id = StateId::default();
    let mut ops = vec![ParseOp::DefState {
        strong: true,
        id: state_id,
    }];

    macro_rules! last_mut_op {
        () => {{
            ops.iter_mut()
                .rev()
                .find(|o| !matches!(o, ParseOp::ApplyIntrinsic(_)))
        }};
    }

    macro_rules! ops_push_def_state {
        (strong) => {
            ops_push_def_state!(true)
        };
        () => {
            ops_push_def_state!(false)
        };
        ($kind:expr) => {
            // never define state twice
            if !matches!(last_mut_op!(), Some(ParseOp::DefState { .. })) {
                state_id.idx += 1;
                ops.push(ParseOp::DefState {
                    id: state_id,
                    strong: $kind,
                });
            }
        };
    }

    // check if token have different indentation than current one, and if so define a next state
    macro_rules! process_indent {
        ($token:ident) => {{
            if single_indent.is_none() && !$token.indentation.is_empty() {
                single_indent = Some($token.indentation);
            }

            let current_depth = state_id.depth;
            if let Some(single) = single_indent {
                state_id.depth = try_get_indent_depth($token.indentation, single, $token.loc);
            }

            if current_depth != state_id.depth {
                // modify State op, pushed by SpaceLine token, which doesn't have indentation info
                if let Some(ParseOp::DefState { id, .. }) = last_mut_op!() {
                    id.depth = state_id.depth;
                } else {
                    ops_push_def_state!();
                }
            }
        }};
    }

    let mut tokens = tokens.into_iter().peekable();
    while let Some(token) = tokens.next() {
        match token.kind {
            TokenKind::Intrinsic(int) => {
                ops.push(ParseOp::ApplyIntrinsic(int));
            }
            TokenKind::Response(phrase) => {
                if !matches!(last_mut_op!(), Some(ParseOp::PushResponse(_))) {
                    ops_push_def_state!();
                }
                process_indent!(token);
                ops.push(ParseOp::PushResponse(phrase.to_string()));
            }
            TokenKind::Option(phrase) => {
                process_indent!(token);
                ops.push(ParseOp::PushOption(YourOption {
                    text: phrase.to_string(),
                    next_state: StateRef::default(),
                }));
            }
            TokenKind::PhraseContinuation(phrase) => match last_mut_op!() {
                Some(ParseOp::PushResponse(ph)) => ph.push_str(phrase),
                Some(ParseOp::PushOption(o)) => o.text.push_str(phrase),
                _ => panic!("lonely lost phrase is not allowed: {phrase}"),
            },
            TokenKind::SpaceLine => {
                if tokens
                    .next_if(|t| matches!(t.kind, TokenKind::SpaceLine))
                    .is_some()
                {
                    ops_push_def_state!(strong);
                }
            }
        }
    }

    (single_indent.unwrap_or_else(Indentation::empty), ops)
}

fn parse_operations_into_unlinked_states<'s>(
    ops: Vec<ParseOp<'s>>,
) -> (Vec<(StateId, State)>, HashMap<String, StateId>) {
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

    let mut ops = ops.into_iter().peekable();
    let mut chunks = Vec::new();
    let mut int_stack = Vec::new();
    let mut labels_map = HashMap::<String, StateId>::new();

    while let Some(op) = ops.next() {
        match op {
            ParseOp::ApplyIntrinsic(int) => {
                int_stack.push(int);
                while let Some(ParseOp::ApplyIntrinsic(i)) =
                    ops.next_if(|o| matches!(o, ParseOp::ApplyIntrinsic(_)))
                {
                    int_stack.push(i);
                }
            }
            ParseOp::DefState { id, strong } => {
                for int in int_stack.drain(..) {
                    match int {
                        Intrinsic::Label(lbl) => {
                            if lbl.is_empty() {
                                panic!("empty label");
                            }

                            // TODO: normal errors
                            assert!(
                                labels_map.insert(lbl.to_string(), id).is_none(),
                                "duplicate label {lbl:?}"
                            );
                        }
                        Intrinsic::Jump(_) => panic!("jump is only allowed for the options"),
                    }
                }
                chunks.push((id, strong, State::default()));
            }
            ParseOp::PushResponse(phrase) => {
                if !int_stack.is_empty() {
                    panic!("intrinsics is not allowed in the context of response");
                }

                chunks.last_mut().unwrap().2.response_phrases.push(phrase)
            }
            ParseOp::PushOption(mut opt) => {
                for int in int_stack.drain(..) {
                    match int {
                        Intrinsic::Label(_) => panic!("can't label option for now"),
                        Intrinsic::Jump(lbl) => opt.next_state = StateRef::Label(lbl.to_string()),
                    }
                }

                chunks.last_mut().unwrap().2.options.push(opt);
            }
        }
    }

    // merge chunks of the same state
    chunks.sort_by(|(id, ..), (other_id, ..)| id.depth.cmp(&other_id.depth));
    let mut states = Vec::new();
    let mut chunks = chunks.into_iter().peekable();

    while let Some((id, _, mut chunk)) = chunks.next() {
        while let Some((.., ochunk)) = chunks.next_if(|(oi, o_strong, os)| {
            !o_strong && oi.depth == id.depth && os.response_phrases.is_empty()
        }) {
            chunk.merge_with(ochunk);
        }

        // trim newlines at the end of the phrases that we left on tokenization
        for phrase in &mut chunk.response_phrases {
            trim_end(phrase);
        }

        for op in &mut chunk.options {
            trim_end(&mut op.text);
        }

        states.push((id, chunk));
    }

    (states, labels_map)
}

// linking means each state and option knows what is the next state
fn link_conversation_states(states: Vec<(StateId, State)>) -> HashMap<StateId, State> {
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
        for opt in state.options.iter_mut() {
            if matches!(opt.next_state, StateRef::Label(_)) {
                continue;
            }

            let mut ns_id = id;
            ns_id.idx += 1;

            // check if option have a child state
            ns_id.depth += 1;
            if linked_states.contains_key(&ns_id) {
                opt.next_state = StateRef::Id(ns_id);
                continue;
            }

            // sub depth until a valid next state's id found
            while let Some(new_depth) = ns_id.depth.checked_sub(1) {
                ns_id.depth = new_depth;

                if linked_states.contains_key(&ns_id) {
                    opt.next_state = StateRef::Id(ns_id);
                    break;
                }
            }
        }

        let s = linked_states.get_mut(&id).unwrap();
        s.options = state.options;
        s.next_state = StateRef::FromOption;
    }

    linked_states
}
