use std::collections::HashMap;
use std::fmt;
use std::iter::Peekable;

// TODO: change all panics to normal error messages
// FIXME: opt after newline doesn't define new state
// FIXME: indented intrinsic defines new state

const TEST_CONVO: &str = "
@lbl ABOBA
- aslkdj

    @lbl SUKA
    - hello
    > opt

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
enum StateRef<'s> {
    Label(&'s str),
    Id(StateId),
    FromOption,
    #[default]
    End,
}

impl fmt::Display for StateRef<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            StateRef::Id(id) => write!(f, "{id}"),
            _ => write!(f, "{self:?}"),
        }
    }
}

struct Conversation<'s> {
    indentation: Indentation,
    states: HashMap<StateId, State<'s>>,
    labels_map: LabelsMap<'s>,
}

impl fmt::Display for Conversation<'_> {
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

        for (id, state) in sorted {
            let indent = make_indent(id.depth);
            let label = self.labels_map.ids_to_labels.get(id).unwrap_or(&"");
            writeln!(f, "{indent}({label}) [{id}] => [{}] ", state.next_state)?;

            for rp in &state.response_phrases {
                writeln!(f, "{indent}- {rp:?}")?;
            }

            for o in &state.options {
                writeln!(f, "{indent}> {:?} => [{}]", o.text, o.next_state)?;
            }

            writeln!(f)?;
        }

        Ok(())
    }
}

// phrases are heap allocated because they may be multi-line and we need to get rid of indentation:
// - some multi-line
//      phrase right here
//      will preserve newlines
//      <- but will remove this indentations
#[derive(Default, Debug, Clone)]
struct State<'s> {
    response_phrases: Vec<String>,
    options: Vec<YourOption<'s>>,
    next_state: StateRef<'s>,
}

#[derive(Default, Clone, Debug)]
struct YourOption<'s> {
    text: String,
    next_state: StateRef<'s>,
}

impl<'s> State<'s> {
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
    Label(&'s str),
}

fn collect_intrinsic<'s>(
    src: &'s str,
    stream: &mut Peekable<impl Iterator<Item = (usize, char, Loc)>>,
) -> Option<Intrinsic<'s>> {
    match collect_word(src, stream)? {
        "lbl" => collect_word(src, stream).map(Intrinsic::Label),
        word => panic!("unknown intrinsic: {word}"),
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
            // let ph = collect_phrase(src, stream)?;
            // println!("--- ph = {ph:?}");
            TokenKind::Response(collect_phrase(src, stream)?)
            // TokenKind::Response(ph)
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

#[derive(Debug)]
enum ParseOp {
    State(StateId),
    Response(String),
    Option(String),
}

fn parse_tokens_into_operations<'s>(
    tokens: Vec<Token<'s>>,
) -> (Indentation, Vec<ParseOp>, Vec<(Intrinsic<'s>, StateId)>) {
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
    let mut ops = Vec::new();
    let mut int_stack = Vec::new();

    macro_rules! op_def_state {
        () => {{
            // NOTE: first state is defined by default in the `parse_operations_into_unlinked_states`
            state_id.idx += 1;
            ops.push(ParseOp::State(state_id));
        }};
    }

    for token in tokens {
        macro_rules! process_indent {
            () => {{
                if single_indent.is_none() && !token.indentation.is_empty() {
                    let _ = single_indent.insert(token.indentation);
                }

                let current_depth = state_id.depth;
                if let Some(single) = single_indent {
                    state_id.depth = try_get_indent_depth(token.indentation, single, token.indentation_loc);
                }

                if current_depth != state_id.depth {
                    // modify State op, pushed by SpaceLine token, which doesn't have indentation info
                    // TODO: skip intrinsics and find
                    if let Some(ParseOp::State(id)) = ops.last_mut() {
                        id.depth = state_id.depth;
                    } else {
                        op_def_state!();
                    }
                }
            }}
        }

        match token.kind {
            TokenKind::SpaceLine => {
                if !matches!(ops.last(), Some(ParseOp::State(_))) {
                    op_def_state!();
                }
            }
            TokenKind::Intrinsic(int) => {
                process_indent!();
                int_stack.push((int, state_id));
            }
            TokenKind::PhraseContinuation(phrase) => match ops.last_mut() {
                Some(ParseOp::Response(ph)) => ph.push_str(phrase),
                Some(ParseOp::Option(o)) => o.push_str(phrase),
                _ => panic!("lonely lost phrase is not allowed: {phrase}"),
            },
            TokenKind::Response(phrase) => {
                process_indent!();
                ops.push(ParseOp::Response(phrase.to_string()));
            }
            TokenKind::Option(phrase) => {
                process_indent!();
                ops.push(ParseOp::Option(phrase.to_string()));
            }
        }
    }

    (
        single_indent.unwrap_or_else(Indentation::empty),
        ops,
        int_stack,
    )
}

struct LabelsMap<'s> {
    labels_to_ids: HashMap<&'s str, StateId>,
    ids_to_labels: HashMap<StateId, &'s str>,
}

fn parse_operations_into_unlinked_states<'s>(ops: Vec<ParseOp>) -> Vec<(StateId, State<'s>)> {
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

    let mut chunks = vec![(StateId::default(), State::default())];

    for op in ops {
        match op {
            ParseOp::State(id) => chunks.push((id, State::default())),
            ParseOp::Response(phrase) => {
                chunks.last_mut().unwrap().1.response_phrases.push(phrase)
            }
            ParseOp::Option(text) => chunks.last_mut().unwrap().1.options.push(YourOption {
                text,
                next_state: StateRef::default(),
            }),
        }
    }

    // merge chunks of the same state
    chunks.sort_by(|(id, _), (other_id, _)| id.depth.cmp(&other_id.depth));
    let mut states = Vec::new();
    let mut chunks = chunks.into_iter().peekable();

    while let Some((id, mut chunk)) = chunks.next() {
        // trim newlines at the end of the phrases that we left on tokenization
        for phrase in &mut chunk.response_phrases {
            trim_end(phrase);
        }

        for op in &mut chunk.options {
            trim_end(&mut op.text);
        }

        while let Some((_, ochunk)) =
            chunks.next_if(|(oi, os)| oi.depth == id.depth && os.response_phrases.is_empty())
        {
            chunk.merge_with(ochunk);
        }

        states.push((id, chunk));
    }

    states
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
    }

    linked_states
}

fn apply_intrinsics<'s>(
    intrinsics_call_stack: Vec<(Intrinsic<'s>, StateId)>,
    _states: &mut HashMap<StateId, State>,
) -> LabelsMap<'s> {
    let mut labels_to_ids = HashMap::<&str, StateId>::new();
    let mut ids_to_labels = HashMap::<StateId, &str>::new();

    for (int, id) in intrinsics_call_stack {
        match int {
            Intrinsic::Label(lbl) => {
                if lbl.is_empty() {
                    panic!("empty label");
                }

                // TODO: normal errors
                assert!(
                    labels_to_ids.insert(lbl, id).is_none(),
                    "duplicate label {lbl:?}"
                );
                assert!(
                    ids_to_labels.insert(id, lbl).is_none(),
                    "only one label allowed"
                );
            }
        }
    }

    LabelsMap {
        labels_to_ids,
        ids_to_labels,
    }
}

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

fn main() {
    let tokens = tokenize(TEST_CONVO);
    // println!("{:#?}", tokens);
    // println!("--------------------------");
    let (indentation, ops, intrinsics_call_stack) = parse_tokens_into_operations(tokens);
    // println!("{:?}\n{:#?}", indentation, ops);
    let unlinked_states = parse_operations_into_unlinked_states(ops);
    let mut states = link_conversation_states(unlinked_states);
    let labels_map = apply_intrinsics(intrinsics_call_stack, &mut states);
    // println!("--------------------------");
    // println!("{:#?}", unlinked_states);

    let convo = Conversation {
        indentation,
        states,
        labels_map,
    };

    println!("--------------------------");
    println!("{}", convo);
    println!("--------------------------");
    println!("{}", TEST_CONVO);
}
