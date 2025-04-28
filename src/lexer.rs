use std::collections::HashMap;
use std::iter::Peekable;

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

    pub fn get_mut_amount(&mut self) -> &mut u32 {
        let (Indentation::Tabs(n) | Indentation::Spaces(n)) = self;
        n
    }

    fn as_str_name(&self) -> &str {
        match self {
            Indentation::Spaces(n) => {
                if *n != 0 && *n > 1 {
                    "spaces"
                } else {
                    "space"
                }
            }
            Indentation::Tabs(n) => {
                if *n != 0 && *n > 1 {
                    "tabs"
                } else {
                    "tab"
                }
            }
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
    Comment,
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
    Import(&'s str),
    Label(&'s str),
    Jump(&'s str),
}

fn collect_intrinsic<'s>(
    src: &'s str,
    stream: &mut Peekable<impl Iterator<Item = (usize, char, Loc)>>,
) -> Option<(Loc, Intrinsic<'s>)> {
    let (loc, word) = collect_word(src, stream)?;
    let int = match word {
        "as" => collect_word(src, stream).map(|(_, w)| Intrinsic::Label(w))?,
        "to" => collect_word(src, stream).map(|(_, w)| Intrinsic::Jump(w))?,
        "import" => collect_word(src, stream).map(|(_, w)| Intrinsic::Import(w))?,
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

// yeah this one kind of similar with previous, but who cares?
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
    let (_, ch, _) = stream.peek().copied()?;

    let mut indent = match ch {
        ' ' => Indentation::Spaces(0),
        '\t' => Indentation::Tabs(0),
        _ => return Some(Indentation::empty()),
    };

    while stream.next_if(|t| t.1 == ch).is_some() {
        amount += 1;
    }

    *indent.get_mut_amount() = amount;

    Some(indent)
}

fn next_token<'s>(
    src: &'s str,
    stream: &mut Peekable<impl Iterator<Item = (usize, char, Loc)>>,
) -> Option<Token<'s>> {
    let indentation = collect_indent(stream)?;
    let (idx, ch, _) = stream.peek()?;

    let (loc, kind) = match ch {
        '/' if matches!(src.get(*idx..=*idx + 1), Some("//")) => {
            let (.., loc) = stream.next()?;
            let _ = collect_phrase(src, stream);
            (loc, TokenKind::Comment)
        }
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

fn get_depth_from_token(token: &Token, single_indent: &Indentation) -> u32 {
    let loc = Loc {
        col: 1,
        ..token.loc
    };

    let (got_amount, single_amount, indent) =
        if let i @ ((Indentation::Spaces(_), Indentation::Tabs(_))
        | (Indentation::Tabs(_), Indentation::Spaces(_))) = (token.indentation, &single_indent)
        {
            panic!(
                "got {} instead of {} at {loc:?}",
                i.0.as_str_name(),
                i.1.as_str_name()
            )
        } else {
            (
                token.indentation.get_amount(),
                single_indent.get_amount(),
                single_indent.as_str_name(),
            )
        };

    if got_amount % single_amount != 0 {
        panic!(
            "inconsistent amount of {indent} at {loc:?}: expected {single_amount} as a single_indent, but got {got_amount} total"
        );
    }

    got_amount / single_amount
}

#[derive(Debug)]
pub(super) enum StateItem {
    Response { phrase: String, options: Vec<usize> },
    Option(String),
}

pub(super) fn parse_tokens_into_items<'s>(
    tokens: Vec<Token<'s>>,
    module_name: &str, // will be used to generate labels for the current conversation file (module)
) -> (Vec<StateItem>, Vec<Option<usize>>) {
    let make_label = |name: &str| module_name.to_owned() + "." + name;

    let mut single_indent: Indentation = Indentation::empty();
    let mut _get_depth_from_token = |token: &Token| -> u32 {
        if single_indent.is_empty() {
            if token.indentation.is_empty() {
                return 0;
            }

            single_indent = token.indentation;
        }

        get_depth_from_token(token, &single_indent)
    };

    let mut idx = 0;
    let mut int_stack = Vec::new();
    let mut labels_map = HashMap::<String, usize>::new();
    let mut label_jumps = HashMap::<usize, String>::new();
    let mut item_depths = Vec::<u32>::new();
    let mut links = Vec::<Option<usize>>::new();
    let mut items = Vec::<StateItem>::new();

    let mut push_state_item = |item: StateItem,
                               items: &mut Vec<StateItem>,
                               int_stack: &mut Vec<Intrinsic>,
                               token: &Token| {
        for int in int_stack.drain(..) {
            match int {
                Intrinsic::Label(lbl) => {
                    if labels_map.insert(make_label(lbl), idx).is_some() {
                        panic!("duplicate label: {lbl}");
                    }
                }
                Intrinsic::Jump(lbl) => {
                    let _ = label_jumps.insert(idx, make_label(lbl));
                }

                Intrinsic::Import(_module) => {
                    todo!("find a file in a current directory with the name of the module and tokenize it's content")
                }
            }
        }

        items.push(item);
        item_depths.push(_get_depth_from_token(token));
        links.push(None);

        idx += 1;
    };

    for token in tokens {
        match token.kind {
            TokenKind::Intrinsic(int) => int_stack.push(int),
            TokenKind::Option(phrase) => {
                push_state_item(
                    StateItem::Option(phrase.to_owned()),
                    &mut items,
                    &mut int_stack,
                    &token,
                );
            }
            TokenKind::Response(phrase) => push_state_item(
                StateItem::Response {
                    phrase: phrase.to_owned(),
                    options: Vec::new(),
                },
                &mut items,
                &mut int_stack,
                &token,
            ),

            TokenKind::PhraseContinuation(text) => match items.last_mut() {
                Some(StateItem::Option(phrase) | StateItem::Response { phrase, .. }) => {
                    phrase.push_str(text);
                }
                None => panic!("lonely phrase is not allowed"),
            },
            TokenKind::Comment | TokenKind::SpaceLine => {}
        }
    }

    for (item, label) in label_jumps.into_iter() {
        if let Some(idx) = labels_map.get(&label) {
            links[item] = Some(*idx);
        } else {
            panic!("label doesn't exist: {label}");
        }
    }

    link_state_items(&mut items, &mut links, &item_depths);

    (items, links)
}

fn link_state_items(items: &mut [StateItem], links: &mut [Option<usize>], item_depths: &[u32]) {
    let mut states = Vec::<usize>::new();
    let mut parents = HashMap::<usize, usize>::new();
    let mut sibling_idx = HashMap::<usize, u32>::new();
    let mut last_modified_state = None::<usize>;

    for this_item in 0..items.len() {
        match items[this_item] {
            StateItem::Response { .. } => {
                update_family(
                    this_item,
                    last_modified_state,
                    &mut parents,
                    &mut sibling_idx,
                    item_depths,
                );

                if let Some(next @ None) = this_item.checked_sub(1).map(|i| &mut links[i]) {
                    *next = Some(this_item);
                }

                states.push(this_item);

                last_modified_state = Some(this_item);
            }
            StateItem::Option { .. } => {
                let Some(state) = states
                    .iter()
                    .rev()
                    .find(|s| item_depths[**s] == item_depths[this_item])
                    .copied()
                else {
                    panic!("could find a state definition for this option");
                };

                let StateItem::Response { options, .. } = &mut items[state] else {
                    unreachable!();
                };

                options.push(this_item);

                last_modified_state = Some(state);
            }
        }
    }

    let mut opts = Vec::<usize>::new();

    for (i, state) in states.iter().copied().enumerate() {
        let mut next_item = None::<Option<usize>>;

        {
            let StateItem::Response { options, phrase } = &mut items[state] else {
                unreachable!();
            };
            trim_end(phrase);
            opts.extend_from_slice(options);
        }

        for item in std::iter::once(state).chain(opts.drain(..)) {
            let (StateItem::Response { phrase, .. } | StateItem::Option(phrase)) = &mut items[item];
            trim_end(phrase);

            let next = &mut links[item];

            if next.is_some() {
                continue;
            }

            if next_item.is_none() {
                next_item = Some(find_next_state_long_long_way(
                    &states[i..],
                    &parents,
                    &sibling_idx,
                    item_depths,
                ));
            }

            *next = next_item.unwrap_or(None);
        }
    }
}

fn update_family(
    new_state: usize,
    last_modified_state: Option<usize>,
    states_parents: &mut HashMap<usize, usize>,
    sibling_idx: &mut HashMap<usize, u32>,
    item_depths: &[u32],
) {
    let mut parent = None;
    let mut sibling_counter = 0;

    if let Some(last_state) = last_modified_state {
        // if last state is a sibling
        if item_depths[last_state] == item_depths[new_state] {
            sibling_counter = sibling_idx[&last_state] + 1;
            parent = states_parents.get(&last_state).copied();
        // if last state is a parent of this new item
        } else if item_depths[last_state] < item_depths[new_state] {
            parent = Some(last_state);
        // if last item is a child of some parent item
        } else if item_depths[last_state] > item_depths[new_state] {
            let mut last_state_parent = last_state;

            // go back along the states_parents tree until we found one with the same depth
            while item_depths[last_state_parent] != item_depths[new_state] {
                let Some(upper_parent) = states_parents.get(&last_state_parent) else {
                    break;
                };

                last_state_parent = *upper_parent;
            }

            sibling_counter = sibling_idx[&last_state_parent] + 1;
            parent = states_parents.get(&last_state_parent).copied();
        }
    }

    sibling_idx.insert(new_state, sibling_counter);
    if let Some(p) = parent {
        states_parents.insert(new_state, p);
    }
}

fn find_next_state_long_long_way(
    states: &[usize],
    states_parents: &HashMap<usize, usize>,
    sibling_idx: &HashMap<usize, u32>,
    item_depths: &[u32],
) -> Option<usize> {
    let mut current = states.first().copied()?;
    loop {
        'searching: for state in states.iter().skip(1) {
            if item_depths[current] != item_depths[*state] {
                continue 'searching;
            } else if sibling_idx[&current] >= sibling_idx[state] {
                break 'searching;
            } else if sibling_idx[&current] + 1 == sibling_idx[state] {
                return Some(*state);
            }
        }

        let Some(parent) = states_parents.get(&current) else {
            break;
        };

        current = *parent
    }

    None
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
