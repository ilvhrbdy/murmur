use crate::Error;
use std::{
    collections::{HashMap, hash_map::Entry},
    iter::Peekable,
    path::Path,
};

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

impl Default for Indentation {
    fn default() -> Self {
        Self::empty()
    }
}


#[derive(Copy, Clone, Debug)]
struct Loc {
    line: u32,
    col: u32,
}

#[derive(Debug)]
enum TokenKind {
    IntrinsicJump(String),
    IntrinsicLabel(String),
    IntrinsicImport(String),
    Response(String),           // `-` - npc's response phrase in conversation
    Option(String),             // `>` - player's option in conversation
    PhraseContinuation(String), // phrases on newlines excluding indentation
    Comment,
    SpaceLine,
}

#[derive(Debug)]
pub(super) struct Token {
    kind: TokenKind,
    loc: Loc,
    indentation: Indentation,
}

fn collect_intrinsic(
    src: &str,
    stream: &mut Peekable<impl Iterator<Item = (usize, char, Loc)>>,
) -> Option<(Loc, TokenKind)> {
    let (loc, word) = collect_word(src, stream)?;
    // TODO: error on None or empty word
    let int = match word {
        "as" => collect_word(src, stream).map(|(_, w)| TokenKind::IntrinsicLabel(w.into()))?,
        "jump" => collect_word(src, stream).map(|(_, w)| TokenKind::IntrinsicJump(w.into()))?,
        "import" => collect_word(src, stream).map(|(_, w)| TokenKind::IntrinsicImport(w.into()))?,
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

fn next_token(
    src: &str,
    stream: &mut Peekable<impl Iterator<Item = (usize, char, Loc)>>,
) -> Option<Token> {
    let indentation = collect_indent(stream)?;
    let (_, ch, _) = stream.peek()?;

    let (loc, kind) = match ch {
        '#' => {
            let (.., loc) = stream.next()?;
            let _ = collect_phrase(src, stream);
            (loc, TokenKind::Comment)
        }
        '@' => {
            let _ = stream.next();
            collect_intrinsic(src, stream)?
        }
        '>' => {
            let _ = stream.next();
            let (loc, phrase) = collect_phrase(src, stream)?;
            (loc, TokenKind::Option(phrase.into()))
        }
        '-' => {
            let _ = stream.next();
            let (loc, phrase) = collect_phrase(src, stream)?;
            (loc, TokenKind::Response(phrase.into()))
        }
        '\n' => {
            let (.., loc) = stream.next()?;
            (loc, TokenKind::SpaceLine)
        }
        _ => {
            let (loc, phrase) = collect_phrase(src, stream)?;
            (loc, TokenKind::PhraseContinuation(phrase.into()))
        }
    };

    Some(Token {
        kind,
        loc,
        indentation,
    })
}

pub(super) fn tokenize(src: &str) -> Vec<Token> {
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

pub(super) fn read_mur_file(path: impl AsRef<Path>) -> Result<String, Error> {
    let src = std::fs::read_to_string(path).map_err(Error::Io)?;

    if src.trim().is_empty() {
        return Err(Error::ZeroStatesDefined);
    }

    Ok(src)
}

#[derive(Debug)]
pub(super) enum StateItem {
    Response { phrase: String, options: Vec<usize> },
    Option(String),
    Jump,
}

#[derive(Default)]
struct ParserData {
    single_indentation: Indentation,
    labels_map: HashMap<String, usize>,
    label_jumps: HashMap<usize, String>, // to store all jumps before all labels are defined

    modules: HashMap<String, bool>, // keep track of what is already imported
    load_queue: Vec<(String, Vec<Token>)>,

    // Result
    errors: Vec<Error>,
    items: Vec<StateItem>,
    links: Vec<Option<usize>>,
}

pub(super) fn parse_tokens_into_items(
    main_module: &str,
    tokens: Vec<Token>,
) -> Result<(Vec<StateItem>, Vec<Option<usize>>), Error> {
    let mut parser = ParserData::default();

    // TODO: avoid second allocation for the name
    parser.modules.insert(main_module.into(), true);
    parser.load_queue.push((main_module.into(), tokens));

    while !parser.load_queue.is_empty() {
        parse_module(&mut parser);
    }

    if !parser.errors.is_empty() {
        return Err(Error::Multiple(parser.errors));
    }

    for (item, label) in parser.label_jumps {
        if let Some(idx) = parser.labels_map.get(&label) {
            parser.links[item] = Some(*idx);
        } else {
            panic!("label doesn't exist: {label}");
        }
    }

    for (module, is_used) in parser.modules {
        if !is_used {
            println!("[Warn] `{module}` was not loaded");
        }
    }

    Ok((parser.items, parser.links))
}

fn parse_module(
    ParserData {
        load_queue,
        modules,
        single_indentation,
        labels_map,
        label_jumps,
        items,
        links,
        errors,
    }: &mut ParserData,
) {
    let link_offset = items.len();
    let Some((module_name, tokens)) = load_queue.pop() else {
        return;
    };

    let make_label = |m: &str, l: &str| m.to_owned() + "." + l;

    let mut get_depth = |indent: Indentation, loc: Loc| -> u32 {
        if single_indentation.is_empty() {
            if indent.is_empty() {
                return 0;
            }

            *single_indentation = indent;
        }

        get_depth_from_indent(
            indent,
            *single_indentation,
            Loc {
                col: 1,
                line: loc.line,
            },
        )
    };

    let mut local_imports = HashMap::<String, bool>::new();
    let mut module_items = Vec::<StateItem>::new();
    let mut item_depths = Vec::<u32>::new();
    let mut label_to_assign = None::<(Indentation, String)>;

    for token in tokens {
        macro_rules! push_state_item {
            ($item:expr) => {{
                // TODO: check indentation
                if let Some((_indent, label)) = label_to_assign.take() {
                    let _ = labels_map.insert(label, module_items.len() + link_offset);
                }

                module_items.push($item);
                item_depths.push(get_depth(token.indentation, token.loc));
            }};
        }

        match token.kind {
            TokenKind::PhraseContinuation(text) => match module_items.last_mut() {
                Some(StateItem::Option(phrase) | StateItem::Response { phrase, .. }) => {
                    phrase.push_str(&text);
                }
                Some(StateItem::Jump) | None => panic!("this text must belong to `-` response or `>` option: {:?}", token.loc)
            },
            TokenKind::Response(phrase) => push_state_item!(StateItem::Response {
                phrase,
                options: Vec::new(),
            }),
            TokenKind::Option(phrase) => push_state_item!(StateItem::Option(phrase)),
            TokenKind::IntrinsicJump(label) => {
                let mut parts = label.split('.');
                let jump_target = match (parts.next(), parts.next()) {
                    (Some(import_name), Some(_)) => {
                        if let Some(is_used) = local_imports.get_mut(import_name) {
                            *is_used = true;
                        } else {
                            panic!("`{import_name}` is not imported");
                        }

                        let Some(is_loaded) = modules.get_mut(import_name) else {
                            panic!("`{import_name}` is not imported, use @import");
                        };

                        if !*is_loaded {
                            let src = match read_mur_file(format!("{import_name}.mur")) {
                                Ok(s) => s,
                                Err(e) => {
                                    errors.push(e);
                                    continue;
                                }
                            };
                            let import_tokens = tokenize(&src);
                            load_queue.push((import_name.to_string(), import_tokens));
                            *is_loaded = true;
                        }

                        label
                    }
                    (Some(label), None) => make_label(&module_name, label),
                    _ => unreachable!(),
                };

                assert!(
                    label_jumps.insert(module_items.len(), jump_target).is_none(),
                    "it seems like jump got overwritten, is it even possilbe?"
                );

                push_state_item!(StateItem::Jump);
            }
            TokenKind::IntrinsicLabel(label) => {
                if label_to_assign.is_some() {
                    panic!("multiple labels is not allowed");
                }

                if label.contains('.') {
                    panic!("`.` is not allowed in the label name: {label}");
                }

                let name = make_label(&module_name, &label);
                if labels_map.contains_key(&name) {
                    panic!("duplicate label: {label}");
                }

                label_to_assign = Some((token.indentation, name));
            }
            TokenKind::IntrinsicImport(module) => {
                if module == module_name {
                    println!("[Warn] self import is weird, not an error but useless");
                } else if local_imports.insert(module.clone(), false).is_some() {
                    println!("[Warn] duplicate import `{module}` at {:?}", token.loc);
                } else if let Entry::Vacant(e) = modules.entry(module.clone()) {
                    e.insert(false);
                }
            }
            TokenKind::Comment | TokenKind::SpaceLine => {}
        }
    }

    let module_links = link_state_items(link_offset, &mut module_items, &item_depths);

    for (import, is_used) in local_imports {
        if !is_used {
            println!("[Warn] `{import}` was imported but never referenced");
        }
    }


    links.extend(module_links);
    items.extend(module_items);
}

fn link_state_items(
    link_offset: usize,
    items: &mut [StateItem],
    item_depths: &[u32],
) -> Vec<Option<usize>> {
    let mut links = vec![None::<usize>; items.len()];
    let mut states = Vec::<usize>::new();
    let mut parents = HashMap::<usize, usize>::new();
    let mut sibling_idx = HashMap::<usize, u32>::new();
    let mut last_modified_state = None::<usize>;

    // first iteration is linking items of the same branch and defining sibling-parent-child relationship
    for this_item in 0..items.len() {
        match items[this_item] {
            StateItem::Jump | StateItem::Response { .. } => {
                update_family(
                    this_item,
                    last_modified_state,
                    &mut parents,
                    &mut sibling_idx,
                    item_depths,
                );

                if let Some(next @ None) = this_item.checked_sub(1).map(|i| &mut links[i]) {
                    *next = Some(this_item + link_offset);
                }

                states.push(this_item);

                last_modified_state = Some(this_item);
            }
            // only Option cannot define states by itself
            StateItem::Option { .. } => {
                let Some(state) = (0..items[..this_item].len())
                    .rev()
                    .find_map(|i| {
                        if item_depths[i] < item_depths[this_item] {
                            panic!("couldn't find an option handler for {this_item} on this depth");
                        } else if let StateItem::Option(_) = &items[i] {
                            None
                        } else if item_depths[i] == item_depths[this_item] {
                            Some(i)
                        } else {
                            None
                        }
                    })
                else {
                    panic!("could find a state definition for this option");
                };

                let options = match &mut items[state] {
                    StateItem::Response { options, .. } => options,
                    StateItem::Jump => panic!(
                        "I cannot assign options for `@jump` intrinsic, you must define response with `-` that will own those options"
                    ),
                    StateItem::Option(_) => unreachable!(),
                };

                options.push(this_item);

                last_modified_state = Some(state);
            }
        }
    }

    let mut opts = Vec::<usize>::new();

    // second is finisher: links end of the branches to it's parent's next sibling
    for (i, state) in states.iter().copied().enumerate() {
        let mut next_item = None::<Option<usize>>;

        match &mut items[state] {
            StateItem::Response { options, phrase } => {
                trim_end(phrase);
                opts.extend_from_slice(options);
            }
            StateItem::Jump => continue,
            StateItem::Option(_) => unreachable!(),
        };

        for item in std::iter::once(state).chain(opts.drain(..)) {
            let (StateItem::Response { phrase, .. } | StateItem::Option(phrase)) = &mut items[item]
            else {
                unreachable!();
            };
            trim_end(phrase);

            let next = &mut links[item];

            if next.is_some() {
                continue;
            }

            if next_item.is_none() {
                next_item = Some(
                    find_next_state_long_long_way(
                        &states[i..],
                        &parents,
                        &sibling_idx,
                        item_depths,
                    )
                    .map(|item_idx| item_idx + link_offset),
                );
            }

            *next = next_item.unwrap_or(None);
        }
    }

    links
}

fn get_depth_from_indent(indentation: Indentation, single_indent: Indentation, loc: Loc) -> u32 {
    let (got_amount, single_amount, indent) =
        if let i @ ((Indentation::Spaces(_), Indentation::Tabs(_))
        | (Indentation::Tabs(_), Indentation::Spaces(_))) = (indentation, &single_indent)
        {
            panic!(
                "got {} instead of {} at {loc:?}",
                i.0.as_str_name(),
                i.1.as_str_name()
            )
        } else {
            (
                indentation.get_amount(),
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
