use std::{
    collections::{HashMap, hash_map::Entry},
    iter::Peekable,
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
    Function {
        name: String,
        args: Vec<(Loc, String)>,
    },
    Response(String),           // `-` - npc's response phrase in conversation
    Option(String),             // `>` - player's option in conversation
    PhraseContinuation(String), // phrases on newlines excluding indentation
    IsolatedBlock(String),      // constructed on parsing stage (label_depth, label_name)
}

#[derive(Debug)]
pub(super) struct Token {
    kind: TokenKind,
    loc: Loc,
    indentation: Indentation,
}

fn collect_word(
    stream: &mut Peekable<impl Iterator<Item = (usize, char, Loc)>>,
) -> Option<(Loc, String)> {
    let mut word = String::new();
    let mut word_loc = None;

    while let Some((_, ch, _)) = stream.next_if(|(_, ch, _)| ch.is_whitespace()) {
        if ch == '\n' {
            return None;
        }
    }

    while let Some((_, ch, loc)) = stream.next_if(|(_, ch, _)| !ch.is_whitespace()) {
        if word_loc.is_none() {
            word_loc = Some(loc);
        }

        word.push(ch);
    }

    (!word.is_empty()).then(|| (word_loc.unwrap(), word))
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

    loop {
        let Some(indentation) = collect_indent(&mut stream) else {
            break;
        };

        let Some((_, ch, loc)) = stream.peek().copied() else {
            break;
        };

        let token = match ch {
            '#' => {
                let _ = stream.next();
                let _ = collect_phrase(src, &mut stream);
                continue;
            }
            '\n' => {
                let _ = stream.next();
                continue;
            }
            '@' => {
                let _ = stream.next();
                let (fn_loc, name) = collect_word(&mut stream).unwrap_or((loc, String::new()));
                let mut args = Vec::<(Loc, String)>::new();

                let mut collect = true;
                while let Some(arg) = collect_word(&mut stream) {
                    if arg.1.starts_with("#") {
                        collect = false
                    }

                    if collect {
                        args.push(arg);
                    }
                }

                Some((fn_loc, TokenKind::Function { name, args }))
            }
            '>' => {
                let _ = stream.next();
                collect_phrase(src, &mut stream)
                    .map(|(loc, phrase)| (loc, TokenKind::Option(phrase.into())))
            }
            '-' => {
                let _ = stream.next();
                collect_phrase(src, &mut stream)
                    .map(|(loc, phrase)| (loc, TokenKind::Response(phrase.into())))
            }
            _ => collect_phrase(src, &mut stream)
                .map(|(loc, phrase)| (loc, TokenKind::PhraseContinuation(phrase.into()))),
        };

        let Some((loc, kind)) = token else {
            break;
        };

        tokens.push(Token {
            kind,
            loc,
            indentation,
        });
    }

    tokens
}

#[derive(Debug)]
pub(super) enum StateItem {
    Response { phrase: String, options: Vec<usize> },
    Option { handler: usize, phrase: String },
    Jump,
    OptionsBlock(Vec<usize>),
}

#[derive(Default)]
struct ParserData {
    single_indentation: Indentation,
    labels_map: HashMap<String, usize>,
    label_jumps: HashMap<usize, String>, // to store all jumps before all labels are defined

    entry_point: Option<usize>,
    modules: HashMap<String, bool>, // keep track of what is already imported
    load_queue: Vec<(String, HashMap<String, bool>, Vec<Token>)>,

    // Result
    items: Vec<StateItem>,
    links: Vec<Option<usize>>,
}

pub(super) fn parse_tokens_into_items(
    main_module: &str,
    tokens: Vec<Token>,
) -> Result<(Vec<StateItem>, Vec<Option<usize>>), ()> {
    let mut parser = ParserData::default();
    let mut failed = false;

    // TODO: avoid second allocation for the name
    parser.modules.insert(main_module.into(), true);
    parser
        .load_queue
        .push((main_module.into(), HashMap::new(), tokens));

    while !parser.load_queue.is_empty() {
        let Ok(parsed_module_name) = parse_module(&mut parser) else {
            failed = true;
            continue;
        };

        if parsed_module_name == main_module && parser.entry_point.is_none() {
            eprintln!("file doesn't contain any entry point state, conversation cannot be started");
            return Err(());
        }
    }

    if failed {
        return Err(());
    }

    for (jump_item, label) in parser.label_jumps {
        assert!(matches!(&parser.items[jump_item], StateItem::Jump));

        if let Some(target_item) = parser.labels_map.get(&label) {
            parser.links[jump_item] = Some(*target_item);
        } else {
            eprintln!("label doesn't exist: {label}");
            return Err(());
        }
    }

    for (module, is_used) in parser.modules {
        if !is_used {
            eprintln!("[Warn] `{module}` was not loaded");
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
        entry_point,
        items,
        links,
    }: &mut ParserData,
) -> Result<String, ()> {
    let link_offset = items.len();
    let Some((module_name, mut local_imports, tokens)) = load_queue.pop() else {
        unreachable!();
    };

    let make_label = |m: &str, l: &str| m.to_owned() + "." + l;
    // println!("{module_name} {tokens:#?}");

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

    let mut module_items = Vec::<StateItem>::new();
    let mut item_depth = Vec::<u32>::new();
    let mut label_to_assign = None::<(u32, String)>;
    let mut tokens = tokens.into_iter().peekable();

    while let Some(token) = tokens.next() {
        let token_depth = get_depth(token.indentation, token.loc);

        macro_rules! push_state_item {
            () => {{
                if let Some(StateItem::Option { phrase, .. } | StateItem::Response { phrase, .. }) =
                    module_items.last_mut()
                {
                    crate::utils::trim_end(phrase)
                }

                if let Some((_, label)) = label_to_assign.take() {
                    let _ = labels_map.insert(label, module_items.len() + link_offset);
                }

                item_depth.push(token_depth);
            }};

            ($item:expr) => {{
                push_state_item!();
                module_items.push($item);
            }};
        }

        // create labeled block
        if let Some((label_depth, _)) = label_to_assign {
            if label_depth < token_depth {
                let (label_depth, label) = label_to_assign.take().unwrap();
                let mut block = vec![
                    Token {
                        kind: TokenKind::IsolatedBlock(label),
                        ..token
                    },
                    token,
                ];

                while let Some(block_token) =
                    tokens.next_if(|t| label_depth < get_depth(t.indentation, t.loc))
                {
                    block.push(block_token);
                }

                // TODO: i thought that i could just push locala imports but at this point not all of the imports availabel
                //          because @import may be located later in the file (after the block)
                load_queue.push((module_name.clone(), local_imports.clone(), block));

                continue;
            } else if label_depth > token_depth {
                eprintln!(
                    "label must be either a parent of next item to define a block or to be at the same indentation depth as the next item"
                );
                return Err(());
            }
        }

        match token.kind {
            TokenKind::IsolatedBlock(label) => {
                let _ = labels_map.insert(label, module_items.len() + link_offset);
                if let Some(Token {
                    kind: TokenKind::Option(_),
                    ..
                }) = tokens.peek()
                {
                    push_state_item!(StateItem::OptionsBlock(Vec::new()));
                }
            }
            TokenKind::PhraseContinuation(text) => match module_items.last_mut() {
                Some(StateItem::Option { phrase, .. } | StateItem::Response { phrase, .. }) => {
                    phrase.push_str(&text);
                }
                _ => {
                    eprintln!(
                        "{text:?} must belong to `-` response or `>` option: {:?}",
                        token.loc
                    );
                    return Err(());
                }
            },
            TokenKind::Response(phrase) => {
                if entry_point.is_none() {
                    *entry_point = Some(module_items.len() + link_offset);
                }

                push_state_item!(StateItem::Response {
                    phrase,
                    options: Vec::new(),
                });
            }
            TokenKind::Option(phrase) => {
                // TODO: push OptionsBlock if option is labeled
                let this_option = module_items.len() + link_offset;
                push_state_item!();
                let Some(Ok(opt_handler)) = (0..module_items.len())
                    .rev()
                    .filter(|i| !matches!(&module_items[*i], StateItem::Option { .. }))
                    .find_map(|i| {
                        if item_depth[i] < item_depth[this_option] {
                            return Some(Err(()));
                        }

                        (item_depth[i] == item_depth[this_option]).then_some(Ok(i))
                    })
                else {
                    eprintln!("couldn't find an option handler for {phrase:?} on this depth");
                    return Err(());
                };

                module_items.push(StateItem::Option {
                    phrase,
                    handler: opt_handler,
                });

                match &mut module_items[opt_handler] {
                    StateItem::Response { options, .. } => options.push(this_option),
                    StateItem::OptionsBlock(options) => options.push(this_option),
                    StateItem::Jump => {
                        eprintln!(
                            "I cannot assign options for `@jump` intrinsic, you must define response with `-` that will own those options"
                        );
                        return Err(());
                    }
                    StateItem::Option { .. } => unreachable!(),
                };
            }
            TokenKind::Function { name, mut args } if name == "jump" => {
                if !func_signature_is_valid((&token.loc, &name), &args, &['.']) {
                    return Err(())
                }

                if args.len() > 1 {
                    eprintln!(
                        "{loc:?} too many arguments for `jump` function, expected only one label",
                        loc = token.loc
                    );
                    return Err(());
                }

                let Some((_, label)) = args.pop() else {
                    eprintln!(
                        "{loc:?} expected label for `jump` function",
                        loc = token.loc
                    );
                    return Err(());
                };

                let mut parts = label.split('.');
                let jump_target = match (parts.next(), parts.next(), parts.next()) {
                    (.., Some(_)) => {
                        eprintln!("{:?} invalid item access syntax, path may only contain one dot", token.loc);
                        return Err(());
                    }
                    (Some(import_name), Some(import_label), _) => {
                        if let Some(is_used) = local_imports.get_mut(import_name) {
                            *is_used = true;
                        } else {
                            eprintln!("`{:?} {import_name}` is not imported, so can't access `{import_label}`", token.loc);
                            return Err(());
                        }

                        let Some(is_loaded) = modules.get_mut(import_name) else {
                            eprintln!("`{import_name}` is not imported, use @import");
                            return Err(());
                        };

                        if !*is_loaded {
                            let src =
                                match crate::utils::read_mur_file(&format!("{import_name}.mur")) {
                                    Ok(s) => s,
                                    Err(_) => continue,
                                };
                            let import_tokens = tokenize(&src);
                            load_queue.push((
                                import_name.to_string(),
                                HashMap::new(),
                                import_tokens,
                            ));
                            *is_loaded = true;
                        }

                        label
                    }
                    (Some(label), None, _) => make_label(&module_name, label),
                    _ => unreachable!(),
                };

                assert!(
                    label_jumps
                        .insert(module_items.len() + link_offset, jump_target)
                        .is_none(),
                    "it seems like jump got overwritten, is it even possilbe?"
                );

                push_state_item!(StateItem::Jump);
            }
            TokenKind::Function { name, mut args } if name == "as" => {
                if !func_signature_is_valid((&token.loc, &name), &args, &[]) {
                    return Err(())
                }

                if args.len() > 1 {
                    let (loc, _) = args[1];

                    eprintln!(
                        "{loc:?} too many arguments for `as` function, expected only one label",
                    );
                    return Err(());
                }

                let Some((_, label)) = args.pop() else {
                    eprintln!(
                        "{loc:?} expected label name for `as` function",
                        loc = token.loc
                    );
                    return Err(());
                };

                if label_to_assign.is_some() {
                    eprintln!("standalone labels is not allowed");
                    return Err(());
                }

                if label.contains('.') {
                    eprintln!("`.` is not allowed in the label name: {label}");
                    return Err(());
                }

                let name = make_label(&module_name, &label);

                if labels_map.contains_key(&name) {
                    eprintln!("duplicate label: {label}");
                    return Err(());
                }

                label_to_assign = Some((token_depth, name));
            }
            TokenKind::Function { name, mut args } if name == "import" => {
                if !func_signature_is_valid((&token.loc, &name), &args, &[]) {
                    return Err(())
                }

                if args.len() > 1 {
                    eprintln!(
                        "{loc:?} too many arguments for `import` function, expected only one module name",
                        loc = token.loc
                    );
                    return Err(());
                }

                let Some((_, module)) = args.pop() else {
                    eprintln!(
                        "{loc:?} expected module name for `import` function",
                        loc = token.loc
                    );
                    return Err(());
                };

                if module.trim().is_empty() {
                    eprintln!("you didn't specify import name at {:?}", token.loc);
                    return Err(());
                } else if module == module_name {
                    eprintln!("self import is weird");
                    return Err(());
                } else if local_imports.insert(module.clone(), false).is_some() {
                    eprintln!("[Warn] duplicate import `{module}` at {:?}", token.loc);
                } else if let Entry::Vacant(e) = modules.entry(module.clone()) {
                    e.insert(false);
                }
            }
            TokenKind::Function { name, .. } => todo!("custom function: {name}"),
        }
    }

    let module_links = link_state_items(link_offset, &mut module_items, &item_depth);

    links.extend(module_links);
    items.extend(module_items);

    Ok(module_name)
}

const RESTRICTED_CHARS: &[char] = &['!', '@', '-', '>', '.'];

fn func_signature_is_valid<'s>(
    func: (&'s Loc, &'s str),
    args: &'s [(Loc, String)],
    exceptions: &'s [char],
) -> bool {
    let mut valid = true;
    let errors = std::iter::once(func)
        .chain(args.iter().map(|(loc, arg)| (loc, arg.as_str())))
        .filter_map(|(loc, word)| {
            let (ch_idx, ch) = word
                .chars()
                .enumerate()
                .find(|(_, ch)| !exceptions.contains(ch) && RESTRICTED_CHARS.contains(ch))?;

            Some((
                Loc {
                    col: loc.col + ch_idx as u32,
                    ..*loc
                },
                ch,
            ))
        });

    for (err_loc, err_char) in errors {
        eprintln!(
            "{err_loc:?} unacceptable character in function signature `{f}`: '{err_char}'",
            f = func.1,
        );
        valid = false
    }

    valid
}

fn link_state_items(
    link_offset: usize,
    items: &mut [StateItem],
    item_depth: &[u32],
) -> Vec<Option<usize>> {
    let mut links = vec![None::<usize>; items.len()];
    let mut parents = HashMap::<usize, usize>::new(); // excluding option items
    let mut sibling_idx = HashMap::<usize, u32>::new(); // excluding option items

    let mut last_modified = None::<usize>;
    let mut states = Vec::<usize>::new();

    for (item, kind) in items.iter().enumerate() {
        // filter out options,
        if let StateItem::Option { handler, .. } = kind {
            last_modified = Some(*handler);

            continue;
        }

        // link previous item with this state item
        if let Some(prev_item_link @ None) =
            item.checked_sub(1).map(|prev_item| &mut links[prev_item])
        {
            *prev_item_link = Some(item);
        }

        // push it's parent and sibling counter
        update_family(
            item,
            last_modified,
            &mut parents,
            &mut sibling_idx,
            item_depth,
        );

        states.push(item);
    }

    for i in 0..states.len() {
        let this_state = states[i];

        let (mut searched, mut found_next_item) = (false, None::<usize>);

        let options = match &items[this_state] {
            StateItem::OptionsBlock(options) => options,
            StateItem::Response { options, .. } => options,
            StateItem::Jump => continue, // already linked
            StateItem::Option { .. } => unreachable!(),
        };

        // link remaining unlinked items with magic function
        for item in std::iter::once(&this_state).chain(options.iter()) {
            let next @ None = &mut links[*item] else {
                continue;
            };

            if !searched {
                searched = true;
                found_next_item =
                    find_next_state_long_long_way(&states[i..], &parents, &sibling_idx, item_depth)
                        .map(|item_idx| item_idx + link_offset);
            }

            *next = found_next_item;
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
    maybe_last_state: Option<usize>,
    states_parents: &mut HashMap<usize, usize>,
    sibling_idx: &mut HashMap<usize, u32>,
    item_depth: &[u32],
) {
    let mut parent = None;
    let mut sibling_counter = 0;

    if let Some(last_state) = maybe_last_state {
        // if last state is a sibling
        if item_depth[last_state] == item_depth[new_state] {
            sibling_counter = sibling_idx[&last_state] + 1;
            parent = states_parents.get(&last_state).copied();
        // if last state is a parent of this new item
        } else if item_depth[last_state] < item_depth[new_state] {
            parent = Some(last_state);
        // if last item is a child of some parent item
        } else if item_depth[last_state] > item_depth[new_state] {
            let mut last_state_parent = last_state;

            // go back along the states_parents tree until we found one with the same depth
            while item_depth[last_state_parent] != item_depth[new_state] {
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
    item_depth: &[u32],
) -> Option<usize> {
    let mut current = states.first().copied()?;

    loop {
        'searching: for state in states.iter().skip(1) {
            if item_depth[current] != item_depth[*state] {
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
