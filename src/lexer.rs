use std::{
    collections::{HashMap, hash_map::Entry},
    fmt,
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
    IsolatedBlock(String),      // constructed on parsing stage (label_name)
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
pub(super) enum Item {
    Response { phrase: Phrase, options: Vec<usize> },
    Option { phrase: Phrase },
    Jump(usize),
    Hide(Vec<usize>),
    Show(Vec<usize>),
    FunctionCall { func: usize, args: Vec<String> },
}

#[derive(Default)]
struct ParserData {
    single_indentation: Indentation,
    labels_map: HashMap<String, usize>,
    label_jumps: HashMap<usize, (Loc, String)>, // to store all jumps, before all labels are defined
    label_vis_changes: HashMap<usize, Vec<(Loc, String)>>, // to store args for `hide` or `show` funcs, before all labels are defined

    entry_point: Option<usize>,
    modules: HashMap<String, bool>, // keep track of what is already imported
    load_queue: Vec<(String, HashMap<String, bool>, Vec<Token>)>, // module name, local imports (if parsing an `as` block), tokens

    // Result
    items: Vec<Item>,
    links: Vec<Option<usize>>,
}

pub enum ReturnValue {
    Bool(bool),
    String(String),
    None,
}

impl From<bool> for ReturnValue {
    fn from(this: bool) -> Self {
        ReturnValue::Bool(this)
    }
}

impl From<String> for ReturnValue {
    fn from(this: String) -> Self {
        ReturnValue::String(this)
    }
}

impl From<&str> for ReturnValue {
    fn from(this: &str) -> Self {
        ReturnValue::String(this.to_owned())
    }
}

impl From<()> for ReturnValue {
    fn from(_: ()) -> Self {
        ReturnValue::None
    }
}

impl fmt::Display for ReturnValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ReturnValue::String(s) => write!(f, "{s}"),
            ReturnValue::Bool(b) => write!(f, "{b}"),
            ReturnValue::None => write!(f, ""),
        }
    }
}

pub(super) type Func<State> = fn(&mut State, args: &[String]) -> ReturnValue;

#[allow(clippy::type_complexity)]
pub(super) fn parse_tokens_into_items<State>(
    main_module: &str,
    tokens: Vec<Token>,
    funcs_map_defs: HashMap<&'static str, Func<State>>,
    state: &mut State,
) -> Result<(Vec<Item>, Vec<Option<usize>>, Vec<Func<State>>), ()> {
    let mut parser = ParserData::default();
    let mut failed = false;

    let mut funcs = Vec::new();
    let mut funcs_map = HashMap::new();

    for (fn_name, func) in funcs_map_defs {
        funcs_map.insert(fn_name, funcs.len());
        funcs.push(func);
    }

    // TODO: avoid second allocation for the name
    parser.modules.insert(main_module.into(), true);
    parser
        .load_queue
        .push((main_module.into(), HashMap::new(), tokens));

    while !parser.load_queue.is_empty() {
        let Ok(parsed_module_name) = parse_module(&mut parser, &funcs_map, &funcs, state) else {
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

    for (jump_item, (loc, label)) in parser.label_jumps {
        // TODO: store location
        let Some(target_item) = parser.labels_map.get(&label) else {
            eprintln!("{loc:?} label doesn't exist: {label}");
            return Err(());
        };

        if let Item::Option { .. } = parser.items[*target_item] {
            eprintln!("{loc:?} jumping on options is not allowed :(");
            return Err(());
        }

        let Item::Jump(location) = &mut parser.items[jump_item] else {
            unreachable!()
        };

        *location = *target_item;
    }

    for (vis_change_item, labels) in parser.label_vis_changes {
        let (Item::Hide(items) | Item::Show(items)) = &mut parser.items[vis_change_item] else {
            unreachable!();
        };

        for (loc, label) in labels {
            let Some(target_item) = parser.labels_map.get(&label) else {
                eprintln!("{loc:?} label doesn't exist: {label}");
                return Err(());
            };

            items.push(*target_item);
        }
    }

    for (module, is_used) in parser.modules {
        if !is_used {
            eprintln!("[Warn] `{module}` was not loaded, because it was never referenced anywhere");
        }
    }

    Ok((parser.items, parser.links, funcs))
}

fn parse_module<State>(
    ParserData {
        load_queue,
        modules,
        single_indentation,
        labels_map,
        label_jumps,
        label_vis_changes,
        entry_point,
        items,
        links,
    }: &mut ParserData,
    funcs_map: &HashMap<&'static str, usize>,
    funcs: &[Func<State>],
    state: &mut State,
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

    let mut parents = Vec::<Option<usize>>::new();
    let mut sibling_idx = Vec::<u32>::new();
    let mut item_depth = Vec::<u32>::new();

    let mut module_items = Vec::<Item>::new();
    let mut label_to_assign = None::<(u32, String)>;
    let mut tokens = tokens.into_iter().peekable();

    while let Some(token) = tokens.next() {
        let this_item = module_items.len() + link_offset;
        let token_depth = get_depth(token.indentation, token.loc);

        macro_rules! register_state_item {
            ($update_family:expr $(, $item:expr)?) => {{
                if let Some(Item::Option { phrase, .. } | Item::Response { phrase, .. }) =
                    module_items.last_mut()
                {
                    let Phrase::Static(text) = phrase else {
                        unreachable!();
                    };

                    crate::utils::trim_end(text);

                    let Ok(parsed_phrase) = parse_phrase(token.loc, text, funcs_map, funcs, state) else {
                        return Err(());
                    };

                    *phrase = parsed_phrase;
                }

                if let Some((_, label)) = label_to_assign.take() {
                    let _ = labels_map.insert(label, this_item);
                }

                item_depth.push(token_depth);
                if $update_family {
                    let this = module_items.len();
                    welcome_to_the_family(
                        this,
                        this.checked_sub(1),
                        &mut parents,
                        &mut sibling_idx,
                        &item_depth,
                    );
                }

                $(module_items.push($item);)?
            }};
        }

        if let Some((label_depth, _)) = label_to_assign {
            // create labeled block
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
                //          because @import may be located later in the file (after the block), which is not a big deal?
                load_queue.push((module_name.clone(), local_imports.clone(), block));

                continue;
            } else if label_depth > token_depth {
                eprintln!(
                    "{:?} label must be either a parent of next item to define a block or to be at the same indentation depth as the next item",
                    token.loc
                );
                return Err(());
            }
        }

        match token.kind {
            TokenKind::IsolatedBlock(label) => {
                let _ = labels_map.insert(label, module_items.len() + link_offset);
            }
            TokenKind::PhraseContinuation(text) => match module_items.last_mut() {
                Some(Item::Option { phrase, .. } | Item::Response { phrase, .. }) => {
                    let Phrase::Static(p) = phrase else {
                        unreachable!();
                    };

                    p.push_str(&text);
                }
                _ => {
                    eprintln!(
                        "{:?} {text:?} must belong to `-` response or `>` option",
                        token.loc
                    );
                    return Err(());
                }
            },
            TokenKind::Response(phrase) => {
                if entry_point.is_none() {
                    *entry_point = Some(module_items.len() + link_offset);
                }

                register_state_item!(
                    true,
                    Item::Response {
                        phrase: Phrase::Static(phrase),
                        options: Vec::new(),
                    }
                );
            }
            TokenKind::Option(phrase) => {
                register_state_item!(false);

                let Some(Ok(opt_handler)) = (0..module_items.len())
                    .rev()
                    .filter(|i| !matches!(&module_items[*i], Item::Option { .. }))
                    .find_map(|i| {
                        if item_depth[i] < item_depth[this_item] {
                            return Some(Err(()));
                        }

                        (item_depth[i] == item_depth[this_item]).then_some(Ok(i))
                    })
                else {
                    eprintln!(
                        "{:?} couldn't find an option handler for {phrase:?} on this depth",
                        token.loc
                    );
                    return Err(());
                };

                sibling_idx.push(sibling_idx[opt_handler]);
                parents.push(Some(opt_handler));
                module_items.push(Item::Option {
                    phrase: Phrase::Static(phrase),
                });

                match &mut module_items[opt_handler] {
                    Item::Response { options, .. } => options.push(this_item),
                    Item::Option { .. } => unreachable!(),
                    _ => {
                        eprintln!(
                            "{loc:?} options can only belong to responses, defined with `-`",
                            loc = token.loc,
                        );
                        return Err(());
                    }
                };
            }
            TokenKind::Function { name, mut args } if name == "jump" => {
                if args.len() > 1 {
                    eprintln!(
                        "{loc:?} too many arguments for `jump` function, expected only one label",
                        loc = token.loc
                    );
                    return Err(());
                }

                if !func_signature_is_valid((&token.loc, &name), &args, &['.']) {
                    return Err(());
                }

                let Some((loc, label)) = args.pop() else {
                    eprintln!(
                        "{loc:?} expected label for `jump` function",
                        loc = token.loc
                    );
                    return Err(());
                };

                let mut parts = label.split('.');
                let jump_target = match (parts.next(), parts.next(), parts.next()) {
                    (.., Some(_)) => {
                        eprintln!(
                            "{:?} invalid item access syntax, path may only contain one dot",
                            token.loc
                        );
                        return Err(());
                    }
                    (Some(import_name), Some(import_label), _) => {
                        if let Some(is_used) = local_imports.get_mut(import_name) {
                            *is_used = true;
                        } else {
                            eprintln!(
                                "{:?} `{import_name}` is not imported, so can't access `{import_label}` item",
                                token.loc
                            );
                            return Err(());
                        }

                        let Some(is_loaded) = modules.get_mut(import_name) else {
                            eprintln!("`{import_name}` is not imported, use @import");
                            return Err(());
                        };

                        if !*is_loaded {
                            let src = crate::utils::read_mur_file(&format!("{import_name}.mur"))?;
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
                        .insert(module_items.len() + link_offset, (loc, jump_target))
                        .is_none(),
                    "it seems like jump got overwritten, is it even possilbe?"
                );

                register_state_item!(true, Item::Jump(0));
            }
            TokenKind::Function { name, mut args } if name == "as" => {
                if args.len() > 1 {
                    let (loc, _) = args[1];

                    eprintln!(
                        "{loc:?} too many arguments for `as` function, expected only one label",
                    );
                    return Err(());
                }

                if !func_signature_is_valid((&token.loc, &name), &args, &[]) {
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
                    eprintln!("function `as` cannot be labeled");
                    return Err(());
                }

                if tokens.peek().is_none() {
                    eprintln!("standalone `as` function is not allowed");
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
                if args.len() > 1 {
                    eprintln!(
                        "{loc:?} too many arguments for `import` function, expected only one module name",
                        loc = token.loc
                    );
                    return Err(());
                }

                if !func_signature_is_valid((&token.loc, &name), &args, &[]) {
                    return Err(());
                }

                if label_to_assign.is_some() {
                    eprintln!(
                        "{loc:?} cannot assign label to the `import` function",
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
                    eprintln!("{:? }you didn't specify import name", token.loc);
                    return Err(());
                } else if module == module_name {
                    eprintln!("{:?} self import is weird", token.loc);
                    return Err(());
                } else if local_imports.contains_key(&module) {
                    eprintln!("{:?} duplicate import `{module}`", token.loc);
                    return Err(());
                } else {
                    local_imports.insert(module.clone(), false);
                    if let Entry::Vacant(e) = modules.entry(module.clone()) {
                        e.insert(false);
                    }
                }
            }
            TokenKind::Function { name, mut args } if name == "hide" || name == "show" => {
                if !func_signature_is_valid((&token.loc, &name), &args, &['.']) {
                    return Err(());
                }

                register_state_item!(true);

                let target_items = if args.is_empty() {
                    let Some(parent) = parents[this_item] else {
                        eprintln!(
                            "{:?} this `hide` function without arguments affect it's parent, which is not present in our case :(",
                            token.loc
                        );
                        return Err(());
                    };

                    vec![parent]
                } else {
                    for (_, label) in &mut args {
                        *label = make_label(&module_name, label);
                    }

                    let len = args.len();
                    label_vis_changes.insert(this_item, args);
                    Vec::with_capacity(len)
                };

                match name.as_str() {
                    "hide" => module_items.push(Item::Hide(target_items)),
                    "show" => module_items.push(Item::Show(target_items)),
                    _ => unreachable!(),
                }
            }
            TokenKind::Function { name, args } => {
                let (is_comptime_call, fn_name) = name
                    .strip_prefix('!')
                    .map_or((false, name.as_str()), |rest| (true, rest));

                let Some(func) = funcs_map.get(fn_name).copied() else {
                    eprintln!("{:?} function `{fn_name}` is undefined", token.loc);
                    return Err(());
                };

                if !func_signature_is_valid((&token.loc, fn_name), &args, &[]) {
                    return Err(());
                }

                let args = args.into_iter().map(|(_, a)| a).collect::<Vec<String>>();

                if is_comptime_call {
                    if label_to_assign.is_some() {
                        eprintln!(
                            "{:?} you cannot assign label for compile time function, it will not be present at runtime, bruh, obviously",
                            token.loc
                        );
                        return Err(());
                    }

                    (funcs[func])(state, &args);
                    continue;
                }

                register_state_item!(true, Item::FunctionCall { func, args })
            }
        }
    }

    let module_links = link_state_items(
        link_offset,
        &mut module_items,
        &item_depth,
        &parents,
        &sibling_idx,
    );

    links.extend(module_links);
    items.extend(module_items);

    Ok(module_name)
}

#[derive(Debug)]
pub(crate) enum Phrase {
    Static(String),
    Dynamic {
        buffer: String,
        parts: Vec<PhraseParts>,
    },
}

impl Phrase {
    pub(super) fn as_str(&self) -> &str {
        match self {
            Phrase::Static(t) => t.as_str(),
            Phrase::Dynamic { buffer, .. } => buffer.as_str(),
        }
    }

    pub(super) fn is_empty(&self) -> bool {
        match self {
            Phrase::Static(t) => t.is_empty(),
            Phrase::Dynamic { buffer, .. } => buffer.is_empty(),
        }
    }

    pub(super) fn update<State>(&mut self, funcs: &[Func<State>], state: &mut State) {
        let Phrase::Dynamic { buffer, parts } = self else {
            return;
        };

        buffer.clear();

        for part in parts {
            match part {
                PhraseParts::Static(t) => buffer.push_str(t),
                PhraseParts::FromFunction { func, args } => {
                    use std::fmt::Write;

                    let _ = match (funcs[*func])(state, args) {
                        ReturnValue::String(t) => write!(buffer, "{t}"),
                        ReturnValue::Bool(b) => write!(buffer, "{b}"),
                        ReturnValue::None => continue, 
                    };
                }
            }
        }
    }
}

#[derive(Debug)]
pub(crate) enum PhraseParts {
    Static(String),
    FromFunction { func: usize, args: Vec<String> },
}

fn parse_phrase<State>(
    mut phrase_loc: Loc,
    input: &str,
    funcs_map: &HashMap<&'static str, usize>,
    funcs: &[Func<State>],
    state: &mut State,
) -> Result<Phrase, ()> {
    // println!("{phrase_loc:?} with input {input:?}");
    fn collect_func(stream: &mut impl Iterator<Item = (Loc, char)>) -> Option<Vec<(Loc, String)>> {
        let mut buf = String::new();
        let mut args = Vec::new();
        let mut arg_loc = None::<Loc>;

        for (loc, ch) in stream {
            if arg_loc.is_none() {
                arg_loc = Some(loc);
            }

            if ch == '}' {
                if !buf.is_empty() {
                    args.push((arg_loc.take().unwrap(), buf.clone()));
                    buf.clear();
                }
                return Some(args);
            }

            if ch.is_whitespace() {
                if !buf.is_empty() {
                    args.push((arg_loc.take().unwrap(), buf.clone()));
                    buf.clear();
                }
            } else {
                buf.push(ch);
            }
        }

        None
    }

    let mut parts = Vec::<PhraseParts>::new();
    let mut stream = input.chars().map(|ch| {
        let t = (phrase_loc, ch);
        if ch == '\n' {
            phrase_loc.line += 1;
            phrase_loc.col = 1;
        } else {
            phrase_loc.col += 1;
        }

        t
    });

    let mut buf = String::new();
    while let Some((_, ch)) = stream.next() {
        if ch != '@' {
            buf.push(ch);
            continue;
        }

        let Some((next_ch_loc, next_ch)) = stream.next() else {
            break;
        };

        if next_ch != '{' {
            buf.push(ch);
            continue;
        }

        if !buf.is_empty() {
            if let Some(PhraseParts::Static(ph)) = parts.last_mut() {
                ph.push_str(&buf);
            } else {
                parts.push(PhraseParts::Static(buf.clone()));
            }

            buf.clear()
        }

        let Some(func_args) = collect_func(&mut stream) else {
            // TODO: display location
            eprintln!("{next_ch_loc:?} could not find '}}' delimiter");
            return Err(());
        };

        let Some(((name_loc, name), args)) = func_args.split_first() else {
            eprintln!("{next_ch_loc:?} expected function name within `@{{}}`");
            return Err(());
        };

        let (is_comptime_call, fn_name) = name
            .strip_prefix('!')
            .map_or((false, name.as_str()), |rest| (true, rest));

        let Some(func) = funcs_map.get(fn_name).copied() else {
            eprintln!("{name_loc:?} function `{fn_name}` is undefined");
            return Err(());
        };

        if !func_signature_is_valid((name_loc, fn_name), args, &[]) {
            return Err(());
        }

        let args = func_args
            .into_iter()
            .skip(1)
            .map(|(_, a)| a)
            .collect::<Vec<String>>();

        if is_comptime_call {
            let output = (funcs[func])(state, &args).to_string();
            if let Some(PhraseParts::Static(ph)) = parts.last_mut() {
                ph.push_str(&output);
                continue;
            } else {
                parts.push(PhraseParts::Static(output));
            }
        } else {
            parts.push(PhraseParts::FromFunction { func, args });
        }
    }

    if !buf.is_empty() {
        parts.push(PhraseParts::Static(buf));
    }

    Ok(
        if parts.len() == 1
            && let Some(PhraseParts::Static(phrase)) =
                parts.pop_if(|p| matches!(p, PhraseParts::Static(_)))
        {
            Phrase::Static(phrase)
        } else {
            Phrase::Dynamic {
                buffer: String::new(),
                parts,
            }
        },
    )
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
    items: &mut [Item],
    items_depth: &[u32],
    parents: &[Option<usize>],
    sibling_idx: &[u32],
) -> Vec<Option<usize>> {
    let mut links = vec![None::<usize>; items.len()];
    let mut states = Vec::<usize>::new();

    for (item, kind) in items.iter().enumerate() {
        // filter out options,
        if let Item::Option { .. } = kind {
            continue;
        }

        // link previous item with this state item
        if let Some(prev_item_link @ None) =
            item.checked_sub(1).map(|prev_item| &mut links[prev_item])
        {
            *prev_item_link = Some(item + link_offset);
        }

        states.push(item);
    }

    for i in 0..states.len() {
        let this_state = states[i];

        let maybe_options = match &items[this_state] {
            Item::Response { options, .. } => Some(options),
            Item::FunctionCall { .. } | Item::Jump(_) | Item::Hide(_) | Item::Show(_) => None,
            Item::Option { .. } => unreachable!(),
        };

        if links[this_state].is_some()
            && (maybe_options.is_none()
                || maybe_options.is_some_and(|opts| opts.iter().all(|o| links[*o].is_some())))
        {
            continue;
        }

        let found_next_item =
            find_next_state_long_long_way(&states[i..], parents, sibling_idx, items_depth)
                .map(|item_idx| item_idx + link_offset);

        links[this_state] = found_next_item;

        let Some(options) = maybe_options else {
            continue;
        };

        for opt in options {
            let next @ None = &mut links[*opt] else {
                continue;
            };

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

fn welcome_to_the_family(
    new_item: usize,
    maybe_prev_item: Option<usize>,
    parents: &mut Vec<Option<usize>>,
    sibling_idx: &mut Vec<u32>,
    item_depth: &[u32],
) {
    let mut sib_idx = 0;
    let mut parent = None::<usize>;

    if let Some(prev_item) = maybe_prev_item {
        // if last state is a sibling
        if item_depth[prev_item] == item_depth[new_item] {
            sib_idx = sibling_idx[prev_item] + 1;
            parent = parents[prev_item];
        // if last state is a parent of this new item
        } else if item_depth[prev_item] < item_depth[new_item] {
            parent = Some(prev_item);
        // if last item is a child of some other parent
        } else if item_depth[prev_item] > item_depth[new_item] {
            let mut last_state_parent = prev_item;

            // go back along the parents tree until we found one with the same depth
            while item_depth[last_state_parent] != item_depth[new_item] {
                let Some(upper_parent) = parents[last_state_parent] else {
                    break;
                };

                last_state_parent = upper_parent;
            }

            sib_idx = sibling_idx[last_state_parent] + 1;
            parent = parents[last_state_parent];
        }
    }

    sibling_idx.insert(new_item, sib_idx);
    parents.push(parent);
}

fn find_next_state_long_long_way(
    states: &[usize],
    parents: &[Option<usize>],
    sibling_idx: &[u32],
    item_depth: &[u32],
) -> Option<usize> {
    let mut current = states.first().copied()?;

    loop {
        'searching: for state in states.iter().skip(1) {
            if item_depth[current] != item_depth[*state] {
                continue 'searching;
            } else if sibling_idx[current] >= sibling_idx[*state] {
                break 'searching;
            } else if sibling_idx[current] + 1 == sibling_idx[*state] {
                return Some(*state);
            }
        }

        let Some(parent) = parents[current] else {
            break;
        };

        current = parent
    }

    None
}
