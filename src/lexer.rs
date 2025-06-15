use std::{collections::HashMap, fmt, iter::Peekable};

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

    fn get_mut_amount(&mut self) -> &mut u32 {
        let (Indentation::Tabs(n) | Indentation::Spaces(n)) = self;
        n
    }

    fn as_string(&self) -> String {
        match self {
            Indentation::Spaces(n) => " ".repeat(*n as _),
            Indentation::Tabs(n) => "\t".repeat(*n as _),
        }
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

#[derive(Copy, Clone, Debug, Default)]
pub struct Loc {
    pub line: u32,
    pub col: u32,
}

impl fmt::Display for Loc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.col)
    }
}

#[derive(Debug)]
enum TokenKind {
    Function {
        kind: FunctionKind,
        args: Vec<(Loc, String)>,
        is_comptime: bool,
    },
    Response(String),           // `-` - npc's response phrase in conversation
    Option(String),             // `>` - player's option in conversation
    PhraseContinuation(String), // phrases on newlines excluding indentation
    IsolatedBlock(String),      // constructed on parsing stage
}

macro_rules! funcs_registry {
    (
        $( $builtin:ident = $builtin_str:expr, )*
    ) => {
        #[derive(Debug, Clone)]
        enum FunctionKind {
            $( $builtin ,)*
            Custom(String),
        }

        impl FunctionKind {
            const BUILTINS: &[FunctionKind] = &[$( FunctionKind::$builtin, )*];
            const BUILTINS_STR: &[&str] = &[$( $builtin_str, )*];

            fn from_str(this: &str) -> Self {
                Self::BUILTINS_STR
                    .iter()
                    .position(|builtin| &this == builtin)
                    .map(|i| Self::BUILTINS[i].clone())
                    .unwrap_or_else(|| Self::Custom(this.to_owned()))
            }
        }

        impl fmt::Display for FunctionKind {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                write!(
                    f,
                    "{}",
                    match self {
                        FunctionKind::Custom(name) => name.as_str(),
                        $( FunctionKind::$builtin => $builtin_str, )*
                    }
                )
            }
        }
    }
}

funcs_registry!(
    End = "end",
    As = "as",
    Jump = "jump",
    Hide = "hide",
    Show = "show",
    Import = "import",
);

#[derive(Debug)]
pub(super) struct Token {
    kind: TokenKind,
    loc: Loc,
    indentation: Indentation,
}

enum WordErr {
    UnclosedQuotes,
    EndOfStream,
    ReachedDelimiter,
}

fn skip_comment_until(
    end: char,
    stream: &mut Peekable<impl Iterator<Item = (Loc, char)>>,
) -> Result<(), WordErr> {
    while let Some((_, ch)) = stream.peek().copied() {
        if ch == end {
            let _ = stream.next();
            return Err(WordErr::ReachedDelimiter);
        } else if ch == '\n' {
            return Ok(());
        }

        let _ = stream.next();
    }

    Err(WordErr::EndOfStream)
}

fn skip_trash_until(
    end: char,
    stream: &mut Peekable<impl Iterator<Item = (Loc, char)>>,
) -> Result<(), WordErr> {
    while let Some((_, ch)) = stream.peek().copied() {
        if ch == end {
            let _ = stream.next();
            return Err(WordErr::ReachedDelimiter);
        } else if ch == '#' {
            skip_comment_until(end, stream)?;
        } else if !ch.is_whitespace() {
            return Ok(());
        }

        let _ = stream.next();
    }

    Err(WordErr::EndOfStream)
}

fn collect_func_name_until(
    end: char,
    stream: &mut Peekable<impl Iterator<Item = (Loc, char)>>,
) -> Result<(Loc, bool, String), WordErr> {
    skip_trash_until(end, stream)?;
    let is_comptime = stream.next_if(|(_, ch)| *ch == '!').is_some();
    collect_word_until(end, stream).map(|(l, n)| (l, is_comptime, n))
}

fn collect_word_until(
    end: char,
    stream: &mut Peekable<impl Iterator<Item = (Loc, char)>>,
) -> Result<(Loc, String), WordErr> {
    skip_trash_until(end, stream)?;

    let Some((word_loc, next_ch)) = stream.peek().copied() else {
        return Err(WordErr::EndOfStream);
    };

    if let '"' = next_ch {
        let mut closed = false;
        let mut end_loc = word_loc;

        let mut chars = stream.by_ref().skip(1).take_while(|(ch_loc, ch)| {
            end_loc = *ch_loc;

            let go_on = *ch != '"';
            if !go_on {
                closed = true;
            }

            go_on
        });

        let word = chars
            .next()
            .map(|(loc, ch)| {
                // waiting for collect_into method 0_o
                let mut word = ch.to_string();
                word.extend(chars.map(|(_, ch)| ch));
                (loc, word)
            })
            .unwrap_or((word_loc, String::new()));

        return if closed {
            Ok(word)
        } else {
            eprintln!("{end_loc} couldn't find closing '\"' for one at {word_loc}");
            Err(WordErr::UnclosedQuotes)
        };
    }

    let mut word = String::new();
    while let Some((_, ch)) = stream.next_if(|(_, ch)| !ch.is_whitespace()) {
        word.push(ch);
    }

    if word.is_empty() {
        Err(WordErr::EndOfStream)
    } else {
        Ok((word_loc, word))
    }
}

fn collect_phrase(
    stream: &mut Peekable<impl Iterator<Item = (Loc, char)>>,
) -> Option<(Loc, String)> {
    let mut phrase = String::new();
    let loc = stream.peek().copied().map(|(l, _)| l)?;

    for (_, ch) in stream {
        // NOTE: End is not trimmed, because the phrases may be combined in the future: Response/Option + PhraseContinuation
        //       So the result Option or Response in the state will most likely contains a newline, which will be removed on parsing stage
        phrase.push(ch);
        if ch == '\n' {
            break;
        }
    }

    (!phrase.is_empty()).then_some((loc, phrase))
}

fn collect_indent(stream: &mut Peekable<impl Iterator<Item = (Loc, char)>>) -> Option<Indentation> {
    let mut amount = 0;
    let (_, ch) = stream.peek().copied()?;

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
        .map(|ch| {
            let it = (loc, ch);

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

    'parsing_lines: loop {
        // TODO: this is collecting indentation for the PhraseContinuation, which i need when pointing on errors within string interpolation...
        let Some(indentation) = collect_indent(&mut stream) else {
            break;
        };

        let Some((_, ch)) = stream.next() else {
            break;
        };

        let token = match ch {
            '#' => {
                let _ = skip_comment_until('\n', &mut stream);
                continue;
            }
            '@' if stream.peek().is_some_and(|(_, next_ch)| *next_ch != '{') => {
                let Ok((fn_loc, is_comptime, fn_name)) = collect_func_name_until('\n', &mut stream)
                else {
                    // TODO: maybe complain?
                    continue;
                };

                let kind = FunctionKind::from_str(&fn_name);
                let mut args = Vec::<(Loc, String)>::new();

                loop {
                    let arg = match collect_word_until('\n', &mut stream) {
                        Ok(a) => a,
                        Err(WordErr::EndOfStream | WordErr::ReachedDelimiter) => {
                            break;
                        }
                        Err(WordErr::UnclosedQuotes) => continue 'parsing_lines,
                    };

                    args.push(arg);
                }

                Some((
                    fn_loc,
                    TokenKind::Function {
                        kind,
                        args,
                        is_comptime,
                    },
                ))
            }
            '>' => {
                collect_phrase(&mut stream).map(|(loc, phrase)| (loc, TokenKind::Option(phrase)))
            }
            '-' => {
                collect_phrase(&mut stream).map(|(loc, phrase)| (loc, TokenKind::Response(phrase)))
            }
            '\n' => {
                if let Some(Token {
                    kind: TokenKind::Option(p) | TokenKind::Response(p) | TokenKind::PhraseContinuation(p),
                    ..
                }) = tokens.last_mut()
                {
                    p.push(ch);
                }

                continue;
            }
            // TODO: i could push a phrase part into the last token?
            _ => collect_phrase(&mut stream).map(|(loc, phrase)| {
                let mut char_buf = [0u8; 4];
                (
                    loc,
                    // putting indentation back, so later on string interpolation we could report correct location of an error
                    TokenKind::PhraseContinuation(
                        indentation.as_string() + ch.encode_utf8(&mut char_buf) + &phrase,
                    ),
                )
            }),
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
    FunctionCall { func: usize, func_data: FuncData },
    End,
    // If { condition_fn: usize },
    // Elif { condition_fn: usize },
    // Else { condition_fn: usize },
}

#[derive(Debug)]
pub struct FuncData {
    pub is_comptime: bool,
    pub args: Vec<String>,
    pub call_location: Loc,
}

pub(super) type Func<State, Ret> = dyn Fn(&mut State, &FuncData) -> Ret;

pub enum Function<State> {
    Bool(Box<Func<State, bool>>),
    String(Box<Func<State, String>>),
    Nothing(Box<Func<State, ()>>),
}

impl<State> Function<State> {
    pub(super) fn call_drop(&self, state: &mut State, func_data: &FuncData) {
        match self {
            Function::String(f) => {
                f(state, func_data);
            }
            Function::Bool(f) => {
                f(state, func_data);
            }
            Function::Nothing(f) => {
                f(state, func_data);
            }
        }
    }
}

// generic is a trick to avoid trait collision because rust is safe and blazingly fast..
pub trait IntoFunction<T, State> {
    fn into_function(self) -> Function<State>;
}

impl<State, F> IntoFunction<(), State> for F
where
    F: Fn(&mut State, &FuncData) + 'static,
{
    fn into_function(self) -> Function<State> {
        Function::Nothing(Box::new(self))
    }
}
impl<State, F> IntoFunction<String, State> for F
where
    F: Fn(&mut State, &FuncData) -> String + 'static,
{
    fn into_function(self) -> Function<State> {
        Function::String(Box::new(self))
    }
}

impl<State, F> IntoFunction<bool, State> for F
where
    F: Fn(&mut State, &FuncData) -> bool + 'static,
{
    fn into_function(self) -> Function<State> {
        Function::Bool(Box::new(self))
    }
}

#[derive(Default)]
struct ParserData {
    single_indentation: Indentation,
    labels_map: HashMap<String, usize>,
    label_jumps: HashMap<usize, (Loc, String)>, // to store all jumps, before all labels are defined
    label_vis_changes: HashMap<usize, Vec<(Loc, String)>>, // to store args for `hide` or `show` funcs, before all labels are defined

    modules: HashMap<String, bool>, // keep track of what is already loaded into queue
    local_imports: HashMap<String, HashMap<String, (Loc, bool)>>,
    load_queue: Vec<(String, Vec<Token>)>, // module name, tokens

    // Result
    items: Vec<Item>,
    links: Vec<Option<usize>>,
    failed: bool,
}

#[allow(clippy::type_complexity)]
pub(super) fn parse_tokens_into_items<State>(
    main_module: &str,
    tokens: Vec<Token>,
    funcs_map_defs: HashMap<&'static str, Function<State>>,
    state: &mut State,
) -> Result<(Vec<Item>, Vec<Option<usize>>, Vec<Function<State>>), ()> {
    let mut parser = ParserData::default();
    let mut funcs = Vec::new();
    let mut funcs_map = HashMap::new();

    for (fn_name, func) in funcs_map_defs {
        funcs_map.insert(fn_name, funcs.len());
        funcs.push(func);
    }

    // TODO: avoid second allocation for the name
    parser.modules.insert(main_module.into(), true);
    parser.load_queue.push((main_module.into(), tokens));

    while !parser.load_queue.is_empty() {
        parse_module(&mut parser, &funcs_map, &funcs, state);
    }

    for (module, imports) in parser.local_imports {
        for (imp, (loc, is_used)) in imports {
            if !is_used {
                eprintln!("{loc} in `{module}` import `{imp}` was never referensed anywhere");
                parser.failed = true;
            }
        }
    }

    // panic!("{:#?}", parser.labels_map);

    for (jump_item, (loc, label)) in parser.label_jumps {
        let Some(target_item) = parser.labels_map.get(&label).copied() else {
            eprintln!("{loc} label doesn't exist: {label}");
            parser.failed = true;
            continue;
        };

        if let Item::Option { .. } = parser.items[target_item] {
            eprintln!("{loc} jumping on options is not allowed :(");
            parser.failed = true;
            continue;
        }

        let Item::Jump(location) = &mut parser.items[jump_item] else {
            unreachable!()
        };

        if target_item == jump_item {
            eprintln!("{loc} you are creating infinite loop by jumping on `jump` itself");
            parser.failed = true;
            continue;
        }

        *location = target_item;
    }

    for (vis_change_item, labels) in parser.label_vis_changes {
        let (Item::Hide(items) | Item::Show(items)) = &mut parser.items[vis_change_item] else {
            unreachable!();
        };

        for (loc, label) in labels {
            let Some(target_item) = parser.labels_map.get(&label) else {
                eprintln!("{loc} label doesn't exist: {label}");
                parser.failed = true;
                continue;
            };

            items.push(*target_item);
        }
    }

    if parser.failed {
        Err(())
    } else {
        Ok((parser.items, parser.links, funcs))
    }
}

fn parse_module<State>(
    ParserData {
        load_queue,
        modules,
        local_imports,
        single_indentation,
        labels_map,
        label_jumps,
        label_vis_changes,
        items,
        links,
        failed,
    }: &mut ParserData,
    funcs_map: &HashMap<&'static str, usize>,
    funcs: &[Function<State>],
    state: &mut State,
) {
    macro_rules! fail {
        () => {{
            *failed = true;
        }};
        ($($msg:tt)*) => {{
            eprintln!($($msg)*);
            fail!();
        }}
    }

    let link_offset = items.len();
    let Some((module_name, tokens)) = load_queue.pop() else {
        unreachable!();
    };

    let make_label = |m: &str, l: &str| m.to_owned() + "." + l;

    let mut get_depth = |indent: Indentation, loc: Loc| -> Result<u32, ()> {
        if single_indentation.is_empty() {
            if indent.is_empty() {
                return Ok(0);
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
        let token_loc = token.loc;

        if let TokenKind::PhraseContinuation(text) = token.kind {
            if !text.trim().is_empty() {
                fail!(
                    "{module_name}:{token_loc} {text:?} must belong to `-` response or `>` option"
                );
            }

            continue;
        }

        let Ok(token_depth) = get_depth(token.indentation, token_loc) else {
            fail!();
            return;
        };

        macro_rules! collect_whole_phrase {
            ($input:expr) => {{
                let mut p = $input;

                while let Some(Token {
                    kind: TokenKind::PhraseContinuation(text),
                    ..
                }) = tokens.peek()
                {
                    p.push_str(&text);
                    let _ = tokens.next();
                }

                if let Ok(parsed_phrase) = parse_phrase((token_loc, &p), funcs_map, funcs, state) {
                    parsed_phrase
                } else {
                    fail!();
                    Phrase::Static(p)
                }
            }};
        }

        macro_rules! register_state_item {
            ($update_family:expr $(, $item:expr)?) => {{
                if let Some((.., label)) = label_to_assign.take() {
                    let _ = labels_map.insert(label, this_item).is_some();
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
            if label_depth > token_depth {
                fail!(
                    "{module_name}:{token_loc} label must be either a parent of next item to define a block or to be at the same indentation depth as the next item"
                );
                continue;
            }

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

                while let Some(next_t) = tokens.peek() {
                    let Ok(next_t_depth) = get_depth(next_t.indentation, next_t.loc) else {
                        fail!();
                        return;
                    };

                    if label_depth < next_t_depth {
                        block.push(tokens.next().unwrap());
                    } else {
                        break;
                    }
                }

                load_queue.push((module_name.clone(), block));

                continue;
            }
        }

        match token.kind {
            TokenKind::PhraseContinuation(_) => unreachable!(),
            TokenKind::IsolatedBlock(label) => {
                *labels_map.get_mut(&label).unwrap() = module_items.len() + link_offset
            }
            TokenKind::Response(phrase) => {
                let phrase = collect_whole_phrase!(phrase);
                register_state_item!(
                    true,
                    Item::Response {
                        phrase,
                        options: Vec::new(),
                    }
                );
            }
            TokenKind::Option(phrase) => {
                let phrase = collect_whole_phrase!(phrase);
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
                    fail!(
                        "{module_name}:{token_loc} couldn't find an option handler for {phrase:?} on this depth"
                    );
                    continue;
                };

                sibling_idx.push(sibling_idx[opt_handler]);
                parents.push(Some(opt_handler));
                module_items.push(Item::Option { phrase });

                match &mut module_items[opt_handler] {
                    Item::Response { options, .. } => options.push(this_item),
                    Item::Option { .. } => unreachable!(),
                    _ => {
                        fail!(
                            "{module_name}:{token_loc} options can only belong to responses, defined with `-`"
                        );
                        continue;
                    }
                };
            }
            TokenKind::Function {
                kind: kind @ FunctionKind::Jump,
                mut args,
                is_comptime,
            } => {
                if is_comptime {
                    fail!(
                        "{module_name}:{token_loc} `{kind}` cannot use `!` compile time evaluation"
                    )
                }

                if args.len() > 1 {
                    fail!(
                        "{module_name}:{token_loc} too many arguments for `{kind}` function, expected only one label"
                    );
                }

                if !func_signature_is_valid(&module_name, &args, &['.']) {
                    fail!();
                }

                let Some((label_loc, target)) = args.pop() else {
                    fail!("{module_name}:{token_loc} expected label for `{kind}` function");
                    continue;
                };

                let mut parts = target.split('.');
                let jump_target = match (parts.next(), parts.next(), parts.next()) {
                    (.., Some(_)) => {
                        fail!(
                            "{label_loc} invalid item access syntax for label, path may only contain one dot: '<module>.<label>'"
                        );
                        continue;
                    }
                    (Some(label), None, None) => make_label(&module_name, label),
                    (Some(import_name), Some(label), _)
                        if import_name == module_name || import_name.is_empty() =>
                    {
                        make_label(&module_name, label)
                    }
                    (Some(import_name), Some(import_label), _) => {
                        if let Some((_, is_used)) = local_imports
                            .get_mut(&module_name)
                            .and_then(|imports| imports.get_mut(import_name))
                        {
                            *is_used = true;
                        } else {
                            fail!(
                                "{label_loc} in `{module_name}`: `{import_name}` is not imported, so can't access `{import_label}` item"
                            );
                        }

                        let is_loaded = modules
                            .get_mut(import_name)
                            .expect("import is registered in both `modules` and `local_imports`");

                        if !*is_loaded {
                            let src = match crate::utils::read_mur_file(format!(
                                "{import_name}.mur"
                            )) {
                                Ok(s) => s,
                                Err(e) => {
                                    fail!(
                                        "{module_name}:{token_loc} can't jump on imported label '{target}': {e}"
                                    );
                                    continue;
                                }
                            };

                            let import_tokens = tokenize(&src);

                            load_queue.push((import_name.to_string(), import_tokens));
                            *is_loaded = true;
                        }

                        target
                    }
                    _ => unreachable!(),
                };
                assert!(
                    label_jumps
                        .insert(module_items.len() + link_offset, (label_loc, jump_target))
                        .is_none(),
                    "it seems like jump got overwritten, is it even possilbe?"
                );

                register_state_item!(true, Item::Jump(0));
            }
            TokenKind::Function {
                kind: kind @ FunctionKind::End,
                is_comptime,
                args,
            } => {
                if is_comptime {
                    fail!(
                        "{module_name}:{token_loc} `{kind}` cannot use `!` compile time evaluation"
                    )
                }

                if !args.is_empty() {
                    let (loc, _) = args[1];
                    fail!(
                        "{module_name}:{loc} too many arguments for `{kind}` function, expected only one label"
                    );
                }

                register_state_item!(true, Item::End);
            }
            TokenKind::Function {
                kind: kind @ FunctionKind::As,
                is_comptime,
                mut args,
            } => {
                if is_comptime {
                    fail!(
                        "{module_name}:{token_loc} `{kind}` cannot use `!` compile time evaluation"
                    )
                }

                if args.len() > 1 {
                    let (loc, _) = args[1];
                    fail!(
                        "{module_name}:{loc} too many arguments for `{kind}` function, expected only one label"
                    );
                }

                if !func_signature_is_valid(&module_name, &args, &[]) {
                    fail!();
                }

                let Some((label_loc, label)) = args.pop() else {
                    fail!("{module_name}:{token_loc} expected label name for `as` function");
                    continue;
                };

                if label_to_assign.is_some() {
                    fail!("{module_name}:{token_loc} function `as` cannot be labeled");
                }

                if tokens.peek().is_none() {
                    fail!("{module_name}:{token_loc} standalone `as` function is not allowed");
                }

                let name = make_label(&module_name, &label);

                if labels_map.contains_key(&name) {
                    fail!("{module_name}:{label_loc} duplicate label `{label}`");
                }

                label_to_assign = Some((token_depth, name.clone()));
                // register label just to be able to catch redefinition
                // later items will assign their indices to the label
                labels_map.insert(name, 0);
            }
            TokenKind::Function {
                kind: kind @ FunctionKind::Import,
                mut args,
                is_comptime,
            } => {
                if is_comptime {
                    fail!(
                        "{module_name}:{token_loc} `{kind}` cannot use `!` compile time evaluation"
                    )
                }

                if args.len() > 1 {
                    fail!(
                        "{module_name}:{token_loc} too many arguments for `{kind}` function, expected only one module name",
                    );
                }

                if !func_signature_is_valid(&module_name, &args, &[]) {
                    fail!();
                }

                if label_to_assign.is_some() {
                    fail!("{module_name}:{token_loc} cannot assign label to the `{kind}` function",);
                }

                let Some((module_name_loc, module)) = args.pop() else {
                    fail!("{module_name}:{token_loc} expected module name for `{kind}` function",);
                    continue;
                };

                if module == module_name {
                    fail!("{module_name}:{module_name_loc} self import is weird");
                    continue;
                }
                if !modules.contains_key(&module) {
                    modules.insert(module.clone(), false);
                }

                if let Some(imps) = local_imports.get_mut(&module_name) {
                    if let Some((prev_loc, m)) = imps.insert(module, (module_name_loc, false)) {
                        fail!(
                            "{module_name}:{module_name_loc} duplicate import `{m}`, previous import at {prev_loc}",
                        );
                    }
                } else {
                    local_imports.insert(
                        module_name.clone(),
                        [(module, (module_name_loc, false))].into(),
                    );
                }
            }
            TokenKind::Function {
                kind: kind @ (FunctionKind::Hide | FunctionKind::Show),
                mut args,
                is_comptime,
            } => {
                if is_comptime {
                    fail!(
                        "{module_name}:{token_loc} `{kind}` cannot use `!` compile time evaluation"
                    )
                }

                if !func_signature_is_valid(&module_name, &args, &['.']) {
                    fail!();
                    continue;
                }

                register_state_item!(true);

                let target_items = if args.is_empty() {
                    let Some(parent) = parents[this_item] else {
                        fail!(
                            "{module_name}:{token_loc} this `hide` function without arguments affect it's parent, which is not present in our case :(",
                        );
                        continue;
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

                match kind {
                    FunctionKind::Hide => module_items.push(Item::Hide(target_items)),
                    FunctionKind::Show => module_items.push(Item::Show(target_items)),
                    _ => unreachable!(),
                }
            }
            TokenKind::Function {
                kind: FunctionKind::Custom(fn_name),
                args,
                is_comptime,
            } => {
                let func = (token_loc, fn_name);

                let Some(func) = funcs_map.get(func.1.as_str()).copied() else {
                    fail!(
                        "{module_name}:{token_loc} function `{}` is undefined",
                        func.1
                    );
                    continue;
                };

                let args = args.into_iter().map(|(_, a)| a).collect::<Vec<String>>();

                let func_data = FuncData {
                    is_comptime,
                    args,
                    call_location: token_loc,
                };

                if is_comptime {
                    if label_to_assign.is_some() {
                        fail!(
                            "{module_name}:{token_loc} you cannot assign label for compile time function, it will not be present at runtime, bruh, obviously",
                        );
                    }

                    funcs[func].call_drop(state, &func_data);
                }

                register_state_item!(true, Item::FunctionCall { func, func_data })
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

    pub(super) fn as_nonempty_str(&self) -> Option<&str> {
        let s = self.as_str();
        (!s.is_empty()).then_some(s)
    }

    pub(super) fn update<State>(&mut self, funcs: &[Function<State>], state: &mut State) {
        let Phrase::Dynamic { buffer, parts } = self else {
            return;
        };

        buffer.clear();

        for part in parts {
            match part {
                PhraseParts::Static(t) => buffer.push_str(t),
                PhraseParts::FromFunction { func, func_data } => {
                    use std::fmt::Write;

                    let Function::String(f) = &funcs[*func] else {
                        unreachable!();
                    };

                    let _ = write!(buffer, "{}", f(state, func_data));
                }
            }
        }
    }
}

#[derive(Debug)]
pub(crate) enum PhraseParts {
    Static(String),
    FromFunction { func: usize, func_data: FuncData },
}

fn parse_phrase<State>(
    (mut phrase_loc, input): (Loc, &str),
    funcs_map: &HashMap<&'static str, usize>,
    funcs: &[Function<State>],
    state: &mut State,
) -> Result<Phrase, ()> {
    let mut failed = false;
    let mut parts = Vec::<PhraseParts>::new();
    let mut stream = input
        .split_inclusive('\n')
        .flat_map(|line| {
            let mut loc = phrase_loc;
            let chs = line
                .chars()
                .map(move |ch| {
                    let t = (loc, ch);
                    loc.col += 1;
                    t
                })
                .skip_while(|(_, ch)| *ch != '\n' && ch.is_whitespace())
                .skip_while(|(_, ch)| *ch == '\\');

            phrase_loc.line += 1;
            phrase_loc.col = 1;

            chs
        })
        .peekable();

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

        let (func_loc, is_comptime, fn_name) = match collect_func_name_until('}', &mut stream) {
            Ok(f) => f,
            Err(WordErr::ReachedDelimiter | WordErr::EndOfStream) => {
                eprintln!("{next_ch_loc} expected function name within `@{{}}`");
                return Err(());
            }
            Err(WordErr::UnclosedQuotes) => return Err(()),
        };

        let mut func_args = Vec::new();

        loop {
            match collect_word_until('}', &mut stream) {
                Ok(arg) => func_args.push(arg),
                Err(WordErr::EndOfStream) => {
                    eprintln!("{next_ch_loc} could not find '}}' closing delimiter");
                    return Err(());
                }
                Err(WordErr::ReachedDelimiter) => break,
                Err(WordErr::UnclosedQuotes) => return Err(()),
            }
        }

        let FunctionKind::Custom(fn_name) = FunctionKind::from_str(&fn_name) else {
            eprintln!("{func_loc} builtin function `{fn_name}` doesn't return displayable value");
            failed = true;
            continue;
        };

        let Some(func) = funcs_map.get(fn_name.as_str()).copied() else {
            eprintln!("{func_loc} function `{fn_name}` is undefined");
            failed = true;
            continue;
        };

        let args = func_args
            .into_iter()
            .skip(1)
            .map(|(_, a)| a)
            .collect::<Vec<String>>();

        let func_data = FuncData {
            args,
            is_comptime,
            call_location: func_loc,
        };

        if is_comptime {
            let Function::String(f) = &funcs[func] else {
                eprintln!("{func_loc} this function doesn't return a displayable value");
                failed = true;
                continue;
            };

            let output = f(state, &func_data);

            if let Some(PhraseParts::Static(ph)) = parts.last_mut() {
                ph.push_str(&output);
                continue;
            } else {
                parts.push(PhraseParts::Static(output));
            }
        } else {
            if !matches!(funcs[func], Function::String(_)) {
                eprintln!("{func_loc} function doesn't return a displayable value");
                failed = true;
                continue;
            };

            parts.push(PhraseParts::FromFunction { func, func_data });
        }
    }

    if !buf.is_empty() {
        parts.push(PhraseParts::Static(buf));
    }

    if failed {
        return Err(());
    }

    if let Some(PhraseParts::Static(phrase)) = parts.last_mut() {
        crate::utils::trim_end(phrase);
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

// TODO: this function could be removed
//      only used for builtins
//      make separate function to validate label aruments or whatever
fn func_signature_is_valid<'s>(
    module_name: &'s str,
    args: impl IntoIterator<Item = &'s (Loc, String)>,
    exceptions: &'s [char],
) -> bool {
    let mut valid = true;
    let errors = args
        .into_iter()
        .map(|(loc, arg)| (loc, arg.as_str()))
        .flat_map(|(&loc, word)| {
            word.chars().enumerate().filter_map(move |(ch_idx, ch)| {
                (RESTRICTED_CHARS.contains(&ch) && !exceptions.contains(&ch)).then_some((
                    Loc {
                        col: loc.col + ch_idx as u32,
                        ..loc
                    },
                    ch,
                ))
            })
        });

    for (err_loc, err_char) in errors {
        eprintln!(
            "{module_name}:{err_loc} unacceptable character in function signature: '{err_char}'",
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
        if let Item::Option { .. } | Item::End = kind {
            continue;
        }

        // link previous item with this state item
        if let Some(prev_item_link @ None) = item
            .checked_sub(1)
            .filter(|prev_item| !matches!(items[*prev_item], Item::End))
            .map(|prev_item| &mut links[prev_item])
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
            Item::End | Item::Option { .. } => unreachable!(),
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

fn get_depth_from_indent(
    indentation: Indentation,
    single_indent: Indentation,
    loc: Loc,
) -> Result<u32, ()> {
    if let (Indentation::Spaces(_), Indentation::Tabs(_))
    | (Indentation::Tabs(_), Indentation::Spaces(_)) = (indentation, &single_indent)
    {
        eprintln!(
            "{loc} got {} instead of {}",
            indentation.as_str_name(),
            single_indent.as_str_name(),
        );
        return Err(());
    }

    let got_amount = indentation.get_amount();
    let single_amount = single_indent.get_amount();
    let indent = single_indent.as_str_name();

    if got_amount % single_amount != 0 {
        eprintln!(
            "{loc} inconsistent amount of {indent}: expected {single_amount} as a single_indent, but got {got_amount} total"
        );
        return Err(());
    }

    Ok(got_amount / single_amount)
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
