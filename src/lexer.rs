use crate::FunctionMap;
use std::{collections::HashMap, fmt, iter::Peekable};

const FUNCTION_CHAR: char = '#';
const FUNCTION_ONCE_EXEC_CHAR: char = '!';
const FUNCTION_COMMENT_CHAR: char = FUNCTION_CHAR;
const RESPONSE_CHAR: char = '-';
const OPTION_CHAR: char = '>';

const TAB: char = '\t';
const SPACE: char = ' ';

#[derive(Copy, Clone, Debug)]
struct Indentation {
    kind: char,
    depth: u32,
}

impl Indentation {
    fn empty() -> Self {
        Self {
            kind: SPACE,
            depth: 0,
        }
    }

    fn as_str_name(&self) -> &str {
        match self.kind {
            SPACE if self.depth > 1 => "spaces",
            SPACE => "space",
            TAB if self.depth > 1 => "tabs",
            TAB => "tab",
            _ => unreachable!(),
        }
    }
}

impl Default for Indentation {
    fn default() -> Self {
        Self {
            kind: SPACE,
            depth: 4,
        }
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
        once: bool,
    },
    Response(String), // `-` - npc's response phrase in conversation
    Choice(String),   // `>` - player's option in conversation
    Comment,
}

macro_rules! funcs_registry {
    (
        $( $builtin:ident = $builtin_str:expr, )*
    ) => {
        #[derive(Debug, Clone)]
        pub(super) enum FunctionKind {
            $( $builtin ,)*
            Custom(String),
        }

        impl FunctionKind {
            const BUILTINS: &[FunctionKind] = &[$( FunctionKind::$builtin, )*];
            pub(super) const BUILTINS_STR: &[&str] = &[$( $builtin_str, )*];

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
    If = "if",
    Elif = "elif",
    Else = "else",
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
    for (_, ch) in stream {
        if ch == end {
            return Err(WordErr::ReachedDelimiter);
        } else if ch == '\n' {
            return Ok(());
        }
    }

    Err(WordErr::EndOfStream)
}

fn skip_func_trash_until(
    end: char,
    stream: &mut Peekable<impl Iterator<Item = (Loc, char)>>,
) -> Result<(), WordErr> {
    while let Some((_, ch)) = stream.peek().copied() {
        if ch == end {
            let _ = stream.next();
            return Err(WordErr::ReachedDelimiter);
        } else if ch == FUNCTION_COMMENT_CHAR {
            skip_comment_until(end, stream)?;
        } else if !ch.is_whitespace() {
            return Ok(());
        }

        let _ = stream.next();
    }

    Err(WordErr::EndOfStream)
}

#[allow(clippy::type_complexity)]
fn collect_func_until(
    end: char,
    stream: &mut Peekable<impl Iterator<Item = (Loc, char)>>,
) -> Result<(Loc, bool, String, Vec<(Loc, String)>), WordErr> {
    skip_func_trash_until(end, stream)?;

    let once = stream
        .next_if(|(_, ch)| *ch == FUNCTION_ONCE_EXEC_CHAR)
        .is_some();

    let (fn_loc, fn_name) = collect_word_until(end, stream)?;
    let mut args = Vec::<(Loc, String)>::new();

    loop {
        let arg = match collect_word_until(end, stream) {
            Ok(a) => a,
            Err(WordErr::EndOfStream | WordErr::ReachedDelimiter) => {
                break;
            }
            Err(err) => return Err(err),
        };

        args.push(arg);
    }

    Ok((fn_loc, once, fn_name, args))
}

fn collect_quoted_word_into(
    buf: &mut String,
    stream: &mut Peekable<impl Iterator<Item = (Loc, char)>>,
) -> Result<(), WordErr> {
    let Some((word_loc, '"')) = stream.next() else {
        unreachable!("hey, bruh");
    };

    let mut closed = false;
    let mut end_loc = word_loc;

    while let Some((ch_loc, ch)) = stream.next() {
        end_loc = ch_loc;
        if ch == '"' {
            closed = true;
            break;
        } else if ch == '\\' {
            let Some((_, next_ch)) = stream.next() else {
                break;
            };
            buf.push(next_ch);
        } else if ch == '\n' {
            buf.push(ch);
            // trim indent
            while stream
                .next_if(|(_, ch)| *ch != '\n' && ch.is_whitespace())
                .is_some()
            {}
        } else {
            buf.push(ch);
        }
    }

    if closed {
        Ok(())
    } else {
        eprintln!("{end_loc} couldn't find closing '\"' for one at {word_loc}");
        Err(WordErr::UnclosedQuotes)
    }
}

fn collect_word_until(
    end: char,
    stream: &mut Peekable<impl Iterator<Item = (Loc, char)>>,
) -> Result<(Loc, String), WordErr> {
    skip_func_trash_until(end, stream)?;

    let mut word = String::new();
    let mut word_loc = None::<Loc>;

    // this will allow to collect empty string via quotes ""
    let mut quoted_word_collected = false;
    let break_reason = loop {
        let Some(&(loc, ch)) = stream.peek() else {
            break WordErr::EndOfStream;
        };

        if word_loc.is_none() {
            word_loc = Some(loc);
        }

        if ch == end || ch.is_whitespace() {
            break WordErr::ReachedDelimiter;
        } else if ch == '"' {
            collect_quoted_word_into(&mut word, stream)?; // short circuit UnclosedQuotes error
            quoted_word_collected = true;
        } else if ch == '\\' {
            let _ = stream.next();
            let Some((_, next_ch)) = stream.next() else {
                break WordErr::EndOfStream;
            };

            word.push(next_ch);
        } else if ch == FUNCTION_COMMENT_CHAR {
            let _ = stream.next();
            if let Err(e) = skip_comment_until(end, stream) {
                break e;
            }
        } else {
            let _ = stream.next();
            word.push(ch);
        }
    };

    if quoted_word_collected || !word.is_empty() {
        Ok((word_loc.unwrap(), word))
    } else {
        Err(break_reason)
    }
}

fn collect_phrase_into(
    buffer: &mut String,
    stream: &mut Peekable<impl Iterator<Item = (Loc, char)>>,
) -> Option<Loc> {
    while stream.next_if(|&(_, ch)| ch.is_whitespace()).is_some() {}
    let loc = stream.peek().copied().map(|(l, _)| l)?;

    for (_, ch) in stream {
        // NOTE: End is not trimmed, because the phrases may be combined in the future: Response/Option + PhraseContinuation
        //       So the result Option or Response in the state will most likely contains a newline, which will be removed on parsing stage
        buffer.push(ch);
        if ch == '\n' {
            break;
        }
    }

    Some(loc)
}

fn collect_indent(stream: &mut Peekable<impl Iterator<Item = (Loc, char)>>) -> Option<Indentation> {
    let mut depth = 0;
    let (_, kind) = stream.peek().copied()?;

    if !(kind == SPACE || kind == TAB) {
        return Some(Indentation::empty());
    }

    while stream.next_if(|t| t.1 == kind).is_some() {
        depth += 1;
    }

    Some(Indentation { kind, depth })
}

pub(super) fn tokenize(module_name: &str, src: &str) -> Result<Vec<Token>, ()> {
    let mut tokens = Vec::new();
    let mut loc = Loc { line: 1, col: 1 };
    let mut failed = false;

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
        .skip_while(|t| t.1 == '\n')
        .peekable();

    loop {
        let Some(indentation) = collect_indent(&mut stream) else {
            break;
        };

        let Some((ch_loc, ch)) = stream.next() else {
            break;
        };

        let token = match ch {
            FUNCTION_CHAR if stream.peek().is_some_and(|(_, next_ch)| *next_ch != '{') => {
                if let Ok((fn_loc, once, fn_name, args)) = collect_func_until('\n', &mut stream) {
                    let kind = FunctionKind::from_str(&fn_name);
                    Some((fn_loc, TokenKind::Function { kind, args, once }))
                } else {
                    Some((ch_loc, TokenKind::Comment))
                }
            }
            OPTION_CHAR => {
                let mut phrase = String::new();
                collect_phrase_into(&mut phrase, &mut stream)
                    .map(|_| (ch_loc, TokenKind::Choice(phrase)))
            }
            RESPONSE_CHAR => {
                let mut phrase = String::new();
                collect_phrase_into(&mut phrase, &mut stream)
                    .map(|_| (ch_loc, TokenKind::Response(phrase)))
            }
            '\n' => {
                if let Some(Token {
                    kind: TokenKind::Choice(p) | TokenKind::Response(p),
                    ..
                }) = tokens.last_mut()
                {
                    p.push(ch);
                }

                continue;
            }
            _ => {
                let Some(Token {
                    kind: TokenKind::Choice(p) | TokenKind::Response(p),
                    ..
                }) = tokens.last_mut()
                else {
                    eprintln!(
                        "{module_name}:{ch_loc} this text must belong to `-` response or `>` choice"
                    );

                    failed = true;
                    // skip err line as if it was a comment ahhahahha
                    if let Err(WordErr::EndOfStream) = skip_comment_until('\n', &mut stream) {
                        break;
                    }

                    continue;
                };

                for _ in 0..indentation.depth {
                    p.push(indentation.kind)
                }

                p.push(ch);

                let _ = collect_phrase_into(p, &mut stream);

                continue;
            }
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

    if failed { Err(()) } else { Ok(tokens) }
}

#[derive(Debug)]
pub(super) enum Item {
    End,
    Response {
        phrase: Phrase,
        choices: Vec<usize>,
    },
    Choice {
        phrase: Phrase,
    },
    Jump {
        once: bool,
        target: usize,
    },
    Hide {
        once: bool,
        targets: Vec<usize>,
    },
    Show {
        once: bool,
        targets: Vec<usize>,
    },
    FunctionCall {
        func_data: FuncData,
        func: usize,
    },
    Block {
        guarded_choices: Option<Vec<usize>>,
        next: Option<usize>,
        kind: BlockKind,
    },
}

#[derive(Debug)]
pub(super) enum Condition {
    AlwaysShown,
    AlwaysHidden,
    Check(usize, FuncData, Option<usize>),
}

#[derive(Debug)]
pub(super) struct ItemVIsibility {
    pub(super) manual: bool,
    pub(super) condition: Condition,
}

impl Default for ItemVIsibility {
    fn default() -> Self {
        Self {
            manual: true,
            condition: Condition::AlwaysShown,
        }
    }
}

#[derive(Debug)]
pub(super) enum BlockKind {
    If,
    Elif, // 'elif' without condition is basically 'else'
    LabeledBlock,
}

#[derive(Debug)]
pub(super) struct FuncData {
    pub(super) once: bool,
    pub(super) call_location: Loc,
    pub(super) args: Vec<String>,
}

impl FuncData {
    pub(super) fn as_context<'a, State>(&'a self, state: &'a mut State) -> Context<'a, State> {
        Context {
            state,
            args: &self.args,
            call_location: self.call_location,
            once: self.once,
        }
    }
}

pub struct Context<'a, State> {
    pub state: &'a mut State,
    pub args: &'a [String],
    pub call_location: Loc,
    pub once: bool,
}

pub(super) type Func<State, Ret> = dyn Fn(Context<State>) -> Ret;

pub enum Function<State> {
    Bool(Box<Func<State, bool>>),
    String(Box<Func<State, String>>),
    Nothing(Box<Func<State, ()>>),
}

impl<State> Function<State> {
    pub(super) fn call_drop(&self, ctx: Context<State>) {
        match self {
            Function::String(f) => {
                f(ctx);
            }
            Function::Bool(f) => {
                f(ctx);
            }
            Function::Nothing(f) => {
                f(ctx);
            }
        }
    }
}

// generic is a trick to avoid trait collision because rust is safe and blazingly fast..
pub trait IntoFunction<State, T> {
    fn into_function(self) -> Function<State>;
}

impl<State, F> IntoFunction<State, ()> for F
where
    F: Fn(Context<State>) + 'static,
{
    fn into_function(self) -> Function<State> {
        Function::Nothing(Box::new(self))
    }
}
impl<State, F> IntoFunction<State, String> for F
where
    F: Fn(Context<State>) -> String + 'static,
{
    fn into_function(self) -> Function<State> {
        Function::String(Box::new(self))
    }
}

impl<State, F> IntoFunction<State, bool> for F
where
    F: Fn(Context<State>) -> bool + 'static,
{
    fn into_function(self) -> Function<State> {
        Function::Bool(Box::new(self))
    }
}

impl<State, T> IntoFunction<State, T>  for Function<State> {
    fn into_function(self) -> Function<State> {
        self
    }
}

#[derive(Default)]
struct Parser {
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
    vis: Vec<ItemVIsibility>,
    failed: bool,
}

#[allow(clippy::type_complexity)]
pub(super) fn parse_tokens_into_items<State>(
    main_module: &str,
    tokens: Vec<Token>,
    funcs_map: &FunctionMap<State>,
) -> Result<(Vec<Item>, Vec<ItemVIsibility>, Vec<Option<usize>>), ()> {
    let mut parser = Parser::default();

    // TODO: avoid second allocation for the name
    parser.modules.insert(main_module.into(), true);
    parser.load_queue.push((main_module.into(), tokens));

    while !parser.load_queue.is_empty() {
        parse_module(&mut parser, funcs_map);
    }

    for (module, imports) in parser.local_imports {
        for (imp, (loc, is_used)) in imports {
            if !is_used {
                eprintln!("{module}:{loc} import `{imp}` was never referensed anywhere");
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

        if let Item::Choice { .. }
        | Item::Block {
            guarded_choices: Some(_),
            ..
        } = parser.items[target_item]
        {
            eprintln!("{loc} jumping on options is not allowed :(");
            parser.failed = true;
            continue;
        }

        let Item::Jump { target, .. } = &mut parser.items[jump_item] else {
            unreachable!()
        };

        if target_item == jump_item {
            eprintln!("{loc} you are creating infinite loop by jumping on `jump` itself");
            parser.failed = true;
            continue;
        }

        *target = target_item;
    }

    for (vis_change_item, labels) in parser.label_vis_changes {
        let (Item::Hide { targets, .. } | Item::Show { targets, .. }) =
            &mut parser.items[vis_change_item]
        else {
            unreachable!();
        };

        for (loc, label) in labels {
            let Some(target_item) = parser.labels_map.get(&label) else {
                eprintln!("{loc} label doesn't exist: {label}");
                parser.failed = true;
                continue;
            };

            targets.push(*target_item);
        }
    }

    if parser.failed {
        Err(())
    } else {
        Ok((parser.items, parser.vis, parser.links))
    }
}

fn parse_module<State>(
    Parser {
        load_queue,
        modules,
        local_imports,
        single_indentation,
        labels_map,
        label_jumps,
        label_vis_changes,
        items,
        links,
        vis,
        failed,
    }: &mut Parser,
    funcs_map: &FunctionMap<State>,
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
        if single_indentation.depth == 0 {
            if indent.depth == 0 {
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
    let mut item_loc = Vec::<Loc>::new();
    let mut item_vis = Vec::<ItemVIsibility>::new();

    let mut module_items = Vec::<Item>::new();
    let mut label_to_assign = None::<String>;
    let mut tokens = tokens
        .into_iter()
        .filter(|t| !matches!(t.kind, TokenKind::Comment))
        .peekable();

    'parsing_loop: while let Some(token) = tokens.next() {
        let this_item = module_items.len() + link_offset;
        let token_loc = token.loc;

        let Ok(token_depth) = get_depth(token.indentation, token_loc) else {
            fail!();
            return;
        };

        macro_rules! register_state_item {
            ($update_family:expr $(, $item:expr)?) => {{
                if let Some(label) = label_to_assign.take() {
                    let _ = labels_map.insert(label, this_item).is_some();
                }

                item_depth.push(token_depth);
                item_loc.push(token_loc);
                item_vis.push(ItemVIsibility::default());

                if $update_family {
                    let this = module_items.len();
                    welcome_to_the_family(
                        this,
                        &mut parents,
                        &mut sibling_idx,
                        &item_depth,
                    );
                }

                $(module_items.push($item);)?
            }};
        }

        match token.kind {
            TokenKind::Comment => unreachable!(),
            TokenKind::Response(phrase) => {
                // phrase phrase phrase phrase phrase phrase
                let phrase = parse_phrase(&module_name, (token_loc, &phrase), funcs_map)
                    .unwrap_or_else(|_| {
                        fail!();
                        Phrase::Static(phrase)
                    });

                register_state_item!(
                    true,
                    Item::Response {
                        phrase,
                        choices: Vec::new(),
                    }
                );
            }
            TokenKind::Choice(phrase) => {
                let phrase = parse_phrase(&module_name, (token_loc, &phrase), funcs_map)
                    .unwrap_or_else(|_| {
                        fail!();
                        Phrase::Static(phrase)
                    });

                register_state_item!(false);

                let mut search_depth = item_depth[this_item];
                let mut choice_idx_to_push = Some(this_item);

                let handler_search_result = (0..module_items.len()).rev().find_map(|prev_item| {
                    let prev_item_depth = item_depth[prev_item];
                    if let Item::Block {
                        guarded_choices,
                        kind,
                        ..
                    } = &mut module_items[prev_item]
                    {
                        if search_depth < prev_item_depth {
                            return None;
                        } else if search_depth == prev_item_depth && prev_item + 1 != this_item {
                            if guarded_choices.is_none() {
                                return Some(Err(()));
                            }

                            return None;
                        }

                        guarded_choices.get_or_insert_default().push(this_item);

                        // TODO: this may be overwritten??
                        choice_idx_to_push = if matches!(kind, BlockKind::Elif) {
                            None
                        } else {
                            Some(prev_item)
                        };

                        search_depth = prev_item_depth;
                        None
                    } else if prev_item_depth < search_depth {
                        Some(Err(()))
                    } else if prev_item_depth > search_depth {
                        None
                    } else if let Item::Choice { .. } = &module_items[prev_item] {
                        None
                    } else {
                        Some(Ok(prev_item))
                    }
                });

                module_items.push(Item::Choice { phrase });

                let opt_handler = match handler_search_result {
                    Some(Ok(h)) => h,
                    Some(Err(())) | None => {
                        fail!(
                            "{module_name}:{token_loc} couldn't find a response for this choice on this depth"
                        );
                        continue;
                    }
                };

                sibling_idx.push(sibling_idx[opt_handler]);
                parents.push(Some(opt_handler));

                if let Item::Response { choices, .. } = &mut module_items[opt_handler] {
                    if let Some(idx) = choice_idx_to_push {
                        choices.push(idx);
                    }
                } else {
                    fail!(
                        "{module_name}:{token_loc} options can only belong to responses, defined with `-`"
                    );
                }
            }
            TokenKind::Function {
                kind: kind @ FunctionKind::Jump,
                mut args,
                once,
            } => {
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

                            let Ok(import_tokens) = tokenize(import_name, &src) else {
                                fail!();
                                continue;
                            };

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

                register_state_item!(true, Item::Jump { target: 0, once });
            }
            TokenKind::Function {
                kind: kind @ FunctionKind::End,
                once,
                args,
            } => {
                if once {
                    fail!(
                        "{module_name}:{token_loc} `{kind}` doesn't support `once` evaluation, cause why lol?"
                    )
                }

                if !args.is_empty() {
                    let (loc, _) = args[1];
                    fail!("{module_name}:{loc} `{kind}` doesn't expect any arguments");
                }

                register_state_item!(true, Item::End);
            }
            TokenKind::Function {
                kind: kind @ FunctionKind::As,
                once,
                mut args,
            } => {
                if once {
                    fail!(
                        "{module_name}:{token_loc} `{kind}` is compile time function, it doesn't support `once` evaluation"
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
                    fail!("{module_name}:{token_loc} expected label name for `{kind}` function");
                    continue;
                };

                if label_to_assign.is_some() {
                    fail!("{module_name}:{token_loc} function `{kind}` cannot be labeled");
                }

                if tokens.peek().is_none() {
                    fail!("{module_name}:{token_loc} standalone `{kind}` function is not allowed");
                }

                let name = make_label(&module_name, &label);

                if labels_map.contains_key(&name) {
                    fail!("{module_name}:{label_loc} duplicate label `{label}`");
                }

                let Some(next_t) = tokens.peek() else {
                    fail!("{module_name}:{token_loc} `{kind}` label doesn't belong to any item");
                    continue;
                };

                let Ok(next_t_depth) = get_depth(next_t.indentation, next_t.loc) else {
                    fail!();
                    return;
                };

                label_to_assign = Some(name);

                if next_t_depth > token_depth {
                    register_state_item!(
                        true,
                        Item::Block {
                            kind: BlockKind::LabeledBlock,
                            guarded_choices: None,
                            next: None,
                        }
                    );
                }
            }
            TokenKind::Function {
                kind: kind @ FunctionKind::Import,
                mut args,
                once,
            } => {
                if once {
                    fail!(
                        "{module_name}:{token_loc} `{kind}` is compile time function, it doesn't support `once` evaluation"
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
                once,
            } => {
                if !func_signature_is_valid(&module_name, &args, &['.']) {
                    fail!();
                    continue;
                }

                register_state_item!(true);

                let targets = if args.is_empty() {
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
                    FunctionKind::Hide => module_items.push(Item::Hide { targets, once }),
                    FunctionKind::Show => module_items.push(Item::Show { targets, once }),
                    _ => unreachable!(),
                }
            }
            TokenKind::Function {
                kind: kind @ (FunctionKind::If | FunctionKind::Elif | FunctionKind::Else),
                args,
                once,
            } => {
                let (cond_kind, check) = if let FunctionKind::If | FunctionKind::Elif = kind {
                    if once {
                        fail!(
                            "{module_name}:{token_loc} `{kind}` is compile time function, it doesn't support `once` evaluation. You may want to make the condition function `once`: '#if !condition ...'"
                        )
                    }

                    let mut args = args.into_iter();
                    let mut once = false;
                    let mut func = None::<usize>;

                    for (fn_loc, fn_name) in args.by_ref() {
                        if fn_name.as_str() == "!" {
                            once = true;
                            continue;
                        }

                        let Some(&func_idx) = funcs_map.indices.get(fn_name.as_str()) else {
                            fail!("{module_name}:{fn_loc} function `{fn_name}` is undefined");
                            continue 'parsing_loop;
                        };

                        if !matches!(&funcs_map.funcs[func_idx], Function::Bool(_)) {
                            fail!(
                                "{module_name}:{fn_loc} function `{fn_name}` must return boolean value for `{kind}`"
                            );
                            continue 'parsing_loop;
                        }

                        func = Some(func_idx);
                        break;
                    }

                    let Some(func) = func else {
                        fail!(
                            "{module_name}:{token_loc} expected condition function name for `{kind}`"
                        );
                        continue;
                    };

                    let args = args.map(|(_, a)| a).collect::<Vec<String>>();

                    let func_data = FuncData {
                        once,
                        args,
                        call_location: token_loc,
                    };

                    let cond_kind = match kind {
                        FunctionKind::If => BlockKind::If,
                        FunctionKind::Elif => BlockKind::Elif,
                        _ => unreachable!(),
                    };

                    (cond_kind, Condition::Check(func, func_data, None))
                } else {
                    if let Some((arg_loc, _)) = args.first() {
                        fail!("{module_name}:{arg_loc} `{kind}` doesn't expect any arguments");
                        continue;
                    }

                    (BlockKind::Elif, Condition::AlwaysShown)
                };

                if let BlockKind::Elif = cond_kind {
                    // TODO: this is better than with sibling_idx? and it is the same as for Kind::Choice search
                    let Some(Ok(prev_sibling)) =
                        (0..module_items.len()).rev().find_map(|prev_item| {
                            if token_depth > item_depth[prev_item] {
                                Some(Err(()))
                            } else if token_depth < item_depth[prev_item] {
                                None
                            } else if let Item::Choice { .. } = &module_items[prev_item] {
                                None
                            } else {
                                Some(Ok(prev_item))
                            }
                        })
                    else {
                        fail!(
                            "{module_name}:{token_loc} couldn't find condition start for this `{kind}`"
                        );
                        continue;
                    };

                    if let Condition::Check(.., fallback_condition) =
                        &mut item_vis[prev_sibling].condition
                    {
                        *fallback_condition = Some(this_item);
                    } else if let Some(Condition::Check(.., fallback_condition)) = prev_sibling
                        .checked_sub(1)
                        .map(|it| &mut item_vis[it].condition)
                    {
                        *fallback_condition = Some(this_item);
                    } else {
                        fail!(
                            "{module_name}:{token_loc} couldn't find condition start for this `{kind}`"
                        );
                        continue;
                    }
                }

                // TODO: this is not need to be block if holds only one item (defined with the same indentation)

                register_state_item!(
                    true,
                    Item::Block {
                        kind: cond_kind,
                        guarded_choices: None,
                        next: None,
                    }
                );

                item_vis[this_item].condition = check;
            }
            TokenKind::Function {
                kind: FunctionKind::Custom(fn_name),
                args,
                once,
            } => {
                let Some(func) = funcs_map.indices.get(fn_name.as_str()).copied() else {
                    fail!("{module_name}:{token_loc} function `{fn_name}` is undefined",);
                    continue;
                };

                let args = args.into_iter().map(|(_, a)| a).collect::<Vec<String>>();

                let func_data = FuncData {
                    once,
                    args,
                    call_location: token_loc,
                };

                register_state_item!(true, Item::FunctionCall { func, func_data })
            }
        }
    }

    let items_len = module_items.len();
    for this_item in 0..items_len {
        let Item::Block {
            guarded_choices,
            kind,
            ..
        } = &module_items[this_item]
        else {
            continue;
        };

        let guards_choices = guarded_choices.is_some();
        let next_item = this_item + 1;

        if module_items.get(next_item).is_none_or(|_| {
            item_depth[next_item] < item_depth[this_item]
                || (item_depth[next_item] == item_depth[this_item]
                    && matches!(&item_vis[next_item].condition, Condition::Check(..)))
        }) {
            fail!(
                "{module_name}:{loc} condition doesn't contain any item",
                loc = item_loc[this_item],
            )
        }

        if guarded_choices.is_some() {
            for next_item in this_item + 1..items_len {
                if item_depth[next_item] <= item_depth[this_item] {
                    break;
                }

                if !matches!(&module_items[next_item], Item::Choice { .. }) {
                    fail!(
                        "{module_name}:{loc} block that starts with choice at {block_start_loc} must hold only choices",
                        loc = item_loc[next_item],
                        block_start_loc = item_loc[this_item]
                    );
                }
            }
        }

        let (BlockKind::If, Condition::Check(.., fallback_condition)) =
            (kind, &item_vis[this_item].condition)
        else {
            continue;
        };

        let mut next_condition = *fallback_condition;
        while let Some(cond_item) = next_condition {
            let Item::Block {
                guarded_choices: fallback_choices,
                ..
            } = &module_items[cond_item]
            else {
                unreachable!();
            };

            if guards_choices != fallback_choices.is_some() {
                let ttype = if guards_choices {
                    "guard choices"
                } else {
                    "hold state items"
                };
                fail!(
                    "{module_name}:{loc} this condition branch doesn't {ttype} as it's 'if' at {if_start_loc}",
                    loc = item_loc[cond_item],
                    if_start_loc = item_loc[this_item]
                )
            }

            if let Condition::Check(.., fallback_condition) = &mut item_vis[cond_item].condition {
                next_condition = *fallback_condition;
            } else {
                break;
            }
        }
    }

    if *failed {
        return;
    }

    let module_links = link_state_items(
        link_offset,
        &mut module_items,
        &item_depth,
        &item_vis,
        &parents,
        &sibling_idx,
    );

    links.extend(module_links);
    items.extend(module_items);
    vis.extend(item_vis);
}

#[derive(Debug)]
pub(super) enum Phrase {
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

        for part in parts.iter_mut() {
            match part {
                PhraseParts::Static(t) => buffer.push_str(t),
                PhraseParts::FromFunction { func, func_data } => {
                    use std::fmt::Write;

                    let Function::String(f) = &funcs[*func] else {
                        unreachable!();
                    };

                    let output = f(func_data.as_context(state));
                    let _ = write!(buffer, "{output}");

                    if func_data.once {
                        *part = PhraseParts::Static(output);
                    }
                }
            }
        }

        if parts.iter().all(|p| matches!(p, PhraseParts::Static(_))) {
            *self = Phrase::Static(std::mem::take(buffer));
        }
    }
}

#[derive(Debug)]
pub(super) enum PhraseParts {
    Static(String),
    FromFunction { func: usize, func_data: FuncData },
}

fn parse_phrase<State>(
    module_name: &str,
    (mut phrase_loc, input): (Loc, &str),
    funcs_map: &FunctionMap<State>,
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
                .skip_while(|&(_, ch)| ch != '\n' && ch.is_whitespace());

            phrase_loc.line += 1;
            phrase_loc.col = 1;

            chs
        })
        .peekable();

    let mut buf = String::new();
    while let Some((_, ch)) = stream.next() {
        if ch == '\\' {
            let Some((_, next_ch)) = stream.next() else {
                break;
            };

            buf.push(next_ch);
            continue;
        } else if ch != FUNCTION_CHAR {
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

        let (func_loc, once, fn_name, args) = match collect_func_until('}', &mut stream) {
            Ok(f) => f,
            Err(WordErr::EndOfStream) => {
                eprintln!("{module_name}:{next_ch_loc} could not find '}}' closing delimiter");
                return Err(());
            }
            Err(WordErr::ReachedDelimiter) => {
                eprintln!("{module_name}:{next_ch_loc} expected function name within `@{{}}`");
                return Err(());
            }
            Err(WordErr::UnclosedQuotes) => return Err(()),
        };

        let FunctionKind::Custom(fn_name) = FunctionKind::from_str(&fn_name) else {
            eprintln!("{func_loc} builtin function `{fn_name}` doesn't return displayable value");
            failed = true;
            continue;
        };

        let Some(func) = funcs_map.indices.get(fn_name.as_str()).copied() else {
            eprintln!("{func_loc} function `{fn_name}` is undefined");
            failed = true;
            continue;
        };

        let args = args
            .into_iter()
            .skip(1)
            .map(|(_, a)| a)
            .collect::<Vec<String>>();

        let func_data = FuncData {
            args,
            once,
            call_location: func_loc,
        };

        if !matches!(&funcs_map.funcs[func], Function::String(_)) {
            eprintln!("{func_loc} function doesn't return a displayable value");
            failed = true;
            continue;
        };

        parts.push(PhraseParts::FromFunction { func, func_data });
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
    item_depth: &[u32],
    item_vis: &[ItemVIsibility],
    parents: &[Option<usize>],
    sibling_idx: &[u32],
) -> Vec<Option<usize>> {
    let mut links = vec![None::<usize>; items.len()];
    let mut states = Vec::<usize>::new();
    let items_len = items.len();

    for item in 0..items_len {
        if let Item::Choice { .. }
        | Item::End
        | Item::Block {
            guarded_choices: Some(_),
            ..
        }
        | Item::Block {
            kind: BlockKind::Elif,
            ..
        } = &items[item]
        {
            continue;
        }

        // link previous item with this state item
        if let Some(prev_item) = item
            .checked_sub(1)
            .filter(|prev_item| !matches!(items[*prev_item], Item::End))
        {
            let link = Some(item + link_offset);

            if let Item::Block { next, .. } = &mut items[prev_item] {
                *next = link;
            } else if let prev_link @ None = &mut links[prev_item] {
                *prev_link = link;
            };
        }

        states.push(item);
    }

    for i in 0..states.len() {
        let this_state = states[i];

        let (search_start, maybe_options) = match &items[this_state] {
            Item::Response { choices, .. } => (i, Some(choices)),
            Item::Block { .. } => {
                // TODO: if it is a single item block don't push the Block item lol
                let start = if item_depth[this_state] == item_depth[this_state + 1] {
                    i + 1
                } else {
                    i
                };

                (start, None)
            }
            Item::FunctionCall { .. }
            | Item::Jump { .. }
            | Item::Hide { .. }
            | Item::Show { .. } => (i, None),
            Item::End | Item::Choice { .. } => unreachable!(),
        };

        if links[this_state].is_some()
            && (maybe_options.is_none()
                || maybe_options.is_some_and(|opts| opts.iter().all(|&o| links[o].is_some())))
        {
            continue;
        }

        let found_next_item = find_next_state_long_long_way(
            &states[search_start..],
            items,
            parents,
            sibling_idx,
            item_depth,
        )
        .map(|item_idx| item_idx + link_offset);

        if let link @ None = &mut links[this_state] {
            *link = found_next_item;
        }

        if let Item::Block {
            kind: BlockKind::If,
            ..
        } = &items[this_state]
            && let Condition::Check(.., fallback_condition) = &item_vis[this_state].condition
        {
            let mut next_condition = *fallback_condition;
            while let Some(cond_item) = next_condition {
                let cond_link = &mut links[cond_item];
                assert!(cond_link.is_none(), "{cond_link:?}");
                *cond_link = found_next_item;

                if let Condition::Check(.., fallback_condition) = &item_vis[cond_item].condition {
                    next_condition = *fallback_condition;
                } else {
                    break;
                }
            }
        }

        let Some(options) = maybe_options else {
            continue;
        };

        for &opt in options {
            let next @ None = &mut links[opt] else {
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
    if let (SPACE, TAB) | (TAB, SPACE) = (indentation.kind, single_indent.kind) {
        eprintln!(
            "{loc} got {} instead of {}",
            indentation.as_str_name(),
            single_indent.as_str_name(),
        );
        return Err(());
    }

    let got_amount = indentation.depth;
    let single_amount = single_indent.depth;
    let indent = single_indent.as_str_name();

    if got_amount % single_amount != 0 {
        eprintln!(
            "{loc} inconsistent amount of {indent}: expected {single_amount} as a single, but got {got_amount} total"
        );
        return Err(());
    }

    Ok(got_amount / single_amount)
}

fn welcome_to_the_family(
    new_item: usize,
    parents: &mut Vec<Option<usize>>,
    sibling_idx: &mut Vec<u32>,
    item_depth: &[u32],
) {
    let mut sib_idx = 0;
    let mut parent = None::<usize>;

    if let Some(prev_item) = new_item.checked_sub(1) {
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
    items: &[Item],
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
            } else if sibling_idx[current] + 1 == sibling_idx[*state]
                && !matches!(
                    items[current],
                    Item::Block {
                        kind: BlockKind::Elif,
                        ..
                    }
                )
            {
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
