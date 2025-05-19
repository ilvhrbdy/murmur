mod lexer;
mod utils;

use lexer::StateItem;
use std::path::Path;

// TODO: `@def <temlate name> [$args] <block>` macro and expanding it with
//      @ <module>.<template> for expanding macro from other module
//      @ .<tempate> for current module
// same for interpolation @{ .<template> }
// so it expands either as text or code depending on the context of expansion
// @def string Aboba
// or
// @def custom_jump $name @jump $name
// or
// @def block $opt
//      - aboba
//      > $opt
// TODO:
// enum Phrase {
//     Static(String),
//     Dynamic {
//         buffer: String,
//         parts: Vec<PhraseParts>
//     }
// }

// enum PhraseParts {
//     Static(String),
//     FromFunction(usize),
// }
// TODO:
//      @ hide [zero or miltiple space separated args]
//      and
//      @ show [zero or miltiple space separated args]
//
//      that will manipulate the visibility of the item
//      hide or show without arguments is going to operate on previous item, so something like this is possible:
//          @as lbl
//          - What?
//          > Nothing
//              @hide
//              @jump lbl # jumping back to this state, but option will be excluded
//
// TODO: compile time "once" function execution with `@ !func` or `@{ !func }`
// TODO: @if @elif @else
// TODO: function mapping and calling via `@ func` or @{ func } or comp time version with "!"
// TODO: @{} - text expansion function (either text of labeled item or function)
//      in this case we cannot have labeled item and function with the same name
// TODO: the `main` module is always available even without @import and it kind of counter-intuitive. should i fix that?
// TODO: write tests, please?
// TODO: normal error messages
// TODO: we could warn for some weird behavior like this:
//
//      @as aboba
//      @jump aboba
//
//      which is a valid murmur syntax, but just an infinite loop
//      maybe i need to be strict with this kind of shit?

#[derive(Debug)]
pub struct Conversation {
    items: Vec<StateItem>,
    links: Vec<Option<usize>>,

    current: usize,
}

impl Conversation {
    pub fn load(path: impl AsRef<Path>) -> Result<Self, ()> {
        let path = path.as_ref();
        let file_name = path.file_name().unwrap().to_str().unwrap();
        let module_name = file_name.strip_suffix(".mur").unwrap_or(file_name);
        let src = utils::read_mur_file(module_name)?;

        Self::from_source(src, module_name)
    }

    pub fn from_source(src: impl AsRef<str>, module_name: &str) -> Result<Self, ()> {
        let tokens = lexer::tokenize(src.as_ref());
        let (items, links) = lexer::parse_tokens_into_items(module_name, tokens)?;

        if items.is_empty() {
            eprintln!("you must define at least one state");
            return Err(());
        }

        let mut it = Self {
            current: 0,
            items,
            links,
        };

        let mut unjumped = 0;
        it.unjump_in_state(&mut unjumped);

        it.current = unjumped;

        Ok(it)
    }

    pub fn response(&self) -> Option<&str> {
        self.response_in_state(self.current)
    }

    pub fn options(&self) -> impl Iterator<Item = &str> {
        self.options_in_state(&self.current)
    }

    pub fn next_state(&mut self) -> Option<()> {
        self.current = self.next_state_after(self.current)?;
        Some(())
    }

    pub fn next_state_from_option(&mut self, option_idx: usize) -> Option<()> {
        // TODO: either option idx is invalid or there is no next_state..
        self.current = self
            .options_indices_in_state(&self.current)
            .get(option_idx)
            .and_then(|opt| self.next_state_after(*opt))?;

        Some(())
    }

    fn response_in_state(&self, state: usize) -> Option<&str> {
        if let StateItem::Response { phrase, .. } = &self.items[state]
            && !phrase.is_empty()
        {
            Some(phrase.as_str())
        } else {
            None
        }
    }

    fn options_indices_in_state<'s>(&'s self, state: &'s usize) -> &'s [usize] {
        match &self.items[*state] {
            StateItem::OptionsBlock(options) => options.as_slice(),
            StateItem::Response { options, .. } => options.as_slice(),
            StateItem::Option { .. } => std::slice::from_ref(state),
            StateItem::Jump => unreachable!(),
        }
    }

    fn options_in_state<'s>(&'s self, state: &'s usize) -> impl Iterator<Item = &'s str> {
        self.options_indices_in_state(state).iter().map(|opt| {
            let StateItem::Option { phrase, .. } = &self.items[*opt] else {
                unreachable!();
            };

            phrase.as_str()
        })
    }

    fn next_state_after(&self, mut state: usize) -> Option<usize> {
        state = self.links[state]?;
        self.unjump_in_state(&mut state);
        Some(state)
    }

    fn unjump_in_state(&self, state: &mut usize) {
        while let StateItem::Jump = &self.items[*state] {
            // print!("# unjumping from {state}");
            *state = self.links[*state].unwrap(); // there is always a labeled item on which jump is pointing
            // println!(" to {state}");
        }
    }
}

fn run(mut conv: Conversation) -> std::io::Result<()> {
    use std::io::Write;

    let out = &mut std::io::stdout();
    let mut input = String::new();

    loop {
        let state = conv.current;

        write!(out, "- ")?;
        if let Some(phrase) = conv.response() {
            write!(out, "{phrase:?}")?;
        }

        write!(out, " [{state} -> ")?;
        match &conv.links[state] {
            Some(next) => writeln!(out, "{next}]")?,
            None => writeln!(out, "End]")?,
        }

        let opt_indices = conv.options_indices_in_state(&state);

        for (i, (phrase, opt)) in conv.options().zip(opt_indices).enumerate() {
            write!(out, "({i}) > {phrase:?} [{opt} -> ")?;
            match &conv.links[*opt] {
                Some(next) => writeln!(out, "{next}]")?,
                None => writeln!(out, "End]")?,
            }
        }

        out.flush()?;

        let next = loop {
            input.clear();
            std::io::stdin().read_line(&mut input)?;
            utils::trim(&mut input);

            if let Ok(opt_idx) = input.parse::<usize>()
                && opt_idx < opt_indices.len()
            {
                break conv.next_state_from_option(opt_idx);
            } else if input.is_empty() {
                break conv.next_state();
            }
        };

        if next.is_some() {
            writeln!(out)?;
        } else {
            break;
        }
    }

    Ok(())
}

// TODO: depth
const TEST_CONVO: &str = "
# nice

@import test

@as suka
    - 23
    @jump start # 24 -> 0

@jump suka # 0 -> 23

@as start
- 0 -> 15
> 1 -> 2
    - 2 -> 15
    > 3 -> 4
        - 4 -> 15
    > 5 -> 6
        - 6 -> 9
        > 7 -> 9
        > 8 -> 9

        - 9 -> 15
        > 10 -> 15
> 11 -> 12
    - 12 -> 13
    - 13 -> 15
    > 14 -> 15

- 15 -> 16

- 16 -> 17
    - 17 -> 19
    > 18 -> 19

- 19 -> 20 which is a jump to a test.mur file -> 21
@jump test.Test # 20 -> 21 which is in test.mur file
";


fn main() {
    // let Ok(mut convo) = Conversation::from_source(TEST_CONVO, "main") else {return;};
    let Ok(mut convo) = Conversation::from_source(TEST_CONVO, "main") else {
        return;
    };
    run(convo).unwrap();
}
