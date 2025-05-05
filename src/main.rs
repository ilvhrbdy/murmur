use std::fmt;
use std::path::Path;
mod lexer;
use lexer::StateItem;

// TODO: label processing:
//      - iterate over defined (label, indent)
//      if indent > labeled_item:
//          define_module(
//              current_module_name,
//              items.take_while(item.depth > label.depth)
//          )
// TODO: disallow putting multiple intrinsics on the same line, each must have it's own as they are legit items of conversation
// TODO: isolated chunks/blocks:
//      @as label1
//      - this item exists in the conversation and linked from/to other items
//
//      @as label2
//          - but this is only exists and
// TODO: @if @elif @else
// TODO: @call and function mapping / linking
// TODO: @{} - text expansion function (either text of labeled item or function)
//      in this case we cannot have labeled item and function with the same name
// TODO: the `main` module is always available even without @import and it kind of counter-intuitive. should i fix that?
// TODO: write tests, please?
// TODO: change all panics to normal error messages
// TODO: we could warn for some weird behavior like this:
//
//      @as aboba
//      @jump aboba
//
//      which is a valid murmur syntax, but just an infinite loop
//      maybe i need to be strict with this kind of shit?

#[derive(Debug)]
pub enum Error {
    Io(std::io::Error),
    ZeroStatesDefined,
    Multiple(Vec<Error>),
}

impl std::error::Error for Error {}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::Io(err) => writeln!(f, "{err}")?,
            Error::ZeroStatesDefined => {
                writeln!(f, "mur file must have at least one state defined")?;
            }
            Error::Multiple(errors) => {
                for err in errors {
                    writeln!(f, "{err}")?;
                }
            }
        }

        Ok(())
    }
}

#[derive(Debug)]
pub struct Conversation {
    items: Vec<StateItem>,
    links: Vec<Option<usize>>,

    current: usize,
}

impl Conversation {
    pub fn load(path: impl AsRef<Path>) -> Result<Self, Error> {
        let path = path.as_ref();
        let file_name = path.file_name().unwrap().to_str().unwrap();
        let module_name = file_name.strip_suffix(".mur").unwrap_or(file_name);
        let src = lexer::read_mur_file(module_name)?;

        Self::from_source(src, module_name)
    }

    pub fn from_source(src: impl AsRef<str>, module_name: &str) -> Result<Self, Error> {
        let tokens = lexer::tokenize(src.as_ref());
        let (items, links) = lexer::parse_tokens_into_items(module_name, tokens)?;

        if items.is_empty() {
            return Err(Error::ZeroStatesDefined);
        }

        Ok(Self {
            current: 0,
            items,
            links,
        })
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
            StateItem::Response { options, .. } => options.as_slice(),
            StateItem::Option(_) => std::slice::from_ref(state),
            StateItem::Jump => unreachable!(),
        }
    }

    fn options_in_state<'s>(&'s self, state: &'s usize) -> impl Iterator<Item = &'s str> {
        self.options_indices_in_state(state).iter().map(|opt| {
            let StateItem::Option(phrase) = &self.items[*opt] else {
                unreachable!();
            };

            phrase.as_str()
        })
    }

    fn next_state_after(&self, mut state: usize) -> Option<usize> {
        state = self.links[state]?;
        while let StateItem::Jump = &self.items[state] {
            state = self.links[state]?;
        }

        Some(state)
    }
}

// TODO: i am not sure about that.. i mean you cannot easily display the convo, that have conditions, options and stuff
impl fmt::Display for Conversation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut state = 0;
        loop {
            write!(f, "- ")?;
            if let Some(phrase) = self.response_in_state(state) {
                write!(f, "{phrase:?}")?;
            }

            write!(f, " [{state} -> ")?;
            match &self.links[state] {
                Some(next) => writeln!(f, "{next}]")?,
                None => writeln!(f, "End]")?,
            }

            for (phrase, opt) in self
                .options_in_state(&state)
                .zip(self.options_indices_in_state(&state))
            {
                write!(f, "> {phrase:?} [{opt} -> ")?;
                match &self.links[*opt] {
                    Some(next) => writeln!(f, "{next}]")?,
                    None => writeln!(f, "End]")?,
                }
            }

            if let Some(next_state) = self.next_state_after(state) {
                state = next_state;
                writeln!(f)?;
            } else {
                break;
            }
        }

        Ok(())
    }
}

const TEST_CONVO: &str = "
# nice

@import test

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
# @jump test.Test // 20 -> 21 which is in test.mur file
";

fn main() {
    let mut convo = Conversation::from_source(TEST_CONVO, "main").unwrap();
    // println!("{:?}", convo.options_in_state(&13).collect::<Vec<_>>());
    println!("{convo}");
}
