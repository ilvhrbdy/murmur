use std::fmt;
use std::path::Path;
mod lexer;
use lexer::StateItem;

// TODO: write tests, please?
// TODO: change all panics to normal error messages

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

    current_item: usize,
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
            current_item: 0,
            items,
            links,
        })
    }

    pub fn response(&self) -> Option<&str> {
        self.response_from_item(self.current_item)
    }

    pub fn options(&self) -> impl Iterator<Item = &str> {
        self.options_from_item(&self.current_item)
    }

    pub fn next_state(&mut self) -> Option<()> {
        self.current_item = self.links[self.current_item]?;
        Some(())
    }

    pub fn next_state_from_option(&mut self, option_idx: usize) -> Option<()> {
        self.current_item = self.options_indices_from_item(&self.current_item)
            .get(option_idx)
            .and_then(|opt| self.links[*opt])?;
        Some(())
    }

    fn response_from_item(&self, item: usize) -> Option<&str> {
        if let StateItem::Response { phrase, .. } = &self.items[item]
            && !phrase.is_empty()
        {
            Some(phrase.as_str())
        } else {
            None
        }
    }

    fn options_indices_from_item<'s>(&'s self, item: &'s usize) -> &'s [usize] {
        match &self.items[*item] {
            StateItem::Response { options, .. } => options.as_slice(),
            StateItem::Option(_) => std::slice::from_ref(item),
        }
    }

    fn options_from_item<'s>(&'s self, item: &'s usize) -> impl Iterator<Item = &'s str> {
        self.options_indices_from_item(item).iter().map(|opt| {
            let StateItem::Option(phrase) = &self.items[*opt] else {
                unreachable!();
            };

            phrase.as_str()
        })
    }
}

// TODO: i am not sure about that..
impl fmt::Display for Conversation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut state = 0;
        loop {
            write!(f, "- ")?;
            if let Some(phrase) = self.response_from_item(state) {
                write!(f, "{phrase:?}")?;
            }

            write!(f, " [{state} -> ")?;
            match &self.links[state] {
                Some(next) => writeln!(f, "{next}]")?,
                None => writeln!(f, "End]")?,
            }

            for (phrase, opt) in self
                .options_from_item(&state)
                .zip(self.options_indices_from_item(&state))
            {
                write!(f, "> {phrase:?} [{opt} -> ")?;
                match &self.links[*opt] {
                    Some(next) => writeln!(f, "{next}]")?,
                    None => writeln!(f, "End]")?,
                }
            }

            if let Some(next_state) = self.links[state] {
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
// nice

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

@to test.Test
- 19 -> 20 which is in test.mur file
";

fn main() {
    let mut convo = Conversation::from_source(TEST_CONVO, "main").unwrap();
    println!("{convo}");
}
