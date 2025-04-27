use std::fmt;
use std::path::Path;
mod lexer;
use lexer::StateItem;

// TODO: write tests, please?
// TODO: change all panics to normal error messages

#[derive(Debug)]
pub struct Conversation {
    items: Vec<StateItem>,
    links: Vec<Option<usize>>,

    selected_option_item: Option<usize>,
    current_item: usize,
}

impl Conversation {
    pub fn load(path: impl AsRef<Path>) -> Result<Self, std::io::Error> {
        // TODO: errors
        let path = path.as_ref();
        let file_name = path.file_name().unwrap().to_str().unwrap();
        let module_name = file_name.strip_suffix(".mur").unwrap_or(file_name);
        let src = std::fs::read_to_string(path)?;

        Ok(Self::from_source(src, module_name))
    }

    pub fn from_source(src: impl AsRef<str>, module_name: &str) -> Self {
        let tokens = lexer::tokenize(src.as_ref());
        let (mut items, mut links, item_depths) =
            lexer::parse_tokens_into_items(tokens, module_name);
        lexer::link_state_items(&mut items, &mut links, &item_depths);

        Self {
            items,
            links,
            selected_option_item: None,
            current_item: 0,
        }
    }

    pub fn response(&self) -> Option<&str> {
        self.response_from_item(self.current_item)
    }

    pub fn options(&self) -> impl Iterator<Item = &str> {
        self.options_from_item(&self.current_item)
    }

    pub fn select_option(&mut self, option_idx: usize) -> Option<&str> {
        let item = self
            .options_indices_from_item(&self.current_item)
            .get(option_idx)
            .copied()?;
        self.selected_option_item = Some(item);
        let StateItem::Option(phrase) = &self.items[item] else {
            unreachable!();
        };

        Some(phrase.as_str())
    }

    pub fn next_state(&mut self) -> bool {
        let Some(next) = self.selected_option_item.or(self.links[self.current_item]) else {
            return false;
        };

        self.current_item = next;
        self.selected_option_item = None;

        true
    }

    fn response_from_item(&self, item: usize) -> Option<&str> {
        if let StateItem::Response { phrase, .. } = &self.items[item]
            && !phrase.is_empty()
        {
            Some(phrase)
        } else {
            None
        }
    }

    fn options_indices_from_item<'s>(&'s self, item: &'s usize) -> &'s [usize] {
        match &self.items[self.current_item] {
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

impl fmt::Display for Conversation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let states = (0..self.items.len())
            .filter(|item| matches!(self.items[*item], StateItem::Response { .. }));

        for state_item in states {
            write!(f, "- ")?;
            if let Some(phrase) = self.response_from_item(state_item) {
                write!(f, "{phrase:?}")?;
            }

            write!(f, " [{state_item} -> ")?;
            match &self.links[state_item] {
                Some(next) => writeln!(f, "{next}]")?,
                None => writeln!(f, "End]")?,
            }

            for (phrase, opt) in self
                .options_from_item(&state_item)
                .zip(self.options_indices_from_item(&state_item))
            {
                write!(f, "> {phrase:?} [{opt} -> ")?;
                match &self.links[*opt] {
                    Some(next) => writeln!(f, "{next}]")?,
                    None => writeln!(f, "End]")?,
                }
            }

            writeln!(f)?;
        }

        Ok(())
    }
}

const TEST_CONVO: &str = "
// nice

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
    - 17 -> End
";

fn main() {
    let mut convo = Conversation::from_source(TEST_CONVO, "main");
    println!(
        "- {:?}\n> {:?}\n",
        convo.response(),
        convo.options().collect::<Vec<_>>()
    );
    convo.select_option(1);
    convo.next_state();
    println!(
        "- {:?}\n> {:?}\n",
        convo.response(),
        convo.options().collect::<Vec<_>>()
    );
}
