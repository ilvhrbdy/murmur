use std::collections::HashMap;
use std::fmt;
use std::path::Path;
mod lexer;
pub use lexer::{Indentation, State, StateId, StateRef};

// TODO: write tests, please?
// TODO: change all panics to normal error messages
// TODO: intrinsic '@include file' will create a name-space 'file'
//      to refer a label inside of include use '.': 'file.label'
//      means all labels must be constructed in a way 'file.label'
//      so we can put all the states with their labels from all the @include into one Conversation

// #[derive(Debug)]
pub struct Conversation {
    pub indentation: Indentation,
    pub states: HashMap<StateId, State>,
    pub labels_map: HashMap<String, StateId>,
}

impl TryFrom<&Path> for Conversation {
    type Error = std::io::Error;

    fn try_from(p: &Path) -> Result<Self, Self::Error> {
        // TODO: errors
        let file_name = p.file_name().unwrap().to_str().unwrap();
        let module_name = file_name.strip_suffix(".mur").unwrap_or(file_name);
        let src = std::fs::read_to_string(p)?;
        Ok(Self::from_source(src, module_name))
    }
}

impl Conversation {
    fn from_source(src: impl AsRef<str>, module_name: &str) -> Self {
        let tokens = lexer::tokenize(src.as_ref());
        lexer::parse_tokens_into_conversation(tokens, module_name)
    }
}

impl fmt::Display for Conversation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut sorted = self.states.iter().collect::<Vec<_>>();
        sorted.sort_by(|(id, _), (oid, _)| id.idx.cmp(&oid.idx));

        macro_rules! find_lbl_by_id {
            ($id:ident) => {
                self.labels_map
                    .iter()
                    .find_map(|(l, i)| (i == $id).then_some(l.as_str()))
            };
        }
        macro_rules! write_state_ref {
            ($ns:expr) => {
                match &$ns {
                    StateRef::Label(name) => {
                        let ns_id = self.labels_map.get(name).expect("valid label");
                        writeln!(f, "[{ns_id}] ({name})")?;
                    }
                    StateRef::Id(ns_id) => {
                        if let Some(name) = find_lbl_by_id!(ns_id) {
                            writeln!(f, "[{ns_id}] ({name})")?;
                        } else {
                            writeln!(f, "[{ns_id}] ()")?;
                        }
                    }
                    ns => writeln!(f, "{ns:?}")?,
                }
            };
        }

        let mut write_state = |id: &StateId, state: &State| -> fmt::Result {
            let label = find_lbl_by_id!(id).unwrap_or("");
            write!(f, "[{id}] ({label}) => ")?;
            write_state_ref!(state.next);

            writeln!(f, "- {:?}", state.response)?;

            for o in &state.options {
                write!(f, "> {:?} => ", o.text)?;
                write_state_ref!(o.next);
            }

            writeln!(f, "\n")?;

            Ok(())
        };

        for (id, state) in sorted {
            write_state(id, state)?;
        }

        Ok(())
    }
}

const TEST_CONVO: &str = "
// nice

- 0:0:0
> will jump to 110
    - 1:1:0
    > will jump to 220
        - 2:2:0 will jump to 701
    > will jump to 320
        - 3:2:0
        > will jump to 412
        > will jump to 412

        - 4:2:1
        > will jump to 710
> will jump to 510
    - 5:1:0
    - 6:1:1
    > will jump to 710
- 7:0:1

- 8:0:2
    - 9:1:0
";

fn main() {
    let convo = Conversation::from_source(TEST_CONVO, "main");
    // let convo = Conversation::try_from(Path::new("test.test")).unwrap();

    println!("--------------------------");
    println!("{}", TEST_CONVO);
    println!("--------------------------");
    println!("{}", convo);
}
