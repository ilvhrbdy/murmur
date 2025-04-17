mod lexer;
pub use lexer::Conversation;

// TODO: write tests, please?
// TODO: do i allow empty responses or options?
// TODO: change all panics to normal error messages
// TODO: intrinsic '@include file' will create a name-space 'file'
//      to refer a label inside of include use '.': 'file.label'
//      means all labels must be constructed in a way 'file.label'
//      so we can put all the states with their labels from all the @include into one Conversation

const TEST_CONVO: &str = "
- aslkdj

    @lbl suka
    - hello

    > opt
    - suka


> >
";

fn main() {
    let convo = Conversation::from(TEST_CONVO);

    println!("--------------------------");
    println!("{}", convo);
    println!("--------------------------");
    println!("{}", TEST_CONVO);
}
