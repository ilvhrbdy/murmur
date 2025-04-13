# WIP
Not ready at all. Building it only for my own needs, so it is never going to be general purpose, instead use [Yarn Spinner](https://www.yarnspinner.dev/).

## Features:
- Game engine agnostic: written as library, easily integrated with any engine, or even without engine by using IO with terminal.
- Non-bloated: not a programming language, but can be manipulated with any programming language through API.
- Generates and consumes csv to be able to easily write dialogues translations.
- Because it is written as a library, you can easily convert dialogue into any format you want.

## Concept
May change in the future.

Conversation consist of states.

State consist of:
- Response phrases - npc's sequence of phrases 
- Options - player's option for npc's response

Each states starts with first `-` response phrase:
```
- Oh, hi Mark!
```

This is one state with two response phrases:
```
- I don't want to talk to you.
- Please, go away!
```

But this is two states separated by empty line:
```
- First state phrase.

- Next state phrase. 
```

Also you start a new state when defining a response after options:

NOTE: `//` is not a comment in the language, but there will be
```
- What are you doing?    // first state
> NON OF YOUR BUSINESS!
- Oh shit, I am sorry..  // next state, even though it doesn't have an empty line before it
```

Each state can have a child state, which means when it ends (and doesn't have any option) it will transition to it's child state, instead of next one:
```
- I don't want to talk to you! // first state
    - I said, I don't want to talk to you! Go away! // child of the first state, defined by indentation
```

In here `state [num]` is just an id of state, the logic of conversation is defined by the order of states and their options
```
- What?                             // state 1, response phrase 1
- What do you want?                 // state 1, response phrase 2
> Nothing, just wondering around.   // state 1, option 1
    - Don't talk to me.             // state 2, response phrase 1
    > Why?                          // state 2, option 1
        - PLEASE, GO AWAY!          // state 3, response phrase 1, will lead to 'End'
    > Ok.                           // state 2, option 2, will lead to 'End'
> I am not talking to you.          // state 1, option 2, will lead to 'End'


- End.
```

phrases support newlines: this is one phrase and one option, newline will be included, but the indentation is not:
```
- I am gonna vomit right now..
  Feeling really shitty..

> I don't wanna see it!
  Gonna vomit too..
```
