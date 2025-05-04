# WIP
Not ready at all. Building it only for my own needs, adding features when needed. Instead use [Yarn Spinner](https:#www.yarnspinner.dev/).

## Concept
The main idea of the project is to just give access to the parser and Conversation struct, that will hold all the information about dialogue from *murmur* file.
Then you can just translate this struct into any format or other languages for any game engine, or just play with it in the terminal.

## Future features
Extensible and flexible: able to hold your generic external game state `Conversation<ExternalState>`, which will be mutated through functions map of `fn(&mut ExternalState) -> OptionalOutput`, these can be called via intrinsics `@call`, `@if @elif @else`, text expansion `@{}`.
This means *murmur* doesn't care about how your game state looks like, and how you define your functions map, it will just call them, when you ask.

Fast: conversation is parsed into static arrays (states, functions, labels etc), which are linked together by indices. This means no runtime hashing or validation.

Portable: *murmur* will be able to convert `.mur` files into CSV, JSON and binary formats.

## Syntax
A conversation consists of states.

State consists of:
- Response - NPC's phrase;
- Options - player options for this response;

Each state starts with `-` response:
```
- Oh, hi Mark!
```

This is two different states that act as a sequence of NPC phrases:
```
- I don't want to talk to you.
- Please, go away!
```

Each state can have multiple options, defined with `>`:
```
- What? Who are you?
> Don't be scared little girl, I am your friend.
> I think I killed somebody..
```

If you want only options without the response, just write an empty response, *murmur* will set it to `None`:
```
-
> Hey are you alive?
```

Each option can have its own child state, defined by indentation, that will be executed only if this option is selected:
```
- How many?
> Many-many..
    - Oh, damn!
```

NOTE: The kind of indentation is defined by the first it's occurrence, which means that when *murmu* detects 4 spaces as first single indentation it will expect you to be consistent with this choice and don't mix it with tabs :)

Phrases support newlines: this is one phrase and one option, newlines will be included, but the indentation will not:
```
- I am gonna vomit right now..
  Feeling really shitty..

> I don't wanna see it!
  Gonna vomit too..
```

Comments begin with `#` at the start of the line:
```
- Wanna talk?   
# > I am gay.

- Ok, it is fine, we can talk later.
```

Here you can get a sense of the logic of state transitioning:
```
- state 1 => state 2
- state 2 => from option or End
> if selected => state 3
    - state 3 => from option or End
    > if selected => state 4
        - state 4 => End
    > if selected => End
> if selected => End

# End of conversation
```

## Builtin functions
NOTE: Function names aren't final..

Each function starts with `@` and operates on next response or option:

- `@as <label name>` will create a label for the next state item:
```
@as Oops
- F**k off!
```

- `@jump <label name>` will lead this item to the specified label:
```
- ..?

> Do I look cute?
    # as a child of an option this jump will be executed
    # only if the option is selected
    @jump Oops
```

- `@import <module>` defines a `<module>` namespace and makes all labels from the `<module>.mur` file available for use, so you can jump in there with `@to <module>.<label>`. The import is **lazy** - it will not be loaded or parsed unless you jump to it:
```
@import angry

- ...!
> Did I do something wrong?
    @jump angry.start
```

In the `angry.mur` file:
```
@as start
- Come closer..
```

