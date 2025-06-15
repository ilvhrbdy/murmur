# WIP
Not ready at all. Building it only for my own needs, adding features when needed. Instead, use [Yarn Spinner](https://www.yarnspinner.dev/), I guess, or I don't know, didn't try it.

## Concept
The main idea of the project is to just give access to the parser and Conversation struct, which will hold all the information about dialogue from *murmur* file.
Then you can just translate this struct into any format or other languages for any game engine or just play with it in the terminal.

## Future features
Extensible and flexible: able to hold your generic external game state `Conversation<ExternalState>`, which will be mutated through a map of functions `fn(&mut ExternalState) -> OptionalOutput`.
This means *murmur* doesn't care about what your game state looks like, and how you define your functions map, it will just call them, when needed.

Fast: conversation is parsed into static arrays (states, functions, labels, etc), which are linked together by indices. This means no runtime hashing or validation.

Portable: *murmur* will be able to convert `.mur` files into CSV, JSON and binary formats.

## Syntax
A conversation consists of states.

State consists of:
- Response - NPC's phrase;
- Options - player options for this response;

Each state starts with a `-` response:
```
- Oh, hi Mark!
```

This is two different states that act as a sequence of NPC phrases:
```
- I don't want to talk to you.
- Please, go away!
```

Each state can have multiple options, defined using `>`:
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

NOTE: The kind of indentation is defined by its first occurrence, which means that when *murmu* detects 4 spaces as first single indentation it will expect you to be consistent with this choice and don't mix it with tabs :)

Phrases support newlines: this is one phrase and one option, newlines will be included, but the indentation will not:
```
- I am gonna vomit right now..
  Feeling really shitty..

> I don't wanna see it!
  Gonna vomit too..
```

Comments begin with `#`:
```
- Wanna talk?   
# > I am gay.

- Ok, it is fine, we can talk later.
```

If you want to have indentation in your text, use `\` to denote start of a new line in the phrase:
```
- I am too tired to make something funny..
  \        < this shit is not trimmed after '\'
  # Hello, I am a comment in the middle of the phrase for some reason!!?!?
  \    #   < look, this is escaped and not treated as a comment 0_0
  And I am just a casual new line without indentation..
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

## Functions
#### `@<func> [args ...]`
Calls a custom function, Where `func` is not one of the built-ins. Your custom functions will be defined via hash map, that you pass to the parser.
```rust
fn tell_a_joke(my_state: &mut MyState, d: FuncData) -> ReturnValue {
    println!("I love you! Why can't you see that, {name}?!", name = d.args[0]);
    my_state.psychological -= 1;

    ReturnValue::None // or ().into(), doesn't matter in this case, because value will be dropped anyway
}

funcs_map.insert("tell_a_joke_to", tell_a_joke as _);
// pass the map to the Conversation::load
```
and then in *murmur* file:
```
@ tell_a_joke_to whore
```
This function will be executed each time it appears, but if you want a function that is executed only once, use `@ !<func>` syntax:
```
@ !tell_a_joke_to bitch # this will be executed once at compile time
```
Signatures that contains spaces must be enclosed in double quotes
```
@ !"weird ass function name" "weird ass argument" # '!' must be a separate token outside of the function name
```

#### `@as <label name>`
Will create a label for the next item:
```
@as Oops
- F**k off!
```

If items are indented under the `@as` function, *murmur* will isolate them from the main conversation. They act as local modules and remain inactive unless you jump to the owner label:
```
@as Condemnation
    - ...
    @jump End

@as Start
- ..?
> She told me she is 18!
    @jump Condemnation
```

#### `@jump <label name>`
Redirects the item to the specified label:
```
- ..?

# You can also label options, but jumping on them is not allowed
@as CuteOpt 
> Do I look cute?
    # as a child of an option, this jump will be executed only if the option is selected
    @jump Oops
```

#### `@import <module>`
Defines a `<module>` namespace and makes all labels from the `<module>.mur` file available for use, so you can jump in there. The import is **lazy** - it will not be loaded or parsed unless you use it:
```
@import angry

- ...!
> Did I do something wrong?
    @jump angry.threat
```

In the `angry.mur` file:
```
@as threat
- Come closer..
```

#### `@hide [labels ...]` / `@show [labels ...]`
Will disable/enable the specified items, causing them to be skipped. If no labels are specified, they will disable/enable the parent item:
```
# Useful for "wiki"-style dialogues like this

@as useful_convo
- What do you want to know?

@as how_much
> How much?
    @hide
    - Not much.
    @jump useful_convo

@as where
> Where?
    @hide
    - Somewhere near.
    @jump useful_convo


@show where how_much
@jump useful_convo
```
#### String interpolation using `@{<func> [args ...]}`
Used in options and responses. This works the same way as calling inline functions. For example, `@{ func }` is evaluated each time it occurs, whereas `@{ !func }` constructs a static string:
```
- @{!player_name}, you are @{random_insult}!
```

Newlines and comments are allowed within interpolation:
```
# imagine that the result will just replace '@'
- aboba @{
    "weird ass func name" # function name
    # args:
    arg1 # 1
    arg2 # 2
    # creativity died for this commit
} aboba
```

- TODO: `@if <func>`, `@elif <func>` and `@else`, where `func` is your `fn(&mut YourCustomState) -> bool`.

- TODO: `@return` will undo the previous `@jump`, continuing from the next item after the jump.

- TODO: `@def <template name> [$args] <content>` defines a macro that generates *murmur* chunks. Chunk is just a string, that behaves differently, depending on the context:
```
@def jump_if $condition $label
    @if $condition
        @jump $label

```
then to call it, you need to tell *murmur* that you are accessing an item defined within the script, using full path `<module>.<def name>` or `.<def name>` for the current module:
```
@ .jump_if too_old grave # this will expand the string and parse it as a murmur script
```
within a string interpolation, chunk is expanded as a plain string:
```
@ def smell psina

- Man, you smell like @{.smell}!

@ .smell # this will put "psina" text in place of `@` and try to parse it as a murmur script
```


