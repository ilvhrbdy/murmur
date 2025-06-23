# WIP
Not ready at all. I'm building it only for my own needs, adding features when needed. 

## Future features
Extensible and flexible: able to hold your generic external game state `Conversation<ExternalState>`, which will be mutated through a map of functions `fn(&mut ExternalState) -> Output`.
This means *murmur* doesn't care about what your game state looks like, and how you define your functions map, it will just call them, when needed.

Dialogue translation: *murmur* will be able to spit out CSV file with all the phrases in your dialogue and then load it back with additional columns of your translation. 

Fast: conversation is compiled into optimized arrays (state items, functions, labels, etc), which are linked together by indices. This means no runtime hashing or validation. Then you can save your compiled conversation as bytecode.

C FFI: access to the `Compiler` and `Conversation` interface functions to be able compile and run the state machine with the preferred language and engine.


## Syntax
A conversation consists of states.

State consists of:
- Response - NPC's phrase;
- Choices - player choices for this response;

Each state starts with a `-` response:
```
- Oh, hi Mark!
```

These are two different states that act as a sequence of NPC phrases:
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

NOTE: The kind of indentation is defined by its first occurrence, which means that when *murmur* detects 4 spaces as the first single indentation it will expect you to be consistent with this choice and not mix it with whatever else :)

Phrases support newlines: this is one phrase and one option, newlines will be included, but the indentation will not:
```
- I am gonna vomit right now..
  Feeling really shitty..

> I don't wanna see it!
  Gonna vomit too..
```

Comments begin with `##`:
```
- Wanna talk?   
## > I am gay.

- Ok, it is fine, we can talk later.
```

If you want to have indentation in your text, use `\` to denote start of a new line in the phrase:
```
- I am too tired to make something funny..
  \        < this shit is not trimmed after '\'
  ## Hello, I am a comment in the middle of the phrase for some reason!!?!?
  \    ##   < look, this is escaped and not treated as a comment 0_0
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

#end
```

## Functions
`#` denotes a single function call with any amount of arguments separated with spaces. Function call ends at the end of the line or after the second `#`, that is why comments are started with `##` which kind of means `"empty function call"`.
The layer between builtin and your externally defined functions is very thin because of the "extensibility" idea, meaning you own the compiler, which is kind of cool.

#### `#<func> [args ...]`
Calls a custom function. Your custom functions will be defined via hash map, that you pass to the parser.
```rust
fn complain(my_state: &mut MyState, d: FuncData) {
    println!("I love you! Why can't you see that, {name}?!", name = d.args[0]);
    my_state.psychological -= 1;
}

compiler.register_function("complain_to", complain);
// pass the map to the Conversation::load
```
and then in *murmur* file:
```
# complain_to Whore
```
This function will be executed each time it appears, but if you want a function that is executed only once, use `# !<func>` syntax:
```
# !complain_to Alisa # this will be executed only once
```
Signatures that contain spaces must be enclosed in double quotes
```
# !"weird ass function name" "weird ass argument" # '!' must be a separate token outside of the function name
```

#### `#as <label name>`
Compile time function, that cannot be called `once` with `!`: will create a label for the next item:
```
#as Oops
- F**k off!
```

#### `#jump <label name>`
Redirects the item to the specified label:
```
- ..?

#as CuteOpt  # You can also label options, but jumping on them is not allowed
> Do I look cute?
    #jump Oops # as a child of an option, this jump will be executed only if the option is selected
```

#### `#import <module>`
Compile time function: defines a `<module>` namespace and makes all labels from the `<module>.mur` file available for use, so you can jump in there. The import is **lazy** - it will not be loaded or parsed unless you use it:
```
#import angry

- ...!
> Did I do something wrong?
    #jump angry.threat
```

In the `angry.mur` file:
```
#as threat
- Come closer..
```

#### `#hide [labels ...]` / `#show [labels ...]`
Will disable/enable the specified items, causing them to be skipped. If no labels are specified, they will disable/enable the parent item:
```
## Useful for "wiki"-style dialogues like this

#as useful_convo
- What do you want to know?

#as how_much
> How much?
    #hide
    - Not much.
    #jump useful_convo

#as where
> Where?
    #hide
    - Somewhere near.
    #jump useful_convo


#show where how_much
#jump useful_convo
```
By indenting 

#### `#end`
Immediately ends the conversation. Cannot call it with `!`, obviously..

#### String interpolation using `#{<func> [args ...]}`
Used in options and responses. This works the same way as calling inline functions. For example, `#{ func }` is evaluated each time it occurs, whereas `#{ !func }` constructs a static string:
```
- #{!player_name}, you are #{random_insult}!
```

Newlines and comments are allowed within interpolation:
```
## imagine that the result will just replace '#'
- aboba #{
    "weird ass func name" # function name
    # args:
    arg1 # 1
    arg2 # 2
    # creativity died for this commit
} aboba
```

- TODO: `#if <func>`, `#elif <func>` and `#else`, where `func` is your `fn(&mut YourCustomState) -> bool`.

- TODO: `#def <template name> [$args] <content>` defines a macro that generates *murmur* chunks. Chunk is just a string, that behaves differently, depending on the context:
```
#def jump_if $condition $label
    #if $condition
        #jump $label

```
then to call it, you need to tell *murmur* that you are accessing an item defined within the script, using full path `<module>.<def name>` or `.<def name>` for the current module:
```
# .jump_if too_old grave # this will expand the string and parse it as a murmur script
```
within a string interpolation, chunk is expanded as a plain string:
```
# def smell psina

- Man, you smell like #{.smell}!

# .smell # this will put "psina" text in place of `#` and try to parse it as a murmur script
```


