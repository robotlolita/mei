# Mei

> **NOTE**: this library is still under development. Fable is partially supported for now.

A type-safe and practical command line interface parsing library for Fable and .NET.

## Getting started

Add the [nuget package][] to your project:

    $ dotnet add <path-to-project> package OrigamiTower.Mei

Describe the command line interface as F# types:

```fsharp
open OrigamiTower.Mei

type Options = {
  [<Alias("-g")>]
  [<ShortDescription("Defines the greeting to show")>]
  [<DefaultValue("Hello")>]
  greeting: string;
}

type Commands =
  | [<MainCommand>]
    [<ShortDescription("Greets some person")>]
    Main of Options * string

  | [<Prefix("--")>]
    [<ShortDescription("Shows version and exit")>]
    Version

  | [<HelpCommand>]
    [<Prefix("--")>]
    [<ShortDescription("Shows how to use this application")>]
    Help
```

Generate a parser from the types, and use it to parse the command line arguments:

```fsharp
let parser = cliParser<Commands>(programName="hello")
match parser.Parse ["--greeting"; "Hi"; "Alice"] with
| Main(opts, name) -> printfn "%s, %s" opts.greeting name
| Version -> printfn "hello version 1.0.0"
| Help -> printfn "%s" (parser.help())
```

If you run the program, it should display `Hi, Alice`.

If you change it to the empty list, it should display a detailed error to the user, and tell them how to correct the error.

    Usage: hello [options] <name>

    `<name>` should be provided, but was missing in the command you've typed:

        hello
              ^^^^^^^^^^^^^^^^^^^^^^
              <name> is missing here

    A valid command would look like:

        hello string
              ^^^^^^
              <name>

    For detailed usage, see "hello --help"

You can look at the `examples` folder for more detailed and complete command line applications.

## Design overview

The design is highly influenced by [Argu][], using reflection on F# types to provide a specification for the arguments parsing. However, Argu uses tagged unions for describing flags, positional arguments, and commands. This feels slightly awkward to me, and the resulting interface isn't as practical as I think it could be--even though the idea is really good!

Mei instead describes _commands_ as tagged unions, and flags as _record_ types. This makes more sense to me since a command is a choice between one of many possibilities, but flags are really an unordered collection of key/value pairs.

In either case, the types of an union case and of a record property determine how we parse the list of arguments following that. In Mei, these F# types are just a convenient way of describing a _type specification_ for the command line interface, and converting from/to programs using that type specification.

To this end, Mei uses an internal type system for its "command line programs". This type system is extremely limited if compared to the rich type system F# has, but it is powerful enough to capture most of the command line applications we care about.

    t ::= int                           ;
        | float                         ; base types
        | string                        ;
        | boolean                       ;

        | t option                      ; optional arguments
        | t list                        ; lists of arguments
        | t * t                         ; sequences of arguments

        | [f]{ k1 : t, ..., kn : t }    ; record types
        | [f]< l1 t, ..., ln t >        ; tagged unions

The simple types at the top map to the expected types in F#, the record and tagged union types also map to F# types, but we have to bundle the type information with it (that's the `[f]` part). For tagged unions we must also include the UnionCaseInfo description for the tag, that's each `l`.

Additionally, we store meta-information with each of these types, such as "what's the short description for this pair?" and "does this pair have a default value?". This, together with the type information, allows us to generate good help and usage messages to describe the command line interface.

Finally, for commands, we allow the type to derive our `ICommand` interface. This interface is used to provide more detailed usage information, examples, etc.

## About the name

Mei (pronounced /mei/, or like English's "May") is one of the readings of the 命 kanji, meaning, among other things, "command". It's also part of the word 命令 (meirei, pronounced /meirei/), which is used to describe instructions in software.

As a mnemonic, you can also read it as the backronym "\[Mei\]stake-free (command-line interfaces)".

## Licence

Copyright (c) Quil, 2018. Licensed under the MIT licence.

[argu]: https://github.com/fsprojects/Argu
