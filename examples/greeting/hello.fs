module Mei.Examples.Greeting

open OrigamiTower.Mei
open Fable.Import.Node.Globals

type Options = {
  greeting: string option
}

type Commands =
  | Version
  | Help
  | Main of Options * string

let args = ``process``.argv |> Seq.skip 2 |> Seq.toList

let parser = cliParser<Commands>()
match parser.Parse args with
| Main(opts, name) -> printfn "%s, %s" (Option.defaultValue "Hello" opts.greeting) name
| Version -> printfn "hello version 1.0.0"
| Help -> printfn "Usage: hello [--greeting <string>] <name>"
