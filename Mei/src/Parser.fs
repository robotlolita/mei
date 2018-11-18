module OrigamiTower.Mei.Parser

open Fable.Core
open OrigamiTower.Mei.Core
open OrigamiTower.Mei.Core.TypeSystem
open OrigamiTower.Mei.Encoding

type CliParser<'T>(spec: MeiType) =
  member self.Parse(args: string list) : 'T =
    match CliParsing.parse spec args with
    | Ok(v, _) ->
        unbox (fromMei v) // FIXME: this only works in Fable because it erases types!
    | Error e ->
        // FIXME: provide some good error reporting here
        failwithf "Failed to parse arguments."

let inline cliParser<'T>() =
  let typeInfo = typeof<'T>
  in CliParser<'T>(Reflection.toMeiType typeInfo)