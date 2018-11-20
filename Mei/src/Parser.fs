/// The user-facing CLI parser module.
[<AutoOpen>]
module OrigamiTower.Mei.Parser

open Fable.Core
open OrigamiTower.Mei.Core
open OrigamiTower.Mei.Core.TypeSystem
open OrigamiTower.Mei.Encoding


/// A module for describing parsing errors
module Describe =
  type WordForm =
    | Singular
    | Plural

  let rec private joinSequence connective (xs:string list) =
    match xs with
    | [] -> ""
    | [x; y] -> x + ", " + connective + " " + y
    | x :: xs -> x + ", " + (joinSequence connective xs)


  /// Describes a command in natural language.
  let describeCommands cs =
    let name (_, x, _) = x
    cs |> List.map (name >> Option.defaultValue "a main command") 
       |> joinSequence "or"


  /// Describes a type in more natural language
  let rec describeType form t =
    match t, form with
    | TUnit, _ -> failwithf "Unit types are not supposed to be reified here."
    | TInt, Singular -> "an integral number"
    | TInt, Plural -> "integral numbers"
    | TFloat, Singular -> "a number"
    | TFloat, Plural -> "numbers"
    | TString, Singular -> "a word"
    | TString, Plural -> "words"
    | TBoolean, Singular -> "a logical value"
    | TBoolean, Plural -> "logical values"
    | TOption(t), f -> sprintf "%s (optionally)" (describeType f t)
    | TList(t), Singular -> sprintf "a list of %s" (describeType Plural t)
    | TList(t), Plural -> sprintf "lists of %s" (describeType Plural t)
    | TTuple(ts), f -> 
        let desc = ts |> List.map (describeType Singular) |> joinSequence "and"
        match f with
        | Singular -> sprintf "a sequence of %s" desc
        | Plural -> sprintf "sequences of %s" desc
    | TRecord(t, _), _ -> sprintf "[%s]" t.Name
    | TUnion(_, cs), _ -> describeCommands cs
          

  /// Describes what went wrong with parsing a command line argument
  let rec describeParsingError e =
    match e with
    | TypeError(expected, _) ->
        sprintf "Expected %s" 
                (describeType Singular expected)
    | NoValues(expected, _) ->
        sprintf "Expected %s, but there was nothing." 
                (describeType Singular expected)
    | UnexpectedFlag(expected, flag, _) ->
        sprintf "Expected %s, but found the flag %s" 
                (describeType Singular expected) flag
    | PartialParse(expected, _, _, _, _) ->
        sprintf "Expected %s"
                (describeType Singular expected)
    | UnrecognisedFlag(expected, flag, _, _, _) ->
        sprintf "Expected %s, but found another flag %s"
                (describeType Singular expected) flag
    | PartialFlagParse(_, missing, _, _, _) ->
        let flagNames = missing |> List.map (fun x -> x.flag)
        sprintf "Not all required flags have been provided. %s are missing"
                (joinSequence "and" flagNames)
    | NoCommandsMatched(commands, _) ->
        sprintf "Expected either %s" (describeCommands commands)
    | CommandFailed((_, name, _), error, _) ->
        let name = Option.defaultValue "the main command" name
        in sprintf "Didn't find a valid %s. %s"
                   name (describeParsingError error)



/// A parser for CLI arguments, constructed from a typed specification
type CliParser<'T>(spec: MeiType) =
  /// Parses the given argument list with this specification.
  /// Throws a more-or-less descriptive error if it fails.
  member self.Parse(args: string list) : 'T =
    match self.TryParse args with
    | Ok v -> v
    | Error e -> failwithf "Failed to parse arguments: %s" (Describe.describeParsingError e)


  /// Tries to parse the given argument list, returns the error if
  /// it fails.
  member self.TryParse(args: string list) : Result<'T, ParsingError> =
    match CliParsing.parse spec args with
    | Ok(v, _) -> Ok (unbox (fromMei v))
    | Error e -> Error e



/// Constructs a CLI parser from the typed specification `'T`.
let inline cliParser<'T>() =
  let typeInfo = typeof<'T>
  in CliParser<'T>(Reflection.toMeiType typeInfo)