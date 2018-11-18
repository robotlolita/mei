/// Provides a way of parsing command line arguments from a typed schema.
/// 
/// This schema acts like a grammar, and the resulting parse is a tree of
/// typed objects. However, these types are internal Mei types, so
/// they're represented as a union in F#. Using them directly is not very
/// practical, but they do provide a good basis for building interfaces
/// on top.
module OrigamiTower.Mei.Core
open OrigamiTower.Mei.Utils


/// Defines the Mei type system.
module TypeSystem =

  open System

  /// Meta-data may be attached to a type to describe what we expect
  /// to be provided to it. This is only used for error reporting and
  /// help screens.
  type Metadata = {
    /// The name to show when describing this type
    name: string option;

    /// A short description of what value this type expects
    shortDescription: string option;
  }

  /// A specification for how to parse a particular command-line flag.
  and MeiFlag = {
    /// The flag (in "--flag" format) this specification describes. This
    /// is what we call its "canonical" form--the name we use througout
    /// parsing to identify the flag and its values.
    flag: string;

    /// A list of aliases this flag supports ("--flag" or "-f" forms)
    aliases: string list;

    /// The type specification we should use to parse values of this flag.
    /// Boolean types are treated specially in flags and consume no input
    /// to determine the value.
    flagType: MeiType;

    /// A possibly default value for the flag (if it's optional) if no
    /// value is provided for it.
    defaultValue: MeiValue option;

    /// A short description describing what this flag stands for.
    shortDescription: string option;
  }

  /// A specification for how to parse a particular command.
  and MeiCommand = {
    /// A short description of what this command does.
    shortDescription: string option;

    /// A list of positional parameters this command expects.
    parameters: MeiType list
  }

  /// The Mei restricted type system.
  /// 
  /// Note that we store the original .NET/F# type for records and
  /// unions so we can convert back to that type. This is only really
  /// usefull when using the reflection API.
  and MeiType =
    | TUnit
    | TInt of Metadata
    | TFloat of Metadata
    | TString of Metadata
    | TBoolean of Metadata
    | TOption of MeiType
    | TList of MeiType
    | TTuple of MeiType list
    | TRecord of Type * (string * MeiFlag) list
    | TUnion of Type * (int * string option * MeiCommand) list

  /// The Mei restricted value system.
  /// 
  /// This is a fat representation with values being tagged with their
  /// original types. Doing so allows us to recover all type information
  /// for the value (and so convert between F#<->Mei)
  and MeiValue =
    | Int of Metadata * int
    | Float of Metadata * double
    | String of Metadata * string
    | Boolean of Metadata * bool
    | Option of MeiType * MeiValue option
    | List of MeiType * MeiValue list
    | Tuple of MeiType list * MeiValue list
    | Record of MeiType * (string * MeiValue) list
    | Case of MeiType * int * MeiValue list

  /// All of the things that can go wrong when trying to parse a command
  /// with a typed specification.
  /// 
  /// Note that we also reify partial parses and other fine-grained errors.
  /// This increases the number of errors, but allows us to give the user
  /// more helpful error messages, indicating exactly where things failed
  /// and why.
  type ParsingError =
    /// The parser expected a value of type T, but the input couldn't be
    /// converted to it.
    | TypeError of expected: MeiType * unparsed: string list

    /// The parser expected at least one value of type T, but there was
    /// no (reasonable) input left to parse.
    | NoValues of expected: MeiType * unparsed: string list

    /// The parser expected a positional value of type T, but found a flag
    | UnexpectedFlag of expected: MeiType * flag: string * unparsed: string list

    /// The parser succeeded in parsing part of a positional type T, but
    /// failed in parsing it completely.
    | PartialParse of expected: MeiType 
                    * parsed: MeiValue list 
                    * failed: MeiType 
                    * unparsed: string list 
                    * unparsedFull: string list

    /// The flag parser hasn't finished parsing, and encountered a flag
    /// that it doesn't recognise.
    | UnrecognisedFlag of expected: MeiType 
                        * flag: string 
                        * parsed: (string * MeiFlag * MeiValue) list 
                        * unparsed: string list 
                        * unparsedFull: string list

    /// The flag parser succeeded in parsing some required flags, but not all.
    | PartialFlagParse of expected: MeiType 
                        * missing: MeiFlag list 
                        * parsed: (string * MeiFlag * MeiValue) list 
                        * unparsed: string list 
                        * unparsedFull: string list

    /// The commands parser couldn't parse the input as any of the commands
    /// it knows of.
    | NoCommandsMatched of commands: (int * string option * MeiCommand) list 
                         * unparsed: string list
    
    /// The commands parser started parsing the input as one particular command,
    /// but failed to parse it completely.
    | CommandFailed of command: (int * string option * MeiCommand) 
                     * error: ParsingError 
                     * unparsed: string list

  /// The flag parser doesn't have access to all of the information the full
  /// parser does, but it has to know what failed to recover from some of the
  /// errors. So we have a separate set of errors it may throw.
  type FlagParsingError =
    /// The flag parser encountered a flag it doesn't recognise
    | FUnrecognisedFlag of flag: string * unparsed: string list

    /// The full parser (for some type T) has failed to parse the input as T
    | FParseError of flag: MeiFlag 
                   * name: string 
                   * ParsingError 
                   * unparsed: string list

    /// The flag parser failed, but parsed some flags. We may recover from
    /// this error if there are default values provided by the flags we
    /// didn't parse.
    | FPartialFlagParse of missing: MeiFlag list 
                         * parsed: Map<string, MeiValue> 
                         * unparsed: string list 
                         * unparsedFull: string list
                       
    /// The flag parser failed because the next item on the input isn't a flag.
    /// We may recover from this error if the parser managed to parse all
    /// required flags.
    | FUnexpectedPositional of positional: string * rest: string list


  /// Constructs an empty meta-data for some type.
  let emptyMeta = {
    name = None;
    shortDescription = None;
  }


  /// A textual representation of a particular type
  let rec typeName (t:MeiType) =
    let presentTuple ts = sprintf "(%s)" (ts |> List.map typeName |> String.concat ", ")
    match t with
    | TUnit _ -> "unit"
    | TInt _ -> "int"
    | TFloat _ -> "float"
    | TString _ -> "string"
    | TBoolean _ -> "boolean"
    | TOption t -> (typeName t) + " option"
    | TList t -> (typeName t) + " list"
    | TTuple ts -> presentTuple ts
    | TRecord (_, ps) -> 
        sprintf "{ %s }" 
                (ps |> List.map (fun (k, t) -> sprintf "%s: %s" k (typeName t.flagType))
                    |> String.concat ", ")
    | TUnion (_, cs) ->
        let name = Option.defaultValue "_"
        let show n ts = sprintf "%s(%s)" (name n) (presentTuple ts)
        sprintf "union { %s }"
                (cs |> List.map (fun (_, n, c) -> show n c.parameters)
                    |> String.concat "; ")


  /// Reconstructs the Mei type of some Mei value.
  let typeOf (v:MeiValue) =
    match v with
    | Int (m, _) -> TInt m
    | Float (m, _) -> TFloat m
    | String (m, _) -> TString m
    | Boolean (m, _) -> TBoolean m
    | Option (t, _) -> TOption t
    | List (t, _) -> TList t
    | Tuple (t, _) -> TTuple t
    | Record (t, _) -> t
    | Case (t, _, _) -> t


  /// Some types accept a natural default value, this function provides it.
  let defaultValueForType (t:MeiType) =
    match t with
    | TUnit _ -> None
    | TInt _ -> None
    | TFloat _ -> None
    | TString _ -> None
    | TBoolean _ -> Some (Boolean (emptyMeta, false))
    | TOption t -> Some (Option (t, None))
    | TList t -> Some (List (t, []))
    | TTuple _ -> None
    | TRecord _ -> None
    | TUnion _ -> None

  /// The complete list of unparsed arguments encoded in the error.
  let unparsedArgumentsFromError e =
    match e with
    | TypeError(_, argv) -> argv
    | NoValues(_, argv) -> argv
    | UnexpectedFlag(_, flag, argv) -> flag :: argv
    | PartialParse(_, _, _, _, argv) -> argv
    | UnrecognisedFlag(_, _, _, _, argv) -> argv
    | PartialFlagParse(_, _, _, _, argv) -> argv
    | NoCommandsMatched(_, argv) -> argv
    | CommandFailed(_, _, argv) -> argv

  /// Converts an error representation of the Flag Parser to a general
  /// parsing error representation.
  /// 
  /// To do so we basically need to stick back in the type the Mei Type
  /// that the Flag Parser was trying to parse, the data we've parsed,
  /// and the whole list of arguments that the flag parser should've
  /// parsed.
  let flagErrorToParseError error meiType parsed argv =
    match error with
    | FUnrecognisedFlag(flag, unparsed) ->
        UnrecognisedFlag(meiType, flag, parsed, flag :: unparsed, argv)
    | FParseError(_, _, error, _) ->
        error
    | FPartialFlagParse(missing, _, unparsed, _) ->
        PartialFlagParse(meiType, missing, parsed, unparsed, argv)
    | FUnexpectedPositional(positional, rest) ->
        failwithf "[internal error] FUnexpectedPositional should've been consumed in an earlier phase of parsing. (%s, %A)" positional rest


  /// Returns the type of a flag
  let flagType flag = flag.flagType


  /// Returns all keys in a record type
  let recordKeys t =
    match t with
    | TRecord(_, pairs) -> List.map fst pairs
    | _ -> failwithf "Not a record type: %s" (typeName t)


  /// Returns the list of types in a record
  let recordTypes t =
    match t with
    | TRecord(_, pairs) -> pairs |> List.map (snd >> flagType)
    | _ -> failwithf "Not a record type: %s" (typeName t)


  /// Returns the F# type in the record
  let recordNetType t =
    match t with
    | TRecord(t, _) -> t
    | _ -> failwithf "Not a record type: %s" (typeName t)

  /// Returns the canonical record entries (in type order)
  let recordEntries r =
    match r with
    | Record(t, pairs) -> 
        let pairMap = Map.ofList pairs
        let find key = Map.find key pairMap
        let keys = recordKeys t
        keys |> List.map find 
             |> List.zip keys

    | _ -> failwithf "Not a record value: %s" (typeName (typeOf r))


  /// Returns the values of a record (in type order)
  let recordValues = recordEntries >> List.map snd


  /// Returns the case information for an union given its tag
  let unionCase u tag = 
    match u with 
    | TUnion(_, cs) -> 
        match cs |> List.filter (fun (t, _, _) -> tag = t) with
        | [c] -> c
        | [] -> failwithf "No tag %i in %s" tag (typeName u)
        | _ -> failwithf "Multiple tags %i in %s" tag (typeName u)
    | _ -> failwithf "Not an union type: %s" (typeName u)


  /// Returns the sequence of types for the case
  let unionCaseTypes u tag =
    let (_, _, command) = unionCase u tag
    in command.parameters


  /// Returns the F# type for the union
  let unionNetType u =
    match u with
    | TUnion(t, _) -> t
    | _ -> failwithf "Not an union type: %s" (typeName u)


  /// Type checks a Mei value
  let rec typeCheck t value =
    match t, value with
    | (TInt _), (Int _) -> true
    | (TFloat _), (Float _) -> true
    | (TString _), (String _) -> true
    | (TBoolean _), (Boolean _) -> true

    | (TOption t1), (Option (t2, Some v)) -> (t1 = t2) && (typeCheck t1 v)
    | (TOption t1), (Option (t2, None)) -> t1 = t2

    | (TList t1), (List (t2, xs)) -> 
        (t1 = t2) && (List.forall (typeCheck t1) xs)

    | (TTuple ts1), (Tuple (ts2, xs)) ->
        (ts1 = ts2) && (ts1.Length = xs.Length)
                    && (List.forall2 typeCheck ts1 xs)

    | (TRecord (_, ts), (Record (tr, vals))) ->
        (t = tr) && (List.forall2 typeCheck (recordTypes t) (recordValues value))

    | (TUnion (_, cs)), (Case (tu, tag, vs)) ->
        let uts = unionCaseTypes t tag
        in (t = tu) && (uts.Length = vs.Length)
                    && (List.forall2 typeCheck (unionCaseTypes t tag) vs)

    | _, _ ->
        false

  
  /// Type checking as an action
  let assertType t value =
    if typeCheck t value then
      value
    else
      failwithf "Couldn't unify types %s and %s" (typeName t) (typeName (typeOf value))



/// Allows parsing command line arguments with a typed specification of the
/// grammar.
module CliParsing =
  open OrigamiTower.Mei.Utils.FSharpParsing  
  open TypeSystem


  /// True if a particular argument is a flag
  let isFlag (argument:string) = argument.StartsWith "-"


  /// Builds a partial result map for the flag parser filled with the
  /// defaults for types/flags that have one.
  let buildMapWithDefaults (flags:MeiFlag list) =
    let builder res (flag:MeiFlag) =
      let defaultValue = 
        flag.defaultValue |> Option.orElseWith (fun () -> defaultValueForType flag.flagType)
      match defaultValue with
      | Some v -> Map.add flag.flag v res
      | None -> res
    in List.fold builder Map.empty flags


  /// Builds a map of `alias -> canonical flag` mappings so we can
  /// use the canonical forms any time we see an alias.
  let buildAliasMapping (flags:MeiFlag list) =
    let builder map flag =
      List.fold (fun m a -> Map.add a flag.flag m) map flag.aliases
    in List.fold builder Map.empty flags


  /// Builds a map of `canonical flag -> flag data` mappings for all
  /// flags the flag parser knows about. This allows we to quickly
  /// retrieve the type of the flag given its canonical name. And
  /// we also use this to check that all known flags have been
  /// ultimately parsed by the flag parser.
  let buildKnownFlagsMap (flags:(string * MeiFlag) list) =
    let builder map (k, flag) =
      Map.add flag.flag (k, flag) map
    in List.fold builder Map.empty flags


  /// Gets the canonical form of a flag.
  let toCanonicalName aliases name =
    Option.defaultValue name (Map.tryFind name aliases)


  /// Gets a list of flags that we haven't parsed yet.
  /// 
  /// Note that flags that we haven't parsed but *have* a default
  /// value aren't considered missing.
  let missingFlags map (flags:(string * MeiFlag) list) =
    flags
      |> List.filter (fun (_, x) -> not (Map.containsKey x.flag map))
      |> List.map (fun (_, x) -> x)


  /// Converts our convenient map of flags to values to the form
  /// the Record type expects.
  let unpackParsedFlags map (flags:(string * MeiFlag) list) =
    flags
      |> List.fold (fun res (key, flag) -> 
           match Map.tryFind flag.flag map with
           | Some v -> List.append res [(key, flag, v)]
           | None -> res
         ) []    


  /// True if we've parsed all of the required flags.
  let isFlagParsingComplete map flags =
    (missingFlags map flags).IsEmpty


  /// Adds a value to a flag in the map.
  /// 
  /// FIXME: this should handle list types by aggregating all values
  ///        in the flag. Currently we override it.
  let addToKey key value map =
    Map.add key value map


  /// Parses a single flag.
  let rec parseFlag aliases knownFlags name argv =
    if not (isFlag name) then
      Error (FUnexpectedPositional (name, argv))
    else
      let canonicalName = toCanonicalName aliases name
      match Map.tryFind canonicalName knownFlags with
      | None -> 
          Error (FUnrecognisedFlag (name, argv))
      | Some (_, flag) ->
          match flag.flagType with
          | TBoolean(m) -> 
              Ok (canonicalName, (Boolean (m, true)), argv)
          | t ->
              match parse t argv with
              | Error e -> Error (FParseError (flag, name, e, argv))
              | Ok (v, rest) -> Ok (canonicalName, v, rest)


  /// Given a flag parsing specification, tries to parse all of the flags
  /// in it.
  /// 
  /// Flags can be parsed in any order, but only known flags are
  /// allowed in the input until we're done. This means that it's not
  /// possible to mix flags *and* positional arguments. Encountering
  /// a positional argument in this parser is considered an error.
  and parseFlags flags originalArgv =
    let result = buildMapWithDefaults (List.map snd flags)
    let aliases = buildAliasMapping (List.map snd flags)
    let knownFlags = buildKnownFlagsMap flags


    let rec go map argv =
      match argv with
      | name :: rest ->
          match parseFlag aliases knownFlags name rest with
          | Error (FUnexpectedPositional (name, rest)) ->
              Ok (map, name :: rest)
          | Error (FUnrecognisedFlag (flag, unparsed)) when isFlagParsingComplete map flags ->
              Ok (map, flag :: unparsed)
          | Error e -> 
              let parsed = unpackParsedFlags map flags
              Error (fun t -> flagErrorToParseError e t parsed originalArgv)
          | Ok (canonicalName, v, rest) ->
              go (addToKey canonicalName v map) rest

      | [] ->
          Ok (map, [])

    match go result originalArgv with
    | Error e -> Error e
    | Ok (map, rest) ->
        match missingFlags map flags with
        | [] -> 
            let simplify ps = ps |> List.map (fun (k, _, v) -> (k, v))
            Ok ((fun t -> Record (t, simplify (unpackParsedFlags map flags))), rest)
        | missing ->
            let parsed = unpackParsedFlags map flags
            Error <| (fun t -> PartialFlagParse (t, missing, parsed, rest, originalArgv))


  /// Parses a single command.
  and parseCommand (tag, name, command) argv =
    match parse (TTuple command.parameters) argv with
    | Ok (Tuple (_, vs), rest) ->
        Ok ((fun t -> (Case (t, tag, vs))), rest)
    | Ok (t, rest) ->
        failwithf "Expected a tuple, got %s" (typeName (typeOf t))
    | Error e ->
        Error (CommandFailed((tag, name, command), e, argv))


  /// Given a command parsing specification, tries to parse all of the types
  /// in it.
  and parseCommands commands argv =
    let rec go cs argv =
      match cs, argv with
      | ((tag, Some name, command) :: crest), (next :: arest) ->
          if name = next then
            parseCommand (tag, Some name, command) arest
          else
            go crest argv
      | ((tag, Some name, command) :: crest), [] ->
          go crest []
      | ((tag, None, command) :: crest), argv ->
          parseCommand (tag, None, command) argv
      | [], argv ->
          Error (NoCommandsMatched(commands, argv))
    in go commands argv


  /// Parses command line arguments from a type specification.
  and parse (spec:MeiType) (argv:string list) =
    match spec, argv with
    | (TInt meta), (x :: rest) -> 
        match tryInt x with
        | Some x -> Ok <| (Int (meta, x), rest)
        | None -> Error <| TypeError (spec, argv)
    
    | (TFloat meta), (x :: rest) ->
        match tryDouble x with
        | Some x -> Ok <| (Float (meta, x), rest)
        | None -> Error <| TypeError (spec, argv)

    | (TString meta), (x :: rest) ->
        if isFlag x then
          Error <| UnexpectedFlag (spec, x, rest)
        else
          Ok <| (String (meta, x), rest)

    | (TBoolean meta), (x :: rest) ->
        match tryBoolean x with
        | Some x -> Ok <| (Boolean (meta, x), rest)
        | None -> Error <| TypeError (spec, argv)

    | (TOption t), argv ->
        match parse t argv with
        | Ok (v, rest) -> Ok (Option (t, Some v), rest)
        | Error _ -> Ok (Option (t, None), argv)

    | (TList t), argv ->
        let rec go argv =
          match parse t argv with
          | Ok (v, rest) -> 
              let (vs, rest) = go rest
              (v :: vs, rest)
          | Error e ->
              ([], unparsedArgumentsFromError e)
        let (v, rest) = go argv
        Ok <| (List (t, v), rest)

    | (TTuple ts), argv ->
        let runNext res t =
          match res with
          | Ok (vs, rest) -> 
              match parse t rest with
              | Ok (v, rest) -> Ok ((List.append vs [v]), rest)
              | Error e -> Error (PartialParse (spec, vs, t, rest, argv))
          | Error e -> Error e

        let result = List.fold runNext (Ok ([], argv)) ts

        Result.map (fun (vs, rest) -> (Tuple (ts, vs), rest)) result

    | TRecord(t, flags), argv ->
        match parseFlags flags argv with
        | Ok (rf, rest) -> Ok <| (rf spec, rest)
        | Error ef -> Error (ef spec) 

    | TUnion(t, commands), argv ->
        match parseCommands commands argv with
        | Ok (cf, rest) -> Ok (cf spec, rest)
        | Error e -> Error e

    | TUnit, [] ->
        failwithf "Can't parse unit types."

    | t, [] ->
        Error (NoValues (t, []))