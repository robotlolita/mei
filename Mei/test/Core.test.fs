module Mei.Tests.Core

open Fable.Import.Jest
open Mei.Tests.Matchers
open OrigamiTower.Mei.Core.TypeSystem
open OrigamiTower.Mei.Core.CliParsing

describe "[Core] parsing specs" <| fun () ->
  describe "Parsing TInt" <| fun () ->
    it "Should succeed if there's an int to parse" <| fun () ->
      parse (TInt) ["2"; "3"] --> Ok (Int 2, ["3"])
      parse (TInt) ["2"] --> Ok (Int 2, [])

    it "Should fail if there's input, but not an int" <| fun () ->
      parse (TInt) ["x"] --> Error (TypeError (TInt, ["x"]))

    it "Should fail if there's no input to parse" <| fun () -> 
      parse (TInt) [] --> Error (NoValues (TInt, []))

  describe "Parsing TFloat" <| fun () ->
    it "Should succeed if there's float input to parse" <| fun () ->
      parse (TFloat) ["2.3"; "2.4"] --> Ok (Float 2.3, ["2.4"])
      parse (TFloat) ["2.3"] --> Ok (Float 2.3, [])
      parse (TFloat) ["2"] --> Ok (Float 2.0, [])

    it "Should fail if there's input, but not a float" <| fun () ->
      parse (TFloat) ["x"] --> Error (TypeError (TFloat, ["x"]))

    it "Should fail if there's no input to parse" <| fun () ->
      parse (TFloat) [] --> Error (NoValues (TFloat, []))

  describe "Parsing TString" <| fun () ->
    it "Should succeed if there's input to parse" <| fun () ->
      parse (TString) ["hello"; "hi"] --> Ok (String "hello", ["hi"])
      parse (TString) ["hello"] --> Ok (String "hello", [])

    it "Should fail if there's no input to parse" <| fun () ->
      parse (TString) [] --> Error (NoValues (TString, []))

    it "Should fail if there's input, but the input starts a flag" <| fun () ->
      parse (TString) ["--flag"] --> Error (UnexpectedFlag (TString, "--flag", []))
      parse (TString) ["-p"; "x"] --> Error (UnexpectedFlag (TString, "-p", ["x"]))

  describe "Parsing TBoolean" <| fun () ->
    it "Should succeed if there's boolean input to parse" <| fun () ->
      parse (TBoolean) ["true"; "true"] --> Ok (Boolean true, ["true"])
      parse (TBoolean) ["true"] --> Ok (Boolean true, [])
      parse (TBoolean) ["false"] --> Ok (Boolean false, [])
      parse (TBoolean) ["yes"] --> Ok (Boolean true, []) 
      parse (TBoolean) ["no"] --> Ok (Boolean false, [])

    it "Should fail if there's input, but not boolean" <| fun () -> 
      parse (TBoolean) ["wat"] --> Error (TypeError (TBoolean, ["wat"]))

    it "Should fail if there's no input to parse" <| fun () ->
      parse (TBoolean) [] --> Error (NoValues (TBoolean, []))

  describe "Parsing TOption" <| fun () ->
    let t = TInt
    it "Should succeed (as Some) if there's input to parse of type t" <| fun () ->
      parse (TOption t) ["1"; "2"] --> Ok (Option (t, Some (Int 1)), ["2"])
      parse (TOption t) ["1"] --> Ok (Option (t, Some (Int 1)), [])

    it "Should succeed (as None) if there's input to parse not of type t" <| fun () ->
      parse (TOption t) ["x"] --> Ok (Option (t, None), ["x"])

    it "Should succeed (as None) if there's no input to parse" <| fun () ->
      parse (TOption t) [] --> Ok (Option (t, None), [])

  it "Parsing TList" <| fun () ->
    let t = TInt
    parse (TList t) [] --> Ok (List (t, []), [])
    parse (TList t) ["1"] --> Ok (List (t, [Int 1]), [])
    parse (TList t) ["1"; "2"; "3"] --> Ok (List (t, [Int 1; Int 2; Int 3]), [])
    parse (TList t) ["1"; "hello"] --> Ok (List (t, [Int 1]), ["hello"])
    parse (TList t) ["hello"] --> Ok (List (t, []), ["hello"])

  describe "Parsing TTuple" <| fun () ->
    let t1 = TInt
    let t2 = TBoolean
    let t3 = TString
    let ts = [t1; t2; t3]
    let tuple = TTuple ts

    it "Should succeed if there're enough arguments for the tuples" <| fun () ->
      parse tuple ["1"; "true"; "hello"; "hi"] 
      --> Ok (Tuple (ts, [Int 1; Boolean true; String "hello"]), ["hi"])

      parse tuple ["1"; "true"; "hello"] 
      --> Ok (Tuple (ts, [Int 1; Boolean true; String "hello"]), [])

    it "Should fail if there's a partial parse" <| fun () -> 
      parse tuple ["1"; "true"]
      --> Error (PartialParse (tuple, [Int 1; Boolean true], t3, [], ["1"; "true"]))

  describe "Parsing TRecord" <| fun () ->
    let t = typeof<unit>  // we don't use this here, but we need a value to fill Type
    let fverbose = {
      flag = "--verbose"
      aliases = []
      flagType = TBoolean
      defaultValue = None
      shortDescription = None
    }
    let fconfig = {
      flag = "--config"
      aliases = []
      flagType = TString
      defaultValue = Some (String "config.json")
      shortDescription = None
    }
    let ftrace = {
      flag = "--trace"
      aliases = []
      flagType = TOption TString
      defaultValue = None
      shortDescription = None
    }
    let ffile = {
      flag = "--file"
      aliases = []
      flagType = TString
      defaultValue = None
      shortDescription = None
    }

    it "Should succeed if all mandatory flags are present" <| fun () ->
      let tr = TRecord(t, ["verbose", fverbose; "file", ffile; "config", fconfig])

      parse tr ["--file"; "a"]
      --> Ok (Record(tr, [
            "verbose", Boolean false
            "file", String "a"
            "config", String "config.json"
          ]), [])

      parse tr ["--verbose"; "--file"; "a"]
      --> Ok (Record(tr, [
            "verbose", Boolean true
            "file", String "a"
            "config", String "config.json"
          ]), [])

      parse tr ["--verbose"; "--config"; "x"; "--file"; "a"]
      --> Ok (Record(tr, [
            "verbose", Boolean true
            "file", String "a"
            "config", String "x"
          ]), [])

    it "Should leave positional arguments alone" <| fun () ->
      let tr = TRecord(t, ["verbose", fverbose; "config", fconfig])

      parse tr ["--config"; "hi"; "hello"]
      --> Ok (Record(tr, [
            "verbose", Boolean false
            "config", String "hi"
          ]), ["hello"])

    it "Should fail on unrecognised flags if it hasn't finished parsing" <| fun () ->
      let tr = TRecord(t, ["verbose", fverbose; "file", ffile])

      parse tr ["--file"; "hi"; "--other"; "hello"]
      --> Ok (Record(tr, [
            "verbose", Boolean false
            "file", String "hi"
          ]), ["--other"; "hello"])

      parse tr ["--verbose"; "--other"; "hello"]
      --> Error (UnrecognisedFlag (
                  tr,
                  "--other",
                  ["verbose", fverbose, Boolean true],
                  ["--other"; "hello"],
                  ["--verbose"; "--other"; "hello"]
                ))

  describe "Parsing commands" <| fun () ->
    let t = typeof<unit>
    let fverbose = {
      flag = "--verbose"
      aliases = []
      flagType = TBoolean
      defaultValue = None
      shortDescription = None
    }
    let fconfig = {
      flag = "--config"
      aliases = []
      flagType = TString
      defaultValue = Some (String "config.json")
      shortDescription = None
    }
    let tr = TRecord(t, ["verbose", fverbose; "config", fconfig])
    let t1 = [TInt; tr]
    let t2 = []
    let t3 = [TOption TString]
    let c0 = 0, None, { shortDescription = None; parameters = t1 }
    let c1 = 1, (Some "version"), { shortDescription = None; parameters = t2 }
    let c2 = 2, (Some "help"), { shortDescription = None; parameters = t3 }
    let ct = TUnion(t, [c1; c2; c0])
    let ct2 = TUnion(t, [c1; c2])
    let p (_, _, c) = c.parameters

    it "Should parse the command if all types parse" <| fun () ->
      parse ct ["1"; "--verbose"]
      --> Ok (Case (ct, 0, [
            Int 1
            Record(tr, ["verbose", Boolean true; "config", String "config.json"])
          ]), [])

      parse ct ["version"]
      --> Ok (Case (ct, 1, []), [])

      parse ct ["help"]
      --> Ok (Case (ct, 2, [Option(TString, None)]), [])

      parse ct ["help"; "version"]
      --> Ok (Case (ct, 2, [Option(TString, Some (String "version"))]), [])

    it "Should fail if the command fails to parse" <| fun () ->
      parse ct ["x"]
      --> Error (CommandFailed(
                  c0, 
                  PartialParse(TTuple (p c0), [], TInt, ["x"], ["x"]),
                  ["x"]
                ))

    it "Should fail if none of the commands succeed" <| fun () ->
      parse ct2 []
      --> Error (NoCommandsMatched([c1; c2], []))