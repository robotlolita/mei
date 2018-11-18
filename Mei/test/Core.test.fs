module Mei.Tests.Core

open Fable.Import.Jest
open Mei.Tests.Matchers
open OrigamiTower.Mei.Core.TypeSystem
open OrigamiTower.Mei.Core.CliParsing

describe "[Core] parsing specs" <| fun () ->
  let m = emptyMeta

  describe "Parsing TInt" <| fun () ->
    it "Should succeed if there's an int to parse" <| fun () ->
      parse (TInt m) ["2"; "3"] --> Ok (Int (m, 2), ["3"])
      parse (TInt m) ["2"] --> Ok (Int (m, 2), [])

    it "Should fail if there's input, but not an int" <| fun () ->
      parse (TInt m) ["x"] --> Error (TypeError (TInt m, ["x"]))

    it "Should fail if there's no input to parse" <| fun () -> 
      parse (TInt m) [] --> Error (NoValues (TInt m, []))

  describe "Parsing TFloat" <| fun () ->
    it "Should succeed if there's float input to parse" <| fun () ->
      parse (TFloat m) ["2.3"; "2.4"] --> Ok (Float (m, 2.3), ["2.4"])
      parse (TFloat m) ["2.3"] --> Ok (Float (m, 2.3), [])
      parse (TFloat m) ["2"] --> Ok (Float (m, 2.0), [])

    it "Should fail if there's input, but not a float" <| fun () ->
      parse (TFloat m) ["x"] --> Error (TypeError (TFloat m, ["x"]))

    it "Should fail if there's no input to parse" <| fun () ->
      parse (TFloat m) [] --> Error (NoValues (TFloat m, []))

  describe "Parsing TString" <| fun () ->
    it "Should succeed if there's input to parse" <| fun () ->
      parse (TString m) ["hello"; "hi"] --> Ok (String (m, "hello"), ["hi"])
      parse (TString m) ["hello"] --> Ok (String (m, "hello"), [])

    it "Should fail if there's no input to parse" <| fun () ->
      parse (TString m) [] --> Error (NoValues (TString m, []))

    it "Should fail if there's input, but the input starts a flag" <| fun () ->
      parse (TString m) ["--flag"] --> Error (UnexpectedFlag (TString m, "--flag", []))
      parse (TString m) ["-p"; "x"] --> Error (UnexpectedFlag (TString m, "-p", ["x"]))

  describe "Parsing TBoolean" <| fun () ->
    it "Should succeed if there's boolean input to parse" <| fun () ->
      parse (TBoolean m) ["true"; "true"] --> Ok (Boolean (m, true), ["true"])
      parse (TBoolean m) ["true"] --> Ok (Boolean (m, true), [])
      parse (TBoolean m) ["false"] --> Ok (Boolean (m, false), [])
      parse (TBoolean m) ["yes"] --> Ok (Boolean (m, true), []) 
      parse (TBoolean m) ["no"] --> Ok (Boolean (m, false), [])

    it "Should fail if there's input, but not boolean" <| fun () -> 
      parse (TBoolean m) ["wat"] --> Error (TypeError (TBoolean m, ["wat"]))

    it "Should fail if there's no input to parse" <| fun () ->
      parse (TBoolean m) [] --> Error (NoValues (TBoolean m, []))

  describe "Parsing TOption" <| fun () ->
    let t = TInt m
    it "Should succeed (as Some) if there's input to parse of type t" <| fun () ->
      parse (TOption t) ["1"; "2"] --> Ok (Option (t, Some (Int (m, 1))), ["2"])
      parse (TOption t) ["1"] --> Ok (Option (t, Some (Int (m, 1))), [])

    it "Should succeed (as None) if there's input to parse not of type t" <| fun () ->
      parse (TOption t) ["x"] --> Ok (Option (t, None), ["x"])

    it "Should succeed (as None) if there's no input to parse" <| fun () ->
      parse (TOption t) [] --> Ok (Option (t, None), [])

  it "Parsing TList" <| fun () ->
    let t = TInt m
    parse (TList t) [] --> Ok (List (t, []), [])
    parse (TList t) ["1"] --> Ok (List (t, [Int (m, 1)]), [])
    parse (TList t) ["1"; "2"; "3"] --> Ok (List (t, [Int (m, 1); Int (m, 2); Int (m, 3)]), [])
    parse (TList t) ["1"; "hello"] --> Ok (List (t, [Int (m, 1)]), ["hello"])
    parse (TList t) ["hello"] --> Ok (List (t, []), ["hello"])

  describe "Parsing TTuple" <| fun () ->
    let t1 = TInt m
    let t2 = TBoolean m
    let t3 = TString m
    let ts = [t1; t2; t3]
    let tuple = TTuple ts

    it "Should succeed if there're enough arguments for the tuples" <| fun () ->
      parse tuple ["1"; "true"; "hello"; "hi"] 
      --> Ok (Tuple (ts, [Int (m, 1); Boolean (m, true); String (m, "hello")]), ["hi"])

      parse tuple ["1"; "true"; "hello"] 
      --> Ok (Tuple (ts, [Int (m, 1); Boolean (m, true); String (m, "hello")]), [])

    it "Should fail if there's a partial parse" <| fun () -> 
      parse tuple ["1"; "true"]
      --> Error (PartialParse (tuple, [Int (m, 1); Boolean (m, true)], t3, [], ["1"; "true"]))

  describe "Parsing TRecord" <| fun () ->
    let t = typeof<unit>  // we don't use this here, but we need a value to fill Type
    let fverbose = {
      flag = "--verbose"
      aliases = []
      flagType = TBoolean m
      defaultValue = None
      shortDescription = None
    }
    let fconfig = {
      flag = "--config"
      aliases = []
      flagType = TString m
      defaultValue = Some (String (m, "config.json"))
      shortDescription = None
    }
    let ftrace = {
      flag = "--trace"
      aliases = []
      flagType = TOption (TString m)
      defaultValue = None
      shortDescription = None
    }
    let ffile = {
      flag = "--file"
      aliases = []
      flagType = TString m
      defaultValue = None
      shortDescription = None
    }

    it "Should succeed if all mandatory flags are present" <| fun () ->
      let tr = TRecord(t, ["verbose", fverbose; "file", ffile; "config", fconfig])

      parse tr ["--file"; "a"]
      --> Ok (Record(tr, [
            "verbose", Boolean(m, false)
            "file", String(m, "a")
            "config", String(m, "config.json")
          ]), [])

      parse tr ["--verbose"; "--file"; "a"]
      --> Ok (Record(tr, [
            "verbose", Boolean(m, true)
            "file", String(m, "a")
            "config", String(m, "config.json")
          ]), [])

      parse tr ["--verbose"; "--config"; "x"; "--file"; "a"]
      --> Ok (Record(tr, [
            "verbose", Boolean(m, true)
            "file", String(m, "a")
            "config", String(m, "x")
          ]), [])

    it "Should leave positional arguments alone" <| fun () ->
      let tr = TRecord(t, ["verbose", fverbose; "config", fconfig])

      parse tr ["--config"; "hi"; "hello"]
      --> Ok (Record(tr, [
            "verbose", Boolean(m, false)
            "config", String(m, "hi")
          ]), ["hello"])

    it "Should fail on unrecognised flags if it hasn't finished parsing" <| fun () ->
      let tr = TRecord(t, ["verbose", fverbose; "file", ffile])

      parse tr ["--file"; "hi"; "--other"; "hello"]
      --> Ok (Record(tr, [
            "verbose", Boolean(m, false)
            "file", String(m, "hi")
          ]), ["--other"; "hello"])

      parse tr ["--verbose"; "--other"; "hello"]
      --> Error (UnrecognisedFlag (
                  tr,
                  "--other",
                  ["verbose", fverbose, Boolean(m, true)],
                  ["--other"; "hello"],
                  ["--verbose"; "--other"; "hello"]
                ))

  describe "Parsing commands" <| fun () ->
    let t = typeof<unit>
    let fverbose = {
      flag = "--verbose"
      aliases = []
      flagType = TBoolean m
      defaultValue = None
      shortDescription = None
    }
    let fconfig = {
      flag = "--config"
      aliases = []
      flagType = TString m
      defaultValue = Some (String (m, "config.json"))
      shortDescription = None
    }
    let tr = TRecord(t, ["verbose", fverbose; "config", fconfig])
    let t1 = [TInt m; tr]
    let t2 = []
    let t3 = [TOption (TString m)]
    let c0 = 0, None, { shortDescription = None; parameters = t1 }
    let c1 = 1, (Some "version"), { shortDescription = None; parameters = t2 }
    let c2 = 2, (Some "help"), { shortDescription = None; parameters = t3 }
    let ct = TUnion(t, [c1; c2; c0])
    let ct2 = TUnion(t, [c1; c2])
    let p (_, _, c) = c.parameters

    it "Should parse the command if all types parse" <| fun () ->
      parse ct ["1"; "--verbose"]
      --> Ok (Case (ct, 0, [
            Int(m, 1)
            Record(tr, ["verbose", Boolean(m, true); "config", String(m, "config.json")])
          ]), [])

      parse ct ["version"]
      --> Ok (Case (ct, 1, []), [])

      parse ct ["help"]
      --> Ok (Case (ct, 2, [Option(TString(m), None)]), [])

      parse ct ["help"; "version"]
      --> Ok (Case (ct, 2, [Option(TString(m), Some (String(m, "version")))]), [])

    it "Should fail if the command fails to parse" <| fun () ->
      parse ct ["x"]
      --> Error (CommandFailed(
                  c0, 
                  PartialParse(TTuple (p c0), [], TInt m, ["x"], ["x"]),
                  ["x"]
                ))

    it "Should fail if none of the commands succeed" <| fun () ->
      parse ct2 []
      --> Error (NoCommandsMatched([c1; c2], []))