module Mei.Tests.Reflection

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import.Jest
open Mei.Tests.Matchers
open OrigamiTower.Mei.Core.TypeSystem
open OrigamiTower.Mei.Encoding
open OrigamiTower.Mei.Encoding.Reflection

open FSharp.Reflection

type Commands =
  | [<MainCommand>] Main
  | [<Prefix("--")>] Happy
  | Happy_World

type SimpleOptions = {
  verbose: bool;
  config: string option;
}

type SimpleCli =
  | Main of options: SimpleOptions * file: string
  | Version
  | Help of command: string option

let recType (t:MeiType) (c:int) (i:int) =
  match t with
  | TUnion(_, cs) ->
      let (_, _, command) = cs |> List.find (fun (ct, _, _) -> ct = c)
      match command.parameters.[i] with
      | TRecord(t, _) -> t
      | _ -> failwithf "Not a record"
  | _ -> failwithf "Not an union"
         

describe "[Reflection]" <| fun () ->
  it "F# name -> Command line name" <| fun () ->
    toCommandLineName "Hello_World" --> "hello-world"
    toCommandLineName "Hello_Happy_World" --> "hello-happy-world"

  describe "Command name from a union case" <| fun () ->
    let cases = FSharpType.GetUnionCases(typedefof<Commands>)
    let chello = cases.[0]
    let chappy = cases.[1]
    let cworld = cases.[2]

    it "Should be none if the command is MainCommand" <| fun () ->
      commandName chello --> None
     
    it "Should be the fixed name if there's no prefix" <| fun () ->
      commandName cworld --> Some "happy-world"

    #if FABLE_COMPILER
    #else
    it "Should be the prefixed name if there's a prefix" <| fun () ->
      commandName chappy --> Some "--happy"
    #endif
  
  describe "To Mei type" <| fun () ->
    let m = emptyMeta

    it "int" <| fun () ->
      Auto.toMeiType<int>()
      --> TInt m
    
    it "bool" <| fun () ->
      Auto.toMeiType<bool>()
      --> TBoolean m

    it "string" <| fun () ->
      Auto.toMeiType<string>()
      --> TString m

    it "double" <| fun () ->
      Auto.toMeiType<double>()
      --> TFloat m

    it "base option" <| fun () ->
      Auto.toMeiType<int option>()
      --> TOption (TInt m)
     
    it "base list" <| fun () ->
      Auto.toMeiType<int list>()
      --> TList (TInt m)

    it "generic list" <| fun () ->
      Auto.toMeiType<int option list>()
      --> TList (TOption (TInt m))

    it "record type" <| fun () ->
      let t = typeof<SimpleOptions>
      toMeiType t
      --> TRecord(t, [
            "verbose", { 
              flag = "--verbose"
              flagType = TBoolean m
              aliases = []
              shortDescription = None
              defaultValue = None
            }
            "config", {
              flag = "--config"
              flagType = TOption (TString m)
              aliases = []
              shortDescription = None
              defaultValue = None
            }
          ])
  
    it "union type" <| fun () ->
      let t = typeof<SimpleCli>
      let mt = toMeiType t
      mt
      --> TUnion(t, [
            0, None, { shortDescription = None; parameters = [
              TRecord(recType mt 0 0, [
                "verbose", { 
                  flag = "--verbose"
                  flagType = TBoolean m
                  aliases = []
                  shortDescription = None
                  defaultValue = None
                }
                "config", {
                  flag = "--config"
                  flagType = TOption (TString m)
                  aliases = []
                  shortDescription = None
                  defaultValue = None
                }
              ])
              TString m
            ]}
            1, Some "version", { shortDescription = None; parameters = [] }
            2, Some "help", { shortDescription = None; parameters = [TOption (TString m)] }
          ])
  
    
    