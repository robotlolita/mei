module Mei.Tests.FullParser

open Fable.Import.Jest
open Mei.Tests.Matchers
open OrigamiTower.Mei

type SimpleOptions = {
  verbose: bool;
  config: string option;
}

type SimpleCli =
  | Version
  | Help
  | Main of SimpleOptions * string

describe "Exposed parser" <| fun () ->
  let parser = cliParser<SimpleCli>()

  it "should convert to Main case" <| fun () ->
    parser.Parse ["--verbose"; "file.txt"]
    --> Main({ verbose = true; config = None }, "file.txt")

  it "should convert to Version case" <| fun () ->
    parser.Parse ["version"]
    --> Version

  it "should convert to Help case" <| fun () ->
    parser.Parse ["help"]
    --> Help