module Mei.Tests.Encoding

open Fable.Import.Jest
open Mei.Tests.Matchers
open OrigamiTower.Mei.Core.TypeSystem
open OrigamiTower.Mei.Encoding


type RecordAB = { a: int; b: bool }

type UnionTest =
  | A of int * bool * string
  | B of int

describe "[Encoding] converting from mei" <| fun () ->
  it "Int -> int" <| fun () -> 
    (fromMei (Int 1) :?> int)
    --> 1

  it "Float -> double" <| fun () ->
    (fromMei (Float 1.0) :?> double)
    --> 1.0

  it "String -> string" <| fun () ->
    (fromMei (String "hello") :?> string)
    --> "hello"

  it "Boolean -> boolean" <| fun () ->
    (fromMei (Boolean true) :?> bool)
    --> true

  it "MeiType option -> T option" <| fun () ->
    Option (TInt, Some (Int 1))
    |> fromMei
    |> fun x -> x :?> obj option --> (Some 1)

  it "MeiType option -> T option (when None)" <| fun () ->
    Option (TUnit, None)
    |> fromMei
    |> fun x -> x :?> obj option --> (None)

  it "MeiType list -> T list" <| fun () ->
    List (TInt, [Int 1; Int 2; Int 3])
    |> fromMei
    |> fun x -> x :?> obj list --> [1; 2; 3]

  it "MeiType tuple -> (t1 * ... * tn)" <| fun () ->
    Tuple ([TInt; TBoolean; TString], [Int 1; Boolean true; String "hello"])
    |> fromMei
    |> fun x -> x :?> obj list --> [box 1; box true; box "hello"]

  it "MeiType record -> (string * obj) list" <| fun () ->
    let fa = { flag = "a"; aliases = []; flagType = TInt; defaultValue = None; shortDescription = None }
    let fb = { flag = "b"; aliases = []; flagType = TBoolean; defaultValue = None; shortDescription = None }
    let tr = TRecord(typeof<RecordAB>, ["a", fa; "b", fb])

    Record (tr, ["b", Boolean true; "a", Int 1])
    |> fromMei
    |> fun x -> x :?> RecordAB --> { a = 1; b = true }

  it "MeiType case -> (int * (t1 * ... * tn))" <| fun () ->
    let tu = TUnion(typeof<UnionTest>, [
      0, Some "A", { shortDescription = None; parameters = [TInt; TBoolean; TString] }
      1, Some "B", { shortDescription = None; parameters = [TInt] }
    ])

    Case (tu, 0, [Int 1; Boolean true; String "hello"])
    |> fromMei
    |> fun x -> x :?> UnionTest --> A(1, true, "hello")

    Case (tu, 1, [Int 1])
    |> fromMei
    |> fun x -> x :?> UnionTest --> B(1)

  describe "Type checking when converting" <| fun () ->
    it "T option with incorrect type" <| fun () ->
      Throws <| fun () ->
        Option (TInt, Some (Float 1.0))
        |> fromMei

    it "T list with incorrect type" <| fun () ->
      Throws <| fun () ->
        List (TInt, [Int 1; Float 2.0; Int 3])
        |> fromMei

    it "tuple with types in wrong order" <| fun () ->
      Throws <| fun () ->
        Tuple ([TInt; TBoolean; TString],
               [Int 1; Int 1; String "hello"])

        |> fromMei
    it "tuple with less types" <| fun () ->
      Throws <| fun () ->
        Tuple ([TInt; TBoolean; TString],
               [Int 1; Boolean true])
        |> fromMei

    it "tuple with more types" <| fun () ->
      Throws <| fun () ->
        Tuple ([TInt; TBoolean; TString],
               [Int 1; Boolean true; String "hello"; Int 2])
        |> fromMei

    describe "record" <| fun () ->
      let fa = { flag = "a"; 
                 aliases = []; 
                 flagType = TInt; 
                 defaultValue = None; 
                 shortDescription = None }
      let fb = { flag = "b"; 
                 aliases = []; 
                 flagType = TBoolean; 
                 defaultValue = None; 
                 shortDescription = None }
      let tr = TRecord(typeof<unit>, ["a", fa; "b", fb])    

      it "Fields with the wrong type" <| fun () ->
        Throws <| fun () ->
          Record (tr, ["a", Int 1; "b", Int 1])
          |> fromMei

      it "Fields missing" <| fun () ->
        Throws <| fun () ->
          Record (tr, ["b", Boolean true])
          |> fromMei

    describe "union" <| fun () ->
      let tu = TUnion(typeof<unit>, [
        0, None, { shortDescription = None; parameters = [TInt; TBoolean; TString] }
        1, None, { shortDescription = None; parameters = [TInt] }
      ])

      it "Fields in the wrong order" <| fun () ->
        Throws <| fun () ->
          Case (tu, 0, [Int 1; String "hello"; Boolean true])
          |> fromMei

      it "Fields missing" <| fun () ->
        Throws <| fun () ->
          Case (tu, 0, [Int 1; String "hello"])
          |> fromMei

      it "Wrong tag" <| fun () ->
        Throws <| fun () ->
          Case (tu, 3, [Int 1; String "hello"; Boolean true])
          |> fromMei

      it "Too many fields" <| fun () ->
        Throws <| fun () ->
          Case (tu, 1, [Int 1; String "hello"; Boolean true])
          |> fromMei


