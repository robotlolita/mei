module Mei.Tests.Encoding

open Fable.Import.Jest
open Mei.Tests.Matchers
open OrigamiTower.Mei.Core.TypeSystem
open OrigamiTower.Mei.Encoding

let m = emptyMeta

type RecordAB = { a: int; b: bool }

type UnionTest =
  | A of int * bool * string
  | B of int

describe "[Encoding] converting from mei" <| fun () ->
  it "Int -> int" <| fun () -> 
    (fromMei (Int (m, 1)) :?> int)
    --> 1

  it "Float -> double" <| fun () ->
    (fromMei (Float (m, 1.0)) :?> double)
    --> 1.0

  it "String -> string" <| fun () ->
    (fromMei (String (m, "hello")) :?> string)
    --> "hello"

  it "Boolean -> boolean" <| fun () ->
    (fromMei (Boolean (m, true)) :?> bool)
    --> true

  it "MeiType option -> T option" <| fun () ->
    Option (TInt m, Some (Int (m, 1)))
    |> fromMei
    |> fun x -> x :?> obj option --> (Some 1)

  it "MeiType option -> T option (when None)" <| fun () ->
    Option (TUnit, None)
    |> fromMei
    |> fun x -> x :?> obj option --> (None)

  it "MeiType list -> T list" <| fun () ->
    List (TInt m, [Int(m, 1); Int(m, 2); Int(m, 3)])
    |> fromMei
    |> fun x -> x :?> obj list --> [1; 2; 3]

  it "MeiType tuple -> (t1 * ... * tn)" <| fun () ->
    Tuple ([TInt(m); TBoolean(m); TString(m)], [Int(m, 1); Boolean(m, true); String(m, "hello")])
    |> fromMei
    |> fun x -> x :?> obj list --> [box 1; box true; box "hello"]

  it "MeiType record -> (string * obj) list" <| fun () ->
    let fa = { flag = "a"; aliases = []; flagType = TInt m; defaultValue = None; shortDescription = None }
    let fb = { flag = "b"; aliases = []; flagType = TBoolean m; defaultValue = None; shortDescription = None }
    let tr = TRecord(typeof<RecordAB>, ["a", fa; "b", fb])

    Record (tr, ["b", (Boolean (m, true)); "a", (Int (m, 1))])
    |> fromMei
    |> fun x -> x :?> RecordAB --> { a = 1; b = true }

  it "MeiType case -> (int * (t1 * ... * tn))" <| fun () ->
    let tu = TUnion(typeof<UnionTest>, [
      0, Some "A", { shortDescription = None; parameters = [TInt m; TBoolean m; TString m] }
      1, Some "B", { shortDescription = None; parameters = [TInt m] }
    ])

    Case (tu, 0, [Int (m, 1); Boolean (m, true); String (m, "hello")])
    |> fromMei
    |> fun x -> x :?> UnionTest --> A(1, true, "hello")

    Case (tu, 1, [Int (m, 1)])
    |> fromMei
    |> fun x -> x :?> UnionTest --> B(1)

  describe "Type checking when converting" <| fun () ->
    it "T option with incorrect type" <| fun () ->
      Throws <| fun () ->
        Option (TInt m, Some (Float (m, 1.0)))
        |> fromMei

    it "T list with incorrect type" <| fun () ->
      Throws <| fun () ->
        List (TInt m, [Int(m, 1); Float(m, 2.0); Int(m, 3)])
        |> fromMei

    it "tuple with types in wrong order" <| fun () ->
      Throws <| fun () ->
        Tuple ([TInt(m); TBoolean(m); TString(m)],
               [Int(m, 1); Int(m, 1); String(m, "hello")])

        |> fromMei
    it "tuple with less types" <| fun () ->
      Throws <| fun () ->
        Tuple ([TInt(m); TBoolean(m); TString(m)],
               [Int(m, 1); Boolean(m, true)])
        |> fromMei

    it "tuple with more types" <| fun () ->
      Throws <| fun () ->
        Tuple ([TInt(m); TBoolean(m); TString(m)],
               [Int(m, 1); Boolean(m, true); String(m, "hello"); Int(m, 2)])
        |> fromMei

    describe "record" <| fun () ->
      let fa = { flag = "a"; 
                 aliases = []; 
                 flagType = TInt m; 
                 defaultValue = None; 
                 shortDescription = None }
      let fb = { flag = "b"; 
                 aliases = []; 
                 flagType = TBoolean m; 
                 defaultValue = None; 
                 shortDescription = None }
      let tr = TRecord(typeof<unit>, ["a", fa; "b", fb])    

      it "Fields with the wrong type" <| fun () ->
        Throws <| fun () ->
          Record (tr, ["a", (Int (m, 1)); "b", (Int (m, 1))])
          |> fromMei

      it "Fields missing" <| fun () ->
        Throws <| fun () ->
          Record (tr, ["b", (Boolean (m, true))])
          |> fromMei

    describe "union" <| fun () ->
      let tu = TUnion(typeof<unit>, [
        0, None, { shortDescription = None; parameters = [TInt m; TBoolean m; TString m] }
        1, None, { shortDescription = None; parameters = [TInt m] }
      ])

      it "Fields in the wrong order" <| fun () ->
        Throws <| fun () ->
          Case (tu, 0, [Int (m, 1); String (m, "hello"); Boolean (m, true)])
          |> fromMei

      it "Fields missing" <| fun () ->
        Throws <| fun () ->
          Case (tu, 0, [Int (m, 1); String (m, "hello")])
          |> fromMei

      it "Wrong tag" <| fun () ->
        Throws <| fun () ->
          Case (tu, 3, [Int (m, 1); String (m, "hello"); Boolean (m, true)])
          |> fromMei

      it "Too many fields" <| fun () ->
        Throws <| fun () ->
          Case (tu, 1, [Int (m, 1); String (m, "hello"); Boolean (m, true)])
          |> fromMei


