/// Allows converting between Mei's type system and F#'s
module OrigamiTower.Mei.Encoding

open System
open System.Reflection
open FSharp.Reflection
open OrigamiTower.Mei.Core.TypeSystem


/// Converts a Mei type to an F# type.
/// 
/// Note that the return of this is an `obj` and you must
/// coerce it to the right type.
let rec fromMei value =
  match value with
  | Int v -> box v
  | Float v -> box v
  | String v -> box v
  | Boolean v -> box v
  
  | Option(t, Some v) -> fromMei (assertType t v) |> Some |> box
  | Option(_, None) -> box None

  | List(t, vs) -> 
      vs |> List.map (assertType t >> fromMei) |> box

  | Tuple(ts, vs) -> 
      assertType (typeOf value) value |> ignore
      vs |> List.map fromMei |> box

  | Record(t, vs) ->
      recordEntries (assertType t value) 
        |> List.map (fun (_, v) -> fromMei v)
        |> fun xs -> FSharpValue.MakeRecord((recordNetType t), Array.ofList xs)
        |> box

  | Case(t, tag, vs) ->
      assertType t value |> ignore
      let unionType = unionNetType t
      let cases = FSharpType.GetUnionCases(unionType)
      let c = cases |> Array.find (fun x -> x.Tag = tag)
      FSharpValue.MakeUnion(c, vs |> List.map fromMei |> Array.ofList)
        |> box


module Reflection =
  open Fable.Core
  open FSharp.Reflection
  open System.Reflection
  open OrigamiTower.Mei.Attributes


  let inline customAttributes<'T when 'T :> Attribute> (t:Type) : seq<'T> = 
    #if FABLE_COMPILER
    Seq.empty
    #else
    t.GetCustomAttributes<'T>()
    #endif


  let getCaseFields (c:UnionCaseInfo) = c.GetFields()

  let toCommandLineName (s:string) =
    s.ToLower().Replace("_", "-")

  let commandName (c:UnionCaseInfo) =
    let t = c.GetType()
    if c.Name = "Main" then
      None
    else
      match customAttributes<MainCommand> t |> Seq.tryLast with
      | Some _ -> None
      | None ->
          match customAttributes<Prefix> t |> Seq.tryLast with
          | Some prefix -> Some (prefix.Prefix + (toCommandLineName c.Name))
          | None -> Some (toCommandLineName c.Name)
               
  let shortDescription (t:Type) =
    customAttributes<ShortDescription> t
      |> Seq.tryLast
      |> Option.map (fun x -> x.Description)

  let aliases (t:Type) =
    customAttributes<Alias> t
      |> Seq.toList
      |> List.map (fun x -> x.Alias)

  let defaultValue (t:Type) =
    customAttributes<DefaultValue> t
      |> Seq.tryLast
      |> Option.map (fun x -> x.DefaultValue)
  

  let rec toMeiType (t:Type) =
    if FSharpType.IsUnion(t) then
      unionToMeiType(t)
    else if FSharpType.IsRecord(t) then
      recordToMeiType(t)
    else if t.IsGenericType then
      match t.GetGenericTypeDefinition().FullName with
      | x when x = typedefof<obj option>.FullName ->
          let t1 = t.GenericTypeArguments.[0]
          in TOption (toMeiType t1)
      | x when x = typedefof<obj list>.FullName ->
          let t1 = t.GenericTypeArguments.[0]
          in TList (toMeiType t1)
      | x ->
          failwithf "Unsupported type %s" x
    else
      match t.FullName with
      | "System.Boolean" -> TBoolean
      | "System.Int32" -> TInt
      | "System.Double" -> TFloat
      | "System.String" -> TString
      | x -> failwithf "Unsupported type %s" x  

  and private unionToMeiType t =
    assert FSharpType.IsUnion(t)

    FSharpType.GetUnionCases(t)
      |> Array.map caseToMei
      |> List.ofArray
      |> packAsMeiUnion t

  and private recordToMeiType t =
    assert FSharpType.IsRecord(t)

    FSharpType.GetRecordFields(t)
      |> Array.map fieldToFlag
      |> List.ofArray
      |> packAsMeiRecord t

  and caseToMei (c:UnionCaseInfo) =
    let types = c.GetFields() |> Array.map (fun x -> toMeiType x.PropertyType)
    in (c.Tag, commandName c, { shortDescription = shortDescription (c.GetType())
                                parameters = List.ofArray types })

  and packAsMeiUnion t cases = TUnion(t, cases)

  and fieldToFlag field =
    (field.Name, {
      flag = "--" + (toCommandLineName field.Name)
      aliases = aliases (field.GetType())
      flagType = toMeiType field.PropertyType
      defaultValue = defaultValue (field.GetType())
      shortDescription = shortDescription (field.GetType())
    })

  and packAsMeiRecord t fields = TRecord(t, fields)


  type Auto =
    static member toMeiType<'T>([<Inject>] ?resolver: ITypeResolver<'T>) =
      let typeInfo = resolver.Value.ResolveType()
      in toMeiType typeInfo

    