/// Allows converting between Mei's type system and F#'s
module OrigamiTower.Mei.Encoding

open System
open System.Reflection
open FSharp.Reflection
open OrigamiTower.Mei.Core.TypeSystem
open OrigamiTower.Mei.Utils.Operators


/// A value that can be converted to a Mei value
type IToMeiValue =
  abstract member ToMeiValue : unit -> MeiValue


/// Converts a Mei type to an F# type.
/// 
/// Note that the return of this is an `obj` and you must
/// coerce it to the right type.
let rec fromMei value =
  match value with
  | Int(_, v) -> box v
  | Float(_, v) -> box v
  | String(_, v) -> box v
  | Boolean(_, v) -> box v
  
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
           
#if FABLE_COMPILER
#else
/// Converts an F# type to a Mei type
let rec toMei (x:obj) =
  let inline tuple xs = let vs = List.map toMei xs 
                        in Tuple (List.map typeOf vs, vs)

  match x with
  | :? (obj * obj) as t2 -> 
      let (a, b) = t2 in tuple [a; b]

  | :? (obj * obj * obj) as t3 ->
      let (a, b, c) = t3 in tuple [a; b; c]

  | :? (obj * obj * obj * obj) as t4 ->
      let (a, b, c, d) = t4 in tuple [a; b; c; d]

  | :? (obj * obj * obj * obj * obj) as t5 ->
      let (a, b, c, d, e) = t5 in tuple [a; b; c; d; e]

  | :? (obj * obj * obj * obj * obj * obj) as t6 ->
      let (a, b, c, d, e, f) = t6 in tuple [a; b; c; d; e; f]

  | :? int as i -> Int (emptyMeta, i)
  | :? float as f -> Float (emptyMeta, f)
  | :? string as s -> String (emptyMeta, s)
  | :? bool as b -> Boolean (emptyMeta, b)

  | :? (obj option) as o -> 
        match o with
        | Some v -> let v = toMei v
                    in Option ((typeOf v), Some v)
        | None -> Option (TUnit, None)

  | :? (obj list) as l -> 
        match l with
        | h :: rest -> let v = toMei h
                       in List ((typeOf v), v :: (List.map toMei rest))
        | [] -> List (TUnit, [])

  | :? IToMeiValue as t -> t.ToMeiValue()
  | _ -> failwithf "%s does not implement IToMei" (x.ToString())
#endif

[<AutoOpen>]
module Attributes =
  [<AttributeUsage(AttributeTargets.Property, AllowMultiple = true)>]
  type Alias(alias:string) = 
    inherit Attribute()
    member __.Alias = alias

  [<AttributeUsage(AttributeTargets.Property, AllowMultiple = false)>]
  type ShortDescription(desc:string) = 
    inherit Attribute()
    member __.Description = desc

  [<AttributeUsage(AttributeTargets.Property, AllowMultiple = false)>]
  type DefaultValue(value: MeiValue) =
    inherit Attribute()
    member __.DefaultValue = value

  [<AttributeUsage(AttributeTargets.Property, AllowMultiple = false)>]
  type MainCommand() = inherit Attribute()
  
  [<AttributeUsage(AttributeTargets.Property, AllowMultiple = false)>]
  type Prefix(prefix: string) =
    inherit Attribute()
    member __.Prefix = prefix


module Reflection =
  open Fable.Core
  open FSharp.Reflection
  open System.Reflection
  open Attributes


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
      | "System.Boolean" -> TBoolean emptyMeta
      | "System.Int32" -> TInt emptyMeta
      | "System.Double" -> TFloat emptyMeta
      | "System.String" -> TString emptyMeta
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

    