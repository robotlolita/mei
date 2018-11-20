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


/// Converts F# to Mei through the reflection API
module Reflection =
  open Fable.Core
  open System.Text.RegularExpressions
  open OrigamiTower.Mei.Attributes


  /// Returns the custom attributes of a type.
  /// 
  /// Note that we need to move it to an inline function because
  /// Fable can't include runtime type information otherwise.
  /// Though it doesn't matter here because Fable doesn't support
  /// reading attributes for now.
  let inline customAttributes<'T when 'T :> Attribute> (t:Type) : seq<'T> = 
    #if FABLE_COMPILER
    Seq.empty
    #else
    t.GetCustomAttributes<'T>()
    #endif


  /// Returns the fields of an union case
  let getCaseFields (c:UnionCaseInfo) = c.GetFields()


  /// Returns the string without "-" at the end
  let removeEndDashes (s:string) =
    Regex.Replace(s, "\\-*$", "")


  /// Converts type/property names to suitable ones to be used in 
  /// the command line.
  let toCommandLineName (s:string) =
    let name = s.ToLower().Replace("_", "-")
    if name.EndsWith "-" then "--" + (removeEndDashes name)
    else name


  /// Converts some reflected union case information into a suitable
  /// command name.
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
               

  /// Gets the short description for a type.
  let shortDescription (t:Type) =
    customAttributes<ShortDescription> t
      |> Seq.tryLast
      |> Option.map (fun x -> x.Description)


  /// Gets the aliases of a type.
  let aliases (t:Type) =
    customAttributes<Alias> t
      |> Seq.toList
      |> List.map (fun x -> x.Alias)


  /// Gets the default value of a type.
  let defaultValue (t:Type) =
    customAttributes<DefaultValue> t
      |> Seq.tryLast
      |> Option.map (fun x -> x.DefaultValue)
  

  /// Converts some F# type to a Mei type.
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


  /// Converts an F# union to a Mei union.
  and private unionToMeiType t =
    assert FSharpType.IsUnion(t)

    FSharpType.GetUnionCases(t)
      |> Array.map caseToMei
      |> List.ofArray
      |> packAsMeiUnion t


  /// Converts an F# record to a Mei record
  and private recordToMeiType t =
    assert FSharpType.IsRecord(t)

    FSharpType.GetRecordFields(t)
      |> Array.map fieldToFlag
      |> List.ofArray
      |> packAsMeiRecord t


  /// Converts an F# union case to a Mei command case
  and caseToMei (c:UnionCaseInfo) =
    let types = c.GetFields() |> Array.map (fun x -> toMeiType x.PropertyType)
    in (c.Tag, commandName c, { shortDescription = shortDescription (c.GetType())
                                parameters = List.ofArray types })


  /// Constructs a Mei union from the reflected/prepared type information
  and packAsMeiUnion t cases = TUnion(t, cases)


  /// Converts a record field to a Mei flag
  and fieldToFlag field =
    (field.Name, {
      flag = "--" + (toCommandLineName field.Name)
      aliases = aliases (field.GetType())
      flagType = toMeiType field.PropertyType
      defaultValue = defaultValue (field.GetType())
      shortDescription = shortDescription (field.GetType())
    })


  /// Packs extracted information about record fields into a Mei record
  and packAsMeiRecord t fields = TRecord(t, fields)


  /// A helper type to convert a type definition to a Mei type automatically
  /// by using reflection.
  /// 
  /// We need this and its accompanying static member (with an injected
  /// parameter) because Fable 2 doesn't keep reflection information by
  /// default in the runtime types, and it can only reify that information
  /// in inline functions where types are statically known (without ambiguity),
  /// or injected type resolvers with equally statically known types.
  type Auto =
    static member toMeiType<'T>([<Inject>] ?resolver: ITypeResolver<'T>) =
      let typeInfo = resolver.Value.ResolveType()
      in toMeiType typeInfo

    