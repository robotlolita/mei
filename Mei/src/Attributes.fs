[<AutoOpen>]
module OrigamiTower.Mei.Attributes

open System
open OrigamiTower.Mei.Core.TypeSystem

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