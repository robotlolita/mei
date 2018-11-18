module OrigamiTower.Mei.Utils

module JsUtils =
  open Fable.Core
  open Fable.Core.JsInterop

  [<Import("inspect", "util")>]
  let private inspect : obj = jsNative

  let show obj = inspect$(obj, false, None, true)


/// Some new basic functional operators for F#
module Operators =
  /// Flips function application
  let flip f x y = f y x


/// Safe parsing for FSharp values.
/// 
/// Note that since Fable doesn't define a System.FormatError we use
/// a catch-all flag for conversions here.
module FSharpParsing =
  let tryInt (x:string) =
    try
      Some (int x)
    with _ -> None

  let tryDouble (x:string) =
    try
      Some (double x)
    with _ -> None

  let tryBoolean x =
    match x with
    | "true" -> Some true
    | "false" -> Some false
    | "yes" -> Some true
    | "no" -> Some false
    | _ -> None