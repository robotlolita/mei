// The naval fate example from http://docopt.org/
module Mei.Examples.NavalFate

open Fable.Import.Node.Globals
open OrigamiTower.Mei

type Commands =
  | Ship of ShipCommands
  | Mine of MineCommands
  | Help
  | Version

and ShipCommands =
  | New of names: string list
  | Shoot of x: int * y: int
  | Move of name: string * x: int * y: int * MoveOptions

and MoveOptions = {
  speed: int option
}

and MineCommands =
  | Set of x: int * y: int * MineOptions
  | Remove of x: int * y: int * MineOptions

and MineOptions = {
  moored: bool;
  drifting: bool
}

let describeMine (opts:MineOptions) =
  [
    (if opts.moored then ["moored"] else [])
    (if opts.drifting then ["drifting"] else [])
  ] |> List.concat
    |> String.concat ", "

let args = ``process``.argv |> Seq.skip 2 |> Seq.toList

let parser = cliParser<Commands>()

match parser.Parse args with
| Ship (New (names)) -> 
    printfn "Creating new ships %s" (String.concat ", " names)

| Ship (Shoot (x, y)) -> 
    printfn "Shooting ship at %i, %i" x y

| Ship (Move (name, x, y, opts)) ->
    let speedDesc = opts.speed |> Option.map (fun x -> sprintf "(at speed %i)" x)
                               |> Option.defaultValue ""
    in printfn "Moving ship %s to %i, %i %s" name x y speedDesc

| Mine (Set (x, y, opts)) ->
    printf "Setting mine at %i, %i %s" x y (describeMine opts)

| Mine (Remove (x, y, opts)) ->
    printf "Removing mine from %i, %i %s" x y (describeMine opts)

| Version ->
    printfn "Naval Fate version 1.0.0"

| Help ->
    printfn 
      """
      Naval Fate.

      Usage:
        naval_fate ship new <name>...
        naval_fate ship <name> move <x> <y> [--speed=<kn>]
        naval_fate ship shoot <x> <y>
        naval_fate mine (set|remove) <x> <y> [--moored|--drifting]
        naval_fate -h | --help
        naval_fate --version

      Options:
        -h --help     Show this screen.
        --version     Show version.
        --speed=<kn>  Speed in knots [default: 10].
        --moored      Moored (anchored) mine.
        --drifting    Drifting mine.
      """
