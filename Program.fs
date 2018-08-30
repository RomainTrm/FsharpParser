// Learn more about F# at http://fsharp.org

open System
open Parser

[<EntryPoint>]
let main argv =
    while true do
        let input = Console.ReadLine()
        let result = parse input
        match result with
        | Success value -> printfn "%d" value
        | Failure error -> printfn "%s" error
    0 // return an integer exit code
