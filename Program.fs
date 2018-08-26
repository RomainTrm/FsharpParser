// Learn more about F# at http://fsharp.org

open System
open Parser

[<EntryPoint>]
let main argv =
    while true do
        let input = Console.ReadLine()
        let result = parse input
        printfn "%s" result
    0 // return an integer exit code
