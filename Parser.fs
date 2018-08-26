module Parser

open System

type Result<'a> = 
    | Success of 'a
    | Failure of string

type Parser<'T> = Parser of (string -> Result<'T * string>)

let pchar charToMatch = 
    let innerFun str = 
        if String.IsNullOrEmpty(str) 
        then Failure "No more input"
        else
            let first = str.[0]
            if first = charToMatch 
            then Success (first, str.[1..])
            else Failure (sprintf "Expecting '%c'. Got '%c'" charToMatch first)
    Parser innerFun 

let run parser input = 
    let (Parser innerFn) = parser 
    innerFn input

let andThen parser1 parser2 =
    let innerFun input =
        match run parser1 input with
        | Failure msg -> Failure msg
        | Success (value1, remaining1) -> 
            match run parser2 remaining1 with
            | Failure msg -> Failure msg
            | Success (value2, remaining2) -> 
                Success ((value1, value2), remaining2)
    Parser innerFun
let (.>>.) = andThen        

let orElse parser1 parser2 =
    let innerFun input =
        let result1 = run parser1 input
        match result1 with
        | Success _ -> result1
        | Failure _ -> run parser2 input
    Parser innerFun    
let (<|>) = orElse    

let choice listOfParsers = 
    List.reduce ( <|> ) listOfParsers 

let anyOf listOfChars = 
    listOfChars
    |> List.map pchar
    |> choice

let mapP f parser =
    let innerFn input =
        match run parser input with
        | Success (value,remaining) ->
            let newValue = f value
            Success (newValue, remaining)
        | Failure err -> Failure err
    Parser innerFn
let ( |>> ) x f = mapP f x



let parseI = pchar 'I'
let test = parseI <|> parseI .>>. parseI <|> (parseI .>>. parseI) .>>. parseI

let parseDigit = anyOf ['0'..'9']

let parseThreeDigitsAsStr =
    (parseDigit .>>. parseDigit .>>. parseDigit)
    |>> fun ((c1, c2), c3) -> String [| c1; c2; c3 |]

let parse value = sprintf "Parsed %s" value