module Parser

open System

type Result<'a> = 
    | Success of 'a
    | Failure of string
    
type Parser<'T> = Parser of (string -> Result<'T * string>)

type Number = One | Five | Ten | Fifty

let pnumber charToMatch number = 
    let innerFun str = 
        if String.IsNullOrEmpty(str) 
        then Failure "No more input"
        else
            let first = str.[0]
            if first = charToMatch 
            then Success (number, str.[1..])
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
    List.reduce (<|>) listOfParsers 

let mapP f parser =
    let innerFn input =
        match run parser input with
        | Success (value,remaining) ->
            let newValue = f value
            Success (newValue, remaining)
        | Failure err -> Failure err
    Parser innerFn
let (|>>) x f = mapP f x

    
let zeroOrOneTime parser =
    let rec innerFun input = 
        match run parser input with
        | Failure _ -> Success ([], input)
        | Success (firstValue, inputAfterFirstParse) -> Success ([firstValue], inputAfterFirstParse) 
    Parser innerFun

let zeroToThreeTimes parser =
    let rec innerFun count input = 
        match run parser input with
        | Failure _ -> ([], input)
        | Success (firstValue, inputAfterFirstParse) when count = 3 ->
            ([firstValue], inputAfterFirstParse) 
        | Success (firstValue, inputAfterFirstParse) ->
            let (subsequentValues, remainingInput) = innerFun (count + 1) inputAfterFirstParse
            let values = firstValue::subsequentValues
            (values, remainingInput) 
    Parser (fun input -> Success (innerFun 1 input))


let parseOne = pnumber 'I' One
let parseFive = pnumber 'V' Five
let parseTen = pnumber 'X' Ten
let parseFifty = pnumber 'L' Fifty

let (<@>) left right = left .>>. right |>> (fun (x, y) -> x@y)

let parse value = 
    let parser = zeroOrOneTime parseFifty <@> zeroToThreeTimes parseTen <@> zeroOrOneTime parseFive <@> zeroToThreeTimes parseOne
    match run parser value with
    | Failure msg -> Failure msg
    | Success (numbers, "") ->
        Success (numbers |> List.sumBy (fun nb -> match nb with
                                                  | One -> 1
                                                  | Five -> 5
                                                  | Ten -> 10
                                                  | Fifty -> 50))
    | Success (_, _) -> Failure "Not a valid input"
