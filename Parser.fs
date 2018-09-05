module Parser

open System

type Result<'a> = 
    | Success of 'a
    | Failure of string

type Parser<'T> = Parser of (string -> Result<'T * string>)

type NumberUnit = One | Five | Ten | Fifty | Hundred

let pnumber charToMatch number = 
    let innerFun str = 
        if String.IsNullOrEmpty(str) 
        then Failure "No input"
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
        | Failure reason -> Failure reason
        | Success (value1, remaining1) -> 
            match run parser2 remaining1 with
            | Failure reason -> Failure reason
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

let once parser =
    let rec innerFun input = 
        match run parser input with
        | Failure reason -> Failure reason
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


let one = pnumber 'I' One
let five = pnumber 'V' Five
let ten = pnumber 'X' Ten
let fifty = pnumber 'L' Fifty
let hundred = pnumber 'C' Hundred

let (<@>) left right = left .>>. right |>> (fun (x, y) -> x@y)

let nine = once one <@> once ten
let four = once one <@> once five
let fourty = once ten <@> once fifty
let ninety = once ten <@> once hundred

let romanParser = choice [ninety; zeroOrOneTime hundred] <@> choice [fourty; zeroOrOneTime fifty <@> zeroToThreeTimes ten] <@> choice [nine; four; zeroOrOneTime five <@> zeroToThreeTimes one]

let numberValue = function
    | One -> 1
    | Five -> 5
    | Ten -> 10
    | Fifty -> 50
    | Hundred -> 100

let sumNumbers numbers = 
        numbers 
        |> List.map numberValue
        |> List.pairwise
        |> List.map (fun (left, right) -> if left < right then -left else left)
        |> fun values -> values@[(numbers |> List.last |> numberValue)]
        |> List.sum

let parse value =
    if String.IsNullOrWhiteSpace(value) 
    then Failure "No input"
    else
        match run romanParser value with
        | Failure reason -> Failure reason
        | Success (numbers, "") -> Success (sumNumbers numbers)
        | Success (_, _) -> Failure  "Not a valid input : some characters are remaining"
