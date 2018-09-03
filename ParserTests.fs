module ParserTests

open Parser
open NUnit.Framework
open FsUnit

type TestCase = { Input: string; Result: Result<int> }

let testCases = [|
        { Input = "Invalid input"; Result = Failure "Not a valid input" }
        { Input = "IIII"; Result = Failure "Not a valid input" }
        { Input = "VV"; Result = Failure "Not a valid input"  }

        { Input = "I"; Result = Success 1 }
        { Input = "II"; Result = Success 2 }
        { Input = "III"; Result = Success 3 }
        { Input = "IV"; Result = Success 4 }
        { Input = "V"; Result = Success 5 }
        { Input = "VI"; Result = Success 6 }
        { Input = "VII"; Result = Success 7 }
        { Input = "VIII"; Result = Success 8 }    
        { Input = "IX"; Result = Success 9 }
        { Input = "X"; Result = Success 10 }
        { Input = "XI"; Result = Success 11 }
        { Input = "XII"; Result = Success 12 }
        { Input = "XIII"; Result = Success 13 }
        //{ Input = "XIV"; Result = Success 14 }
        { Input = "XV"; Result = Success 15 }
        { Input = "XVI"; Result = Success 16 }
        { Input = "XVII"; Result = Success 17 }
        { Input = "XVIII"; Result = Success 18 }
        //{ Input = "XIX"; Result = Success 19 }
        { Input = "XX"; Result = Success 20 }
        { Input = "XXI"; Result = Success 21 }
        { Input = "XXII"; Result = Success 22 }
        { Input = "XXIII"; Result = Success 23 }
        //{ Input = "XXIV"; Result = Success 24 }
        { Input = "XXV"; Result = Success 25 }
        { Input = "XXVI"; Result = Success 26 }
        { Input = "XXVII"; Result = Success 27 }
        { Input = "XXVIII"; Result = Success 28 }
        //{ Input = "XXIX"; Result = Success 29 }
        { Input = "XXX"; Result = Success 30 }
        { Input = "XXXI"; Result = Success 31 }
        { Input = "XXXII"; Result = Success 32 }
        { Input = "XXXIII"; Result = Success 33 }
        //{ Input = "XXXIV"; Result = Success 34 }
        { Input = "XXXV"; Result = Success 35 }
        { Input = "XXXVI"; Result = Success 36 }
        { Input = "XXXVII"; Result = Success 37 }
        { Input = "XXXVIII"; Result = Success 38 }
        //{ Input = "XXXIX"; Result = Success 39 }
        //{ Input = "XL"; Result = Success 40 }
        //{ Input = "XLI"; Result = Success 41 }
        //{ Input = "XLII"; Result = Success 42 }
        //{ Input = "XLIII"; Result = Success 43 }
        //{ Input = "XLIV"; Result = Success 44 }
        //{ Input = "XLV"; Result = Success 45 }
        //{ Input = "XLVI"; Result = Success 46 }
        //{ Input = "XLVII"; Result = Success 47 }
        //{ Input = "XLVIII"; Result = Success 48 }
        //{ Input = "XLIX"; Result = Success 49 }
        { Input = "L"; Result = Success 50 }
    |]

[<Test>]
[<TestCaseSource "testCases">]
let ``Should parse roman number`` case = 
    parse case.Input |> should equal case.Result