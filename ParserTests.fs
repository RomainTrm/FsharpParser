module ParserTests

open Parser
open NUnit.Framework
open FsUnit

type TestCase = { Input: string; Result: Result<int> }

[<Test>]
[<TestCaseSource "testCases">]
let ``Should parse roman number`` case = 
    parse case.Input |> should equal case.Result

let testCases = [|
        { Input = "Invalid input"; Result = Failure "Not a valid input : some characters are remaining" }
        { Input = "IIII"; Result = Failure "Not a valid input : some characters are remaining" }
        { Input = "VV"; Result = Failure "Not a valid input : some characters are remaining"  }

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
        { Input = "XIV"; Result = Success 14 }
        { Input = "XV"; Result = Success 15 }
        { Input = "XVI"; Result = Success 16 }
        { Input = "XVII"; Result = Success 17 }
        { Input = "XVIII"; Result = Success 18 }
        { Input = "XIX"; Result = Success 19 }
        { Input = "XX"; Result = Success 20 }
        { Input = "XXI"; Result = Success 21 }
        { Input = "XXII"; Result = Success 22 }
        { Input = "XXIII"; Result = Success 23 }
        { Input = "XXIV"; Result = Success 24 }
        { Input = "XXV"; Result = Success 25 }
        { Input = "XXVI"; Result = Success 26 }
        { Input = "XXVII"; Result = Success 27 }
        { Input = "XXVIII"; Result = Success 28 }
        { Input = "XXIX"; Result = Success 29 }
        { Input = "XXX"; Result = Success 30 }
        { Input = "XXXI"; Result = Success 31 }
        { Input = "XXXII"; Result = Success 32 }
        { Input = "XXXIII"; Result = Success 33 }
        { Input = "XXXIV"; Result = Success 34 }
        { Input = "XXXV"; Result = Success 35 }
        { Input = "XXXVI"; Result = Success 36 }
        { Input = "XXXVII"; Result = Success 37 }
        { Input = "XXXVIII"; Result = Success 38 }
        { Input = "XXXIX"; Result = Success 39 }
        { Input = "XL"; Result = Success 40 }
        { Input = "XLI"; Result = Success 41 }
        { Input = "XLII"; Result = Success 42 }
        { Input = "XLIII"; Result = Success 43 }
        { Input = "XLIV"; Result = Success 44 }
        { Input = "XLV"; Result = Success 45 }
        { Input = "XLVI"; Result = Success 46 }
        { Input = "XLVII"; Result = Success 47 }
        { Input = "XLVIII"; Result = Success 48 }
        { Input = "XLIX"; Result = Success 49 }
        { Input = "L"; Result = Success 50 }
        { Input = "LI"; Result = Success 51 }
        { Input = "LII"; Result = Success 52 }
        { Input = "LIII"; Result = Success 53 }
        { Input = "LIV"; Result = Success 54 }
        { Input = "LV"; Result = Success 55 }
        { Input = "LVI"; Result = Success 56 }
        { Input = "LVII"; Result = Success 57 }
        { Input = "LVIII"; Result = Success 58 }
        { Input = "LIX"; Result = Success 59 }
        { Input = "LX"; Result = Success 60 }
        { Input = "LXI"; Result = Success 61 }
        { Input = "LXII"; Result = Success 62 }
        { Input = "LXIII"; Result = Success 63 }
        { Input = "LXIV"; Result = Success 64 }
        { Input = "LXV"; Result = Success 65 }
        { Input = "LXVI"; Result = Success 66 }
        { Input = "LXVII"; Result = Success 67 }
        { Input = "LXVIII"; Result = Success 68 }
        { Input = "LXIX"; Result = Success 69 }
        { Input = "LXX"; Result = Success 70 }
        { Input = "LXXI"; Result = Success 71 }
        { Input = "LXXII"; Result = Success 72 }
        { Input = "LXXIII"; Result = Success 73 }
        { Input = "LXXIV"; Result = Success 74 }
        { Input = "LXXV"; Result = Success 75 }
        { Input = "LXXVI"; Result = Success 76 }
        { Input = "LXXVII"; Result = Success 77 }
        { Input = "LXXVIII"; Result = Success 78 }
        { Input = "LXXIX"; Result = Success 79 }
        { Input = "LXXX"; Result = Success 80 }
        { Input = "LXXXI"; Result = Success 81 }
        { Input = "LXXXII"; Result = Success 82 }
        { Input = "LXXXIII"; Result = Success 83 }
        { Input = "LXXXIV"; Result = Success 84 }
        { Input = "LXXXV"; Result = Success 85 }
        { Input = "LXXXVI"; Result = Success 86 }
        { Input = "LXXXVII"; Result = Success 87 }
        { Input = "LXXXVIII"; Result = Success 88 }
        { Input = "LXXXIX"; Result = Success 89 }
        { Input = "XC"; Result = Success 90 }
        { Input = "XCI"; Result = Success 91 }
        { Input = "XCII"; Result = Success 92 }
        { Input = "XCIII"; Result = Success 93 }
        { Input = "XCIV"; Result = Success 94 }
        { Input = "XCV"; Result = Success 95 }
        { Input = "XCVI"; Result = Success 96 }
        { Input = "XCVII"; Result = Success 97 }
        { Input = "XCVIII"; Result = Success 98 }
        { Input = "XCIX"; Result = Success 99 }
        { Input = "C"; Result = Success 100 }
        { Input = "CI"; Result = Success 101 }
        { Input = "CII"; Result = Success 102 }
        { Input = "CIII"; Result = Success 103 }
        { Input = "CIV"; Result = Success 104 }
        { Input = "CV"; Result = Success 105 }
        { Input = "CVI"; Result = Success 106 }
        { Input = "CVII"; Result = Success 107 }
        { Input = "CVIII"; Result = Success 108 }
        { Input = "CIX"; Result = Success 109 }
        { Input = "CX"; Result = Success 110 }
        { Input = "CXI"; Result = Success 111 }
        { Input = "CXII"; Result = Success 112 }
        { Input = "CXIII"; Result = Success 113 }
        { Input = "CXIV"; Result = Success 114 }
        { Input = "CXV"; Result = Success 115 }
        { Input = "CXVI"; Result = Success 116 }
        { Input = "CXVII"; Result = Success 117 }
        { Input = "CXVIII"; Result = Success 118 }
        { Input = "CXIX"; Result = Success 119 }
        { Input = "CXX"; Result = Success 120 }
        { Input = "CXXI"; Result = Success 121 }
        { Input = "CXXII"; Result = Success 122 }
        { Input = "CXXIII"; Result = Success 123 }
        { Input = "CXXIV"; Result = Success 124 }
        { Input = "CXXV"; Result = Success 125 }
        { Input = "CXXVI"; Result = Success 126 }
        { Input = "CXXVII"; Result = Success 127 }
        { Input = "CXXVIII"; Result = Success 128 }
        { Input = "CXXIX"; Result = Success 129 }
        { Input = "CXXX"; Result = Success 130 }
        { Input = "CXXXI"; Result = Success 131 }
        { Input = "CXXXII"; Result = Success 132 }
        { Input = "CXXXIII"; Result = Success 133 }
        { Input = "CXXXIV"; Result = Success 134 }
        { Input = "CXXXV"; Result = Success 135 }
        { Input = "CXXXVI"; Result = Success 136 }
        { Input = "CXXXVII"; Result = Success 137 }
        { Input = "CXXXVIII"; Result = Success 138 }
        { Input = "CXXXIX"; Result = Success 139 }
        { Input = "CXL"; Result = Success 140 }
        { Input = "CXLI"; Result = Success 141 }
        { Input = "CXLII"; Result = Success 142 }
        { Input = "CXLIII"; Result = Success 143 }
        { Input = "CXLIV"; Result = Success 144 }
        { Input = "CXLV"; Result = Success 145 }
        { Input = "CXLVI"; Result = Success 146 }
        { Input = "CXLVII"; Result = Success 147 }
        { Input = "CXLVIII"; Result = Success 148 }
        { Input = "CXLIX"; Result = Success 149 }
        { Input = "CL"; Result = Success 150 }
    |]