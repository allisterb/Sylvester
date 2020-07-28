#r "C:\\Users\\Allister\\.nuget\\packages\\fparsec\\1.0.3\\lib\\netstandard1.6\\FParsec.dll"
#r "C:\\Users\\Allister\\.nuget\\packages\\fparsec\\1.0.3\\lib\\netstandard1.6\\FParsecCS.dll"
#r "C:\\Users\\Allister\\.nuget\\packages\\mathnet.numerics\\4.11.0\\lib\\netstandard2.0\MathNet.Numerics.dll"
#r "C:\\Users\\Allister\\.nuget\\packages\\mathnet.numerics.fsharp\\4.11.0\\lib\\netstandard2.0\\MathNet.Numerics.FSharp.dll"
#r "bin\\Debug\\netstandard2.0\\MathNet.Symbolics.dll"
#r "bin\\Debug\\netstandard2.0\\Unquote.dll"
#r "bin\\Debug\\netstandard2.0\\Sylvester.Expressions.dll"

open MathNet.Symbolics

open Sylvester

let j = Infix.parse "A + B"

let x = 0
let q = <@ x + 1 @>
let v = get_vars q
let e = Quotations.parse q


MathExpr.toQuotation (Quotations.parse <@ x @>) v typeof<float>