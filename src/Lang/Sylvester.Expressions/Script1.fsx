#r "C:\\Users\\Allister\\.nuget\\packages\\fparsec\\1.0.3\\lib\\netstandard1.6\\FParsec.dll"
#r "C:\\Users\\Allister\\.nuget\\packages\\fparsec\\1.0.3\\lib\\netstandard1.6\\FParsecCS.dll"
#r "C:\\Users\\Allister\\.nuget\\packages\\mathnet.numerics\\4.11.0\\lib\\netstandard2.0\MathNet.Numerics.dll"
#r "C:\\Users\\Allister\\.nuget\\packages\\mathnet.numerics.fsharp\\4.11.0\\lib\\netstandard2.0\\MathNet.Numerics.FSharp.dll"
#r "bin\\Debug\\netstandard2.0\\MathNet.Symbolics.dll"
#r "bin\\Debug\\netstandard2.0\\Unquote.dll"
#r "bin\\Debug\\netstandard2.0\\Sylvester.Expressions.dll"

open Sylvester
open FSharp.Quotations
open FSharp.Quotations.Patterns

let var<'t> = Unchecked.defaultof<'t>

let x,y,z = var<float>, var<float>, var<float>

let eqns = <@[ 
    3. * x + 2. * y + 6. * y  + z + 3. * y= 0.
    2. * x - z = 4.
    z - y = 6.
 ]@>

eqns |> polyn_eqn_all_coeffs
//eqns |> expand_list |> List.item 0 |> expand_equality |> fst |> polyn_all_coeffs