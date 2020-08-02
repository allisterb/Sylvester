#r "C:\\Users\\Allister\\.nuget\\packages\\fparsec\\1.0.3\\lib\\netstandard1.6\\FParsec.dll"
#r "C:\\Users\\Allister\\.nuget\\packages\\fparsec\\1.0.3\\lib\\netstandard1.6\\FParsecCS.dll"
#r "C:\\Users\\Allister\\.nuget\\packages\\mathnet.numerics\\4.11.0\\lib\\netstandard2.0\MathNet.Numerics.dll"
#r "C:\\Users\\Allister\\.nuget\\packages\\mathnet.numerics.fsharp\\4.11.0\\lib\\netstandard2.0\\MathNet.Numerics.FSharp.dll"
#r "bin\\Debug\\netstandard2.0\\MathNet.Symbolics.dll"
#r "bin\\Debug\\netstandard2.0\\Unquote.dll"
#r "bin\\Debug\\netstandard2.0\\Sylvester.Expressions.dll"


open Sylvester


let var<'t> = Unchecked.defaultof<'t>

let x = var<float>


let L,H = var<float>, var<float>


let zz = <@ L+H+L+H - (L+H) + 4. * L - 5. * H @> |> algeb_simplify |> algeb_expand 
printf "%A" (src zz)

polyn_coeffs <@ H @> zz |> Array.map src



//get_vars <@ x @>

//let v = <@ x @> |> expand |> get_vars

//<@ 4. * (x ** 2.) + x * (x + 6.) @> |> polyn_coeffs <@ x ** 2. @>
