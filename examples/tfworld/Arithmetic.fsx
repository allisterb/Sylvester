#r ".\\..\\..\\src\\Providers\\Sylvester.Provider.Arithmetic\\src\\Sylvester.Provider.Arithmetic.Runtime\\bin\\Release\\net45\\Sylvester.Provider.Arithmetic.Runtime.dll"
//#r ".\\..\\..\\src\\Base\\Sylvester.Collections\\bin\\Debug\\net45\\Sylvester.Collections.dll"


type Table = Table of string[,]

#if HAS_FSI_ADDHTMLPRINTER
fsi.AddHtmlPrinter(fun (Table t) ->
  let body = 
    [ yield "<table>"
      for i in 0 .. t.GetLength(0)-1 do
        yield "<tr>"
        for j in 0 .. t.GetLength(1)-1 do
          yield "<td>" + t.[i,j] + "</td>" 
        yield "</tr>"
      yield "</table>" ] 
    |> String.concat ""
  seq [ "style", "<style>table { background:#f0f0f0; }</style>" ],
  body )
#endif


open FSharp.Reflection
open Microsoft.FSharp.Quotations
open Sylvester.Arithmetic

//type Display() =
//    static member Show([<ReflectedDefinitionAttribute(true)>] f:Expr<int->int>) = f

[<StructuredFormatDisplay("{F}")>]
type Display([<ReflectedDefinitionAttribute>] f:Expr<_->int>) =
    member val F = f


let f x:int = x + 3

Display f
//Display.Show(add)

//let show (f:int->int) = Display.Show(f)

//show add
//let f [<ReflectedDefm
//type one = dim<N<1>>

//type r = Mat<dim<3, 4>, float>
//let v = Vec<dim<4>()
//printf "%s" typeof<dim<4, 7>>.Name
//typeof<dim<4, 7>>.Gen
//Type-level arithmetic
//let a = new n<500>()

//let b = new n<125>()

//let c = a + b


//Type-level comparison
//let d = c +> a

//let e = c * a +< b

//Type-level static checks
//check(b +> zero)
//check(b +< zero)
//c