#load "MathInclude.fsx"
open System
open System.Linq
open System.Text
open Microsoft.FSharp.Reflection
open Sylvester

let dice = seq {1..6} |> Seq
dice.Length

let s = Subsets(dice, Seq [Empty; Seq [2; 3]; Seq [4; 5]; dice]) 
let g = SigmaAlgebra(s)

g.AsLattice.Least

let x = dice.Subsets(fun s -> s = Empty || s.Length = 2)
x.Count()

let gn = Seq [|5;6;6;7;8;9|] 
Nz.Set.Subset(fun x -> x % 2 = 0) |> Seq.take 10 |> Seq.toArray 

let printimefn fmt =
    let time = System.DateTime.Now
    Printf.ksprintf (
        fun s ->
            printfn "%02d:%02d:%02d %s" time.Hour time.Minute time.Second s)
        
       

//printimefn "foo"
type Stmt = string * obj list
let PrintfFormatProc (worker: string * obj list -> 'd)  (query: PrintfFormat<'a, _, _, 'd>) : 'a =
    if not (FSharpType.IsFunction typeof<'a>) then
        unbox (worker (query.Value, []))
    else
        let rec getFlattenedFunctionElements (functionType: Type) =
            let domain, range = FSharpType.GetFunctionElements functionType
            if not (FSharpType.IsFunction range)
                then domain::[range]
                else domain::getFlattenedFunctionElements(range)
        let types = getFlattenedFunctionElements typeof<'a>
        let rec proc (types: Type list) (values: obj list) (a: obj) : obj =
            let values = a::values
            match types with
            | [x;_] -> 
                let result = worker (query.Value, List.rev values)
                box result
            | x::y::z::xs -> 
                let cont = proc (y::z::xs) values
                let ft = FSharpType.MakeFunctionType(y,z)
                let cont = FSharpValue.MakeFunction(ft, cont)
                box cont
            | _ -> failwith "shouldn't happen"
-        
        let handler = proc types []
        unbox (FSharpValue.MakeFunction(typeof<'a>, handler))

let simpleProcessor (str: string, values: obj list) : Stmt =
    let stripFormatting s =
        let i = ref -1
        let eval (rxMatch: RegularExpressions.Match) =
            incr i
            sprintf "@p%d" !i
        RegularExpressions.Regex.Replace(s, "%.", eval)
    let sql = stripFormatting str
    Stmt(sql, values)
    //do values |> Seq.iter (fun x -> printf "Value: %A\n" x)
    //do printf "%s" sql

let simple a = PrintfFormatProc simpleProcessor a

//simple "fun (%s %i)" "foo" 5

let r = dice |>>| fun x -> x = Empty || x.Length = 2
r.Length
let d2 = dice.Prod |>| fun (x,y) -> x + y = 5 
//d2.First()
let urn = seq {1..5} |> Seq
let urn2 = urn.Prod
urn2.Length


//let d = (Z.Set.Prod) |>| fun (x,y) -> x > 5 

//d |> Seq.take 5 |> Seq.toArray

#load "MathInclude.fsx"
open System
open System.Linq
open System.Text
open Microsoft.FSharp.Reflection
open Sylvester

let dice = seq {1..6} |> Seq
dice.