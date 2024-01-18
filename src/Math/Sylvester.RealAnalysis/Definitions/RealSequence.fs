namespace Sylvester

open FSharp.Quotations

[<StructuredFormatDisplay("{UnicodeDisplay}")>]
type RealSequence(f:Expr<int->real>, ?c:real->bool, ?symbol:string, ?indexvar:IndexVar) =
    inherit RealFunction<int>(Ring.N, Field.R, f, ?symbol=symbol)
    member val Set = infinite_seq f (defaultArg c nocontainsimpl)
    member val IndexVar = defaultArg indexvar (new IndexVar("n"))
    member x.UnicodeDisplay =
        [|0..10|] |> Array.map x.Item
        |> Array.skip 1 
        |> Array.fold(fun s e -> sprintf "%s, %s" s (sprinte e.Expr)) (sprinte x.[0].Expr) 
        |> sprintf "(%s)"
    interface ISet<real> with
        member x.Set = infinite_seq f (defaultArg c nocontainsimpl)
        member x.Equals b = x.Set = b

[<AutoOpen>]
module RealSequence =
    let realseq s (n:string) f = RealSequence(f, indexvar=new IndexVar(n), symbol=s) 