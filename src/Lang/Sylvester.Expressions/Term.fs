namespace Sylvester

open System
open FSharp.Quotations

type Term<'t> (expr:Expr<'t>) =
    member x.Expr = expr
    member x.Item(i:int) = Unchecked.defaultof<'t>
    override a.GetHashCode() = (a.Expr.ToString()).GetHashCode()
    override a.Equals (_b:obj) = 
            match _b with 
            | :? Term<'t> as e -> (a :> IEquatable<Term<'t>>).Equals e
            | _ -> false
    override x.ToString() = Swensen.Unquote.Operators.decompile (x.Expr)
    interface IComparable<Term<'t>> with member a.CompareTo b = a.ToString().CompareTo(b.ToString())
    interface IComparable with
        member a.CompareTo b = 
            match b with
            | :? Term<'t> as Term -> (a :> IComparable<Term<'t>>).CompareTo Term
            | _ -> failwith "This object is not a Term."
    interface IEquatable<Term<'t>> with member a.Equals b = a.Expr.ToString() = b.Expr.ToString()

[<RequireQualifiedAccess>]
module Term =
    let inline add (l:Term<'t>) (r:Term<'t>) = 
        let ll, rr = l.Expr, r.Expr
        let e = <@ %ll + %rr @>
        Term e

    let inline zero() = 
        let z = LanguagePrimitives.GenericZero  
        Term <@ z @>

    let var<'t> n = 
        let v = Expr.Var(Var(n, typeof<'t>)) in <@ %%v:'t @> |> Term

    let expr (t:Term<'t>) = t.Expr

    let src(t:Term<'t>) = t |> (expr >> Swensen.Unquote.Operators.decompile)