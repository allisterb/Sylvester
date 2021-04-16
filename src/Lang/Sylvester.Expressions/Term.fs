namespace Sylvester

open System
open FSharp.Quotations

type Term<'t> (expr:Expr<'t>) =
    let src expr = Swensen.Unquote.Operators.decompile expr
    let formula = Unchecked.defaultof<'t>
    member x.Expr = expr
    member x.Item(i:int) = formula
    override a.GetHashCode() = (a.Expr.ToString()).GetHashCode()
    override a.Equals (_b:obj) = 
            match _b with 
            | :? Term<'t> as e -> (a :> IEquatable<Term<'t>>).Equals e
            | _ -> false
    override x.ToString() = src (x.Expr)
    interface IComparable<Term<'t>> with member a.CompareTo b = a.ToString().CompareTo(b.ToString())
    interface IComparable with
       member a.CompareTo b = 
           match b with
           | :? Term<'t> as Term -> (a :> IComparable<Term<'t>>).CompareTo Term
           | _ -> failwith "This object is not a Term."
    interface IEquatable<Term<'t>> with member a.Equals b = a.Expr.ToString() = b.Expr.ToString()
    static member (+)(l:Term<'t>, r:Term<'t>) = Unchecked.defaultof<'t>
    static member (+)(l:'t, r:Term<'t>) = Unchecked.defaultof<'t>
    static member (+)(l:Term<'t>, r:'t) = Unchecked.defaultof<'t>
    static member (*)(l:Term<'t>, r:Term<'t>) = Unchecked.defaultof<'t>
    static member (*)(l:'t, r:Term<'t>) = Unchecked.defaultof<'t>
    static member (*)(l:Term<'t>, r:'t) = Unchecked.defaultof<'t>
    static member (-)(l:Term<'t>, r:Term<'t>) = Unchecked.defaultof<'t>
    static member (-)(l:'t, r:Term<'t>) = Unchecked.defaultof<'t>
    static member (-)(l:Term<'t>, r:'t) = Unchecked.defaultof<'t>
    static member (^^)(l:Term<'t>, r:Term<'t>) = Unchecked.defaultof<'t>
    static member (^^)(l:'t, r:Term<'t>) = Unchecked.defaultof<'t>
    static member (^^)(l:Term<'t>, r:'t) = Unchecked.defaultof<'t>
    static member (^^)(l:Term<'t>, r:int) = Unchecked.defaultof<'t>
    static member (+..+)(l:Term<'t>, r:Term<'t>) = Unchecked.defaultof<seq<Term<'t>>>
    static member (+..+)(l:'t, r:Term<'t>) = Unchecked.defaultof<seq<Term<'t>>>
    static member (+..+)(l:Term<'t>, r:'t) = Unchecked.defaultof<seq<Term<'t>>>
    static member Zero = Unchecked.defaultof<'t>
    static member One = Unchecked.defaultof<'t>

[<AutoOpen>]
module Term =
    let private src expr = Swensen.Unquote.Operators.decompile expr
    
    let term_var<'t> n = 
        let v = Expr.Var(Var(n, typeof<'t>)) in <@ %%v:'t @> |> Term

    let term_expr (t:Term<'t>) = t.Expr

    let term_src(t:Term<'t>) = t |> (term_expr >> src) 


