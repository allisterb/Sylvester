namespace Sylvester

open System
open System.Collections.Generic

open FSharp.Quotations

[<CustomEquality; CustomComparison>]
type SymExpr<'t> = SymExpr of Expr<'t> with
     member x.Expr = let (SymExpr e) = x in e
     interface IEquatable<SymExpr<'t>> with member a.Equals b = a.Expr.ToString() = b.Expr.ToString()
     interface IComparable<SymExpr<'t>> with
         member a.CompareTo b = if a.Expr = b.Expr then 0 else if b.Expr.ToString().Length > a.Expr.ToString().Length then -1 else 1
     interface IComparable with
         member a.CompareTo b = 
             match b with
             | :? SymExpr<'t> as e -> (a :> IComparable<SymExpr<'t>>).CompareTo e
             | _ -> failwith "This object is not an EExpr."
     override a.GetHashCode() = (a.Expr.ToString()).GetHashCode()
     override a.Equals (_b:obj) = 
             match _b with 
             | :? SymExpr<'t> as e -> (a :> IEquatable<SymExpr<'t>>).Equals e
             | _ -> false
     override x.ToString() = src (x.Expr)

type any = SymExpr<obj>

/// A statement that formally defines a set using a range, body, and an optional F# function for computing set membership.
type SetComprehension<'t when 't: equality>(range:Expr<bool>, body: Expr<'t>, ?hasElement:SetComprehension<'t> ->'t -> bool) = 
    member val Range = expand range
    member val internal Range' = range
    member val Body = expand body
    member val internal Body' = body
    member val HasElement = defaultArg hasElement (fun (sc:SetComprehension<'t>) (_:'t) -> failwithf "No set membership function is defined for the set comprehension %A." sc)
    override x.ToString() = 
        let vars = body |> get_vars
        let v = if Seq.isEmpty vars then "" else vars.[0].ToString() + "|"
        sprintf "{%s%s:%s}" v (src range) (src body)
    interface IEquatable<SetComprehension<'t>> with member a.Equals(b) = a.ToString() = b.ToString()
    override a.GetHashCode() = (a.ToString()).GetHashCode()
    override a.Equals (_b:obj) = 
            match _b with 
            | :? SetComprehension<'t> as b -> (a :> IEquatable<SetComprehension<'t>>).Equals b
            | _ -> false
    new(body: Expr<'t>, test: SetComprehension<'t> -> 't -> bool) = 
        let b = getExprFromReflectedDefinition<'t> body in 
        SetComprehension(<@ true @>, body, test)