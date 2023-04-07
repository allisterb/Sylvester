namespace Sylvester

open FSharp.Quotations

open Arithmetic

/// A mathematical structure consisting of a set together with a collection of n closed operations on elements of type t.
type IStruct<'t, 'n when 't: equality and 'n :> Number> = 
    inherit ISet<'t>
    abstract Ops: Ops<'n, 't>
     
/// Base implementation of a mathematical structure consisting of a set together with a collection of n closed operations on elements of type t.
/// This type is inherited by all other mathematical structure types.
type Struct<'t, 'n when 't: equality and 'n :> Number>(set: ISet<'t>, ops: Ops<'n, 't>) =  
    member val Set = set.Set
    member val Ops = ops    
    interface IStruct<'t, 'n> with
        member val Set = set.Set
        member x.Equals y = x.Set.Equals y
        member val Ops = ops

    static member (|+|) (l:Struct<'t, 'n>, r:ISet<'t>) = l.Set |+| r.Set

    static member (|+|) (l:ISet<'t>, r:Struct<'t, 'n>) = l.Set |+| r.Set

    static member (|+|) (l:Struct<'t, 'n>, r:SetTerm<'t>) = l.Set |+| r

    static member (|+|) (l:SetTerm<'t>, r:Struct<'t, 'n>) = l |+| r.Set
    
    static member (|*|) (l:Struct<'t, 'n>, r:ISet<'t>) = l.Set |*| r.Set

    static member (|*|) (l:ISet<'t>, r:Struct<'t, 'n>) = l.Set |*| r.Set

    static member (|*|) (l:Struct<'t, 'n>, r:SetTerm<'t>) = l.Set |*| r
    
    static member (|*|) (l:SetTerm<'t>, r:Struct<'t, 'n>) = l |*| r.Set

    static member (|?|) (l:'t, r:Struct<'t, 'n>) = r.Set.HasElement l

    static member (|?|) (l:Term<'t>, r:Struct<'t, 'n>) : Scalar<bool> = l |?| r.Set

    static member (|<|) (l:Struct<'t, 'n>, r:ISet<'t>) = r.Set.HasSubset l.Set
   
    static member (|<|) (l:ISet<'t>, r:Struct<'t, 'n>) = r.Set.HasSubset l.Set

    /// Set element difference operator.
    static member (|^|) (l:Struct<'t, 'n>, r:'t) = l.Set.ElementDifference r

    /// Set element difference operator.
    static member (|^|) (l:ISet<'t>, r:'t) = l.Set.ElementDifference r

    /// Set relative complement operator: A |/| B = B \ A.
    static member (|/|) (l:Set<'t>, r:Set<'t>) = l.Complement r

    /// Set absolute complement operator. -A = U \ A
    static member (~-) (l:Set<'t>) = l.Complement Set.U

    static member (|>|) (l:Struct<'t, 'n>, [<ReflectedDefinition>] r:Expr<'t->bool>) = l.Set.Subset r

    static member (|>>|) (l:Struct<'t, 'n>, r:Expr<Set<'t> -> bool>) = l.Set.Powerset.Subset r

/// A structure that has an identity element.
type IIdentity<'t when 't : equality> = 
    abstract member Identity:'t

/// A structure that has an identity element.
type IInverse<'t when 't : equality> = 
    abstract member Inverse:UnaryOp<'t>