namespace Sylvester

open FSharp.Quotations

type ProbabilityEvent<'t when 't: equality>(sample_space:ISet<'t>, subset:ISet<'t>) =
    member x.SampleSpace = sample_space.Set
    member x.Set = subset.Set
    member x.Prob = 
        if x.Set |<| x.SampleSpace  then measure x.Set / measure x.SampleSpace else 0.

    interface ISet<'t> with 
        member val Set = subset.Set
        member a.Equals b = a.Set = b

    static member (|+|) (l:ProbabilityEvent<'t>, r:ProbabilityEvent<'t>) =
        do if l.SampleSpace <> r.SampleSpace then failwith "The two events do not have the same sample space."
        ProbabilityEvent<'t>(l.SampleSpace, l.Set |+| r.Set)

    static member (|+|) (l:ProbabilityEvent<'t>, r:ISet<'t>) = l.Set |+| r.Set
    
    static member (|+|) (l:ISet<'t>, r:ProbabilityEvent<'t>) = l.Set |+| r.Set
    
    static member (|+|) (l:ProbabilityEvent<'t>, r:SetTerm<'t>) = l.Set |+| r
    
    static member (|+|) (l:SetTerm<'t>, r:ProbabilityEvent<'t>) = l |+| r.Set
        
    static member (|*|) (l:ProbabilityEvent<'t>, r:ProbabilityEvent<'t>) =
        do if l.SampleSpace <> r.SampleSpace then failwith "The two events do not have the same sample space."
        ProbabilityEvent<'t>(l.SampleSpace, l.Set |*| r.Set)

    static member (|*|) (l:ProbabilityEvent<'t>, r:ISet<'t>) = l.Set |*| r.Set
    
    static member (|*|) (l:ISet<'t>, r:ProbabilityEvent<'t>) = l.Set |*| r.Set
    
    static member (|*|) (l:ProbabilityEvent<'t>, r:SetTerm<'t>) = l.Set |*| r
        
    static member (|*|) (l:SetTerm<'t>, r:ProbabilityEvent<'t>) = l |*| r.Set
    
    static member (|?|) (l:'t, r:ProbabilityEvent<'t>) = r.Set.HasElement l
    
    static member (|?|) (l:Term<'t>, r:ProbabilityEvent<'t>) : Scalar<bool> = l |?| r.Set
    
    static member (|<|) (l:ProbabilityEvent<'t>, r:ISet<'t>) = r.Set.HasSubset l.Set
       
    static member (|<|) (l:ISet<'t>, r:ProbabilityEvent<'t>) = r.Set.HasSubset l.Set
    
    /// Set element difference operator.
    static member (|^|) (l:ProbabilityEvent<'t>, r:'t) = l.Set.ElementDifference r
    
    /// Set element difference operator.
    static member (|^|) (l:ISet<'t>, r:'t) = l.Set.ElementDifference r
    
    /// Set relative complement operator: A |/| B = B \ A.
    static member (|/|) (l:Set<'t>, r:Set<'t>) = l.Complement r
    
    /// Set absolute complement operator. -A = U \ A
    static member (~-) (l:Set<'t>) = l.Complement Set.U
    
    static member (|>|) (l:ProbabilityEvent<'t>, [<ReflectedDefinition>] r:Expr<'t->bool>) = l.Set.Subset r
    
    static member (|>>|) (l:ProbabilityEvent<'t>, r:Expr<Set<'t> -> bool>) = l.Set.Powerset.Subset r

    static member op_Stroke (l:ProbabilityEvent<'t>, r:ProbabilityEvent<'t>) =
           do if l.SampleSpace <> r.SampleSpace then failwith "The two events do not have the same sample space."
           ConditionalProbabilityEvent<'t>(r, l)
           
and ConditionalProbabilityEvent<'t when 't:equality>(prior:ProbabilityEvent<'t>, event:ProbabilityEvent<'t>) =
    inherit ProbabilityEvent<'t>(prior.SampleSpace, event)
    do if prior.SampleSpace <> event.SampleSpace then failwith "The two events do not have the same sample space."

    member val Prior = prior
    member val Prob = measure (event.Set |*| prior.Set) / (measure prior.Set)