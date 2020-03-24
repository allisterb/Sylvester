namespace Sylvester

module SetAlgebraTheory =
    open BooleanAlgebraTheory    

    /// Print set algebra operator symbols
    let print_set_algebra_operators (s:string) = 
        s.Replace("|+|", "\u222A")
         .Replace("|*|", "\u2229")

    let set_algebra<'t when 't: equality> = BooleanAlgebraTheory("Set Algebra", <@ Set.(|+|) @>, <@ Set.(|*|) @>, <@ Set.Empty @>, <@ Set.U<'t> @>, <@ id @>, print_set_algebra_operators)

    let ReduceIdemp = set_algebra.Rules.[0]

    let ReduceIdent = set_algebra.Rules.[1]

    let ReduceComp = set_algebra.Rules.[2]

    let LeftAssoc = set_algebra.Rules.[3]

    let RightAssoc = set_algebra.Rules.[4]

    let Commute = set_algebra.Rules.[5]

    let Distrib = set_algebra.Rules.[6]