namespace Sylvester

module SetAlgebraTheory =
    open BooleanAlgebraTheory    

    /// Print set algebra operator symbols
    let print_set_algebra_operators (s:string) = 
        s.Replace("|+|", "\u222A")
         .Replace("|*|", "\u2229")
         .Replace("Empty", "\u2205")
         .Replace("U", "\uD835")

    let set_algebra<'t when 't: equality> = BooleanAlgebraTheory("Set Algebra", <@ Set.(|+|) @>, <@ Set.(|*|) @>, <@ Set.Empty @>, <@ Set.U<'t> @>, <@ id @>, print_set_algebra_operators)

    let ReduceIdemp = set_algebra.Rules.[0]

    let ReduceIdent = set_algebra.Rules.[1]

    let ReduceComp = set_algebra.Rules.[2]

    let LeftAssoc = set_algebra.Rules.[3]

    let RightAssoc = set_algebra.Rules.[4]

    let Commute = set_algebra.Rules.[5]

    let Distrib = set_algebra.Rules.[6]

    (* proof step shortcuts*)
     
    let set_id_ax expr = id_ax set_algebra expr 
    let set_id_ax_ab expr = id_ax_ab set_algebra expr
    let set_id_ax_a expr = id_ax_a set_algebra expr
    let set_id_ax_b expr = id_ax_b set_algebra expr
    let set_id_ax_r_a expr = id_ax_r_a set_algebra expr
    let set_id_ax_r_b expr = id_ax_r_b set_algebra expr
    let set_id_ax_l_a expr = id_ax_l_a set_algebra expr
    let set_id_ax_l_b expr = id_ax_l_b set_algebra expr

    let set_id expr = id_lem set_algebra expr
    let set_id_ab proof expr = id_ab set_algebra proof expr
    let set_id_a proof expr = id_a set_algebra proof expr
    let set_id_b proof expr = id_b set_algebra proof expr
    let set_id_r_a proof expr = id_r_a set_algebra proof expr
    let set_id_r_b proof expr = id_r_b set_algebra proof expr
    let set_id_l_a proof expr = id_l_a set_algebra proof expr
    let set_id_l_b proof expr = id_l_b set_algebra proof expr