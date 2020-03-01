namespace Sylph

/// Boolean algebra using the axioms and rules of S.
module BooleanAlgebra =
    let boolean_algebra = Theory.S

    /// Reduce logical constants in expression.
    let reduce_constants = EquationalLogic.reduce_constants

    /// Logical operators in expression
    let commute = EquationalLogic.commute

    /// Reduce logical constants in expression. 
    let Reduce = Theory.S.Rules.[0]

    /// Logical expression is left associative.
    let LeftAssoc = Theory.S.Rules.[1]

    /// Logical expression is right associative.
    let RightAssoc = Theory.S.Rules.[2]
  
    /// Logical expression is commutative.
    let Commute = Theory.S.Rules.[3]

    /// Distribute logical terms in expression.
    let Distrib = Theory.S.Rules.[4]

    /// Collect distributed logical terms in expression.
    let Collect = Theory.S.Rules.[5]

    /// Substitute identical logical terms in expression.
    let Ident = Theory.S.Rules.[6]

    /// Substitute idempotent logical terms in expression.
    let Idemp = Theory.S.Rules.[7]

    /// Logical expression satisfies law of excluded middle.
    let ExcludedMiddle = Theory.S.Rules.[8]

    /// Logical expression satisfies golden rule.
    let GoldenRule = Theory.S.Rules.[9]


