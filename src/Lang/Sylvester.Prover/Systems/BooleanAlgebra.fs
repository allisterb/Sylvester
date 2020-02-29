namespace Sylph

/// Boolean algebra using the axioms and rules of S.
module BooleanAlgebra =
    let boolean_algebra = Theory.S

    let reduce_constants = EquationalLogic.reduce_constants


    let commute = EquationalLogic.commute

    //let 
    let Reduce = Theory.S.Rules.[0]

    /// Lo is left associative.
    let LeftAssoc = Theory.S.Rules.[1]

    /// Expression is right associative.
    let RightAssoc = Theory.S.Rules.[2]
  
    /// Expression is commutative.
    let Commute = Theory.S.Rules.[3]

    /// Multiplication distributes over addition in expression.
    let Distrib = Theory.S.Rules.[4]

    /// Collect distributed logical terms in expression.
    let Collect = Theory.S.Rules.[5]

    /// Replace identical logical terms in expression.
    let Ident = Theory.S.Rules.[6]

