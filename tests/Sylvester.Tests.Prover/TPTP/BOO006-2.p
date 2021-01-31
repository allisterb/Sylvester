%--------------------------------------------------------------------------
% File     : BOO006-1 : TPTP v6.4.0. Released v1.0.0.
% Domain   : Boolean Algebra
% Problem  : Multiplication is bounded (X * 0 = 0)
% Version  : [MOW76] axioms.
% English  :

% Refs     : [Whi61] Whitesitt (1961), Boolean Algebra and Its Applications
%          : [MOW76] McCharen et al. (1976), Problems and Experiments for a
%          : [OMW76] Overbeek et al. (1976), Complexity and Related Enhance
% Source   : [MOW76]
% Names    : B3 part 2 [MOW76]
%          : B6 [MOW76]
%          : Lemma proved [OMW76]
%          : prob3_part2.ver1 [ANL]

% Status   : Unsatisfiable
% Rating   : 0.00 v6.0.0, 0.11 v5.5.0, 0.19 v5.4.0, 0.13 v5.3.0, 0.25 v5.2.0, 0.12 v5.1.0, 0.14 v4.1.0, 0.11 v4.0.1, 0.17 v3.7.0, 0.00 v2.4.0, 0.17 v2.3.0, 0.00 v2.1.0, 0.00 v2.0.0
% Syntax   : Number of clauses     :   23 (   0 non-Horn;  11 unit;  13 RR)
%            Number of atoms       :   61 (   2 equality)
%            Maximal clause size   :    5 (   3 average)
%            Number of predicates  :    3 (   0 propositional; 2-3 arity)
%            Number of functors    :    6 (   3 constant; 0-2 arity)
%            Number of variables   :   82 (   0 singleton)
%            Maximal term depth    :    2 (   1 average)
% SPC      : CNF_UNS_RFO_SEQ_HRN

% Comments :
%--------------------------------------------------------------------------
%----Include boolean algebra axioms

cnf(closure_of_multiplication,axiom,
    ( product(X,Y,multiply(X,Y)) )).

cnf(commutativity_of_addition,axiom,
    ( ~ sum(X,Y,Z)
    | sum(Y,X,Z) )).

cnf(commutativity_of_multiplication,axiom,
    ( ~ product(X,Y,Z)
    | product(Y,X,Z) )).

cnf(additive_identity1,axiom,
    ( sum(additive_identity,X,X) )).

cnf(additive_identity2,axiom,
    ( sum(X,additive_identity,X) )).

cnf(multiplicative_identity1,axiom,
    ( product(multiplicative_identity,X,X) )).

cnf(multiplicative_identity2,axiom,
    ( product(X,multiplicative_identity,X) )).

cnf(distributivity1,axiom,
    ( ~ product(X,Y,V1)
    | ~ product(X,Z,V2)
    | ~ sum(Y,Z,V3)
    | ~ product(X,V3,V4)
    | sum(V1,V2,V4) )).

cnf(distributivity2,axiom,
    ( ~ product(X,Y,V1)
    | ~ product(X,Z,V2)
    | ~ sum(Y,Z,V3)
    | ~ sum(V1,V2,V4)
    | product(X,V3,V4) )).

cnf(distributivity3,axiom,
    ( ~ product(Y,X,V1)
    | ~ product(Z,X,V2)
    | ~ sum(Y,Z,V3)
    | ~ product(V3,X,V4)
    | sum(V1,V2,V4) )).

cnf(distributivity4,axiom,
    ( ~ product(Y,X,V1)
    | ~ product(Z,X,V2)
    | ~ sum(Y,Z,V3)
    | ~ sum(V1,V2,V4)
    | product(V3,X,V4) )).

cnf(distributivity5,axiom,
    ( ~ sum(X,Y,V1)
    | ~ sum(X,Z,V2)
    | ~ product(Y,Z,V3)
    | ~ sum(X,V3,V4)
    | product(V1,V2,V4) )).

cnf(distributivity6,axiom,
    ( ~ sum(X,Y,V1)
    | ~ sum(X,Z,V2)
    | ~ product(Y,Z,V3)
    | ~ product(V1,V2,V4)
    | sum(X,V3,V4) )).

cnf(distributivity7,axiom,
    ( ~ sum(Y,X,V1)
    | ~ sum(Z,X,V2)
    | ~ product(Y,Z,V3)
    | ~ sum(V3,X,V4)
    | product(V1,V2,V4) )).

cnf(distributivity8,axiom,
    ( ~ sum(Y,X,V1)
    | ~ sum(Z,X,V2)
    | ~ product(Y,Z,V3)
    | ~ product(V1,V2,V4)
    | sum(V3,X,V4) )).

cnf(additive_inverse1,axiom,
    ( sum(inverse(X),X,multiplicative_identity) )).

cnf(additive_inverse2,axiom,
    ( sum(X,inverse(X),multiplicative_identity) )).

cnf(multiplicative_inverse1,axiom,
    ( product(inverse(X),X,additive_identity) )).

cnf(multiplicative_inverse2,axiom,
    ( product(X,inverse(X),additive_identity) )).

%-----Well definedness of the operations
cnf(addition_is_well_defined,axiom,
    ( ~ sum(X,Y,U)
    | ~ sum(X,Y,V)
    | U = V )).

cnf(multiplication_is_well_defined,axiom,
    ( ~ product(X,Y,U)
    | ~ product(X,Y,V)
    | U = V )).
%--------------------------------------------------------------------------
cnf(prove_equations,negated_conjecture,
    ( ~ product(x,additive_identity,additive_identity) )).

%--------------------------------------------------------------------------
