# Sylph
[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/allisterb/sylph-notebooks.git/master?filepath=Sylph.ipynb)
[![NuGet](https://img.shields.io/nuget/v/Sylph.svg)](https://www.nuget.org/packages/Sylph/)

[Sylph](https://github.com/allisterb/Sylvester/tree/master/src/Lang/Sylvester.Prover) (symbolic proof helper) is a language-integrated proof assistant for F#.

```fsharp
// Load the Sylvester abstract algebra package which contains the integer_algebra theory
#r "nuget: Sylvester.AbstractAlgebra" 
// Load Jupyter helpers for the .NET Core kernel
#load "netcore.fsx"
```
```fsharp
open Sylvester
open IntegerAlgebra 

// Declare some integer variables for use in formulae
let a,b,c = var3<int>

// Prove the identity a * 0 = 0 use the rules and axioms of integer algebra
let p1 = proof <@ a * 0 = 0 @> integer_algebra [
    // a * 0 = a * 0 + 0 is axiomatic in the integer_algebra theory.
    let lemma1 = <@ a * 0 = a * 0 + 0 @> |> int_id_ax
    
    // 0 = -(a * 0 ) + (a * 0) can be proved in the integer_algebra theory.
    let lemma2 = <@ 0 = -(a * 0) + (a * 0) @> |> int_id [Commute |> EntireB]
    
    // Substitute the identity in lemma1 into A
    lemma1 |> EntireA
    
    // A is commutative
    Commute |> EntireA
    // Subsititute the identity in lemma2 into the left of A
    lemma2 |> LeftA    
    // Subsititute the identity in lemma2 into B
    lemma2 |> EntireB
    RightAssoc |> EntireA
    LeftCancel |> AB
    Collect |> EntireA
    Reduce |> EntireA
]
```

    [Lemma] Proof of A: a ⋅ 0 ≡ B: a ⋅ 0 + 0:
    [Lemma] ⊢ a ⋅ 0 ≡ a ⋅ 0 + 0. [Axiom of Identity]
    [Lemma] Proof complete.
    
    [Lemma] Proof of A: 0 ≡ B: -(a ⋅ 0) + a ⋅ 0:
    [Lemma] 1. B is commutative: -(a ⋅ 0) + a ⋅ 0 ≡ a ⋅ 0 + -(a ⋅ 0).
    [Lemma] ⊢ 0 ≡ a ⋅ 0 + -(a ⋅ 0). [Definition of Inverse]
    [Lemma] Proof complete.
    
    Proof of A: a ⋅ 0 ≡ B: 0:
    1. Substitute a ⋅ 0 in A with a ⋅ 0 + 0.
    Proof incomplete. Current state: a ⋅ 0 + 0 ≡ 0.
    2. A is commutative: a ⋅ 0 + 0 ≡ 0 + a ⋅ 0.
    Proof incomplete. Current state: 0 + a ⋅ 0 ≡ 0.
    3. Substitute 0 in A with -(a ⋅ 0) + a ⋅ 0.
    Proof incomplete. Current state: -(a ⋅ 0) + a ⋅ 0 + a ⋅ 0 ≡ 0.
    4. Substitute 0 in A with -(a ⋅ 0) + a ⋅ 0.
    Proof incomplete. Current state: -(a ⋅ 0) + a ⋅ 0 + a ⋅ 0 ≡ -(a ⋅ 0) + a ⋅ 0.
    5. A is right-associative: -(a ⋅ 0) + a ⋅ 0 + a ⋅ 0 ≡ -(a ⋅ 0) + (a ⋅ 0 + a ⋅ 0).
    Proof incomplete. Current state: -(a ⋅ 0) + (a ⋅ 0 + a ⋅ 0) ≡ -(a ⋅ 0) + a ⋅ 0.
    6. Cancel equivalent terms on the LHS in A and B: (-(a ⋅ 0) + (a ⋅ 0 + a ⋅ 0), -(a ⋅ 0) + a ⋅ 0) ≡ (a ⋅ 0 + a ⋅ 0, a ⋅ 0).
    Proof incomplete. Current state: a ⋅ 0 + a ⋅ 0 ≡ a ⋅ 0.
    7. Collect multiplication terms distributed over addition in A: a ⋅ 0 + a ⋅ 0 ≡ a ⋅ (0 + 0).
    Proof incomplete. Current state: a ⋅ (0 + 0) ≡ a ⋅ 0.
    8. Reduce integer constants in A: a ⋅ (0 + 0) ≡ a ⋅ 0.
    ⊢ a ⋅ 0 ≡ a ⋅ 0. [Logical Axiom of Equality]
    Proof complete.
    

Unlike other theorem provers Sylph does not require an external DSL or parser for expressing theorem statements, or an external interactive environment for creating and storing the state of proofs. Theorems are expressed as the equivalence of 2 formulas and a [formula](https://github.com/allisterb/Sylvester/blob/master/src/Lang/Sylvester.Prover/Formula.fs) is defined as any F# expression of a particular type for which a code quotation and full expression tree is available. Formulas in a theorem do not have to be logical formulas but any 2 valid F# expressions of the same type where it makes sense to reason about them equationally.


```fsharp
// Define a formula of interest using an ordinary function with the Formula attribute
[<Formula>]
let f1 x = 3 * x + 6 + 2 * x + 4

// Or use an expression directly
let f2 = <@ a * a + 6 * b + 5@>
```


```fsharp
// Each formula has a symbolic expression
expand <@ f1 @>
```




Lambda (x,
        Call (None, op_Addition,
              [Call (None, op_Addition,
                     [Call (None, op_Addition,
                            [Call (None, op_Multiply, [Value (3), x]), Value (6)]),
                      Call (None, op_Multiply, [Value (2), x])]), Value (4)]))




```fsharp
// And can also be decompiled to the F# source
src f2
```




    a * a + 6 * b + 5



Proofs are constructed according to the axioms and rules of [theories](https://github.com/allisterb/Sylvester/blob/master/src/Math/Sylvester.AbstractAlgebra/Theories/) which define the rules that can be used to match and transform formula expressions that preserve equivalence.


```fsharp
//Some theorems are true axiomatically 
integer_algebra |- <@ (a + b) = (b + a) @>  
```




    True




```fsharp
//Provable directly from axioms
let t2 = ident <@ a + b + c = a + (b + c)@> integer_algebra []
```

    Proof of A: a + b + c ≡ B: a + (b + c):
    ⊢ a + b + c ≡ a + (b + c). [Axiom of Associativity]
    Proof complete.
    

Axioms are pure functions or schemas that match patterns in primitive unary and binary formulas, which define a set of formulae that are always equivalent in a theory e.g an identity axiom for a theory is defined as:

````fsharp
/// x + 0 == x
let (|Identity|_|) (op: Expr<'t->'t->'t>) (zero:Expr<'t>)   = 
    function
    | Binary op (a1, z), a2 when sequal a1 a2 && sequal zero z -> Some (pattern_desc "Identity" <@ fun (x:'t) -> (%op) x (%zero) = (%zero) @>)
    | _ -> None
````


```fsharp
// True by the addition identity axiom
integer_algebra |- <@ c + a + 0 = c + a @> 
```




    True



Theores also contain rules that are valid ways to transform two formulas when they are not in a primitive unary or binary form. Theorems that two formulae are equivalent usully require a *proof* which is just a `list` of rule applications that must all be instances of rules defined only by the proof system.


```fsharp
// Not provable directly from axioms: 2a + 5 + 3 = 2a + 8 
let p3 = proof <@ 2 * a + 5 + 3 = 2 * a + 8@> integer_algebra []
```

    Proof of A: 2 ⋅ a + 5 + 3 ≡ B: 2 ⋅ a + 8:
    Proof incomplete. Current state: 2 ⋅ a + 5 + 3 ≡ 2 ⋅ a + 8.
    


```fsharp
// Proof of 2a + 5 + 3 = 2a + 8 using two steps
let p3 = proof <@ 2 * a + 5 + 3 = 2 * a + 8@> integer_algebra [
        RightAssoc |> EntireA
        Reduce |> EntireA
    ]
```

    Proof of A: 2 ⋅ a + 5 + 3 ≡ B: 2 ⋅ a + 8:
    1. A is right-associative: 2 ⋅ a + 5 + 3 ≡ 2 ⋅ a + (5 + 3).
    Proof incomplete. Current state: 2 ⋅ a + (5 + 3) ≡ 2 ⋅ a + 8.
    2. Reduce integer constants in A: 2 ⋅ a + (5 + 3) ≡ 2 ⋅ a + 8.
    ⊢ 2 ⋅ a + 8 ≡ 2 ⋅ a + 8. [Logical Axiom of Equality]
    Proof complete.
    

Rules are defined as recursive pure functions that preserve equivalence between two formulae e.g the rule of right associativity for arithmetic operators is implemented as:

````fsharp
let rec right_assoc =
    function
    | Add(Add(a1, a2), a3) -> <@@ %%a1 + (%%a2 + %%a3) @@>
    | Subtract(Subtract(a1, a2), a3) -> <@@ %%a1 - (%%a2 + %%a3) @@>
    | Multiply(Multiply(a1, a2), a3) -> <@@ %%a1 * (%%a2 * %%a3) @@>
    | expr -> traverse expr right_assoc
````


```fsharp
// Apply the right_assoc rule to a formula expression and compare
<@ 2 * a + 5 + 3 = 2 * a + 8@> |> src, <@ 2 * a + 5 + 3 = 2 * a + 8@> |> right_assoc |> src
```




<table><thead><tr><th>Item1</th><th>Item2</th></tr></thead><tbody><tr><td>2 * a + 5 + 3 = 2 * a + 8</td><td>2 * a + (5 + 3) = 2 * a + 8</td></tr></tbody></table>



Rules are normal F# functions that can be chained together:


```fsharp
// Rules on formula expressions can be chained together.
<@ 2 * a + 5 + 3 = 2 * a + 8@> |> right_assoc |> reduce_constants |> src 
```




    2 * a + 8 = 2 * a + 8



In the above case we can see that identity is true since one can be transformed into another and we use these two rules in our proof.


```fsharp
p3.Steps
```




<table><thead><tr><th><i>index</i></th><th>Item</th><th>Tag</th><th>IsEntireA</th><th>IsEntireB</th><th>IsLeftA</th><th>IsLeftB</th><th>IsRightA</th><th>IsRightB</th><th>IsAB</th><th>Rule</th><th>RuleName</th></tr></thead><tbody><tr><td>0</td><td>{ Sylvester.Rule+Rule: Item1: (expression) is right-associative, Item2: { Sylvester.IntegerAlgebra+RightAssoc@107:  }, Tag: 0, IsRule: True, IsSubst: False, Name: (expression) is right-associative, Apply: { Sylvester.IntegerAlgebra+RightAssoc@107:  } }</td><td>0</td><td>True</td><td>False</td><td>False</td><td>False</td><td>False</td><td>False</td><td>False</td><td>{ Sylvester.Rule+Rule: Item1: (expression) is right-associative, Item2: { Sylvester.IntegerAlgebra+RightAssoc@107:  }, Tag: 0, IsRule: True, IsSubst: False, Name: (expression) is right-associative, Apply: { Sylvester.IntegerAlgebra+RightAssoc@107:  } }</td><td>(expression) is right-associative</td></tr><tr><td>1</td><td>{ Sylvester.Rule+Rule: Item1: Reduce integer constants in (expression), Item2: { Sylvester.IntegerAlgebra+Reduce@101:  }, Tag: 0, IsRule: True, IsSubst: False, Name: Reduce integer constants in (expression), Apply: { Sylvester.IntegerAlgebra+Reduce@101:  } }</td><td>0</td><td>True</td><td>False</td><td>False</td><td>False</td><td>False</td><td>False</td><td>False</td><td>{ Sylvester.Rule+Rule: Item1: Reduce integer constants in (expression), Item2: { Sylvester.IntegerAlgebra+Reduce@101:  }, Tag: 0, IsRule: True, IsSubst: False, Name: Reduce integer constants in (expression), Apply: { Sylvester.IntegerAlgebra+Reduce@101:  } }</td><td>Reduce integer constants in (expression)</td></tr></tbody></table>



When a proof is constructed each step is checked and executed and the resulting state of the pair of formulae logged and stored. This is a longer proof using more rules of inference for integer algebra:


```fsharp
// 3 * x + 6 + 2 * x + 4 = 5 * x + 10
let p4 = proof <@ fun x -> 3 * x + 6 + 2 * x + 4 = 5 * x + 10 @> integer_algebra [
    RightAssoc |> EntireA
    Commute |> RightA
    RightAssoc |> EntireA 
    LeftAssoc |> RightA
    Reduce |> AB
    Commute |> RightA
    LeftAssoc |> EntireA
    Collect |> LeftA
    Reduce |> EntireA
    Commute |> LeftA
    ]
```

    Proof of A: 3 ⋅ x + 6 + 2 ⋅ x + 4 ≡ B: 5 ⋅ x + 10:
    1. A is right-associative: 3 ⋅ x + 6 + 2 ⋅ x + 4 ≡ 3 ⋅ x + 6 + (2 ⋅ x + 4).
    Proof incomplete. Current state: 3 ⋅ x + 6 + (2 ⋅ x + 4) ≡ 5 ⋅ x + 10.
    2. A is commutative: 3 ⋅ x + 6 + (2 ⋅ x + 4) ≡ 3 ⋅ x + 6 + (4 + 2 ⋅ x).
    Proof incomplete. Current state: 3 ⋅ x + 6 + (4 + 2 ⋅ x) ≡ 5 ⋅ x + 10.
    3. A is right-associative: 3 ⋅ x + 6 + (4 + 2 ⋅ x) ≡ 3 ⋅ x + (6 + (4 + 2 ⋅ x)).
    Proof incomplete. Current state: 3 ⋅ x + (6 + (4 + 2 ⋅ x)) ≡ 5 ⋅ x + 10.
    4. A is left-associative: 3 ⋅ x + (6 + (4 + 2 ⋅ x)) ≡ 3 ⋅ x + (6 + 4 + 2 ⋅ x).
    Proof incomplete. Current state: 3 ⋅ x + (6 + 4 + 2 ⋅ x) ≡ 5 ⋅ x + 10.
    5. Reduce integer constants in A: 3 ⋅ x + (6 + 4 + 2 ⋅ x) ≡ 3 ⋅ x + (10 + 2 ⋅ x).
    Proof incomplete. Current state: 3 ⋅ x + (10 + 2 ⋅ x) ≡ 5 ⋅ x + 10.
    6. A is commutative: 3 ⋅ x + (10 + 2 ⋅ x) ≡ 3 ⋅ x + (2 ⋅ x + 10).
    Proof incomplete. Current state: 3 ⋅ x + (2 ⋅ x + 10) ≡ 5 ⋅ x + 10.
    7. A is left-associative: 3 ⋅ x + (2 ⋅ x + 10) ≡ 3 ⋅ x + 2 ⋅ x + 10.
    Proof incomplete. Current state: 3 ⋅ x + 2 ⋅ x + 10 ≡ 5 ⋅ x + 10.
    8. Collect multiplication terms distributed over addition in A: 3 ⋅ x + 2 ⋅ x + 10 ≡ x ⋅ (3 + 2) + 10.
    Proof incomplete. Current state: x ⋅ (3 + 2) + 10 ≡ 5 ⋅ x + 10.
    9. Reduce integer constants in A: x ⋅ (3 + 2) + 10 ≡ x ⋅ 5 + 10.
    Proof incomplete. Current state: x ⋅ 5 + 10 ≡ 5 ⋅ x + 10.
    10. A is commutative: x ⋅ 5 + 10 ≡ 5 ⋅ x + 10.
    ⊢ 5 ⋅ x + 10 ≡ 5 ⋅ x + 10. [Logical Axiom of Equality]
    Proof complete.
    


```fsharp
// Proof state after sixth step
p4.State.[5]
```




<table><thead><tr><th>Item1</th><th>Item2</th><th>Item3</th></tr></thead><tbody><tr><td>{ Microsoft.FSharp.Quotations.FSharpExpr: CustomAttributes: [  ], Type: System.Int32 }</td><td>{ Microsoft.FSharp.Quotations.FSharpExpr: CustomAttributes: [  ], Type: System.Int32 }</td><td>6. A is commutative: 3 * x + (10 + 2 * x) == 3 * x + (2 * x + 10).</td></tr></tbody></table>



There are two kinds of rules: rules derived from axioms of a particular theory and a general substitution rule (derived from the axiom of symbolic equality) which says that in any proof a formula B can be substituted for a formula A when a proof exists for A = B in the same system. The substitution rule is what allows proofs to be created in stages e.g we can create a partial proof of the last theorem.


```fsharp
[<Formula>]
let f4 x = 3 * x + 6 + 2 * x + 4 = 5 * x + 10 
    
let p5 = proof <@ f4 @> integer_algebra [
    RightAssoc |> EntireA
    Commute |> RightA
    RightAssoc |> EntireA 
    LeftAssoc |> RightA
    Reduce |> AB
]
```

    Proof of A: 3 ⋅ x + 6 + 2 ⋅ x + 4 ≡ B: 5 ⋅ x + 10:
    1. A is right-associative: 3 ⋅ x + 6 + 2 ⋅ x + 4 ≡ 3 ⋅ x + 6 + (2 ⋅ x + 4).
    Proof incomplete. Current state: 3 ⋅ x + 6 + (2 ⋅ x + 4) ≡ 5 ⋅ x + 10.
    2. A is commutative: 3 ⋅ x + 6 + (2 ⋅ x + 4) ≡ 3 ⋅ x + 6 + (4 + 2 ⋅ x).
    Proof incomplete. Current state: 3 ⋅ x + 6 + (4 + 2 ⋅ x) ≡ 5 ⋅ x + 10.
    3. A is right-associative: 3 ⋅ x + 6 + (4 + 2 ⋅ x) ≡ 3 ⋅ x + (6 + (4 + 2 ⋅ x)).
    Proof incomplete. Current state: 3 ⋅ x + (6 + (4 + 2 ⋅ x)) ≡ 5 ⋅ x + 10.
    4. A is left-associative: 3 ⋅ x + (6 + (4 + 2 ⋅ x)) ≡ 3 ⋅ x + (6 + 4 + 2 ⋅ x).
    Proof incomplete. Current state: 3 ⋅ x + (6 + 4 + 2 ⋅ x) ≡ 5 ⋅ x + 10.
    5. Reduce integer constants in A: 3 ⋅ x + (6 + 4 + 2 ⋅ x) ≡ 3 ⋅ x + (10 + 2 ⋅ x).
    Proof incomplete. Current state: 3 ⋅ x + (10 + 2 ⋅ x) ≡ 5 ⋅ x + 10.
    

If we then work on another proof which completes this proof we can join these two proofs together


```fsharp
let p6 = proof <@ fun x -> 3 * x + (10 + 2 * x) = 5 * x + 10 @> integer_algebra [    
    Commute |> RightA
    LeftAssoc |> EntireA
    Collect |> LeftA
    Reduce |> EntireA
    Commute |> LeftA
]
```

    Proof of A: 3 ⋅ x + (10 + 2 ⋅ x) ≡ B: 5 ⋅ x + 10:
    1. A is commutative: 3 ⋅ x + (10 + 2 ⋅ x) ≡ 3 ⋅ x + (2 ⋅ x + 10).
    Proof incomplete. Current state: 3 ⋅ x + (2 ⋅ x + 10) ≡ 5 ⋅ x + 10.
    2. A is left-associative: 3 ⋅ x + (2 ⋅ x + 10) ≡ 3 ⋅ x + 2 ⋅ x + 10.
    Proof incomplete. Current state: 3 ⋅ x + 2 ⋅ x + 10 ≡ 5 ⋅ x + 10.
    3. Collect multiplication terms distributed over addition in A: 3 ⋅ x + 2 ⋅ x + 10 ≡ x ⋅ (3 + 2) + 10.
    Proof incomplete. Current state: x ⋅ (3 + 2) + 10 ≡ 5 ⋅ x + 10.
    4. Reduce integer constants in A: x ⋅ (3 + 2) + 10 ≡ x ⋅ 5 + 10.
    Proof incomplete. Current state: x ⋅ 5 + 10 ≡ 5 ⋅ x + 10.
    5. A is commutative: x ⋅ 5 + 10 ≡ 5 ⋅ x + 10.
    ⊢ 5 ⋅ x + 10 ≡ 5 ⋅ x + 10. [Logical Axiom of Equality]
    Proof complete.
    


```fsharp
// Join p5 and p6 together to complete the proof of f4
let p7 = p5 + p6
```

    [Lemma] Proof of A: 3 ⋅ x + 6 + 2 ⋅ x + 4 ≡ B: 3 ⋅ x + (10 + 2 ⋅ x):
    [Lemma] 1. A is right-associative: 3 ⋅ x + 6 + 2 ⋅ x + 4 ≡ 3 ⋅ x + 6 + (2 ⋅ x + 4).
    [Lemma] Proof incomplete. Current state: 3 ⋅ x + 6 + (2 ⋅ x + 4) ≡ 3 ⋅ x + (10 + 2 ⋅ x).
    [Lemma] 2. A is commutative: 3 ⋅ x + 6 + (2 ⋅ x + 4) ≡ 3 ⋅ x + 6 + (4 + 2 ⋅ x).
    [Lemma] Proof incomplete. Current state: 3 ⋅ x + 6 + (4 + 2 ⋅ x) ≡ 3 ⋅ x + (10 + 2 ⋅ x).
    [Lemma] 3. A is right-associative: 3 ⋅ x + 6 + (4 + 2 ⋅ x) ≡ 3 ⋅ x + (6 + (4 + 2 ⋅ x)).
    [Lemma] Proof incomplete. Current state: 3 ⋅ x + (6 + (4 + 2 ⋅ x)) ≡ 3 ⋅ x + (10 + 2 ⋅ x).
    [Lemma] 4. A is left-associative: 3 ⋅ x + (6 + (4 + 2 ⋅ x)) ≡ 3 ⋅ x + (6 + 4 + 2 ⋅ x).
    [Lemma] Proof incomplete. Current state: 3 ⋅ x + (6 + 4 + 2 ⋅ x) ≡ 3 ⋅ x + (10 + 2 ⋅ x).
    [Lemma] 5. Reduce integer constants in A: 3 ⋅ x + (6 + 4 + 2 ⋅ x) ≡ 3 ⋅ x + (10 + 2 ⋅ x).
    [Lemma] ⊢ 3 ⋅ x + (10 + 2 ⋅ x) ≡ 3 ⋅ x + (10 + 2 ⋅ x). [Logical Axiom of Equality]
    [Lemma] Proof complete.
    
    Proof of A: 3 ⋅ x + 6 + 2 ⋅ x + 4 ≡ B: 5 ⋅ x + 10:
    1. Joining proof of 3 ⋅ x + 6 + 2 ⋅ x + 4 ≡ 3 ⋅ x + (10 + 2 ⋅ x) to proof of 3 ⋅ x + (10 + 2 ⋅ x) ≡ 5 ⋅ x + 10.
    Proof incomplete. Current state: 3 ⋅ x + (10 + 2 ⋅ x) ≡ 5 ⋅ x + 10.
    2. A is commutative: 3 ⋅ x + (10 + 2 ⋅ x) ≡ 3 ⋅ x + (2 ⋅ x + 10).
    Proof incomplete. Current state: 3 ⋅ x + (2 ⋅ x + 10) ≡ 5 ⋅ x + 10.
    3. A is left-associative: 3 ⋅ x + (2 ⋅ x + 10) ≡ 3 ⋅ x + 2 ⋅ x + 10.
    Proof incomplete. Current state: 3 ⋅ x + 2 ⋅ x + 10 ≡ 5 ⋅ x + 10.
    4. Collect multiplication terms distributed over addition in A: 3 ⋅ x + 2 ⋅ x + 10 ≡ x ⋅ (3 + 2) + 10.
    Proof incomplete. Current state: x ⋅ (3 + 2) + 10 ≡ 5 ⋅ x + 10.
    5. Reduce integer constants in A: x ⋅ (3 + 2) + 10 ≡ x ⋅ 5 + 10.
    Proof incomplete. Current state: x ⋅ 5 + 10 ≡ 5 ⋅ x + 10.
    6. A is commutative: x ⋅ 5 + 10 ≡ 5 ⋅ x + 10.
    ⊢ 5 ⋅ x + 10 ≡ 5 ⋅ x + 10. [Logical Axiom of Equality]
    Proof complete.
    


```fsharp
p7 |- <@ f4 @>
```




    True




```fsharp

```
