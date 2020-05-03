# Sylph
[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/allisterb/sylph-notebooks.git/master)
[![NuGet](https://img.shields.io/nuget/v/Sylph.svg)](https://www.nuget.org/packages/Sylph/)

[Sylph](https://github.com/allisterb/Sylvester/tree/master/src/Lang/Sylvester.Prover) (symbolic proof helper) is a language-integrated proof assistant for F#.


```fsharp
// Load the Sylph NuGet package
#r "nuget: Sylph"
// Load helpers for the .NET Core Jupyter kernel.
#load "netcore.fsx"
```

```fsharp
// Open the Sylvester namespace which contains the Sylph types and functions.
open Sylvester
// Open the PropCaclulus namespace which contains the axioms and rules for the propositional calculus theory.
open PropCalculus 

// Declare some bool variables for use in formulae
let p,q,r = var3<bool>

// Prove the identity p = p = q = q use the rules and axioms of propositional calculus
let p1 = proof prop_calculus <@ p = p = q = q @>  [
    // Operators in formula are right-associative
    RightAssoc |> LR
    // Substitute the definition true = (p = p)
    def_true <@ q @> |> Trn |> R
    // Operators in formula are commutative
    Commute |> LR
]
```

    [Lemma] true = (false = false):
            ⊢ true = (false = false). [Definition of true]
            Proof complete.
    
    [Lemma] true = (false = false):
            ⊢ true = (false = false). [Definition of true]
            Proof complete.
    
    [Lemma] false = false = true:
            1. Logical operators in expression are commutative: false = false = true → true = (false = false).
            ⊢ true = (false = false). [Definition of true]
            Proof complete.
    
    [Lemma] ¬ false = true:
            1. Logical operators in expression are commutative: ¬ false = true → true = ¬ false.
            Proof incomplete. Current state: true = ¬ false.
            2. Substitute true ≡ false = false into left of expression.
            Proof incomplete. Current state: false = false = ¬ false.
            3. Logical operators in expression are right-associative: false = false = ¬ false → false = (false = ¬ false).
            Proof incomplete. Current state: false = (false = ¬ false).
            4. Logical operators in right of expression are commutative: false = (false = ¬ false) → false = (¬ false = false).
            Proof incomplete. Current state: false = (¬ false = false).
            5. Collect distributed logical terms in right of expression: false = (¬ false = false) → false = ¬ (false = false).
            Proof incomplete. Current state: false = ¬ (false = false).
            6. Substitute false = false ≡ true into right of expression.
            ⊢ false = ¬ true. [Definition of false]
            Proof complete.
    
    [Lemma] true = (q = q):
            ⊢ true = (q = q). [Definition of true]
            Proof complete.
    
    [Lemma] q = q = true:
            1. Logical operators in expression are commutative: q = q = true → true = (q = q).
            ⊢ true = (q = q). [Definition of true]
            Proof complete.
    
    Proof of p = p = q = q:
    1. Logical operators in expression are right-associative: p = p = q = q → p = p = (q = q).
    Proof incomplete. Current state: p = p = (q = q).
    2. Substitute q = q ≡ true into right of expression.
    Proof incomplete. Current state: p = p = true.
    3. Logical operators in expression are commutative: p = p = true → true = (p = p).
    ⊢ true = (p = p). [Definition of true]
    Proof complete.
    

Unlike other theorem provers Sylph does not require an external DSL or parser for expressing theorem statements, or an external interactive environment for creating and storing the state of proofs. Theorems are expressed as the equivalence of 2 formulas and a formula is defined as any F# expression of a particular type for which a code quotation and full expression tree is available.


```fsharp
// Define a formula of interest using an ordinary function with the Formula attribute
[<Formula>]
let f1 a b = a * a + 6 * b + 5

// Or use an expression directly
let f2 = <@ fun x -> 3 * x + 5@>
```


```fsharp
// Each formula has a symbolic expression
<@ f1 @> |> expand
```




Lambda (a,
        Lambda (b,
                Call (None, op_Addition,
                      [Call (None, op_Addition,
                             [Call (None, op_Multiply, [a, a]),
                              Call (None, op_Multiply, [Value (6), b])]),
                       Value (5)])))




```fsharp
// And can also be decompiled to the F# source
src f2
```




    fun x -> 3 * x + 5



Proofs are constructed according to the axioms and rules of [theories](https://github.com/allisterb/Sylvester/blob/master/src/Math/Sylvester.AbstractAlgebra/Theories/) which define the rules that can be used to match and transform formula expressions that preserve equivalence.


```fsharp
// Define 3 boolean variables for use in formulas
let p, q, r = var3<bool>

// Some theorems are true axiomatically 
prop_calculus |- <@ (p ||| q) = (q ||| p) @>  
```




    True




```fsharp
// Provable directly from axioms
let t2 = axiom prop_calculus <@ p ||| q ||| r = (p ||| (q ||| r))@>
```

    [Lemma] p ∨ q ∨ r = p ∨ (q ∨ r):
            ⊢ p ∨ q ∨ r = p ∨ (q ∨ r). [Axiom of Associativity]
            Proof complete.
    
    

Axioms are pure functions or schemas that match patterns in primitive unary and binary formulas, which define a set of formulae that are always equivalent in a theory e.g an identity axiom for a theory is defined as:

````fsharp
/// x + 0 == x
let (|Identity|_|) (op: Expr<'t->'t->'t>) (zero:Expr<'t>)   = 
    function
    | Binary op (a1, z), a2 when sequal a1 a2 && sequal zero z -> Some (pattern_desc "Identity" <@ fun (x:'t) -> (%op) x (%zero) = (%zero) @>)
    | _ -> None
````

Theores also contain rules that are valid ways to transform two formulas when they are not in a primitive unary or binary form. Theorems that two formulae are equivalent usully require a *proof* which is just a `list` of rule applications that must all be instances of rules defined only by the proof system.


```fsharp
// Not provable directly from axioms: not p = q = p = not q
let p2 = proof prop_calculus <@ not p = q = p = not q @> [] 
```

    Proof of ¬ p = q = p = ¬ q:
    Proof incomplete. Current state: ¬ p = q = p = ¬ q.
    


```fsharp
// Prove not p = q = p = not q in 5 steps.
let p3 = proof prop_calculus <@ not p = q = p = not q @> [
        Collect |> L
        RightAssoc |> LR
        Commute |> R
        Collect |> R
        Commute |> R
    ] 
```

    Proof of ¬ p = q = p = ¬ q:
    1. Collect distributed logical terms in left of expression: ¬ p = q = p = ¬ q → ¬ (p = q) = p = ¬ q.
    Proof incomplete. Current state: ¬ (p = q) = p = ¬ q.
    2. Logical operators in expression are right-associative: ¬ (p = q) = p = ¬ q → ¬ (p = q) = (p = ¬ q).
    Proof incomplete. Current state: ¬ (p = q) = (p = ¬ q).
    3. Logical operators in right of expression are commutative: ¬ (p = q) = (p = ¬ q) → ¬ (p = q) = (¬ q = p).
    Proof incomplete. Current state: ¬ (p = q) = (¬ q = p).
    4. Collect distributed logical terms in right of expression: ¬ (p = q) = (¬ q = p) → ¬ (p = q) = ¬ (q = p).
    Proof incomplete. Current state: ¬ (p = q) = ¬ (q = p).
    5. Logical operators in right of expression are commutative: ¬ (p = q) = ¬ (q = p) → ¬ (p = q) = ¬ (p = q).
    ⊢ ¬ (p = q) = ¬ (p = q). [Axiom of Symbolic Equality]
    Proof complete.
    
