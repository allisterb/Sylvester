# Sylph
[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/allisterb/sylph-notebooks.git/master?filepath=Sylph.ipynb)

[Sylph](https://github.com/allisterb/Sylvester/tree/master/src/Lang/Sylvester.Prover) (symbolic proof helper) is a language-integrated interactive theorem prover for F# which helps a user formally prove two F# functions or expressions are equivalent according to the axioms and rules of a particular proof system.

Unlike other theorem provers Sylph does not require an external DSL or parser for expressing theorem statements, or an external interactive environment for creating and storing the state of proofs. Theorems are expressed as the equivalence of 2 formulas with the same domain and co-domain and a [formula](https://github.com/allisterb/Sylvester/blob/master/src/Lang/Sylvester.Prover/Formula.fs) is defined as any F# function of a particular type for which a code quotation and full expression tree are available. Formulas in a theorem do not have to be logical formulas but any 2 formulas where it makes sense to reason about equationally.


```fsharp
// Use the Sylph NuGet package
#r "nuget: Sylph"
```


```fsharp
open Sylph

// Define some integer formulae of interest
let F1 = F (fun x -> 2 * x + 8)
let F2 = F (fun x -> 2 * x + 3 + 5)
let F3 = F (fun x -> 3 * x + 6 + 2 * x + 4)
let F4 = F (fun x -> 5 * x + 10)
```


```fsharp
// Or use a reflected definition

[<ReflectedDefinition>]
let f5 x = x * x + 4 * x

let F5 = F f5

// Each formula has a symbolic expression
F1.Expr
```




    Lambda (x,
            Call (None, op_Addition,
                  [Call (None, op_Multiply, [Value (2), x]), Value (8)]))




```fsharp
// And can also be decompiled to the F# source
F1.Src
```




    "fun x -> 2 * x + 8"



Proofs are constructed according to the axioms and rules of a [proof system](https://github.com/allisterb/Sylvester/blob/master/src/Lang/Sylvester.Prover/Systems/IntegerArithmetic.fs) which define the rules that can be used to match and transform formula expressions that preserve equivalence.


```fsharp
// Open the integer arithmetic proof system
open IntegerArithmetic

// Define some integer arithmetic formulae
let a = F (fun x -> 3 * x + 5)
let b = F (fun x -> 5 + 3 * x)
let c = F (fun x -> 6 * x)

//Some theorems are true axiomatically 
// e.g the functions a and b are equivalent because of the commutativity axiom of integer arithmtic.
integer_arithmetic |- (a <=> b)
```




    true



[Axioms](https://github.com/allisterb/Sylvester/blob/5811b1f544d94057b40728b9086b7ccd940428ab/src/Lang/Sylvester.Prover/Systems/IntegerArithmetic.fs#L13) are pure functions that match patterns in primitive unary and binary formulas e.g the addition identity axiom for integer arithmetic is [defined](https://github.com/allisterb/Sylvester/blob/3117e5611d7f258290853a10d8c3925e39194836/src/Lang/Sylvester.Prover/Systems/IntegerArithmetic.fs#L47) as:

````fsharp
 let (|AddIdentity|_|) = 
    function
    | a1, Add(a2, Int32 0) when sequal a1 a2 -> Some true
    | Lambda(_, a1), Lambda(_, Add(a2, Int32 0)) when sequal a1 a2 -> Some true
    | Add(a1, Int32 0), a2 when sequal a1 a2 -> Some true
    | Lambda(_, Add(a1, Int32 0)), Lambda(_, a2) when sequal a1 a2 -> Some true
    | _ -> None
````


```fsharp
// True because of the addition identity axiom
integer_arithmetic |- (c <=> F(fun x -> 6*x + 0))
```




    true



Proof systems also contain [rules](https://github.com/allisterb/Sylvester/blob/5811b1f544d94057b40728b9086b7ccd940428ab/src/Lang/Sylvester.Prover/Systems/IntegerArithmetic.fs#L60) that are valid ways to transform two function expressions when they are not in a primitive unary or binary form. Theorems usully require a *proof* which is just a `list` of rule applications that must all be instances of rules defined only by the proof system.


```fsharp
// Not provable directly from axioms: 2x + 5 + 3 <=> 2x + 8 
integer_arithmetic |- (F1 <=> F2)
```




    false




```fsharp
// Proof of F1 <=> F2 using two steps
let p1 = proof (F1 <=> F2) integer_arithmetic [
        right_assoc_b 
        equal_constants_a_b
    ]
```


    Proof of A: fun x -> 2 * x + 8 <=> B: fun x -> 2 * x + 3 + 5:
    1. B is right-associative: fun x -> 2 * x + 3 + 5 <=> fun x -> 2 * x + (3 + 5)
    Proof incomplete.
    2. Reduce equal constants in A and B: fun x -> 2 * x + (3 + 5) <=> fun x -> 2 * x + 8
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
F2.Expr, right_assoc F2.Expr
```




    (Lambda (x,
            Call (None, op_Addition,
                  [Call (None, op_Addition,
                         [Call (None, op_Multiply, [Value (2), x]), Value (3)]),
                   Value (5)])),
     Lambda (x,
            Call (None, op_Addition,
                  [Call (None, op_Multiply, [Value (2), x]),
                   Call (None, op_Addition, [Value (3), Value (5)])])))



Rules are normal F# functions that can be chained together:


```fsharp
// Rules on formula expressions can be chained together.
(right_assoc >> equal_constants) F2.Expr
```




    Lambda (x,
            Call (None, op_Addition,
                  [Call (None, op_Multiply, [Value (2), x]), Value (8)]))




```fsharp
// Apply two rules and compare the resulting source
src F2.Expr, (right_assoc >> equal_constants >> src) F2.Expr
```




    ("fun x -> 2 * x + 3 + 5", "fun x -> 2 * x + 8")



In the above case we can see that the two formulae F1 and F2 are equivalent since one can be transformed into another and we use these two rules in our proof `p1`.


```fsharp
p1.Steps
```




    [Rule ("B is right-associative",<fun:right_assoc_b@218>);
     Rule ("Reduce equal constants in A and B",<fun:equal_constants_a_b@206>)]




```fsharp
p1 |- (F1 <=> F2)
```




    true



When a proof is constructed each step is checked and executed and the resulting state of the pair of formulae logged and stored. This is a longer proof using more rules of inference for integer arithmetic:


```fsharp
// 3 * x + 6 + 2 * x + 4 <=> 5 * x + 10
let p2 = proof (F3 <=> F4) integer_arithmetic [
    right_assoc_a 
    commute_a_right
    right_assoc_a 
    left_assoc_a_right
    equal_constants_a_b
    commute_a_right
    left_assoc_a
    collect_a_left
    equal_constants_a_b
    commute_a_left
    ]
```


    Proof of A: fun x -> 3 * x + 6 + 2 * x + 4 <=> B: fun x -> 5 * x + 10:
    1. A is right-associative: fun x -> 3 * x + 6 + 2 * x + 4 <=> fun x -> 3 * x + 6 + (2 * x + 4)
    Proof incomplete.
    2. Right side of A is commutative: fun x -> 3 * x + 6 + (2 * x + 4) <=> fun x -> 3 * x + 6 + (4 + 2 * x)
    Proof incomplete.
    3. A is right-associative: fun x -> 3 * x + 6 + (4 + 2 * x) <=> fun x -> 3 * x + (6 + (4 + 2 * x))
    Proof incomplete.
    4. Right side of A is left-associative: fun x -> 3 * x + (6 + (4 + 2 * x)) <=> fun x -> 3 * x + (6 + 4 + 2 * x)
    Proof incomplete.
    5. Reduce equal constants in A and B: fun x -> 3 * x + (6 + 4 + 2 * x) <=> fun x -> 3 * x + (10 + 2 * x)
    Proof incomplete.
    6. Right side of A is commutative: fun x -> 3 * x + (10 + 2 * x) <=> fun x -> 3 * x + (2 * x + 10)
    Proof incomplete.
    7. A is left-associative: fun x -> 3 * x + (2 * x + 10) <=> fun x -> 3 * x + 2 * x + 10
    Proof incomplete.
    8. Collect multiplication terms distributed over addition in left-side of A: fun x -> 3 * x + 2 * x + 10 <=> fun x -> x * (3 + 2) + 10
    Proof incomplete.
    9. Reduce equal constants in A and B: fun x -> x * (3 + 2) + 10 <=> fun x -> x * 5 + 10
    Proof incomplete.
    10. Left side of A is commutative: fun x -> x * 5 + 10 <=> fun x -> 5 * x + 10
    Proof complete.




```fsharp
// Proof state after sixth step
p2.State.[5]
```




    (Lambda (x,
            Call (None, op_Addition,
                  [Call (None, op_Multiply, [Value (3), x]),
                   Call (None, op_Addition,
                         [Call (None, op_Multiply, [Value (2), x]), Value (10)])])),
     Lambda (x,
            Call (None, op_Addition,
                  [Call (None, op_Multiply, [Value (5), x]), Value (10)])),
     "6. Right side of A is commutative: fun x -> 3 * x + (10 + 2 * x) <=> fun x -> 3 * x + (2 * x + 10)")



There are two kinds of rules: rules derived from axioms of a particular proof system and a general substitution rule (derived from the axiom of symbolic equality) which says that in any proof a formula B can be substituted for a formula A when a proof exists for A <=> B in the same system. The substitution rule is what allows proofs to be created in stages e.g we can create a partial proof of F3 <=> F4.


```fsharp
// 3 * x + 6 + 2 * x + 4 <=> 5 * x + 10
let p3 = proof (F3 <=> F4) integer_arithmetic [
    right_assoc_a 
    commute_a_right
    right_assoc_a 
    left_assoc_a_right
    equal_constants_a_b
]
```


    Proof of A: fun x -> 3 * x + 6 + 2 * x + 4 <=> B: fun x -> 5 * x + 10:
    1. A is right-associative: fun x -> 3 * x + 6 + 2 * x + 4 <=> fun x -> 3 * x + 6 + (2 * x + 4)
    Proof incomplete.
    2. Right side of A is commutative: fun x -> 3 * x + 6 + (2 * x + 4) <=> fun x -> 3 * x + 6 + (4 + 2 * x)
    Proof incomplete.
    3. A is right-associative: fun x -> 3 * x + 6 + (4 + 2 * x) <=> fun x -> 3 * x + (6 + (4 + 2 * x))
    Proof incomplete.
    4. Right side of A is left-associative: fun x -> 3 * x + (6 + (4 + 2 * x)) <=> fun x -> 3 * x + (6 + 4 + 2 * x)
    Proof incomplete.
    5. Reduce equal constants in A and B: fun x -> 3 * x + (6 + 4 + 2 * x) <=> fun x -> 3 * x + (10 + 2 * x)
    Proof incomplete.



If we then work on another proof which completes this proof we can join these two proofs together


```fsharp
let p4 = proof (F(fun x -> 3 * x + (10 + 2 * x)) <=> F4) integer_arithmetic [
    commute_a_right
    left_assoc_a
    collect_a_left
    equal_constants_a_b
    commute_a_left
]
```


    Proof of A: fun x -> 3 * x + (10 + 2 * x) <=> B: fun x -> 5 * x + 10:
    1. Right side of A is commutative: fun x -> 3 * x + (10 + 2 * x) <=> fun x -> 3 * x + (2 * x + 10)
    Proof incomplete.
    2. A is left-associative: fun x -> 3 * x + (2 * x + 10) <=> fun x -> 3 * x + 2 * x + 10
    Proof incomplete.
    3. Collect multiplication terms distributed over addition in left-side of A: fun x -> 3 * x + 2 * x + 10 <=> fun x -> x * (3 + 2) + 10
    Proof incomplete.
    4. Reduce equal constants in A and B: fun x -> x * (3 + 2) + 10 <=> fun x -> x * 5 + 10
    Proof incomplete.
    5. Left side of A is commutative: fun x -> x * 5 + 10 <=> fun x -> 5 * x + 10
    Proof complete.




```fsharp
// Join p3 and p4 together to complete the proof of F3 <=> F4 
let p5 = p3 + p4
```


    Proof of A: fun x -> 3 * x + 6 + 2 * x + 4 <=> B: fun x -> 5 * x + 10:
    1. Joining proof of fun x -> 3 * x + 6 + 2 * x + 4 <=> fun x -> 3 * x + (10 + 2 * x) to proof of fun x -> 3 * x + (10 + 2 * x) <=> fun x -> 5 * x + 10.: fun x -> 3 * x + 6 + 2 * x + 4 <=> fun x -> 3 * x + (10 + 2 * x)
    Proof incomplete.
    2. Right side of A is commutative: fun x -> 3 * x + (10 + 2 * x) <=> fun x -> 3 * x + (2 * x + 10)
    Proof incomplete.
    3. A is left-associative: fun x -> 3 * x + (2 * x + 10) <=> fun x -> 3 * x + 2 * x + 10
    Proof incomplete.
    4. Collect multiplication terms distributed over addition in left-side of A: fun x -> 3 * x + 2 * x + 10 <=> fun x -> x * (3 + 2) + 10
    Proof incomplete.
    5. Reduce equal constants in A and B: fun x -> x * (3 + 2) + 10 <=> fun x -> x * 5 + 10
    Proof incomplete.
    6. Left side of A is commutative: fun x -> x * 5 + 10 <=> fun x -> 5 * x + 10
    Proof complete.




```fsharp
p5 |- (F3 <=> F4)
```




    true


