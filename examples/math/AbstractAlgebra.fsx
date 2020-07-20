#load "Include.fsx"

open Sylvester
open Sylvester.Arithmetic
open SetAlgebra

let A = var<Family<int>>
let B,C = var2<Set<int>>
let i = var<int>

let gg = infiniteSeq (fun n -> n + 4)
gg |> Seq.take 10 |> Seq.toArray
//<@ A @> |> expand
//<@ U<int> @> |> expand
proof set_algebra <@ union i (i > 5) A.[i] = (Empty |+| Empty)@> []

//(<@ union i (i > 5) A @> |> expand)
Display.print_formula (<@ union i (i > 5) A.[i] @> |> expand)
Display.print_formula <@ B |+| C@> 
//((<@ (B |+| C |+| EmptySet) @>)) |> expand
//let i = var<int>
//Display.print_formula 
//<@ EmptySet<int>@>
let hh = 5
//let j =  infiniteSeq (fun n -> n * n)
//j.Take(5).ToArray()
//let c = SetComprehension(<@ let n , ) |> Set 
//c.Test 5

//let d = Pr(fun x -> x = 0) |> Set 

//Z5.Set
// Define an infinte seq
//let c = infiniteSeq ((+) 65 >> Char.ConvertFromUtf32) 


//c.Take(26) |> Array.ofSeq
//let cats = Monoid(c, (+), "")

//cats.Take 3 |> Array.ofSeq

//let a = "Nancy"
//let b = "Drew"
//alpha.Op a b
//let alpa = alpha.Op
//"nancy" ++ "drew"
//let prefix  = (+) "prefix-"  
//let a' = prefix a
//let b' = prefix b
//let h = Groupoids(alpha, alpha, prefix)
//h.[a']

//(alpha.["nancy", "drew"]) 
//let h = Groupoids(alpha, alpha, prefix)
//alpha.["nancy", "drew"]


//let n = d.["nancy", "drew"]
//let o = d.["herman", "melville"]
//h.[n, o]
//d.[h.[n], h.[o]] 
//let m = Morph(d,d,id)

//let g = Seq ["A"; "A"; "B"]

//g.Take(30) |> Array.ofSeq

//Integers.Group.Take(10) |> Array.ofSeq

[<ReflectedDefinition>]
let f x = 
    let a = 12. * 4. - float x
    a ** 2.

let a = Seq [|0;6;9|]
let b = Seq [|0;9;6|]
a.Equals b
a.Powerset |> Seq.toArray
let fs = FSharp.Collections.Set.ofSeq [0;5;7]
fs.GetType().Name
let t = new Tag<"gg">()
let name = t.Name
type Tagged<'o, 't when 't :> Number>(h:'o, g:'t) =
    member x.Oo = h
//a.Subsets.HasElement (Seq([1]))
//exprToString d.Expr
//c.Equals d
//Z.Set.Take(10)
//let g = FsExpr(f)
///g.Expr
//let x = Seq ([4;5])
//let z  = match Zpos with | Seq s -> s.GetType().Name |_ -> failwith ""

//typeof<FsExpr<_> list>.GetG
//let o = Poset(Z, (<))
//o.[4, 2]

//FsExpr(seq { yield 1}).Expr
//z.Expr

//let y n = (1 + n) * (1 + n) + 2
//let b = Pred(fun x -> x > 0) |> Set
//let g = Gen((fun b -> b > 0), (fun n -> n + 1))
//g.GetType().Name
//b.Builder.Expr
//let expr = FsExpr(y)
//expr.Expr
//generatePattern (FsExpr(y).Expr) []
//g.Expr

//let zss = GroupElement<card.seven>(6)
//zss * zss

//let sb = build (fun x -> x > 0)
//let ttt = Builder(fun x -> x > 0) |> Set
//ttt.Builder.Expr

//Builder(fun x -> x > 0)

//sb
//sb.Contains 4
//sb.Builder.Expr
//let paq = SetGenerator ((fun x -> x = 16), ([0]))
//paq

//let s = infiniteSeq (fun n -> n * n)
//FsExpr(s).Expr

//let paq = SetGenerator ((fun x -> x = 16), fun n -> n * n)
//paq.First()