namespace Sylvester.Tensors

[<AutoOpen>]
module Logic =

    open Sylvester.Arithmetic.N10
    open FSharp.Data.UnitSystems.SI.UnitNames

   
    (*
    let inline vinit (dim0:IEnumerable<'t>) (v:Vector<'d0, 't>) =
            do if Seq.length dim0 <> v.[zero].IntLength then raise(ArgumentOutOfRangeException("dim0"))
            do if Seq.length dim1 <> m.[one].IntLength then raise(ArgumentOutOfRangeException("dim1"))
            Seq.iteri (fun i x -> m.[zero].SetVal(i, x)) dim0
            Seq.iteri (fun i x -> m.[one].SetVal(i, x)) dim1

    let inline minit (dim0:IEnumerable<'t>) (dim1:IEnumerable<'t>) (m:Matrix<'d0, 'd1, 't>) =
            do if Seq.length dim0 <> m.[zero].IntLength then raise(ArgumentOutOfRangeException("dim0"))
            do if Seq.length dim1 <> m.[one].IntLength then raise(ArgumentOutOfRangeException("dim1"))
            Seq.iteri (fun i x -> m.[zero].SetVal(i, x)) dim0
            Seq.iteri (fun i x -> m.[one].SetVal(i, x)) dim1

*)


