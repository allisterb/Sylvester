namespace Sylvester.Tensors

open System

open MathNet.Numerics.LinearAlgebra

open Sylvester.Arithmetic
open Sylvester.Arithmetic.N10
open Sylvester.Collections

[<AutoOpen>]
module Logic =

    
    let inline scalar (x:'t) = Scalar<'t>(x)

    let inline vec (n:N10<'d10,'d9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>) (arr:'t[]) =  
        Vector<'t, 'd10,'d9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>(n, arr)

    let inline vnew n x = vec n (Array.create (n |> int) x)

    let inline vrand (n:N10<'d10,'d9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>)  =  
        Vector<float32, 'd10,'d9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>.Rand

    let inline vone (n:N10<'d10,'d9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>)  =  
        Vector<float32, 'd10,'d9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>.One

    let inline vzero (n:N10<'d10,'d9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>)  =  
        Vector<float32, 'd10,'d9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>.Zero

    let inline mat (dim0:N10<'d10,'d9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>) (dim1:N10<'e10,'e9, 'e8, 'e7, 'e6, 'e5, 'e4, 'e3, 'e2, 'e1>) (x:'t[,]) =
        Matrix<'t, 'd10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1, 'e10, 'e9, 'e8, 'e7, 'e6, 'e5, 'e4, 'e3, 'e2, 'e1> (dim0, dim1, x)

    let inline mnew d0 d1 x = mat d0 d1 (Array2D.create (d0 |> int) (d1 |> int) x)

    let inline mrand (dim0:N10<'d10,'d9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>) (dim1:N10<'e10,'e9, 'e8, 'e7, 'e6, 'e5, 'e4, 'e3, 'e2, 'e1>)  =  
        Matrix<float32, 'd10,'d9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1, 'e10,'e9, 'e8, 'e7, 'e6, 'e5, 'e4, 'e3, 'e2, 'e1>.Rand

    let inline mone (dim0:N10<'d10,'d9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>) (dim1:N10<'e10,'e9, 'e8, 'e7, 'e6, 'e5, 'e4, 'e3, 'e2, 'e1>)  =  
        Matrix<float32, 'd10,'d9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1, 'e10,'e9, 'e8, 'e7, 'e6, 'e5, 'e4, 'e3, 'e2, 'e1>.One

    let inline mzero (dim0:N10<'d10,'d9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>) (dim1:N10<'e10,'e9, 'e8, 'e7, 'e6, 'e5, 'e4, 'e3, 'e2, 'e1>)  =  
        Matrix<float32, 'd10,'d9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1, 'e10,'e9, 'e8, 'e7, 'e6, 'e5, 'e4, 'e3, 'e2, 'e1>.Zero

    let inline mident (dim0:N10<'d10,'d9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>) (dim1:N10<'e10,'e9, 'e8, 'e7, 'e6, 'e5, 'e4, 'e3, 'e2, 'e1>)  =  
        Matrix<float32, 'd10,'d9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1, 'e10,'e9, 'e8, 'e7, 'e6, 'e5, 'e4, 'e3, 'e2, 'e1>.Identity

    
    let inline vconj(v:Vector<'t, 'd10,'d9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>) = 
        v._Vector.Conjugate().ToArray() |> Vector<'t, 'd10,'d9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1> 

    let inline vl1norm(v:Vector<'t, 'd10,'d9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>) = 
        v._Vector.L1Norm() |> Scalar 

    let inline vl2norm(v:Vector<'t, 'd10,'d9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>) = 
        v._Vector.L2Norm() |> Scalar
        
    let inline vpnorm(v:Vector<'t, 'd10,'d9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>) p = 
        v._Vector.Norm(p) |> Scalar 


    let inline vsum(v:Vector<'t, 'd10,'d9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>) = 
        v._Vector.Sum() |> Scalar 

    let inline vsumm(v:Vector<'t, 'd10,'d9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>) = 
        v._Vector.SumMagnitudes() |> Scalar
        
    let inline vmin(v:Vector<'t, 'd10,'d9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>) = 
        v._Vector.Minimum() |> Scalar
        
    let inline vmax(v:Vector<'t, 'd10,'d9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>) = 
        v._Vector.Maximum() |> Scalar 
   
    let inline vmini(v:Vector<'t, 'd10,'d9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>) = 
        v._Vector.MinimumIndex() |> Scalar 

    let inline vmaxi(v:Vector<'t, 'd10,'d9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>) = 
        v._Vector.MaximumIndex() |> Scalar 

    let inline vmina(v:Vector<'t, 'd10,'d9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>) = 
        v._Vector.AbsoluteMinimum() |> Scalar 

    let inline vmaxa(v:Vector<'t, 'd10,'d9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>) = 
        v._Vector.AbsoluteMaximum() |> Scalar 

    let inline minsrow m p v  = 
        checklt(p, !+ m)
        checkeq((!+v), !++ m)
        let newdim0 = (!+ m) + one
        let dim1 = !++ m
        mat newdim0 dim1 ((Matrix.insertRow (p |> int) (!@@ v) (!@@ m)).ToArray())

    let inline minscol m p v  = 
        checklt(p, !++ m)
        checkeq((!+v), !+ m)
        let newdim1 = (!++ m) + one
        let dim0 = !+ m
        mat newdim1 dim0 ((Matrix.insertCol (p |> int) (!@@ v) (!@@ m)).ToArray())

    let inline maprow m v  = 
        checkeq((!+v), !++ m)
        let newdim0 = (!+ m) + one
        let dim1 = !++ m
        mat newdim0 dim1 ((Matrix.appendRow (!@@ v) (!@@ m)).ToArray())

    let inline mapcol m v  = 
        checkeq((!+v), !+ m)
        let newdim1 = (!++ m) + one
        let dim0 = !+ m
        mat dim0 newdim1 ((Matrix.appendCol (!@@ v) (!@@ m)).ToArray())

    let inline mpprow m v  = 
        checkeq((!+v), !++ m)
        let newdim0 = (!+ m) + one
        let dim1 = !++ m
        mat newdim0 dim1 ((Matrix.prependRow (!@@ v) (!@@ m)).ToArray())

    let inline mppcol m v  = 
        checkeq((!+v), !+ m)
        let newdim1 = (!++ m) + one
        let dim0 = !+ m
        mat dim0 newdim1 ((Matrix.prependCol (!@@ v) (!@@ m)).ToArray())

    let inline (!+?) v = (!?) << (!+) <| v

    let inline (!++?) v = (!?) << (!++) <| v

    let inline (+!>) l r = (+>)  ((!+) l) ((!+) r)
    
    let inline (++>) l r = (+>)  ((!++) l) ((!++) r)

    let inline (+!<) l r = (+<)  ((!+) l) ((!+) r)
    
    let inline (++<) l r = (+<)  ((!++) l) ((!++) r)

    let inline (+!>=) l r = (+>=)  ((!+) l) ((!+) r)
    
    let inline (++>=) l r = (+>=)  ((!++) l) ((!++) r)

    let inline (+!<=) l r = (+<=)  ((!+) l) ((!+) r)
    
    let inline (++<=) l r = (+<=)  ((!++) l) ((!++) r)

    let inline (+!==) l r = (+==)  ((!+) l) ((!+) r)

    let inline (+!!=) l r = (+!=)  ((!+) l) ((!+) r)

    let inline (++==) l r = (+==)  ((!++) l) ((!++) r)
    
    let inline (++!==>) l r = (+!=)  ((!++) l) ((!++) r)

    let inline (+@) m (p,v) = minsrow m p v

    let inline (+@.) m v = maprow m v

    let inline (+@@) m (p,v) = minscol m p v

    let inline (+@@.) m v = mapcol m v

    let inline (+.@) m v = mpprow m v

    let inline msum (m:Matrix<'t, 'd10,'d9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1, 'e10,'e9, 'e8, 'e7, 'e6, 'e5, 'e4, 'e3, 'e2, 'e1>) = 
        Matrix.sum m._Matrix |> scalar

    let inline msumrows (m:Matrix<'t, 'd10,'d9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1, 'e10,'e9, 'e8, 'e7, 'e6, 'e5, 'e4, 'e3, 'e2, 'e1>) = 
        let sum = Matrix.sumRows m._Matrix in sum.ToArray() |> vec m.Dim1

    let inline msumcols (m:Matrix<'t, 'd10,'d9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1, 'e10,'e9, 'e8, 'e7, 'e6, 'e5, 'e4, 'e3, 'e2, 'e1>) = 
        let sum = Matrix.sumCols m._Matrix in sum.ToArray() |> vec m.Dim0

    let inline mtrans (m:Matrix<'t, 'd10,'d9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1, 'e10, 'e9, 'e8, 'e7, 'e6, 'e5, 'e4, 'e3, 'e2, 'e1>) = 
        let trans = m._Matrix.Transpose() in trans.ToArray() |> mat m.Dim1 m.Dim0

    let inline minv (m:Matrix<'t, 'd10,'d9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1, 'e10, 'e9, 'e8, 'e7, 'e6, 'e5, 'e4, 'e3, 'e2, 'e1>) = 
        checkeq(m.Dim0, m.Dim1)
        let inv = m._Matrix.Inverse() in inv.ToArray() |> mat m.Dim0 m.Dim1

    let inline ml2norm (m:Matrix<'t, 'd10,'d9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1, 'e10, 'e9, 'e8, 'e7, 'e6, 'e5, 'e4, 'e3, 'e2, 'e1>) = 
        let norm = m._Matrix.L2Norm() in norm |> scalar

    let inline mrank (m:Matrix<'t, 'd10,'d9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1, 'e10, 'e9, 'e8, 'e7, 'e6, 'e5, 'e4, 'e3, 'e2, 'e1>) = 
        let rank = m._Matrix.Rank() in rank |> scalar

    let inline mtrace (m:Matrix<'t, 'd10,'d9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1, 'e10, 'e9, 'e8, 'e7, 'e6, 'e5, 'e4, 'e3, 'e2, 'e1>) = 
        checkeq(m.Dim0, m.Dim1)
        let trace = m._Matrix.Trace() in trace |> scalar

    let inline mdet (m:Matrix<'t, 'd10,'d9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1, 'e10, 'e9, 'e8, 'e7, 'e6, 'e5, 'e4, 'e3, 'e2, 'e1>) = 
        checkeq(m.Dim0, m.Dim1)
        let det = m._Matrix.Determinant() in det |> scalar

    let inline mcond (m:Matrix<'t, 'd10,'d9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1, 'e10, 'e9, 'e8, 'e7, 'e6, 'e5, 'e4, 'e3, 'e2, 'e1>) = 
        let cond = m._Matrix.ConditionNumber() in cond |> scalar

    let inline mbasis (m:Matrix<'t, 'd10,'d9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1, 'e10, 'e9, 'e8, 'e7, 'e6, 'e5, 'e4, 'e3, 'e2, 'e1>) = 
        let range = m._Matrix.Range() in Seq.map (fun (v:Vector<'t>) -> Vector<'t, 'd10,'d9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>(v.ToArray())) range 
        


    


