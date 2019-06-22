namespace Sylvester.Tensors

open MathNet.Numerics.LinearAlgebra
[<AutoOpen>]
module Logic =

    open Sylvester.Arithmetic
    open Sylvester.Arithmetic.N10
    open MathNet.Numerics.LinearAlgebra

    let inline scalar (x:'t) = Scalar<'t>(x)

    let inline vec (n:N10<'d10,'d9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>) (arr:'t[]) =  
        Vector<'t, 'd10,'d9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>(n, arr)

    let inline vnew n x = vec n (Array.create (n |> int) x)

    let inline mat (dim0:N10<'d10,'d9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>) (dim1:N10<'e10,'e9, 'e8, 'e7, 'e6, 'e5, 'e4, 'e3, 'e2, 'e1>) (x:'t[,]) =
        Matrix<'t, 'd10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1, 'e10, 'e9, 'e8, 'e7, 'e6, 'e5, 'e4, 'e3, 'e2, 'e1> (dim0, dim1, x)

    let inline mnew d0 d1 x = mat d0 d1 (Array2D.create (d0 |> int) (d1 |> int) x)

    let inline vconj(v:Vector<'t, 'd10,'d9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>) = 
        v._Vector.Conjugate().AsArray() |> Vector<'t, 'd10,'d9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1> 

    let inline vnorm(v:Vector<'t, 'd10,'d9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>) = 
        v._Vector.L2Norm() |> Scalar 

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
        mat newdim0 dim1 ((Matrix.insertRow (p |> int) (!@@ v) (!@@ m)).AsArray())

    let inline minscol m p v  = 
        checklt(p, !++ m)
        checkeq((!+v), !+ m)
        let newdim1 = (!++ m) + one
        let dim0 = !+ m
        mat newdim1 dim0 ((Matrix.insertCol (p |> int) (!@@ v) (!@@ m)).AsArray())

    let inline maprow m v  = 
        checkeq((!+v), !++ m)
        let newdim0 = (!+ m) + one
        let dim1 = !++ m
        mat newdim0 dim1 ((Matrix.appendRow (!@@ v) (!@@ m)).AsArray())

    let inline mapcol m v  = 
        checkeq((!+v), !+ m)
        let newdim1 = (!++ m) + one
        let dim0 = !+ m
        mat newdim1 dim0 ((Matrix.appendCol (!@@ v) (!@@ m)).AsArray())

    let inline mpprow m v  = 
        checkeq((!+v), !++ m)
        let newdim0 = (!+ m) + one
        let dim1 = !++ m
        mat newdim0 dim1 ((Matrix.prependRow (!@@ v) (!@@ m)).AsArray())

    let inline mppcol m v  = 
        checkeq((!+v), !+ m)
        let newdim1 = (!++ m) + one
        let dim0 = !+ m
        mat newdim1 dim0 ((Matrix.prependCol (!@@ v) (!@@ m)).AsArray())

    let inline (+@) m (p,v) = minsrow m p v

    let inline (+@.) m v = maprow m v

    let inline (+@@) m (p,v) = minscol m p v

    let inline (+@@.) m v = mapcol m v

    let inline (+.@) m v = mpprow m v

    let inline msum (m:Matrix<'t, 'd10,'d9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1, 'e10,'e9, 'e8, 'e7, 'e6, 'e5, 'e4, 'e3, 'e2, 'e1>) = 
        Matrix.sum m._Matrix |> scalar

    let inline msumrows (m:Matrix<'t, 'd10,'d9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1, 'e10,'e9, 'e8, 'e7, 'e6, 'e5, 'e4, 'e3, 'e2, 'e1>) = 
        let sum = Matrix.sumRows m._Matrix in sum.AsArray() |> vec m.Dim1

    let inline msumcols (m:Matrix<'t, 'd10,'d9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1, 'e10,'e9, 'e8, 'e7, 'e6, 'e5, 'e4, 'e3, 'e2, 'e1>) = 
        let sum = Matrix.sumCols m._Matrix in sum.AsArray() |> vec m.Dim0

    let inline mtrans (m:Matrix<'t, 'd10,'d9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1, 'e10, 'e9, 'e8, 'e7, 'e6, 'e5, 'e4, 'e3, 'e2, 'e1>) = 
        let trans = m._Matrix.Transpose() in trans.AsArray() |> mat m.Dim1 m.Dim0

    let inline minv (m:Matrix<'t, 'd10,'d9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1, 'd10,'d9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>) = 
        let inv = m._Matrix.Inverse() in inv.AsArray() |> mat m.Dim0 m.Dim1

    let inline mnorm (m:Matrix<'t, 'd10,'d9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1, 'e10, 'e9, 'e8, 'e7, 'e6, 'e5, 'e4, 'e3, 'e2, 'e1>) = 
        let norm = m._Matrix.L2Norm() in norm |> scalar

    let inline mrank (m:Matrix<'t, 'd10,'d9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1, 'e10, 'e9, 'e8, 'e7, 'e6, 'e5, 'e4, 'e3, 'e2, 'e1>) = 
        let rank = m._Matrix.Rank() in rank |> scalar

    let inline mtrace (m:Matrix<'t, 'd10,'d9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1, 'd10,'d9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>) = 
        let trace = m._Matrix.Trace() in trace |> scalar

    let inline mdet (m:Matrix<'t, 'd10,'d9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1, 'd10,'d9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>) = 
        let det = m._Matrix.Determinant() in det |> scalar

