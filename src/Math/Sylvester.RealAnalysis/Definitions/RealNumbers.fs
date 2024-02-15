namespace Sylvester

open System

module RealNumbers =
    let sin (s:Scalar<'t>) = call_sin s.Expr |> expand_as<'t> |> Scalar<'t>
    
    let cos (s:Scalar<'t>) = call_cos s.Expr |> expand_as<'t> |> Scalar<'t>
    
    let sqrt (s:Scalar<'t>) = call_sqrt s.Expr |> expand_as<'t> |> Scalar<'t>
        
    let min<'t when 't:equality and 't :> ValueType and 't :> IEquatable<'t> and 't:comparison> (x:Scalar<'t>) (y:Scalar<'t>)  = 
        <@ min %x.Expr %y.Expr @> |> Scalar<'t>
    
    let ln (s:Scalar<real>) = <@ log %s.Expr @> |> Scalar<real>


