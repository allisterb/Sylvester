namespace Sylvester

type Constraint<'d9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1 when 'd9 :> Base10Digit and 'd8 :> Base10Digit and 'd7 :> Base10Digit and 'd6 :> Base10Digit
    and 'd5 :> Base10Digit and 'd4 :> Base10Digit and 'd3 :> Base10Digit and 'd2 :> Base10Digit and 'd1 :> Base10Digit>
    (n9:'d9, n8:'d8, n7:'d7, n6:'d6, n5:'d5, n4:'d4, n3:'d3, n2:'d2, n1:'d1) = 
        inherit N9<'d9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>(n9, n8, n7, n6, n5, n4, n3, n2, n1)

 
 type True() = inherit N0()

 type tff<'d1, 'd2 when 'd1:>Base10Digit and 'd2:>Base10Digit> = N1<'d1> * N1<'d2>

 //type z<'d> when 'd :> N0

 module X = 
    let dr = True()


