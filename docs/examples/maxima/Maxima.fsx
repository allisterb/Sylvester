#load "Include.fsx"

open Sylvester

let m = Maxima.start "C:\\maxima-5.43.2\\bin\\maxima.bat"

let send = Maxima.send m

//let a = send "2 + 2;" 

send "display2d:false;"
send "expand(taylor(atan(x), x, 0, 7));"
//Maxima.stop m