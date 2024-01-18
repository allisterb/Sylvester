#load "Include.fsx"

open Sylvester

let Sn = realseq "S" "n" <@ fun n -> (1. / (real n))@>

Sn.UnicodeDisplay