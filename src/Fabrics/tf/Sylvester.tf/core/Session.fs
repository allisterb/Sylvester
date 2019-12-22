namespace Sylvester.tf

open TensorFlow

open Sylvester

type Session(graph:Graph) = 
    inherit Api()