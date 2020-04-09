//#r ".\\..\\..\\src\\Providers\\Sylvester.Provider.Arithmetic\\src\\Sylvester.Provider.Arithmetic.Runtime\\bin\\Release\\netstandard2.0\\Sylvester.Provider.Arithmetic.Runtime.dll"
//#r ".\\..\\..\\src\\Base\\Sylvester.Collections\\bin\\Debug\\netstandard2.0\\Sylvester.Collections.dll"
//#r ".\\..\\..\\src\\Lang\\Sylvester.Expressions\\bin\\Debug\\netstandard2.0\\Sylvester.Expressions.dll"

#r ".\\..\\..\\ext\\FunScript\\src\\main\\FunScript\\bin\\Debug\\netstandard2.0\\FunScript.Util.dll"
#r ".\\..\\..\\ext\\FunScript\\src\\main\\FunScript\\bin\\Debug\\netstandard2.0\\FunScript.Interop.NETStandard.dll"
#r ".\\..\\..\\ext\\FunScript\\src\\main\\FunScript.Bindings.Base\\bin\\Debug\\netstandard2.0\\FunScript.Bindings.Base.dll"
#r ".\\..\\..\\ext\\FunScript\\src\\main\\FunScript\\bin\\Debug\\netstandard2.0\\FunScript.dll"

open System
open FunScript
open FunScript.Compiler
open FunScript.Bindings

[<ReflectedDefinition>]
[<AutoOpen>]
module Foo =
    // Create a function that will be compiled into JavaScript...
    let htmlCanvasTS(f)=
        let canvas = Globals.document.getElementsByTagName_canvas().[0]
        Globals.alert(f)

let js =  Compiler.Compile(<@ htmlCanvasTS @>, noReturn=true)
<@ htmlCanvasTS("foo")@>
