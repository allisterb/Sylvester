# Sylvster.tf
## About
Sylvester.tf is a high-level functional, verifiable TensorFlow 2.0 API designed to embrace the overall Sylvester language goals of safety, expresiveness and interoperability

_Simple example_
```fsharp
let g = TensorGraph<dim<6>, dim<1>>("g") |> setDefaultGraph
```
## Inspiration
One of the ironies of today's computer programming landscape is that functional languages directly inspired by the declarative languages for expressing abstractions and equations of logic and mathematics, have been sidelined for mathematical and scientific computing in favor of imperative, dynamically-typed languages like Python and Julia. The ease-of-use and simplicity of a language like Python is the biggest selling point for its primary position in the machine learning and data science frameworks being released today.

But there are other dimensions to ease-of-use...one is where a language helps the user avoid errors in the mathematical expressions and functions he or she must construct to solve a problem in a particular domain, using for instance [type-level units of measure](https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/units-of-measure). Another is where a language can express mathematical and scientific computing programs elegantly and concisely while still retaining these safety benefits of static type checking. The safety and correctness of scientific programs has always been a huge issue for the industry, and Python, for all its virtues, is not a great step forward in that particular direction. Modern functional languages like Haskell, F#, and Scala have implemented advanced type checkers and type inference that eliminate almost all the burden of using static types, and that also make higher-order type-level programming a viable possibility for scientific computing libraries:

_Type-level programming using Sylvester and F#_
```fsharp
let v1 = Vec<1000>.Rand //Create a vector of length 1000
let v2 = Vec<500>.Rand //Create a vector of length 500
v1 + v2 //Can we do this?
```

    Type constraint mismatch. The type 
        'Vector<float32,0,0,0,0,0,0,0,5,0,0>'    
    is not compatible with type
        'Vector<float32,0,0,0,0,0,0,1,0,0,0>'  

Modern statically-typed open-source functional languages like F# with their support for advanced type checking and inference, language extension, meta-programming, and interoperability with lower-level libraries like the C/C++ core of TensorFlow, can offer an attractive, easy-to-use, safety-focused, alternative to mainstream scientific computing languages, especially for sectors like finance, aerospace, automotive or any industry where critical systems and decisions depend on mathematical programming, machine learning and data science. 

## What it does
A 'tensor fabric' is a framework or DSL that provides types and abstractions for creating computational graphs of tensor operations which can then be saved, loaded and executed on a variety of hardware platforms and devices. [Sylvester.tf](https://github.com/allisterb/Sylvester/tree/master/src/Fabrics/tf) provides a higher-order statically verifiable type system implemented in F# over the Tensorflow 2 C API and data structures for creating, saving, and loading TensorFlow format computational tensor graphs. Sylvester.tf combines the safety and productivity features of functional programming and higher-order type systems, with the hardware performance and interoperability of Google's TensorFlow and its large eco-system of models, tooling, and hardware execution environments, together with the language eco-system and tooling of a modern mainstream open-source functional programming language backed by Microsoft and the huge .NET user community.

The core [Sylvester](https://github.com/allisterb/Sylvester) language project is an F# DSL that attempts to bring the [safety benefits](https://youtu.be/-7SLsMog6w8) of dependently typed languages like Idris and ATS to mainstream scientific computing, by implementing an advanced type system that provides [type-level static guarantees](http://okmij.org/ftp/Computation/lightweight-static-guarantees.html)  about number-parameterized types for arrays, vectors, matrices, tensors and other types commonly used in scientific computing, data science and machine learning.

[Lightweight dependent typing](http://okmij.org/ftp/Haskell/dependent-types.html) describes a family of approaches to [approximating](https://www.cs.bu.edu/~hwxi/academic/papers/pldi98.pdf) the safety benefits of full-weight dependent typed languages like Idris, Agda, ATS et.al, in mainstream functional languages like Haskell and the ML family. Sylvester implements types that depend on [numeric values as parameters](http://okmij.org/ftp/Haskell/number-parameterized-types.html).and extends the syntax of F# to support using natural number parameters to array, vector, matrix, tensor and other mathematical types. These types encode values like length, rank and dimension size within the type which allows the F# type checker to act as a type checker for arithmetic, linear algebra and other mathematical domains.

Sylvester provides higher-order types for mathematical computing in F# as well as types like a [data frame type] (https://github.com/allisterb/Sylvester/tree/master/src/Data/Sylvester.DataFrame.Dynamic) that combines static and dynamic typing to allow Sylvester users to perform exploratory data analysis with all the ease-of-use and [conciseness](https://notebooks.azure.com/api/user/allisterb/project/sylvester/html/Sylvester.DataFrame.ipynb#Titanic-survivor-analysis-in-17-lines) of Python and R. 

Sylvester.tf binds to the TF2 C API and provides type-safe expressions of scalar, vector, matrix and higher-order tensor operations, which resolve to native TensorFlow graph definitions. These graph definitions can be saved and loaded and executed on any hardware platform or device supported by TensorFlow. Thus the benefits of higher-order types and functional programming can be leveraged without sacrificing performance or interoperability with existing scientific computing and machine learning tools and environments.

## How I built it
I have been developing the core language Sylvester features [for a couple of months](https://youtu.be/3zdlQ_HjKl4?list=PLNoHgLVTxtaorTczyo8NA3tg_vK8WC5rD) and the release of TensorFlow 2 and its retooled [C API](https://www.tensorflow.org/install/lang_c) and Google's commitment to non-Python language interoperability made starting work on a TF2-based tensor fabric a good choice..


## Challenges I ran into

## Accomplishments that I'm proud of

## What I learned

## What's next for Sylvester.tf
