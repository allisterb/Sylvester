#r "C:\\Users\\Allister\\.nuget\\packages\\fsharp.interop.dynamic\\4.0.3.130\\lib\\netstandard1.6\\FSharp.Interop.Dynamic.dll"
#r "C:\\Users\\Allister\\.nuget\\packages\\csvhelper\\12.1.2\\lib\\netstandard2.0\\CsvHelper.dll"
#r "C:\\Users\\Allister\\.nuget\\packages\\dynamitey\\2.0.9.136\\lib\\netstandard1.5\\Dynamitey.dll"
#r ".\\..\\..\\src\\Lang\\Sylvester.DataFrame.Dynamic\\bin\\Debug\\netstandard2.0\\Sylvester.Base.Backend.dll"
#r ".\\..\\..\\src\\Lang\\Sylvester.DataFrame.Dynamic\\bin\\Debug\\netstandard2.0\\Sylvester.Data.dll"
#r ".\\..\\..\\src\\Lang\\Sylvester.DataFrame.Dynamic\\bin\\Debug\\netstandard2.0\\Sylvester.DataFrame.Dynamic.dll"

open System
open System.Collections.Generic
open System.Linq;

open FSharp.Interop.Dynamic
open Sylvester.Data

let titanic = CsvFile("https://raw.githubusercontent.com/datasciencedojo/datasets/master/titanic.csv")
titanic.["Pclass"].Type <- typeof<int>
let dt = Frame(titanic)

let r:IEnumerable<int * float * float> = query {
    for r in dt do
    groupBy r?Pclass into g
    sortBy g.Key
    select (
        let survived = (g.Where(fun p -> p?Survived = "1").Count()) |> float
        let died = (g.Where(fun p -> p?Survived = "0").Count()) |> float
        let ctotal = survived + died
        let psurvived = round(100.0 * survived / ctotal)
        let pdied = round(100.0 * died / ctotal) 
        (g.Key, pdied, psurvived)
)}
for (t1, t2, t3) in r do
    printf "%i %f %f\n" t1 t2 t3