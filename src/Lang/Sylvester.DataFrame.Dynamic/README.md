
# Sylvester.DataFrame

## Introduction
Sylvester has a [data frame](https://www.nuget.org/packages/Sylvester.DataFrame/) type which uses the [.NET Dynamic Language Runtime](https://docs.microsoft.com/en-us/dotnet/framework/reflection-and-codedom/dynamic-language-runtime-overview) to provide a dynamic data structure for series data that still retains the advantages of static typing for data access and allows .NET's powerful LINQ query operators to be used seamlessly.


```fsharp
/// Use the Sylvester.DataFrame NuGet package in this notebook
#load "Paket.fsx"
Paket.Package["Sylvester.DataFrame";"FSharp.Interop.Dynamic"] 
#load "Paket.Generated.Refs.fsx"
```


```fsharp
open System
open System.Collections.Generic
open System.Linq;

open FSharp.Interop.Dynamic

open Sylvester
open Sylvester.Data

//Download a schema from a CSV file 
let msft = new CsvFile("https://raw.githubusercontent.com/matplotlib/sample_data/master/msft.csv")

// Set the first CSV field to a DateTime
msft.[0].Type <- typeof<DateTime>

// Set the remaining fields to floating point
for j in 1..msft.Fields.Count - 1 do msft.[j].Type <- typeof<float> 

// Show all the field labels in the schema
query { for f in msft do select (f.Label + ":" + f.Type.Name)}
```




    seq ["Date:DateTime"; "Open:Double"; "High:Double"; "Low:Double"; ...]




```fsharp
//Now create a frame from the fields defined
let df = new Frame(msft)

df
```




    seq
      [seq [29.97; 29.52; 29.96; 92433800.0; ...];
       seq [09/18/2003 00:00:00; 28.49; 29.51; 28.42; ...];
       seq [09/17/2003 00:00:00; 28.76; 28.95; 28.47; ...];
       seq [09/16/2003 00:00:00; 28.41; 28.95; 28.32; ...]; ...]




```fsharp
// The Date property is a dynamic member of df with a static series type
let date:Sd = df?Date
date
```




    seq
      [09/19/2003 00:00:00; 09/18/2003 00:00:00; 09/17/2003 00:00:00;
       09/16/2003 00:00:00; ...]



The High property is a series of floating-point data.


```fsharp
for i in df?High do printf "%.2f " i
```


    29.97 29.51 28.95 28.95 28.61 28.40 28.11 28.18 28.71 28.92 28.75 28.47 28.40 27.30 26.55 26.58 26.58 26.67 26.54 26.95 26.73 26.53 26.65 25.83 25.66 25.71 25.89 25.77 25.99 25.98 25.81 26.19 26.54 26.41 26.51 26.99 26.57 26.90 27.00 26.95 26.92 26.65 26.56 26.91 27.23 27.27 27.62 27.53 27.81 27.45 27.42 27.70 27.80 27.55 26.95 26.93 26.20 26.12 26.34 26.51 25.99 26.04 26.24 26.38 26.39 



```fsharp
// Frames implement IEnumerable and can be queried using LINQ
query {
    for r in df do
    sortByDescending r?Volume
    select r.["Date"]
}
```




    seq
      [09/03/2003 00:00:00; 07/02/2003 00:00:00; 09/19/2003 00:00:00;
       07/07/2003 00:00:00; ...]




```fsharp
// Select a tuple of 2 fields from the frame
query {
    for r in df do 
    sortBy r?High 
    select (r.["Date"], r.["High"])}
```




    seq
      [(08/15/2003 00:00:00, 25.66); (08/14/2003 00:00:00, 25.71);
       (08/12/2003 00:00:00, 25.77); (08/07/2003 00:00:00, 25.81); ...]




```fsharp
// The original MSFT dataset has 7 series
df.Series.Count
```




    7




```fsharp
// Columns can be added to frames dynamically

//Add a column of random numbers to the MSFT dataset
df?Foo<-Sn<double>.Rnd(df.Length)
df.OrderBy(fun r -> r?Date)
```




    seq
      [seq [26.39; 26.01; 26.07; 63626900.0; ...];
       seq [06/20/2003 00:00:00; 26.34; 26.38; 26.01; ...];
       seq [06/23/2003 00:00:00; 26.14; 26.24; 25.49; ...];
       seq [06/24/2003 00:00:00; 25.65; 26.04; 25.52; ...]; ...]




```fsharp
df.Series.Count
```




    8




```fsharp
query {for r in df do select (r.["Date"], r.["Foo"])}
```




    seq
      [(09/19/2003 00:00:00, 0.371624422); (09/18/2003 00:00:00, 0.6463783019);
       (09/17/2003 00:00:00, 0.1650539568); (09/16/2003 00:00:00, 0.6392976924); ...]



Rows in data frames forward data access calls to their parent frame. No additional storage for querying by row or column is allocated.


```fsharp
printfn "%.4f" df.[16]?Foo
```


    0.7808



Sylvester can make exploratory data analysis with F# easier and faster than existing .NET libraries. 


```fsharp
//Use the Titanic CSV dataset 
let titanic = new CsvFile("https://raw.githubusercontent.com/datasciencedojo/datasets/master/titanic.csv")

titanic.Select (fun f -> f.Label + ":" + f.Type.Name)
```




    seq
      ["PassengerId:String"; "Survived:String"; "Pclass:String"; "Name:String"; ...]




```fsharp
// Adjust the columns to what we want for querying
titanic.["PassengerId"].First().Type <- typeof<int>
titanic.["Survived"].First().Type <- typeof<int>
titanic.[5].Type <- typeof<int>

titanic.Select (fun f -> f.Label + ":" + f.Type.Name)
```




    seq ["PassengerId:Int32"; "Survived:Int32"; "Pclass:String"; "Name:String"; ...]




```fsharp
//Then load the CSV data
let dt = new Frame(titanic)
```


```fsharp
dt.GroupBy(fun r -> (int) r?Survived)
```




    seq
      [seq
         [seq ["male"; 22; "1"; "0"; ...];
          seq [5; 0; "3"; "Allen, Mr. William Henry"; ...];
          seq [6; 0; "3"; "Moran, Mr. James"; ...];
          seq [7; 0; "1"; "McCarthy, Mr. Timothy J"; ...]; ...];
       seq
         [seq
            [2; 1; "1"; "Cumings, Mrs. John Bradley (Florence Briggs Thayer)"; ...];
          seq [3; 1; "3"; "Heikkinen, Miss. Laina"; ...];
          seq [4; 1; "1"; "Futrelle, Mrs. Jacques Heath (Lily May Peel)"; ...];
          seq [9; 1; "3"; "Johnson, Mrs. Oscar W (Elisabeth Vilhelmina Berg)"; ...];
          ...]]



Using LINQ can make queries a lot less verbose than other .NET data frame libraries like [Deedle](https://bluemountaincapital.github.io/Deedle/)


```fsharp
// Add a new survived column with boolean type
dt?Survived2<-new Sn<bool>(dt.Select(fun r -> if r?Survived = 1 then true else false))

// Print out some values
for i in 0..10 do printfn "Name: %s Survived: %A" dt.[i]?Name dt.[i]?Survived2
```


    Name: Braund, Mr. Owen Harris Survived: false
    Name: Cumings, Mrs. John Bradley (Florence Briggs Thayer) Survived: true
    Name: Heikkinen, Miss. Laina Survived: true
    Name: Futrelle, Mrs. Jacques Heath (Lily May Peel) Survived: true
    Name: Allen, Mr. William Henry Survived: false
    Name: Moran, Mr. James Survived: false
    Name: McCarthy, Mr. Timothy J Survived: false
    Name: Palsson, Master. Gosta Leonard Survived: false
    Name: Johnson, Mrs. Oscar W (Elisabeth Vilhelmina Berg) Survived: true
    Name: Nasser, Mrs. Nicholas (Adele Achem) Survived: true
    Name: Sandstrom, Miss. Marguerite Rut Survived: true




```fsharp
// Create a frame view with just the Name and Survived2 columns

dt.SelC("Name", "Survived2").Take(10)
```




    seq
      [seq []; seq ["Cumings, Mrs. John Bradley (Florence Briggs Thayer)"; true];
       seq ["Heikkinen, Miss. Laina"; true];
       seq ["Futrelle, Mrs. Jacques Heath (Lily May Peel)"; true]; ...]




```fsharp
// Create a frame window with a string index

let w = dt.SWnd(dt?Name)
w.["Chaffee, Mr. Herbert Fuller"]
```




    seq ["1"; "Chaffee, Mr. Herbert Fuller"; "male"; 46; ...]




```fsharp
//Elements of all frame objects are strong-typed 

printfn "%s" w.["Chaffee, Mr. Herbert Fuller"]?Pclass
```


    1




```fsharp
// Create a new frame with just chosen columns
let dt2 = dt.SelF("Name", "Sex", "Age","Pclass", "Survived2")
dt2.Take(10)
```




    seq
      [seq ["male"; 22; false];
       seq
         ["1"; "Cumings, Mrs. John Bradley (Florence Briggs Thayer)"; "female"; 38;
          ...]; seq ["3"; "Heikkinen, Miss. Laina"; "female"; 26; ...];
       seq ["1"; "Futrelle, Mrs. Jacques Heath (Lily May Peel)"; "female"; 35; ...];
       ...]




```fsharp
query {
    for p in dt2 do
    groupBy p.["Pclass"] into g
    select (g.Key, g.Count())
} |>Util.Table
```




<table><thead><tr><th>Item1</th><th>Item2</th></tr></thead><tbody><tr><td>3</td><td>491</td></tr><tr><td>1</td><td>216</td></tr><tr><td>2</td><td>184</td></tr></tbody><tbody></tbody></table>




```fsharp

```
