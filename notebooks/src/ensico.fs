module ENSICO // (c) ENSICO, 2026.03.07

open System
open Plotly.NET
open Microsoft.FSharp.Reflection
open Microsoft.DotNet.Interactive.Formatting

/// begin | haskell-style functions
 
let ($) f x = f x

let (..) = (<<)

let (=?) = (=)

let (.|.) f g x = (f x, g x)

let flip f x y = f y x

let konst x _ = x

/// end | haskell-style functions

let even n = n % 2 = 0
let odd n = not (even n)

let curry f x y = f (x, y)
let uncurry f (x, y) = f x y

// Pretty-printing for F# types in .NET Interactive

//Formatter.SetPreferredMimeTypesFor(typeof<obj>, "text/plain")

//Formatter.Register((fun (o: obj) -> sprintf "%A" o),mimeType = "text/plain")

Formatter.SetPreferredMimeTypesFor(typeof<System.Collections.IEnumerable>, "text/plain")

Formatter.SetPreferredMimeTypesFor(typeof<System.Tuple<System.Int32,System.Int32,System.Int32>>, "text/plain")

Formatter.Register<System.Collections.IEnumerable>((fun (xs: System.Collections.IEnumerable) (w: System.IO.TextWriter) ->
    match xs with
    | :? string as s -> w.Write(sprintf "%A" s)
    | _ -> let items = [ for x in xs -> sprintf "%A" x ] in w.Write("[" + System.String.Join("; ", items) + "]")), mimeType = "text/plain")

//Formatter.Register<System.Collections.IEnumerable>((fun (xs: System.Collections.IEnumerable) (w: System.IO.TextWriter) -> let items = [ for x in xs -> sprintf "%A" x ] in w.Write("[" + System.String.Join("; ", items) + "]")), mimeType = "text/plain")

// Pretty-print the signature of a function

let printSignature f =
    let t = f.GetType()
    printfn "Runtime type: %A" t
    if FSharpType.IsFunction t then
        let domain, range = FSharpType.GetFunctionElements t
        printfn "Domain: %A" domain
        printfn "Range: %A" range

/// Pretty-print an F# type name (short form)
let rec  prettyTypeName (t: Type) =
    if t.IsGenericType then
        let genericDef = t.GetGenericTypeDefinition()
        if genericDef = typedefof<Option<_>> then
            sprintf "%s option" (prettyTypeName (t.GetGenericArguments()[0]))
        elif genericDef = typedefof<list<_>> then
            sprintf "%s list" (prettyTypeName (t.GetGenericArguments()[0]))
        elif genericDef = typedefof<Result<_,_>> then
            let args = t.GetGenericArguments() |> Array.map prettyTypeName
            sprintf "%s result" (String.Join(" * ", args))
        else
            let args = t.GetGenericArguments() |> Array.map prettyTypeName
            sprintf "%s<%s>" (t.Name.Split('`')[0]) (String.Join(", ", args))
    else
        match t.FullName with
        | "System.Int32" -> "int"
        | "System.String" -> "string"
        | "System.Boolean" -> "bool"
        | "System.Double" -> "float"
        | null -> t.Name
        | _ -> t.Name

/// Extracts the full F# function signature as a string
let signature (f: obj) =
    let rec loop (t: Type) acc =
        if FSharpType.IsFunction t then
            let argType, returnType = FSharpType.GetFunctionElements t
            loop returnType (acc @ [prettyTypeName argType])
        else
            acc @ [prettyTypeName t]
    let parts = loop (f.GetType()) []
    String.Join(" -> ", parts)

module List =
    
    let reverse = List.rev

    let nub = List.distinct

    let maximum = List.max

    let duplicate l = l @ l

    let join l1 l2 = l1 @ l2

module String =
    
    let max (s: string) = s |> Seq.max

    let maximum = max

    let rev (s: string) = s |> Seq.rev |> String.Concat

    let reverse = rev

    let sort (s: string) = s |> Seq.sort |> String.Concat
        //new string (s.ToCharArray() |> Array.rev)
    let distinct (s: string) = s |> Seq.distinct |> String.Concat

    let nub = distinct

    let words (s: string) = s.Split([| " " |], System.StringSplitOptions.None) |> Array.toSeq |> Seq.toList

    let unwords (s: string list) = String.Join(" ", s)

module Graphics =

    let red r (_,g,b) = (r % 256,g,b)

    let green g (r,_,b) = (r,g % 256,b)

    let blue b (r,g,_) = (r,g,b % 256)

    let draw x = x |> List.chunkBySize 3 |> List.map (List.map (fun (a,b,c) -> [a;b;c])) |> Chart.Image

    let draw' n x = x |> List.chunkBySize n |> List.map (List.map (fun (a,b,c) -> [a;b;c])) |> Chart.Image

