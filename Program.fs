// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open System.Text.RegularExpressions
open System.IO

let (|Optional|) flagName input =
    let rec recur input =
        match input with
        | [] -> None, input
        | name::v::rest when name = "-" + flagName ->
            Some v, rest
        | h::t as orig ->
            match recur t with
            | Some v, lst -> Some v, h::lst
            | None, _ -> None, orig
    recur input

let flip f x y = f y x

[<EntryPoint>]
let main argv =
    match argv |> List.ofArray with
    | Optional "d" (dir, Optional "df" (df, Optional "f" (fileFilter, (_::_ as patterns)))) ->
        let dir =
            match dir with
            | None -> Environment.CurrentDirectory
            | Some v -> Path.Combine(Environment.CurrentDirectory, v)
        let isMatch (pattern: string) input = Regex.IsMatch(input, (pattern.Replace(".", "\.").Replace("*", ".*")))
        let pathFilter =
            match fileFilter with
            | Some pattern -> isMatch pattern
            | None -> fun _ -> true
        let directoryFilter =
            match df with
            | Some pattern -> isMatch pattern
            | None -> fun _ -> true
        let rec recur dir =
            for file in Directory.EnumerateFiles dir |> Seq.filter pathFilter do
                let fileName = Path.GetFileName file
                for line in File.ReadLines file |> Seq.filter (fun ln -> patterns |> List.exists (flip isMatch ln))  do
                    printfn $"{fileName}: {line}"
            for d in Directory.EnumerateDirectories dir |> Seq.filter directoryFilter do
                recur d
        recur dir
    | _ -> printfn "Usage: matchLines [-d <directory>] [-df <directoryFilter>] [-f <pathFilter>] <patterns...>"
    0 // return an integer exit code