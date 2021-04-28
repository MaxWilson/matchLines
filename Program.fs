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

let (|Replace|) input =
    let rec recur input =
        let continueWith rest item' =
            let result, input = recur rest
            (item'::result), input
        match input with
        | [] -> [], input
        | name::pattern::v::rest when name = "-replace" && (not <| v.StartsWith ("-")) ->
            (Some pattern, v) |> continueWith rest
        | name::v::rest when name = "-replace" ->
            (None, v) |> continueWith rest
        | h::t as orig ->
            let result, input = recur t
            result, (h::input)
    recur input

let flip f x y = f y x

module Seq =
    let every f seq = Seq.exists (f >> not) seq |> not

[<EntryPoint>]
let main argv =
    match argv |> List.ofArray with
    | Optional "d" (dir, Optional "de" (de, Optional "df" (df, Optional "f" (fileFilter, Replace(replacements, (_::_ as patterns)))))) ->
        let rootDir =
            match dir with
            | None -> Environment.CurrentDirectory
            | Some v -> Path.Combine(Environment.CurrentDirectory, v)
        let isMatch (pattern: string) input = Regex.IsMatch(input, (pattern.Replace(".", "\.").Replace("*", ".*")), RegexOptions.IgnoreCase)
        let pathFilter =
            match fileFilter with
            | Some pattern -> fun (filePath: string) -> filePath |> Path.GetFileName |> (isMatch pattern)
            | None -> fun _ -> true
        let directoryFilter =
            match df, de with
            | Some include, Some exclude -> (fun x -> isMatch include x && not (isMatch exclude x))
            | None, Some exclude -> isMatch exclude >> not
            | Some include, None-> isMatch include
            | None, None -> fun _ -> true
        let rec recur dir =
            for file in Directory.EnumerateFiles dir |> Seq.filter pathFilter do
                let fileName = Path.GetFileName file
                // has to match all patterns in order to qualify for printout
                if File.ReadLines file |> Seq.filter (fun ln -> patterns |> Seq.every(flip isMatch ln)) |> Seq.isEmpty |> not then
                    printfn "%s" (file.Replace(rootDir + Path.DirectorySeparatorChar.ToString(), "")) // trim root directory from output
                    if replacements |> Seq.isEmpty then
                        for line in File.ReadLines file |> Seq.filter (fun ln -> patterns |> Seq.every(flip isMatch ln)) do
                            printfn "%s" line
                    else
                        let mutable modified = false
                        let mutable lines = File.ReadAllLines file
                        for ix in [0..lines.Length - 1] do
                            let mutable isMatch = false
                            let line = lines.[ix]
                            for rep in replacements do
                                match rep with
                                | Some key, value ->
                                    if Regex.IsMatch(line, key) then
                                        lines.[ix] <- Regex.Replace(line, key, value)
                                        modified <- true
                                        isMatch <- true
                                | None, value ->
                                    let mutable line = line
                                    for pattern in patterns do
                                        if Regex.IsMatch(line, pattern) then
                                            lines.[ix] <- Regex.Replace(line, pattern, value)
                                            modified <- true
                                            isMatch <- true
                            if isMatch then
                                printfn "%s" (lines.[ix])
                        if modified then
                            File.WriteAllLines(file, lines)
                    printfn ""
            for d in Directory.EnumerateDirectories dir |> Seq.filter directoryFilter do
                recur d
        recur rootDir
    | _ -> printfn "Usage: matchLines [-d <directory>] [-df <directoryFilter>] [-de <directories to exclude>] [-f <pathFilter>] [-replace [<pattern>] <value>] <patterns...>"
    0 // return an integer exit code