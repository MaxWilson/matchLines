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

module Seq =
    let every f seq = Seq.exists (f >> not) seq |> not

[<EntryPoint>]
let main argv =
    match argv |> List.ofArray with
    | Optional "d" (dir, Optional "de" (de, Optional "df" (df, Optional "f" (fileFilter, (_::_ as patterns))))) ->
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
                let lines = File.ReadLines file |> Seq.filter (fun ln -> patterns |> List.exists (flip isMatch ln)) |> Array.ofSeq
                // has to match all patterns in order to qualify for printout
                if patterns |> Seq.every (fun p -> lines |> Seq.exists (isMatch p)) then
                    printfn "%s" (file.Replace(rootDir + Path.DirectorySeparatorChar.ToString(), "")) // trim root directory from output
                    for line in lines do
                        printfn "%s" line
                    printfn ""
            for d in Directory.EnumerateDirectories dir |> Seq.filter directoryFilter do
                recur d
        recur rootDir
    | _ -> printfn "Usage: matchLines [-d <directory>] [-df <directoryFilter>] [-de <directories to exclude>] [-f <pathFilter>] <patterns...>"
    0 // return an integer exit code