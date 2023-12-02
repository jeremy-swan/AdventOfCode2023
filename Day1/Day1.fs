module Day1 
open TypeExtensions
open System

let PART_1_TEST_INPUT = "day1_part1_test.txt"
let PART_2_TEST_INPUT = "day1_part2_test.txt"
let REAL_INPUT = "day1_real.txt"

let parseInt (x : string) = 
    match Int32.TryParse(x) with
    | true, n -> Some n
    | _ -> None


let matchDigit (str : string) = 
    let (|StartingWith|_|) (pattern : string) (str: string)=
        match str.StartsWith(pattern) with
        | true -> Some str
        | false -> None

    match str with
    | StartingWith "1" _ | StartingWith "one" _ -> Some 1
    | StartingWith "2" _ | StartingWith "two" _ -> Some 2
    | StartingWith "3" _ | StartingWith "three" _ -> Some 3
    | StartingWith "4" _ | StartingWith "four" _ -> Some 4
    | StartingWith "5" _ | StartingWith "five" _ -> Some 5
    | StartingWith "6" _ | StartingWith "six" _ -> Some 6
    | StartingWith "7" _ | StartingWith "seven" _ -> Some 7
    | StartingWith "8" _ | StartingWith "eight" _ -> Some 8
    | StartingWith "9" _ | StartingWith "nine" _ -> Some 9
    | _ -> None


let part1 (input : string list) =
    input
    |> List.sumBy(fun line -> 
        line
        |> Seq.map (string >> parseInt) 
        |> Seq.filter Option.isSome
        |> Seq.map (fun x -> x.Value)
        |> fun numbers -> 
            sprintf "%i%i" (Seq.head numbers) (Seq.last numbers)
            |> parseInt
            |> Option.defaultValue 0)

let part2 (input : string list) =
    input
    |> List.sumBy(fun line -> 
        [0..(line.Length - 1)]
        |> List.map (fun i -> 
            line.Substring(i) 
            |> matchDigit
        )
        |> Seq.filter Option.isSome
        |> Seq.map (fun x -> x.Value)
        |> fun numbers -> 
            sprintf "%i%i" (Seq.head numbers) (Seq.last numbers)
            |> parseInt
            |> Option.defaultValue 0)