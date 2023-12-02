module Helpers
open System
open System.Text.RegularExpressions;

let parseInt (x : string) = 
    match Int32.TryParse(x) with
    | true, n -> Some n
    | _ -> None

let regexMatch (pattern : string) (str : string) =
    let regex = new Regex(pattern)
    regex.Match(str).Value

let allMatches (pattern : string) (str : string) =
    let regex = new Regex(pattern)
    regex.Matches(str)
    |> Seq.cast<Match>
    |> Seq.map(fun m -> m.Value)