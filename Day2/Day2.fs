module Day2
open Helpers

let TEST_INPUT = "day2_test.txt"
let REAL_INPUT = "day2_real.txt"

type Game = {
    id: int;
    maxRedsShown: int;
    maxGreensShown: int;
    maxBluesShown: int;
}

let parseGame (game : string) : Game = 
    let getColour colour = 
        game 
        |> allMatches (sprintf "\d*(?= %s)" colour) 
        |> Seq.map (parseInt >> Option.defaultValue 0)

    { id = regexMatch "(?<=Game )\d*" game |> parseInt |> Option.get
      maxRedsShown = getColour "red" |> Seq.max
      maxGreensShown = getColour "green" |> Seq.max
      maxBluesShown = getColour "blue" |> Seq.max }

let isGameValid maxReds maxGreens maxBlues (game : Game) = 
    game.maxRedsShown <= maxReds &&
    game.maxGreensShown <= maxGreens &&
    game.maxBluesShown <= maxBlues


let part1 (input : string list) =
    let gameCheck = isGameValid 12 13 14

    input
    |> List.map parseGame
    |> List.filter gameCheck
    |> List.sumBy (fun game -> game.id)


let part2 (input : string list) =
    input
    |> List.map parseGame
    |> List.sumBy (fun game -> game.maxRedsShown * game.maxGreensShown * game.maxBluesShown)