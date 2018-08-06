module Poker

open System
open Cards

type Hand =
    | HighCard of Rank List
    | Straight of highest : Rank
    | Flush of Rank List
    | FullHouse of triplet : Rank * pair : Rank

module internal Handlers =

    let fullHouseHandler cards = 
        let groups = cards |> List.groupBy(fun c -> c.Rank)
        let triplets = groups |> List.filter(fun (_,c) -> c.Length = 3)
        let pairs    = groups |> List.filter(fun (_,c) -> c.Length = 2)
        if List.isEmpty triplets || List.isEmpty pairs then None
        else
            ( triplets |> List.exactlyOne |> fst,
              pairs |> List.maxBy fst |> fst )
            |> FullHouse |> Some

    let flushHandler cards = 
        let groups = cards |> List.groupBy(fun c -> c.Suit)
        let flush = groups |> List.exists(fun (_,c) -> c.Length > 4)
        if not flush then None
        else
            groups 
            |> List.filter(fun (_,c) -> c.Length > 4)
            |> List.exactlyOne |> snd
            |> List.sortByDescending(fun c -> c.Rank)
            |> List.take(5)
            |> List.map (fun c -> c.Rank)
            |> Flush |> Some

    let straightHandler cards = 
        let ranks = cards |> List.map(fun c -> Convert.ToInt32 c.Rank) |> Set.ofList
        let binary = Convert.ToString(Seq.sum ranks, 2)
        let straight = binary.Contains("11111")
        if not straight then None
        else
            let highestIndex = binary.Length - 1 - binary.IndexOf("11111")
            let rankFor index : Rank = 
                if index = 0 || index = 13 then Rank.Ace 
                else enum <| pown 2 index
            Some <| Straight(rankFor highestIndex) 

    let highCardHandler cards = 
        cards
        |> List.sortByDescending(fun c -> c.Rank)
        |> List.take(5)
        |> List.map (fun c -> c.Rank)
        |> HighCard |> Some