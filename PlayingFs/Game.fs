module Game

open System
open Cards
open Poker
open Poker.Handlers

type Player = { Name:string; HoleCards:Card List; Hand:Hand Option; Ranking:int }

type internal ChainWorkflow () =
    member __.ReturnFrom(x) = x
    member __.Combine (a,b) = 
        match a with
        | Some _ -> a
        | None -> b
    member __.Delay(f) = f()

type Game (numberOfPlayers) =
    let deck = Deck()
    let chainOfHandlers = new ChainWorkflow()

    let deal cards =
        let initializePlayer i = { Name=sprintf"Player %d" i; HoleCards=Deck.Deal 2 cards; Hand=None; Ranking=0 }
        List.init numberOfPlayers initializePlayer, Deck.Deal 5 cards

    let rateHands players communityCards =
        let eliminateHands cards = chainOfHandlers {
            return! fullHouseHandler cards
            return! flushHandler cards
            return! straightHandler cards
            return! highCardHandler cards
            }

        let calculateHand holeCards communityCards =
            let cards = holeCards @ communityCards
            if cards.Length <> 7 then failwith "Each player should have 7 cards"
            cards |> eliminateHands

        let compareHands (r, h) p =
            let r' = r + if h = p.Hand then 0 else 1
            { p with Ranking = r' } , (r',p.Hand)

        players 
        |> List.map (fun p -> { p with Hand = calculateHand p.HoleCards communityCards })
        |> List.sortByDescending (fun p -> p.Hand.Value)
        |> List.mapFold compareHands (0, None) |> fst

    do if (numberOfPlayers < 2 || numberOfPlayers > 20) then raise <| ArgumentException("numberOfPlayers")

    member __.Play() =
        deck.Initialize()
        |> deal 
        ||> rateHands