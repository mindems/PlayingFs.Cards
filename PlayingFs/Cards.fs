module Cards

open System
open System.Collections.Generic

    type Suit = Club=0 | Diamond=1 | Spade=2 | Heart=3

    type Rank = 
        | Two   = 0b00000000000010
        | Three = 0b00000000000100
        | Four  = 0b00000000001000
        | Five  = 0b00000000010000
        | Six   = 0b00000000100000
        | Seven = 0b00000001000000
        | Eight = 0b00000010000000
        | Nine  = 0b00000100000000
        | Ten   = 0b00001000000000
        | Jack  = 0b00010000000000
        | Queen = 0b00100000000000
        | King  = 0b01000000000000
        | Ace   = 0b10000000000001//<-

    type Card = { Suit:Suit; Rank:Rank }

    type internal Deck () =
        let random = Random()

        let newDeck() =
            ( Enum.GetValues typeof<Suit> |> Seq.cast<Suit> , 
              Enum.GetValues typeof<Rank> |> Seq.cast<Rank> )
            ||> Seq.allPairs
            |> Seq.map (fun (s,r) -> { Suit=s; Rank=r }) 
            |> Seq.toArray

        let shuffle (array : 'a []) =
            for i in array.Length - 1 .. -1 .. 1 do
                let temp = array.[i]
                let index = random.Next(0, i + 1)
                array.[i] <- array.[index]
                array.[index] <- temp
            array

        member __.Initialize () = newDeck() |> shuffle |> Stack

        static member Deal numberOfCards (deck: Stack<Card>) = 
            [ for _ in 1 .. numberOfCards do yield deck.Pop() ]