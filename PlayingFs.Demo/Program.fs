// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open Game

[<EntryPoint>]
let main _ = 
    let game = Game(4)
    let printStats player = printfn "%d %s: %A" player.Ranking player.Name player.Hand.Value |> ignore

    for _ in 1 .. 10 do
        for player in game.Play() do
            printStats player
        printfn "" |> ignore

    System.Console.ReadKey() |> ignore
    0