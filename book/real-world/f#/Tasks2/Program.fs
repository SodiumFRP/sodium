open System

[<EntryPoint>]
let main argv =
    Tasks.App.run () |> Async.RunSynchronously
    0