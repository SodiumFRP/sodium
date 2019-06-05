namespace Sodium.Frp.Tests.Internal

module internal Async =
    open System.Threading.Tasks
    
    let internal StartAsVoidTask (a : Async<unit>) : Task = upcast (a |> Async.StartAsTask)