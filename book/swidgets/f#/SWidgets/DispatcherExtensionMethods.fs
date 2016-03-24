module SWidgets.DispatcherExtensionMethods

open System.Windows.Threading

type Dispatcher with
    member this.InvokeIfNecessary action =
        if this.CheckAccess() then action () else this.Invoke(action)
