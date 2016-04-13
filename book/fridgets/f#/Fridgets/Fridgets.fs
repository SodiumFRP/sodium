namespace Fridgets

open System.Windows
open Sodium

type Output = { drawable : DrawableDelegate Cell; desiredSize : Size Cell; sChangeFocus : int64 Stream }

type IFridget =
    abstract member Reify : size : Size option Cell -> sMouse : MouseEvent Stream -> sKey : KeyEvent Stream -> focus : int64 Cell -> idSupply : Supply.T -> Output

module Fridget =
    let reify (fridget : IFridget) size sMouse sKey focus idSupply =
        fridget.Reify size sMouse sKey focus idSupply