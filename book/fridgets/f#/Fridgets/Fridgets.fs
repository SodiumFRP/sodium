namespace Fridgets

open System.Windows
open Sodium.Frp

type Output = { drawable : Cell<DrawableDelegate>; desiredSize : Cell<Size>; sChangeFocus : Stream<int64> }

type IFridget =
    abstract member Reify : size : Cell<Size option> -> sMouse : Stream<MouseEvent> -> sKey : Stream<KeyEvent> -> focus : Cell<int64> -> idSupply : Supply.T -> Output

module Fridget =
    let reify (fridget : IFridget) size sMouse sKey focus idSupply =
        fridget.Reify size sMouse sKey focus idSupply