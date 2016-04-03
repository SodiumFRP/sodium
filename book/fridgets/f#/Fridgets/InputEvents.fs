namespace Fridgets

open System.Windows
open System.Windows.Input

type KeyEvent =
    | BackspaceKeyEvent
    | StringKeyEvent of string

type MouseEvent = { args : MouseEventArgs; getPosition : unit -> Point }