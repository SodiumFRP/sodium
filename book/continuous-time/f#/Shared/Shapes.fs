module Shared.Shapes

open System.Windows
open System.Windows.Media
open Sodium

let circle color = Cell.constant (DrawableDelegate (fun d h o s ->
    let radius = s
    d.DrawEllipse(SolidColorBrush(color), Pen(Brushes.Black, 1.0), Point(o.X, h - o.Y), radius, radius)))

let scale drawable scale =
    let getScaleDrawableDelegate dr ns = DrawableDelegate (fun d h o s -> DrawableDelegate.invoke dr d h o (s * ns))
    Cell.lift2 getScaleDrawableDelegate drawable scale

let translate drawable offset =
    let getTranslateDrawableDelegate dr no = DrawableDelegate (fun d h o s -> DrawableDelegate.invoke dr d h (Point.add no (Point.multiply o s)) s)
    Cell.lift2 getTranslateDrawableDelegate drawable offset

let over a b =
    let getOverDrawableDelegate a b = DrawableDelegate (fun d h o s ->
        DrawableDelegate.invoke b d h o s
        DrawableDelegate.invoke a d h o s)
    Cell.lift2 getOverDrawableDelegate a b