module Shared.Shapes

open System.Windows
open System.Windows.Media
open Sodium.Frp

let circle color = constantB (DrawableDelegate (fun d h o s ->
    let radius = s
    d.DrawEllipse(SolidColorBrush(color), Pen(Brushes.Black, 1.0), Point(o.X, h - o.Y), radius, radius)))

let scale drawable scale =
    let getScaleDrawableDelegate dr ns = DrawableDelegate (fun d h o s -> DrawableDelegate.invoke dr d h o (s * ns))
    lift2B getScaleDrawableDelegate (drawable, scale)

let translate drawable offset =
    let getTranslateDrawableDelegate dr no = DrawableDelegate (fun d h o s -> DrawableDelegate.invoke dr d h (Point.add no (Point.multiply o s)) s)
    lift2B getTranslateDrawableDelegate (drawable, offset)

let over a b =
    let getOverDrawableDelegate a b = DrawableDelegate (fun d h o s ->
        DrawableDelegate.invoke b d h o s
        DrawableDelegate.invoke a d h o s)
    lift2B getOverDrawableDelegate (a, b)