module Shared.Point

open System.Windows

let add (first : Point) (second : Point) = Point(first.X + second.X, first.Y + second.Y)
let subtract (first : Point) (second : Point) = Point(first.X - second.X, first.Y - second.Y)
let multiply (p : Point) (a : float) = Point(p.X * a, p.Y * a)