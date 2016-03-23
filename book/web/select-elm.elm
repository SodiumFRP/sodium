import Window
import Mouse
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Color exposing (..)
import Dict exposing (Dict)

shapes : Dict String (List (Float, Float))
shapes = Dict.fromList [
    ("cat",[(-85,12),(-73,48),(-68,13),(-41,14),(-34,48),(-25,11),(-17,-4),
            (-40,-32),(-52,-28),(-60,-32),(-92,-6)]),
    ("dog",[(31,44),(14,22),(16,-18),(26,-8),(26,20),(43,-28),(62,-25),
            (81,24),(85,-9),(97,-17),(91,43),(71,36),(55,42),(40,30)])]

toScreenSpace : Signal (Int, Int) -> Signal (Float, Float)
toScreenSpace sPos = Signal.map2 (\(w, h) (x, y) ->
        (toFloat x - toFloat w/2, toFloat h/2 - toFloat y)
    ) Window.dimensions sPos

insidePolygon : (Float, Float) -> List (Float, Float) -> Bool
insidePolygon (x, y) coords =
    case List.drop (List.length coords - 1) coords of
        [lastCoord] ->
            let (inside, _) =
                  List.foldl (\(x2, y2) (inside, (x1, y1)) ->
                    let inside' =
                          if ((y1<y && y2>=y) || (y2<y && y1>=y))
                                && (x1+(y-y1) / (y2-y1) * (x2-x1) < x)
                              then not inside
                              else inside
                    in (inside', (x2, y2))
                  ) (False, lastCoord) coords
            in inside
        _ -> False

main : Signal Element
main =
   let sMousePressed = Signal.filter (\down -> down) False Mouse.isDown
       sMouseDown = Signal.sampleOn sMousePressed
                                         (toScreenSpace Mouse.position)
       selected = Signal.map (\xy ->
           List.foldl (\(ident, coords) mSel ->
               if insidePolygon xy coords
                   then Just ident
                   else mSel
           ) Nothing (Dict.toList shapes)
         ) sMouseDown
   in Signal.map2 render Window.dimensions selected

render : (Int, Int) -> Maybe String -> Element
render (w, h) mSel = collage w h (draw mSel shapes)

draw : Maybe String -> Dict String (List (Float, Float)) -> List Form
draw mSel shapes = 
    List.concatMap (\(ident, coords) ->
        let poly = polygon coords
        in (if mSel == Just ident then [filled red poly] else []) ++
           [outlined (solid black) poly]
    ) (Dict.toList shapes)

