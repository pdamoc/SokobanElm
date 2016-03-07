module Level where

import Definitions  exposing (..)

import Svg            exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events     exposing (..)

import String
import Time   exposing (Time)

type Action = ActNop | ActWalk Pos | ActRestart Model | ActTick Time

type alias Model =
    { tiles     : List Tile
    , holes     : List Pos
    , boxes     : List Pos
    , player    : Pos
    , won       : Bool
    , steps     : Int
    , timeStart : Time
    , timestamp : Time }

drawTile : Tile -> Svg
drawTile tile =
    let
        (x', y')   = getScreenPos tile.pos
        image_path =      if tile.block == BFloor then "/img/2.gif" 
                     else if tile.block == BWall  then "/img/1.gif" else ""
    in
        if image_path /= "" then
            image [ x <| toString x', y <| toString y', xlinkHref <| image_path  
                  , width "32px", height "32px" ] []
        else svg [] []

drawHole : Pos -> Svg
drawHole pos =
    let
        (x', y') = getScreenPos pos
    in
        image [ x <| toString x', y <| toString y', xlinkHref <| "/img/7.gif"  
                  , width "32px", height "32px" ] []

drawPlayer : Bool -> Pos -> Svg
drawPlayer holed pos =
    let
        (x', y')   = getScreenPos pos
        image_path = if holed then "/img/6.gif" else "/img/5.gif"
    in
        image [ x <| toString x', y <| toString y', xlinkHref <| image_path  
              , width "32px", height "32px" ] []

drawBox : Bool -> Pos -> Svg
drawBox correct pos =
    let
        (x', y')   = getScreenPos pos
        image_path = if correct then "/img/4.gif" else "/img/3.gif" 
    in
        image [ x <| toString x', y <| toString y', xlinkHref <| image_path  
              , width "32px", height "32px" ] []

drawModel : Model -> Svg
drawModel model =
    svg [ width "500", height "500", viewBox "0 0 500 500" ]
     <| [ text' [ x "24", y "15", fill "black" ] [ text "Sokoban" ] ]
     ++ (List.map drawTile model.tiles)
     ++ (List.map drawHole model.holes)
     ++ (List.map (\bpos -> drawBox (List.any ((==) bpos) model.holes) bpos) model.boxes)
     ++ [ drawPlayer (List.any ((==) model.player) model.holes) model.player
        , text' [ x "24", y "32", fill "black" ] [ text <| "Steps: " ++ toString model.steps ]
        , text' [ x "104", y "32", fill "black" ]
                [ text <| ((++) "Time: ") <| toString <| flip (/) 100 <| toFloat <| round <| flip (/) 10 <| model.timestamp - model.timeStart ]
        ]

isWalkable : Pos -> Model -> Bool
isWalkable pos model =
    let
        tile = model.tiles
            |> List.filter (.pos >> (==) pos)
            |> List.head
    in
        case tile of
            Nothing -> False
            Just tile' -> tile'.block == BFloor

canMoveBoxes : Pos -> Pos -> List Pos -> List Tile -> Maybe (List Pos)
canMoveBoxes player_pos (dx, dy) boxes tiles =
    let
        boxes' = List.map (\(bx, by) ->
                     if (bx, by) == player_pos
                     then (bx + dx, by + dy)
                     else (bx, by))  boxes
        zip_boxes' = List.map2 (,) [1..List.length boxes'] boxes'
        colided_with_box  =
            flip List.any zip_boxes'
                (\(i, bpos) -> flip List.any zip_boxes'
                    (\(i2, bpos2) -> i /= i2 && bpos == bpos2))
        colided_with_wall =
            flip List.any boxes' (\bpos ->
                flip List.any tiles (\t -> t.pos == bpos && t.block == BWall))
    in
        if colided_with_box || colided_with_wall
        then Nothing
        else Just boxes'

update : Action -> Model -> Model
update action model =
    case action of
        ActNop     -> model
        ActRestart model -> model
        ActTick t -> { model | timestamp = t }
        ActWalk (x, y) ->
            if model.won then model
            else
            let
                pos     = (fst model.player + x, snd model.player + y)
                boxes'  = canMoveBoxes pos (x, y) model.boxes model.tiles
                success = abs x + (abs y) == 1
                       && isWalkable pos model
                       && boxes' /= Nothing
                won     = success
                       && flip List.all (Maybe.withDefault model.boxes boxes')
                                        (\bpos -> List.any ((==) bpos) model.holes)
            in
                if not success
                then model
                else { model | player = pos
                             , boxes  = Maybe.withDefault model.boxes boxes'
                             , won    = won
                             , steps  = model.steps + 1 }

strToLevel : String -> Model
strToLevel str =
    let 
        xs    : List (Int, Char)
        xs    = List.map2 (,) [1..String.length str] (String.toList <| String.toLower <| str)
        tiles = xs
             |> List.map (\(i, c) ->
                    Tile (getPosByIndex i) (if c == 'x' then BWall
                                            else if c == '.' then BVoid else BFloor))
        holes = xs
             |> List.filter (\(i, c) -> c == 'o' || c == 'k' )
             |> List.map (fst >> getPosByIndex)             
        boxes = xs
             |> List.filter (\(i, c) -> c == 'b' || c == 'k' )
             |> List.map (fst >> getPosByIndex)
        player = xs
              |> List.filter (\(i, c) -> c == 'p' )
              |> List.map (fst >> getPosByIndex)
              |> List.head
              |> Maybe.withDefault (5, 5)
    in
        Model tiles holes boxes player False 0 0.0 0.0
