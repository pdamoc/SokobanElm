module Main where

import Game

import Svg exposing (Svg)
import StartApp
import Effects exposing (Effects)


import Keyboard
import Time

-- http://www.boxworld.matsel.de/daten/deutsch/1.htm


effectless : (a -> b -> c) -> a -> b -> ( c, Effects d )
effectless update action model = (update action model, Effects.none)

app : StartApp.App Game.Model
app = StartApp.start 
  { init = (Game.initialModel, Effects.none)
  , update = effectless Game.update
  , view = Game.drawModel
  , inputs = 
    [ Signal.merge Keyboard.arrows Keyboard.wasd
      |> Signal.map (\p -> Game.ActArrows (p.x, -p.y))
    , Keyboard.enter
      |> Signal.merge Keyboard.space
      |> Time.timestamp
      |> Signal.map (fst >> Game.ActEnter) 
      , Keyboard.isDown 77
       |> Signal.map (\b -> if b then Game.ActNop else Game.ActMenu)
      , Keyboard.isDown 82
       |> Time.timestamp 
       |> Signal.map (\(t, b) -> if b then Game.ActNop else Game.ActRestartLevel t)
      , Time.every 10
       |> Signal.map Game.ActTick
    ]
  }

main : Signal Svg
main = app.html