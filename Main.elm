module Main where

import Game
import Level
import Definitions

import Svg exposing (Svg)

import Keyboard
import Time

mailbox : Signal.Mailbox Game.Action
mailbox = Signal.mailbox Game.ActNop
 
-- http://www.boxworld.matsel.de/daten/deutsch/1.htm

main : Signal Svg
main =
    Signal.mergeMany [ mailbox.signal
                     , Signal.merge Keyboard.arrows Keyboard.wasd
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
    |> Signal.foldp Game.update Game.initialModel
    |> Signal.map (Game.drawModel mailbox.address)
