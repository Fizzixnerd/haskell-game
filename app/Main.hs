module Main where

import Game.Main
import Foreign.Resource

main :: IO ()
main = runResourceTChecked gameMain
