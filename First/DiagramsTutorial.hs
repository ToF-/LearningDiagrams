{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

myCircle :: Diagram B
myCircle = circle 1 # fc red # lw none ||| circle 1 # fc green # lw none ||| (square 1 # fc aqua `atop` circle 1 # showOrigin) === (square 2 # fc purple)

main = mainWith myCircle

