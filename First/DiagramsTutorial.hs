{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

myCircle :: Diagram B
myCircle = circle 1 # fc red # lw none ||| circle 1 # fc green # lw none ||| (square 1 # fc aqua `atop` circle 1 # showOrigin) === (square 2 # fc purple)

circles :: Diagram B
circles = hcat (map circle [1..6])

example :: Diagram B
example = vcat (replicate 3 circles)

circleSqV1 :: Diagram B
circleSqV1 = beside (r2 (1,1)) (circle 1) (square 2)

circleSqV2 :: Diagram B
circleSqV2 = beside (r2 (1,-0.2)) (circle 2) (square 1)

main = mainWith $ hcat [circleSqV1, strutX 1, circleSqV2]

