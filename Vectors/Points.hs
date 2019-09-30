{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.TwoD.Vector

example1 :: Diagram B
example1 = flip atPoints (repeat (circle 0.2 # fc green))
    $ map p2 $ [(1,1),(0,3),(-2,1),(-1,-4),(2,0)]

example2 :: Diagram B
example2 = atPoints (nonagon 1) (repeat (circle 0.2 # fc green))

example3 :: Diagram B
example3 = drawPts sqPts blue
        <> drawPts (sqPts # scale 2 # rotateBy (1/8) # translateX 0.2) red

sqPts = square 1

drawPts pts c = pts # map (\p -> (p, dot' c)) # position

dot' c = circle 0.2 # fc c

main = mainWith $ example3
