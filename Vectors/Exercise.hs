{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.TwoD.Vector

example1 :: Diagram B
example1 = fromOffsets . map (\a -> 1 *^ e (a @@ deg)) $ take 10 $ cycle [45,-45]

example2 :: Diagram B
example2 = position (zip pts (repeat c))

c = circle 1 # fc blue 
pts = map mkPoint $ take 7 $ iterate (+ (pi/7)) 0
    where
    mkPoint x = p2 (5*sin x,5*cos x)

main = mainWith (example1 === example2)
