{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.TwoD.Vector
import Diagrams.TwoD.Arc

flower :: Diagram B
flower = atPoints (fromOffsets pts) (take 5 arccs)
    where
    pts :: [V2 Double]
    pts = [1 *^ e (a @@rad) | a <- [0,tau/5..tau]]
    arcs :: [Diagram B]
    arcs= [arc (direction (1 *^ e (d @@ rad))) ((3*pi/5) @@ rad) 
          | d <- [0,tau/5..]]
    arccs = zipWith (\a c -> a # lc c) arcs [black,purple,green,blue,red,orange]

main = mainWith $ flower
