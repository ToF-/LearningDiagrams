{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.TwoD.Vector
import Diagrams.TwoD.Arc

flower :: Diagram B
flower = atPoints (fromOffsets pts) arcs
    where
    pts :: [V2 Double]
    pts = [1 *^ e (a @@rad) | a <- [0,tau/5..tau]]
    arcs :: [Diagram B]
    arcs= [arc (direction (1 *^ e (d @@ rad))) (a @@ rad) #showOrigin  
          | d <- [tau/5..tau], let a = (tau)]


main = mainWith $ flower
