{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.TwoD.Vector

flower :: Diagram B
flower = arc d a  # showOrigin
    where
    d :: Direction V2 Double
    d = yDir 
    a :: Angle Double
    a = (tau/5) @@ rad


main = mainWith $ flower
