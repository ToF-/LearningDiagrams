{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.TwoD.Vector

diagram :: Diagram B
diagram = mempty
    where
    r :: V2 Double
    r = unitY

    p :: Path V2 Double
    p = atPoints r

main = mainWith $ diagram


-- import Diagrams.Trail  -- for trailPoints
--
-- visPoints :: [P2 Double] -> Diagram B
-- visPoints pts = atPoints pts (repeat (circle 0.05 # lw none # fc blue))
--
-- example = hsep 0.5
--  [ circle 1 `beneath` visPoints (trailVertices (circle 1))
--  , circle 1 `beneath` visPoints (trailPoints (circle 1))
--  , hexagon 1 `beneath` visPoints (trailVertices (hexagon 1))
--  ]
