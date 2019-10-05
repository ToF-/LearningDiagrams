{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.TwoD.Vector

diagram :: Diagram B
diagram = pentagon 1 # lc gray 
      `atop` fromOffsets [vcs!!0] # lc purple # dashingG [0.01,0.01] 0
      `atop` pentagon 1 # scaleRotateo (vcs!!0) # lc gray
      `atop` fromOffsets [vcs!!1] # lc green # dashingG [0.01,0.01] 0
      `atop` fromOffsets [vcs!!2] # lc blue # dashingG [0.01,0.01] 0
      `atop` fromOffsets [vcs!!3] # lc red # dashingG [0.01,0.01] 0
      `atop` fromOffsets [vcs!!4] # lc orange # dashingG [0.01,0.01] 0
      
    where
    vts = trailVertices (pentagon 1)
    vcs = zipWith (\p q -> (fromPoint p) ^+^ (fromPoint q)) vts (tail vts ++ [vts!!0])
    fromPoint = r2 . unp2
    v  = (r2.unp2) (vts!!0)
    w  = (r2.unp2) (vts!!1)
    vw = v ^+^ w
    x  = (r2.unp2) (vts!!2)
    y  = (r2.unp2) (vts!!3)
    z  = (r2.unp2) (vts!!4)


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
