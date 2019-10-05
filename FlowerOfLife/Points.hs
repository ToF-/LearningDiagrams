{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.TwoD.Vector

diagram :: Diagram B
diagram = penta pts0 black
   `atop` nextPoint (pts0!!0) a (penta pts0 purple)
   `atop` nextPoint (pts0!!1) (2*a) (penta pts0 green)
   `atop` nextPoint (pts0!!2) (3*a) (penta pts0 blue)
   `atop` nextPoint (pts0!!3) (4*a) (penta pts0 red)
   `atop` nextPoint (pts0!!4) (5*a) (penta pts0 orange)
    where
    a = 1/5
    penta pts c = atPoints pts (map (\n ->  (text (show n) # fontSizeL 0.05 # fc white <> circle 0.05 # lw none # fc c)) [0..4]) # showOrigin
    nextPoint pt an = reflectAbout (x ^& y) (rotateBy an xDir)
        where
        (x,y) = unp2 pt


pts0 :: [P2 Double]
pts0 = trailVertices (pentagon 1)

pts1 :: [P2 Double]
pts1 = trailVertices (pentagon 1) # translateX t  # rotate (a@@rad) 
    where
    t = 1
    a = -pi/5
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
--  reflectAbout (0 ^& 0) (rotateBy (- fromIntegral i / 6) xDir)
--
