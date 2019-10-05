{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.TwoD.Vector

diagram :: Diagram B
diagram = penta pts0 black
   `atop` nextPoint (penta pts0 blue)
    where
    penta pts c = atPoints pts (map (\n ->  (text (show n) # fontSizeL 0.05 # fc white <> circle 0.05 # lw none # fc c)) [0..4]) # showOrigin
    nextPoint = reflectAbout (d ^& d) (rotateBy (fromIntegral 1 / 5) xDir)
    a = 0
    d = 1 

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
