{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine


node :: Int -> Diagram B
node n = text (show n) # fontSizeL 0.2 # fc white <> circle 0.2 # fc green

tournament :: Int -> Diagram B 
tournament n = atPoints (trailVertices $ regPoly n 1) (map node [1..n])



main = mainWith $ tournament 6
