{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

example :: Diagram B
example = fromOffsets [unitX, unitY, 2*^ unit_X, unit_Y] # centerXY

main = mainWith example
