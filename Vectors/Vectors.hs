{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.TwoD.Vector

example1 :: Diagram B
example1 = lwG 0.05 . mconcat . map fromOffsets
    $ [ [r *^ e (r @@ rad )]
      | r <- [33 * tau/32, 34 * tau/32 .. 2 * tau]
      ]

example2 :: Diagram B
example2 = mconcat 
    [drawV p # lc green # lwG 0.03
    ,drawV u # lc blue
    ,drawV v # lc red
    ,drawV (p ^-^ v) # translate v # dashingG [0.1,0.1] 0
    ]

drawV = fromOffsets . return

u = r2 (1,2)
v = 2 *^ (unitY # rotateBy (1/19))
p = project u v
main = mainWith $ example2
