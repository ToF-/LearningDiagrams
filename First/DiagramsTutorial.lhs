> {-# LANGUAGE NoMonomorphismRestriction #-}
> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE TypeFamilies #-}
> 
> import Diagrams.Prelude
> import Diagrams.Backend.SVG.CmdLine
> 
> myCircle :: Diagram B
> myCircle = circle 1 # fc red
>                     # lw none
                      |||
                      circle 1 
                      # fc green
                      # lw none
> 
> main = mainWith myCircle

