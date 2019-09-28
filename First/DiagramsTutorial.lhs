> {-# LANGUAGE NoMonomorphismRestriction #-}
> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE TypeFamilies #-}
> 
> import Diagrams.Prelude
> import Diagrams.Backend.SVG.CmdLine
> 
> myCircle :: Diagram B
> myCircle = circle 1 # fc blue
>                     # lw veryThick
>                     # lc purple
>                     # dashingG [0.2, 0.05] 0
> 
> main = mainWith myCircle

