{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.TwoD.Vector

example1 :: Diagram B
example1 = fromOffsets . map (\a -> 1 *^ e (a @@ deg)) $ take 10 $ cycle [45,-45]

example2 :: Diagram B
example2 = position (zip pts (repeat c))

c = circle 1 # fc blue 
pts = map mkPoint $ take 7 $ iterate (+ (pi/7)) 0
    where
    mkPoint x = p2 (5*sin x,5*cos x)

example3 :: Diagram B
-- example3 = mconcat . map fromOffsets $ [ [r *^ e (r @@ rad)] | r <- ]

example3 = mconcat . map fromOffsets $ [ [(1 + fromIntegral (n `mod` 3)) *^ e ((tau/30 +(fromIntegral n) * (tau/30)) @@ rad)] | n <- [0..29]]

example4 :: Diagram B
example4 = vTriangle (unitX # scale 2) (unitX # rotateBy (1/8) ) 

vTriangle :: V2 Double -> V2 Double -> Diagram B
vTriangle a b = mconcat
    [drawV a
    ,drawV b
    ,drawV c # translateX x
    ]
    where c = negate (a ^+^ negate b) 
          (x,_) = unr2 a

drawV = fromOffsets . return

dash = dashingG [0.1,0.1] 0

example5 :: Diagram B
example5 = mconcat 
    [fromOffsets [a] # lc blue
    ,fromOffsets [b] # lc red
    ,fromOffsets [a ^+^ b] # lc purple
    ,fromOffsets [a] # lc blue # dash # translate b
    ,fromOffsets [b] # lc red # dash # translate a
    ]
    where
    a = unitX # scale 2 # rotateBy (1/20)
    b = unitX # rotateBy (1/8) 
    c = a ^+^ b
    d = a ^* 2 
    e = b # translate a

main = mainWith (example5)
