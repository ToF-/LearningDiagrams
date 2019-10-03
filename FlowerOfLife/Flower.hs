{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.TwoD.Vector
import Diagrams.TwoD.Arc

flowerOfLife :: Diagram B
flowerOfLife = atPoints (fromOffsets pts) (take 5 arccs)
    where
    pts :: [V2 Double]
    pts = [1 *^ e (a @@rad) | a <- [0,tau/5..tau]]
    arcs :: [Diagram B]
    arcs= [arc (direction (1 *^ e (d @@ rad))) ((3*pi/5) @@ rad) 
          | d <- [0,tau/5..]]
    arccs = zipWith (\a c -> a # lc c) arcs [black,purple,green,blue,red,orange]

flowerOfElements :: Diagram B
flowerOfElements = atPoints (fromOffsets pts) (take 4 arccs)
    where
    pts :: [V2 Double]
    pts = [1 *^ e (a @@rad) | a <- [0,tau/4..tau]]
    arcs :: [Diagram B]
    arcs= [arc (direction (1 *^ e (d @@ rad))) ((2*pi/4) @@ rad) 
          | d <- [0,tau/4..]]
    arccs = zipWith (\a c -> a # lc c) arcs [black,purple,green,blue,red,orange]

flowerOfCreation :: Diagram B
flowerOfCreation = atPoints (fromOffsets pts) (take 6 arccs)
    where
    pts :: [V2 Double]
    pts = [1 *^ e (a @@rad) | a <- [0,tau/6..tau]]
    arcs :: [Diagram B]
    arcs= [arc (direction (1 *^ e (d @@ rad))) ((4*pi/6) @@ rad) 
          | d <- [0,tau/6..]]
    arccs = zipWith (\a c -> a # lc c) arcs [black,purple,green,blue,red,orange,brown]

flowerOfIntelligence :: Diagram B
flowerOfIntelligence = atPoints (fromOffsets pts) (take 7 arccs)
    where
    pts :: [V2 Double]
    pts = [1 *^ e (a @@rad) | a <- [0,tau/7..tau]]
    arcs :: [Diagram B]
    arcs= [arc (direction (1 *^ e (d @@ rad))) ((5*pi/7) @@ rad) 
          | d <- [0,tau/7..]]
    arccs = zipWith (\a c -> a # lc c) arcs [black,purple,green,blue,red,orange,brown]
main = mainWith $ flowerOfLife === flowerOfCreation === flowerOfIntelligence
