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
    arcs= [arc  (direction (1 *^ e (d@@rad)))  ((3*pi/5)@@rad) # showOrigin # scale 1
          | d <- [0,p..]]
    p = tau/5
    s = sqrt ((5-sqrt 5) / 2)
    arccs = zipWith (\a c -> a # lc c) arcs [black,purple,green,blue,red,orange]

planetOfLife :: Diagram B
planetOfLife = foldr copy mempty $ take 2 
    [(0, 0)
    ,(1.6882, tau/10)
    ,(2, 2*(tau/5)) 
    ,(2, 3*(tau/5)) 
    ,(2, 4*(tau/5)) 
    ]
    where
    copy :: (Double,Double) -> Diagram B -> Diagram B
    copy (l,a) d = d `atop` flowerOfLife # translate (l *^ e (a @@ rad)) # rotate (a@@rad) # scale 1.2

-- concat $ zipWith (\d pt -> d # translate pt) (repeat [flowerOfLife]) 
    -- [r2 (1,1), r2 (2,2), r2 (3,4)]

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

main = mainWith $ planetOfLife
