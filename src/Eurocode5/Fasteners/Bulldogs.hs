{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards  #-}
module Eurocode5.Fasteners.Bulldogs where

import qualified Eurocode5.Wood.WoodCommon as WC

data BDCat = C1 | C2 | C3 | C4 | C5 | C6 | C7 | C8 | C9 | C10 | C11  deriving (Eq,Show)

-- ^ Ensidig: C2,C4,C7,C9,C11
-- ^ Tosidig: C1,C1,C3,C5,C6,C8,C10
--
data Bulldog = 
    -- | Typene C1,C2,C6,C7,C10,C11
    Bulldog {
        bcat :: BDCat,       -- ^ Bulldog type 
        dc :: Double,      -- ^ Diameter bulldog [mm]
        d  :: Double,      -- ^ Diameter bolt [mm]
        he :: Double      -- ^ Tennenes inntrengningsdybde [mm]
        -- t1 :: WC.Wood,      -- ^ (Tykkelse) ytre trevirke [mm]
        -- t2 :: Maybe WC.Wood -- ^ (Tykkelse) indre/midterste trevirke [mm]
    } deriving Show

k1 :: Bulldog 
      -> WC.Wood        -- ^ (Tykkelse) ytre trevirke [mm]
      -> Maybe WC.Wood  -- ^ (Tykkelse) indre/midterste trevirke [mm]
      -> Double
k1 Bulldog { he } t1 t2 = 
    case t2 of Just t2' ->  minimum [1.0, (WC.t t1)/(3*he), (WC.t t2')/(5*he)]
               Nothing  ->  minimum [1.0, (WC.t t1)/(3*he)]

a3t :: Bulldog -> Double
a3t Bulldog { bcat,dc,d } = maximum [80, dcf*dc, 7*d ]
    where dcf | bcat == C10 = 1.5 
              | bcat == C11 = 1.5 
              | otherwise = 1.1 
    
k2 :: Bulldog -> Double
k2 b = minimum [1.0, (a3t b)/(dcf*(dc b))]
    where bt' = bcat b
          dcf | bt' == C10 = 2.0
              | bt' == C11 = 2.0
              | otherwise = 1.5
              
k3 :: WC.Wood -> Double
k3 w = min 1.5 ((WC.rho w)/350.0)

checkT :: Bulldog 
          -> WC.Wood   
          -> Bool
checkT b w = (WC.t w)  > 2.25 * (he b)

fvrk :: Bulldog
        -> WC.Wood       -- ^ Ytre trevirke
        -> Maybe WC.Wood -- ^ Indre/midterste trevirke
        -> Double        -- ^ Capacity bulldog [kN]
fvrk b w1 w2 = fk*k1'*(k2 b)*k3'*(dc'**1.5)/1000.0                -- fk*(k11 b w)*(k2 b)*(k3 w)*(dc'**1.5)/1000.0
    where bt' = bcat b
          dc' = dc b
          fk | bt' == C10 = 25
             | bt' == C11 = 25
             | otherwise = 18
          k1' = k1 b w1 w2
          k3' = case w2 of Just w2' -> min (k3 w1) (k3 w2')
                           Nothing -> k3 w1 


{-
fvrk21 :: Bulldog 
          -> WC.Wood 
          -> Double  -- ^ Capacity bulldog [kN]
fvrk21 b w = fk*(k11 b w)*(k2 b)*(k3 w)*(dc'**1.5)/1000.0
    where bt' = bcat b
          dc' = dc b
          fk | bt' == C10 = 25
             | bt' == C11 = 25
             | otherwise = 18

fvrk22 :: Bulldog 
          -> WC.Wood 
          -> WC.Wood 
          -> Double  -- ^ Capacity bulldog [kN]
fvrk22 b w1 w2 = fk*(k12 b w1 w2)*(k2 b)*(k3 w)*(dc'**1.5)/1000.0
    where bt' = bcat b
          dc' = dc b
          fk | bt' == C10 = 25
             | bt' == C11 = 25
             | otherwise = 18

fvrk :: Bulldog
        -> WC.Wood 
        -> Maybe WC.Wood 
        -> Double -- ^ Capacity bulldog [kN]
fvrk b t1 t2 = 
    case t2 of Just t2' -> min (fvrk2 b t1) (fvrk2 b t2')
               Nothing  -> fvrk2 b t1 
-}
