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
        he :: Double,      -- ^ Tennenes inntrengningsdybde [mm]
        t1 :: Double,      -- ^ Tykkelse ytre trevirke [mm]
        t2 :: Maybe Double -- ^ Tykkelse indre/midterste trevirke [mm]
    } deriving Show

k1 :: Bulldog -> Double
k1 Bulldog { t1,t2,he } = 
    case t2 of Just t2' ->  minimum [1.0, t1/(3*he), t2'/(5*he)]
               Nothing  ->  minimum [1.0, t1/(3*he)]

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
          -> Bool
checkT b = (t1 b) > 2.25 * (he b)

fvrk :: Bulldog 
        -> WC.Wood 
        -> Double  -- ^ Capacity bulldog [kN]
fvrk b w = fk*(k1 b)*(k2 b)*(k3 w)*(dc'**1.5)/1000.0
    where bt' = bcat b
          dc' = dc b
          fk | bt' == C10 = 25
             | bt' == C11 = 25
             | otherwise = 18

