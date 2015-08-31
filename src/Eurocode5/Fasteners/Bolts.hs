{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards  #-}
module Eurocode5.Fasteners.Bolts where

import qualified Eurocode5.Wood.WoodCommon as WC

data Bolt =
    Bolt {
        diam :: Double   -- ^ Diameter of bolt [mm]
    } deriving Show


type Degrees = Double
type Radians = Double

toRadians :: Degrees -> Radians
toRadians d = (d/180.0) * pi 

toDegrees :: Radians -> Degrees 
toDegrees r = (r/pi) * 180.0

k90 :: Bolt -> WC.Wood -> Double
k90 Bolt { diam } WC.Wood { wcat } = wcf + (0.015*diam)
    where wcf | wcat == WC.SoftWood = 1.35
              | wcat == WC.HardWood = 0.9
              | otherwise = 1.3

-- | Characteristic capacity of bolt against wood 
-- | in the fiber direction
fh0k :: Bolt 
        -> WC.Wood 
        -> Double   -- ^ [N/mm2]
fh0k Bolt { diam } WC.Wood { rho } = 0.082 * (1.0 - (0.01*diam)) * rho 

-- | Characteristic capacity of bolt against wood 
-- | at an angle between force and fiber direction 
fhak :: Bolt 
        -> WC.Wood 
        -> Double   -- ^ Angle between force and fiber direction [degrees]
        -> Double   -- ^ [N/mm2]
fhak b w ang = (fh0k b w) / ((k90'*s) + c)
    where k90' = k90 b w
          rads = toRadians ang
          s = (sin rads)**2.0
          c = (cos rads)**2.0

holeEdgePressure :: Bolt
                    -> WC.Wood
                    -> Double   -- ^ Angle between force and fiber direction [degrees]
                    -> Double   -- ^ [kN]
holeEdgePressure b w ang = (fhak b w ang) * (diam b) * (WC.t w) / 1000.0
