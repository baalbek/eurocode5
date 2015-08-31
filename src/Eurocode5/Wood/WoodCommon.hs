{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards  #-}
module Eurocode5.Wood.WoodCommon where

data WoodCat = SoftWood | HardWood | PVeneer deriving (Eq,Show)

data Wood = 
    Wood {
        t    :: Double, -- ^ Thickness [mm]
        rho  :: Double, -- ^ Karakteristisk densitet [kg/m3] 
        wcat :: WoodCat  
    } deriving Show


woodOf :: String -> Double -> Wood
woodOf "gran" tx = Wood tx 430 SoftWood
woodOf "furu" tx = Wood tx 490 SoftWood
woodOf "bjork" tx = Wood tx 580 SoftWood
woodOf "eik" tx = Wood tx 650 HardWood
woodOf "lind" tx = Wood tx 350 SoftWood

