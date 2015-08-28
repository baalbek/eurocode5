{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards  #-}
module Eurocode5.Wood.WoodCommon where

data Wood = 
    Wood {
        rho :: Double -- ^ Karakteristisk densitet [kg/m3] 
    } deriving Show


woodOf :: String -> Wood
woodOf "gran" = Wood 430
woodOf "furu" = Wood 490
woodOf "bjork" = Wood 580
woodOf "eik" = Wood 650
woodOf "lind" = Wood 350

