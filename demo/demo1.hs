{-# LANGUAGE CPP,NamedFieldPuns,RecordWildCards  #-}

import qualified Eurocode5.Fasteners.Bolts as B
import qualified Eurocode5.Fasteners.Bulldogs as BD
import qualified Eurocode5.Wood.WoodCommon as WC

wood = WC.woodOf "gran" 48

bull = BD.Bulldog BD.C2 60 20 9 48 Nothing 

bolt = B.Bolt 5 






