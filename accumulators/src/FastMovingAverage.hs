{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module FastMovingAverage where

import Clash.Prelude

topEntity ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System InternalOrder ->
  Signal System MovingAverageValue
topEntity = exposeClockResetEnable parser
{-# ANN
  topEntity
  ( Synthesize
      { t_name = "FastMovingAverageTop",
        t_inputs =
          [ PortName "CLK",
            PortName "RST",
            PortName "EN",
            PortName "DIN"
          ],
        t_output = PortName "DOUT"
      }
  )
  #-}
