{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

import YTSafe

-- レイヤー型定義と期待される形状を受取、形状一致検査
-- 以下の何方かのケースで静的に形状不一致が検出
-- 1. 各レイヤーの形状計算のためのtype family (AddShapeなど)の型展開でパターンマッチに失敗
-- 2. モデルの計算された出力形状が、MkModelの第二引数の期待形状と不一致 (CheckCompatile型制約に違反)
data MkNetwork (layer :: Layer) (outputShape :: Shape) where
    MkNetwork :: (Out layer ~ outputShape) => MkNetwork layer outputShape


validModelFlatten :: MkNetwork ('Flatten ('Input ('D3 2 3 1))) ('D1 6)
validModelFlatten = MkNetwork

-- invalidModelFlatten :: MkNetwork ('Flatten ('Input ('D3 2 3 1))) ('D1 8)
-- invalidModelFlatten = MkNetwork

validAddModel :: MkNetwork ('Add ('Input ('D2 2 3)) ('Input ('D2 2 3))) ('D2 2 3)
validAddModel = MkNetwork

-- type Input = 'Input ('D3 28 28 1)
-- type Layer1 = 'Flatten Input
-- type Layer2 = 'Dense 10 Relu Layer1
-- type Layer3 = 'Dense 9 Sigmoid Layer1
-- type Layer4 = 'Add Layer2 Layer3
-- type MyModel = MkNetwork Layer4 ('D1 10)

-- invalidModel1 :: MyModel 
-- invalidModel1 = MkNetwork



-- invalidAddModel :: MkNetwork ('Add ('Input ('D2 2 3)) ('Input ('D2 3 2))) ('D2 2 3)
-- invalidAddModel = MkNetwork

-- type Source = 'Input ('D1 1)
-- type Input1 = 'Input ('D2 2 3)
-- type Input2= 'Input ('D2 3 2)
-- type Flatten1 = 'Flatten Source
-- type Flatten2 = 'Flatten Source
-- type Dense1 = 'Dense 10 Relu Flatten1
-- type Junction = 'Add Input1 Input2

-- example1 :: MkNetwork Junction ('D1 10)
-- example1 = MkNetwork


