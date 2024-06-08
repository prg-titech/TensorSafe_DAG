{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}

module InvalidTs (main) where

import TensorSafe
import TensorSafe.Layers
import TensorSafe.Network
import TensorSafe.Shape

type MyModel =
    MkINetwork
    '[ Add [Input, Flatten] '[Input], Relu]
    ('D2 3 2)    -- Input
    ('D1 6)      -- Output

resnet50 :: MyModel
resnet50 = mkINetwork

main :: IO ()
main = putStrLn "Success!"

-- これは形状不一致を含むプログラムのはずだが、TensorSafeでは形状不一致エラーを検出しない。
-- $ stack run ./invalid/invalidcInvalidTs.hs
-- Model successfully created with type-level shape checking.

-- これはTensorSafeでは以下の2箇所でしか形状検査をしていないのが理由。
-- 1. MkINetworkの第一引数のリスト内の連続するレイヤー間
-- 2. モデルの出力テンソルの形状が期待と一致しているか
-- ↑のプログラムでは2.の検査しかしておらず、Addの引数に関しては何もしていない