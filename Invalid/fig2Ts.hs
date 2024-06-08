{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}

module InvalidTs (main) where

import TensorSafe
import TensorSafe.Layers
import TensorSafe.Network
import TensorSafe.Shape

type MyModel =
    MkINetwork
    '[ Flatten
     , Add [Input, Dense 784 10, Relu] '[Dense 784 9, Sigmoid]
     ]
    ('D3 28 28 1)    -- Input
    ('D1 10)      -- Output

resnet50 :: MyModel
resnet50 = mkINetwork

main :: IO ()
main = putStrLn "Success!"

-- これは形状不一致を含むプログラムのはずだが、TensorSafeでは形状不一致エラーを検出しない。
-- $ stack runghc ./Invalid/fig2Ts.hs
-- Success!
