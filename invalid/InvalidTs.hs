{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}

module InvalidTs (main) where

import TensorSafe
import TensorSafe.Layers
import TensorSafe.Network
import TensorSafe.Shape

type MyModel =
    MkINetwork
    '[ Add [Input, Flatten] '[Input]]
    ('D2 3 2)    -- Input
    ('D1 6)      -- Output

resnet50 :: MyModel
resnet50 = mkINetwork

main :: IO ()
main = putStrLn "Success!"

-- これは形状不一致を含むプログラムのはずだが、TensorSafeでは形状不一致エラーを検出しない。
-- $ stack runghc ./invalid/InvalidTs.hs
-- Success!
