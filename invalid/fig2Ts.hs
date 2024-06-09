{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}

module InvalidTs (main) where

import TensorSafe
import TensorSafe.Layers
import TensorSafe.Network
import TensorSafe.Shape

type MyModel = MkINetwork
  '[ Flatten
   , Add '[ Input
          , Dense 784 10
          , Relu ]
         '[ Input
          , Dense 784 9
          , Sigmoid ]
   ]
  ('D3 28 28 1)
  ('D1 10)

invalid :: MyModel
invalid = mkINetwork

main :: IO ()
main = putStrLn "Success!"

-- これは形状不一致を含むプログラムのはずだが、TensorSafeでは形状不一致エラーを検出しない。
-- $ stack runghc ./invalid/fig2Ts.hs
-- Success!
