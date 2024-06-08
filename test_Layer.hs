{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

import YTSafe

data EqualShape (s1 :: Shape) (s2 :: Shape) where
    EqualShape :: (s1 ~ s2) => EqualShape s1 s2

-- Test of Flatten Layer
testFlatten1 :: MkNetwork ('Flatten ('Input ('D1 10))) ('D1 10)
testFlatten1 = MkNetwork
testFlatten2 :: MkNetwork ('Flatten ('Input ('D1 100))) ('D1 10)
testFlatten2 = MkNetwork -- error
testFlatten3 :: MkNetwork ('Flatten ('Input ('D2 2 3))) ('D1 6)
testFlatten3 = MkNetwork
testFlatten4 :: MkNetwork ('Flatten ('Input ('D2 3 4))) ('D1 10)
testFlatten4 = MkNetwork -- error
testFlatten5 :: MkNetwork ('Flatten ('Input ('D3 2 3 4))) ('D1 24)
testFlatten5 = MkNetwork
testFlatten6 :: MkNetwork ('Flatten ('Input ('D3 3 4 5))) ('D1 10)
testFlatten6 = MkNetwork -- error


-- Test of Dense Layer
testDense1 :: MkNetwork ('Dense 10 Relu ('Input ('D1 100))) ('D1 10)
testDense1 = MkNetwork
testDense2 :: MkNetwork ('Dense 20 Relu ('Input ('D1 100))) ('D1 10)
testDense2 = MkNetwork -- error
testDense3 :: MkNetwork ('Dense 10 Relu ('Input ('D2 2 3))) ('D1 10)
testDense3 = MkNetwork -- error
testDense4 :: MkNetwork ('Dense 10 Relu ('Input ('D3 2 3 4))) ('D1 10)
testDense4 = MkNetwork -- error


-- Test of Concatenate Layer

-- Test of Conv2D Layer
