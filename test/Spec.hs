{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

import TensorSafeDAG

-------------
-- Flatten --
-------------

-- Truthy Test
testFlatten1 :: MkNetwork ('Flatten ('Input ('D1 10))) ('D1 10)
testFlatten1 = MkNetwork
testFlatten3 :: MkNetwork ('Flatten ('Input ('D2 2 3))) ('D1 6)
testFlatten3 = MkNetwork
testFlatten5 :: MkNetwork ('Flatten ('Input ('D3 2 3 4))) ('D1 24)
testFlatten5 = MkNetwork

-- Fathy Test
-- testFlatten2 :: MkNetwork ('Flatten ('Input ('D1 100))) ('D1 10)
-- testFlatten2 = MkNetwork -- error
-- testFlatten4 :: MkNetwork ('Flatten ('Input ('D2 3 4))) ('D1 10)
-- testFlatten4 = MkNetwork -- error
-- testFlatten6 :: MkNetwork ('Flatten ('Input ('D3 3 4 5))) ('D1 10)
-- testFlatten6 = MkNetwork -- error

-----------
-- Dense --
-----------

-- Truthy test
testDense1 :: MkNetwork ('Dense 10 Relu ('Input ('D1 100))) ('D1 10)
testDense1 = MkNetwork

-- Falthy Test
-- testDense2 :: MkNetwork ('Dense 20 Relu ('Input ('D1 100))) ('D1 10)
-- testDense2 = MkNetwork -- error
-- testDense3 :: MkNetwork ('Dense 10 Relu ('Input ('D2 2 3))) ('D1 10)
-- testDense3 = MkNetwork -- error
-- testDense4 :: MkNetwork ('Dense 10 Relu ('Input ('D3 2 3 4))) ('D1 10)
-- testDense4 = MkNetwork -- error


------------
-- Conv2D --
------------

------------
-- Conv3D --
------------

----------------
-- MaxPooling --
----------------

---------
-- Add --
---------

-- Truthy test from [セマンティクスのブロードキャスト](https://www.tensorflow.org/xla/broadcasting?hl=ja)
type Test1 = MkNetwork ('Add ('Input ('D1 3))     ('Input ('D1 3)))     ('D1 3)
type Test2 = MkNetwork ('Add ('Input ('D2 2 3))   ('Input ('D1 3)))     ('D2 2 3)
type Test3 = MkNetwork ('Add ('Input ('D3 4 3 2)) ('Input ('D2 3 2)))   ('D3 4 3 2)
type Test4 = MkNetwork ('Add ('Input ('D3 4 1 2)) ('Input ('D2 3 2)))   ('D3 4 3 2)
type Test5 = MkNetwork ('Add ('Input ('D3 4 3 2)) ('Input ('D3 1 3 2))) ('D3 4 3 2)
type Test6 = MkNetwork ('Add ('Input ('D2 1 3))   ('Input ('D2 2 3)))   ('D2 2 3)
type Test7 = MkNetwork ('Add ('Input ('D1 1))     ('Input ('D3 3 2 1))) ('D3 3 2 1)
type Test8 = MkNetwork ('Add ('Input ('D1 1))     ('Input ('D2 1 2)))   ('D2 1 2)
type Invalid = MkNetwork ('Add ('Flatten ('Input ('D2 3 2))) ('Input ('D2 3 2))) ('D1 6)

valid1 :: Test1
valid1 = MkNetwork
valid2 :: Test2
valid2 = MkNetwork
valid3 :: Test3
valid3 = MkNetwork
valid4 :: Test4
valid4 = MkNetwork
valid5 :: Test5
valid5 = MkNetwork
valid6 :: Test6
valid6 = MkNetwork
valid7 :: Test7
valid7 = MkNetwork
valid8 :: Test8
valid8 = MkNetwork
invalid :: Invalid
invalid = MkNetwork

-- Falthy Test


-----------------
-- Concatenate --
-----------------


------------------
-- Mixed Layers --
------------------

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

main :: IO ()
main = putStrLn "Success!"