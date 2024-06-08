{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}

module YTSafe where

import qualified GHC.TypeLits as N
import GHC.TypeLits
import GHC.Exts (Constraint)
import Data.Type.Bool
import Data.Type.Ord
import Data.Type.Equality (type (==))
import Text.Read.Lex (Number)

-- 形状を表すデータ型(各コンストラクタを型にリフトして使う)
data Shape = D1 Nat | D2 Nat Nat | D3 Nat Nat Nat 

type family ShowShape (shape :: Shape) :: ErrorMessage where
    ShowShape (D1 n) = Text "D1 " :<>: ShowType n
    ShowShape (D2 n m) = Text "D2 " :<>: ShowType n :<>: Text "x" :<>: ShowType m
    ShowShape (D3 n m p) = Text "D3 " :<>: ShowType n :<>: Text "x" :<>: ShowType m :<>: Text "x" :<>: ShowType p

type Axis = Nat


-- 表面言語としてユーザーがモデルを定義する用のLayer定義
data Layer = Input Shape | Flatten Layer | Dense Nat Activation Layer | Conv2D Nat Nat Nat Activation Layer | Conv3D | MaxPooling
    | Add Layer Layer | Concatenate Axis Layer Layer

-- 活性化関数
data Activation = Relu | Sigmoid

-- 形状の等価性判定の定義
type family CheckCompatible (shape1 :: Shape) (shape2 :: Shape) :: Constraint where
    CheckCompatible (D1 x1) (D1 x2) = (x1 ~ x2)
    CheckCompatible (D2 x1 y1) (D2 x2 y2) = (x1 ~ x2, y1 ~ y2)
    CheckCompatible (D3 x1 y1 z1) (D3 x2 y2 z2) = (x1 ~ x2, y1 ~ y2, z1 ~ z2)
    CheckCompatible shape1 shape2 = TypeError (Text "Shapes are not compatible: " :<>: ShowType shape1 :<>: Text " and " :<>: ShowType shape2)


type family Broadcastable (a :: Shape) (b :: Shape) :: Bool where
    Broadcastable ('D1 x) ('D1 y) = (x == y) || (x == 1) || (y == 1)
    Broadcastable ('D2 x1 x2) ('D2 y1 y2) =
        (x1 == y1 || x1 == 1 || y1 == 1) &&
        (x2 == y2 || x2 == 1 || y2 == 1)
    Broadcastable ('D3 x1 x2 x3) ('D3 y1 y2 y3) =
        (x1 == y1 || x1 == 1 || y1 == 1) &&
        (x2 == y2 || x2 == 1 || y2 == 1) &&
        (x3 == y3 || x3 == 1 || y3 == 1)
    Broadcastable ('D1 x) ('D2 y1 y2) = (x == y2) || (x == 1)
    Broadcastable ('D2 x1 x2) ('D1 y) = (x2 == y) || (x2 == 1) || (y == 1)
    Broadcastable ('D1 x) ('D3 y1 y2 y3) = (x == y3) || (x == 1)
    Broadcastable ('D3 x1 x2 x3) ('D1 y) = (x3 == y) || (x3 == 1) || (y == 1)
    Broadcastable ('D2 x1 x2) ('D3 y1 y2 y3) =
        (x1 == y2 || x1 == 1 || y2 == 1) &&
        (x2 == y3 || x2 == 1 || y3 == 1)
    Broadcastable ('D3 x1 x2 x3) ('D2 y1 y2) =
        (x2 == y1 || x2 == 1 || y1 == 1) &&
        (x3 == y2 || x3 == 1 || y2 == 1)
    Broadcastable _ _ = 'False

type family Broadcast (s1 :: Shape) (s2 :: Shape) :: Shape where
    Broadcast ('D1 x) ('D1 y) = 'D1 (Max x y)
    Broadcast ('D2 x1 x2) ('D2 y1 y2) = 'D2 (Max x1 y1) (Max x2 y2)
    Broadcast ('D3 x1 x2 x3) ('D3 y1 y2 y3) = 'D3 (Max x1 y1) (Max x2 y2) (Max x3 y3)
    Broadcast ('D1 x) ('D2 y1 y2) = 'D2 y1 (Max x y2)
    Broadcast ('D2 x1 x2) ('D1 y) = 'D2 x1 (Max x2 y)
    Broadcast ('D1 x) ('D3 y1 y2 y3) = 'D3 y1 y2 (Max x y3)
    Broadcast ('D3 x1 x2 x3) ('D1 y) = 'D3 x1 x2 (Max x3 y)
    Broadcast ('D2 x1 x2) ('D3 y1 y2 y3) = 'D3 y1 (Max x1 y2) (Max x2 y3)
    Broadcast ('D3 x1 x2 x3) ('D2 y1 y2) = 'D3 x1 (Max x2 y1) (Max x3 y2)

type family TryBroadcast (s1 :: Shape) (s2 :: Shape) :: Shape where
    TryBroadcast s1 s2 = If (Broadcastable s1 s2)
                               (Broadcast s1 s2)
                               (TypeError (Text "Cannot broadcast shapes: " :<>: ShowShape s1 :<>: Text " and " :<>: ShowShape s2))




-- Layerの出力形状を計算するType Familyを定義
type family Out (layer :: Layer) :: Shape where
    Out ('Input shape) = shape
    Out ('Flatten layer) = FlattenShape (Out layer)
    Out ('Dense n acv layer) = DenseShape n (Out layer)
    Out ('Conv2D filters kernel_size_x kernel_size_y acv layer) = Conv2DShapes filters kernel_size_x kernel_size_y  acv (Out layer)

    Out ('Add layer1 layer2) = AddShapes (Out layer1) (Out layer2)
    Out ('Concatenate axis layer1 layer2) = ConcatenateShapes axis (Out layer1) (Out layer2)

type family FlattenShape (shape :: Shape) :: Shape where
    FlattenShape ('D1 x) = ('D1 x)
    FlattenShape ('D2 x y) = 'D1 (x N.* y)
    FlattenShape ('D3 x y z) = 'D1 (x N.* y N.* z)
    FlattenShape shape = TypeError (Text "Cannot flatten shape: " :<>: ShowType shape)

type family DenseShape (n :: Nat) (shape :: Shape) :: Shape where
    DenseShape n ('D1 x) = ('D1 n)
    DenseShape n shape = TypeError (Text "Cannot Dense shape: " :<>: ShowType shape)

type family AddShapes (s1 :: Shape) (s2 :: Shape) :: Shape where
    AddShapes s1 s2 = TryBroadcast s1 s2

type family ConcatenateShapes (axis :: Axis) (shape1 :: Shape) (shape2 :: Shape) :: Shape where
    ConcatenateShapes 1 ('D1 x1) ('D1 x2) = 'D1 (x1 + x2)
    ConcatenateShapes 0 ('D1 x1) ('D1 x2) = If (x1 == x2) ('D1 x1) (TypeError (Text "Shapes are not compatible: D1 " :<>: ShowType x1 :<>: Text " and D1 " :<>: ShowType x2))
    ConcatenateShapes 1 ('D2 x1 y1) ('D2 x2 y2) = If (y1 == y2) ('D2 (x1 + x2) y1) (TypeError (Text "Shapes are not compatible: D2 " :<>: ShowType (D2 x1 y1) :<>: Text " and D2 " :<>: ShowType (D2 x2 y2)))
    ConcatenateShapes 2 ('D2 x1 y1) ('D2 x2 y2) = If (x1 == x2) ('D2 x1 (y1 + y2)) (TypeError (Text "Shapes are not compatible: D2 " :<>: ShowType (D2 x1 y1) :<>: Text " and D2 " :<>: ShowType (D2 x2 y2)))
    ConcatenateShapes 1 ('D3 x1 y1 z1) ('D3 x2 y2 z2) = If ((y1 == y2) && (z1 == z2)) ('D3 (x1 + x2) y1 z1) (TypeError (Text "Shapes are not compatible: D3 " :<>: ShowType x1 :<>: Text " and D3 " :<>: ShowType x2))
    ConcatenateShapes 2 ('D3 x1 y1 z1) ('D3 x2 y2 z2) = If ((x1 == x2) && (z1 == z2)) ('D3 x1 (y1 + y2) z1) (TypeError (Text "Shapes are not compatible: " ))
    ConcatenateShapes 3 ('D3 x1 y1 z1) ('D3 x2 y2 z2) = If ((x1 == x2) && (y1 == y2)) ('D3 x1 y1 (z1 + z2)) (TypeError (Text "Shapes are not compatible: " ))
    ConcatenateShapes 0 ('D3 x1 y1 z1) ('D3 x2 y2 z2) = If ((x1 == x2) && (y1 == y2) && (z1 == z2)) ('D3 x1 y1 z1) (TypeError (Text "Shapes are not compatible: " ))
    ConcatenateShapes axis shape1 shape2 = TypeError (Text "Shapes are not compatible: " )

-- Not support padding
type family Conv2DShapes (filters :: Nat) (kernel_size_x :: Nat) (kernel_size_y :: Nat) (acv :: Activation) (shape :: Shape) :: Shape where
    Conv2DShapes filter kernel_size_x  kernel_size_y acv ('D2 x y) = ('D3 (x - kernel_size_x + 1 ) (y - kernel_size_y + 1) filter)
    Conv2DShapes filter kernel_size_x  kernel_size_y acv ('D3 x y z) = ('D3 (x - kernel_size_x + 1 ) (y - kernel_size_y + 1) filter)
    Conv2DShapes filters kernel_size_x kernel_size_y acv shape = TypeError (Text "Shapes are not compatible: " )


main :: IO ()
main = do
    putStrLn "Model successfully created with type-level shape checking."