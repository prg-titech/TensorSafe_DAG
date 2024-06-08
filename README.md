# TensorSafeDAG

## TensorSafeの不完全な形状一致検査の再現
`./invalid/invalid_keras.py`のモデルは不正で、長さ6のベクトルと3x2の行列をAddレイヤーの入力に与えている。
TensorSafeDAGで書くと以下
```haskell
type Input = Input ('D2 3 2)
type L1 = Flatten Input
type Output = 'Add L1 Input -- error!

invalid :: MkNetwork Output ('D1 6)
invalid = MkNetwork
```

実際、これに対応する深層学習モデル`./invalid/invalid_keras.py`をkerasで実行すると以下の形状不一致エラーが出る
```sh
$ python3 ./invalid/invalid_keras.py
Error during model creation:
Inputs have incompatible shapes. Received shapes (3, 2) and (6,)
```
しかし、まったく同じモデルがTensorSafeでは形状不一致エラーを検出しない。
```sh
$ stack run ./invalid/invalidcInvalidTs.hs
Model successfully created with type-level shape checking
```
これはTensorSafeでは以下の2箇所でしか形状検査をしていないのが理由。
1. MkINetworkの第一引数のリスト内の連続するレイヤー間
2. モデルの出力テンソルの形状が期待と一致しているか
↑のプログラムでは2.の検査しかしておらず、Addの引数を使った形状一致検査については何もしていない。

## Installation
The TensorSafeDAG is implemented in Haskell, and the latest version of GHC is recommended.
Please install the necessary packages using the steps below.

```
# TensorSafeDAG dependencies
TensorSafeDAG
└── GHC (latest)
```

### Install (on Ubuntu 22.04)
Install the packages necessary for building GHC (including libGMP, libtinfo, and gcc).
```bash
sudo apt update
sudo apt install libgmp-dev libtinfo-dev build-essential
```

Install GHC, stack, and cabal via [GHCup](https://www.haskell.org/ghcup/#).
```bash
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
source ~/.bashrc # enabling ghcup
```

Switch version of GHC on GHCup tui.
```bash
ghcup tui
# Move the cursor to the GHC tab on the GHCup GUI.
# Press 'a' to display past versions.
# Press 'i' to Install the latest GHC version
✔✔ GHC x.y.z
```

## Run
```bash
stack run
...
Model successfully created with type-level shape checking.
```

## Test
```bash
stack test
...
Success!
```
