# TensorSafeDAG

まず一番下の[Installation](#installation)を読むこと。

## TensorSafeの不完全な形状一致検査の再現
`./Invalid/invalid_keras.py`のモデルは不正で、長さ6のベクトルと3x2の行列を加算レイヤーの入力に与えている。
TensorSafeDAGで書くと以下の型定義である。TensorSafeDAGではこの形状不一致を正しく捕捉する。
```haskell
type Input = Input ('D2 3 2)
type L1 = Flatten Input
type Output = 'Add L1 Input -- error!

invalid :: MkNetwork Output ('D1 6)
invalid = MkNetwork
```

Kerasでも、対応する深層学習モデル`./Invalid/invalid_keras.py`を実行すると以下の形状不一致エラーが出力される。
```sh
$ python3 ./Invalid/invalid_keras.py
ValueError: Inputs have incompatible shapes. Received shapes (3, 2) and (6,)
```
しかし、まったく同じモデルに対し、TensorSafeでは形状不一致エラーを検出できない。
```sh
$ stack run ./Invalid/InvalidTs.hs
Model successfully created with type-level shape checking
```
これはTensorSafeでは以下の2箇所でしか形状検査をしていないのが理由。
1. `MkINetwork`の第一引数のリスト内の連続するレイヤー間
2. モデルの出力テンソルの形状と与えられた期待テンソル形状

上記のプログラムでは2.の検査しかしておらず、`Add`の引数を使った形状一致検査については何もしていない。

## Installation
The TensorSafeDAG is implemented in Haskell, and the latest version of GHC is recommended.
Please install the necessary packages using the steps below.

```
# TensorSafeDAG dependencies
TensorSafeDAG
└── GHC (latest)
```

### Installation
Install the packages necessary for building GHC (including libGMP, libtinfo, and gcc).
```bash
--- Ubuntu
sudo apt update
sudo apt install libgmp-dev libtinfo-dev build-essential

-- Mac
brew update && brew install gmp ncurses
xcode-select --install
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
