# TensorSafeDAG

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