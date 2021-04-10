setup
-----
1. get nix 
    left as an exercise to the reader, but this worked for me
    * WSL_1_, ubuntu userland 20.04, nix 2.3.10
    * you may or may not need the sqlite WAL patches described in nix installation guides, i've got by without them to date
    * gpg verification requires --keyserver hkps://keyserver.ubuntu.com before --recv-keys
    * do not supply --daemon to sh install-nix; do not sudo

2. global installs which should probably be part of an environment or something
    `nix-env -iA cachix -f https://cachix.org/api/v1/install`
    `nix-env -iA cabal-install -f '<nixpkgs>'`

3. miso binary cache (*probably* works in WSL)
    `cachix use miso-haskell`

development
-----------
`nix-build -A dev` builds with GHC and JSaddle

`nix-shell --run reload` will use the alias defined in `shell.nix` to hot-reload using GHCID

for working HLS in vscode, run in nix-shell or use Nix Environment Selector and pick `shell.nix`

release build
-------------
`nix-build -A release` builds with GHCJS

`result/` is a symlink into the nix store! `wslview` inside it to get your output
