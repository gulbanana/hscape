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
`nix-shell` defines aliases `reload` for hot-reload serving and `output` for easy access to the build products

for working HLS in vscode, run in nix-shell or use Nix Environment Selector and pick `shell.nix`

deployment
----------
`nix-build` builds with GHCJS and performs post-processing