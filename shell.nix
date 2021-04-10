with (import ./miso.nix);
let
  reload-script = pkgs.writeScriptBin "reload" ''
      ${pkgs.haskell.packages.ghc865.ghcid}/bin/ghcid -c \
        '${pkgs.haskell.packages.ghc865.cabal-install}/bin/cabal new-repl exe:app' \
        -T 'Main.main'
'';
  output-script = pkgs.writeScriptBin "output" ''
      cd result && wslview . && cd ..
'';
in dev.env.overrideAttrs (old: {
  buildInputs = old.buildInputs ++ [reload-script] ++ [output-script];
})
