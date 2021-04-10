with import <nixpkgs> {}; 
with import ./miso.nix;
stdenv.mkDerivation {
  name = "scape";
  src = ./.;
  buildInputs = [release];
  buildPhase = ''  
    mkdir $out
    cp ${release}/bin/app.jsexe/* $out/
  '';
  installPhase = ''
    cp wwwroot/* $out/
  '';
}