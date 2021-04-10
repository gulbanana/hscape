with import <nixpkgs> {}; 
with import ./miso.nix;
stdenv.mkDerivation {
  name = "scape";
  src = ./.;
  buildInputs = [release pkgs.closurecompiler pkgs.zopfli];
  buildPhase = ''  
    mkdir $out
    cp ${release}/bin/app.jsexe/* $out/
  '';
  installPhase = ''
    rm $out/index.html
    cp wwwroot/* $out/
    ${pkgs.closurecompiler}/bin/closure-compiler --compilation_level ADVANCED_OPTIMIZATIONS \
      --jscomp_off=checkVars \
      --externs=$out/all.js.externs \
      $out/all.js > $out/all.min.js
    ${pkgs.zopfli}/bin/zopfli $out/all.min.js
  '';
}