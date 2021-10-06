
# ghc862, ghc861, ghcHEAD, 844, 822
{ nixpkgs    ? import <nixpkgs> { }
, compiler   ? "ghc863"
, useDefault ? true
}:

let inherit (nixpkgs) pkgs; 

  hpkgs = if useDefault
          then pkgs.haskellPackages
          else pkgs.haskell.packages.${compiler};
  hp = hps: with hps; [
    aeson base lens split wreq alex happy directory
    #gutenhasktags
    hasktags hindent hlint pretty-simple pointfree
    http-client http-client-tls http-types
    # Haskell Packages go here.
    xmobar
  ];
  myGHC = hpkgs.ghcWithPackages hp;

in

  pkgs.stdenv.mkDerivation {
    name = "my-haskell-environment";
    buildInputs = [myGHC] ++ (hp hpkgs) ++ (with pkgs; [
      gnumake
      # Other build Dependencies go here.
    ]);
  shellHook = ''
    eval $(egrep ^export ${myGHC}/bin/ghc)
    ghc --version
  '';

}
