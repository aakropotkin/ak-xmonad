with (import <nixpkgs> { config.allowUnfree = true; });
let
  hpkgs = haskell.packages.ghc865.extend
    (hself: hsuper: { inherit myXmonadLib; });
  myGHC = hpkgs.ghcWithPackages (hps: (pkgs.allXmonadDeps hps)
                                      ++ [hps.myXmonadLib]);
in haskell.lib.buildStackProject {
  ghc = myGHC;
  name = "myXmonad-Env";
  buildInputs = [
    myGHC
    xorg.libX11
    xorg.libXinerama
    xorg.libXext
    xorg.libXrandr
    xorg.libXScrnSaver
    zlib.dev
    hpkgs.myXmonadLib
  ] ++ (pkgs.allXmonadDeps hpkgs);
}
