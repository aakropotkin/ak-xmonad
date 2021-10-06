self: super:

let

    myXmonadDeps = hpkgs: with hpkgs; [
      # These must be identical to the modules listed in .cabal
      X11
      base
      containers
      directory
      lens
      neat-interpolation
      pretty-simple
      prizm
      text
      xmobar
      xmonad
      xmonad-contrib
      xmonad-extras
      xmonad-spotify
    ];

    xmonad-configuredDeps = hpkgs: with hpkgs; [
      alsa-mixer
      colour
      neat-interpolation
      xmonad
      xmonad-contrib
      xmonad-extras
      xmonad-spotify
    ];

in {

  # The point of doing this here and in `home.nix` is for Nix-Shell
  xmonad-configured = super.xmonad-with-packages.override {
    packages = xmonad-configuredDeps;
  };

  myXmonad = super.xmonad-with-packages.override {
    packages = myXmonadDeps;
  };

  myXmonadLib = super.haskellPackages.mkDerivation rec {
    pname = "myXmonad";
    version = "0.0.1";
    src = ./.;
    isLibrary = true;
    isExecutable = false;
    executableHaskellDepends = myXmonadDeps super.haskellPackages;
    license = super.lib.licenses.gpl3;
  };

  XMonadTheme = super.callPackage ./src/theme {};

  myXmobars = super.callPackage ./src/extras/myXmobars.nix
    { ghcWithPackages = super.haskellPackages.ghcWithPackages; };

  volumebar = super.callPackage ./src/extras/volumebar.nix
    { ghcWithPackages = super.haskellPackages.ghcWithPackages; };

  allXmonadDeps = hpkgs: (myXmonadDeps hpkgs) ++ (xmonad-configuredDeps hpkgs);

}
