
{ stdenv, ghcWithPackages, XMonadTheme, lib }:

stdenv.mkDerivation rec {

  pname   = "myXmobars";
  version = "0.1.0";

  buildInputs = [
    ( ghcWithPackages ( hps: with hps; [
        xmobar alsa-mixer
      ] ) )
  ];

  src = ./.;

  installPhase = ''
    mkdir -p $out/bin
    ghc XMobarConf.hs ${XMonadTheme} -threaded -o $out/bin/${pname}
  '';

  meta.license = lib.licenses.gpl3;

}
