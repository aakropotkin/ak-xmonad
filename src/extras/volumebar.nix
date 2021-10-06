
{ stdenv, lib,
, ghcWithPackages
, libGL, libGLU, freeglut
}:
stdenv.mkDerivation rec {

  pname = "volumebar";
  version = "0.1.0";

  buildInputs = [ ( ghcWithPackages ( hps: with hps; [ gloss GLUT ] ) ) ] ++
                [ libGL libGLU freeglut ];
  runtimeInputs = [ libGL libGLU freeglut ];

  src = ./.;

  installPhase = ''
    mkdir -p $out/bin
    ghc VolumeBar.hs          \
      -optl=-L${libGL}/lib    \
      -optl=-L${libGLU}/lib   \
      -optl=-L${freeglut}/lib \
      -o $out/bin/${pname}
  '';

  meta.license = lib.licenses.gpl3;

  # `ldd $(which volumebar)` shows that the 3 libs above do not properly link.

}
