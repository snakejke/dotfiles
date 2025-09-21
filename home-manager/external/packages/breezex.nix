{ stdenv, lib, sources, ... }:

stdenv.mkDerivation {
  pname = "BreezeX-Cursors";
  version = "2.0.1";
  srcs = [ sources.BreezeX_Black.src sources.BreezeX_Dark.src ];

  dontUnpack = true;
  dontBuild = true;

  installPhase = ''
    mkdir -p $out/share/icons/BreezeX/Black
    mkdir -p $out/share/icons/BreezeX/Dark

    for f in $srcs; do
      case "$f" in
        *Black*) tar -xf $f -C $out/share/icons/BreezeX/Black ;;
        *Dark*)  tar -xf $f -C $out/share/icons/BreezeX/Dark ;;
      esac
    done
  '';

  meta = with lib; {
    description = "BreezeX cursor themes";
    homepage = "https://github.com/ful1e5/BreezeX_Cursor";
    license = licenses.mit;
    maintainers = [];
    platforms = [ "x86_64-linux" ];
  };
}
