{ stdenv, makeWrapper, writeShellScript, lib, sources, ... }:

let
  palantirScript = writeShellScript "palantir-cli" ''
    exec java -jar @out@/share/java/palantir-cli.jar "$@"
  '';
  
  googleJavaFormatScript = writeShellScript "google-java-format" ''
    exec java -jar @out@/share/java/palantir-cli.jar "$@"
  '';
in
stdenv.mkDerivation {
  pname = sources.palantir-cli.pname;
  version = sources.palantir-cli.version;
  src = sources.palantir-cli.src;
  
  dontUnpack = true;
  dontBuild = true;
  
  installPhase = ''
    mkdir -p $out/bin $out/share/java
    
    cp ${sources.palantir-cli.src} $out/share/java/palantir-cli.jar
    
    cp ${palantirScript} $out/bin/palantir-cli
    cp ${googleJavaFormatScript} $out/bin/google-java-format
    
    substituteInPlace $out/bin/palantir-cli \
      --replace "@out@" "$out"
    substituteInPlace $out/bin/google-java-format \
      --replace "@out@" "$out"
    
    chmod +x $out/bin/palantir-cli
    chmod +x $out/bin/google-java-format
  '';
  
  meta = {
    description = "Palantir Java code formatter (uses system Java via PATH)";
    homepage = "https://github.com/jsonschema2dataclass/palantir-cli";
    platforms = lib.platforms.all;
  };
}
