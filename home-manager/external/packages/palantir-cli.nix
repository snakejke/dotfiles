{ stdenv, makeWrapper, lib, sources, ... }:

stdenv.mkDerivation {
  pname = sources.palantir-cli.pname;
  version = sources.palantir-cli.version;
  src = sources.palantir-cli.src;
  
  nativeBuildInputs = [ makeWrapper ];
  
  dontUnpack = true;
  dontBuild = true;
  
  installPhase = ''
    mkdir -p $out/bin $out/share/java
    cp ${sources.palantir-cli.src} $out/share/java/palantir-cli.jar
    
    # Находим системную java (если есть в PATH при сборке)
    # Или используем простой подход - makeWrapper прямо с java
    makeWrapper ${stdenv.shell} $out/bin/palantir-cli \
      --add-flags "-c" \
      --add-flags "java -jar $out/share/java/palantir-cli.jar \"\$@\""
      
    makeWrapper ${stdenv.shell} $out/bin/google-java-format \
      --add-flags "-c" \
      --add-flags "java -jar $out/share/java/palantir-cli.jar \"\$@\""
  '';
  
  meta = {
    description = "Palantir Java code formatter (uses system Java via PATH)";
    homepage = "https://github.com/jsonschema2dataclass/palantir-cli";
    platforms = lib.platforms.all;
  };
}
