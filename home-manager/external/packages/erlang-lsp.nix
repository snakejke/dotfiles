{ stdenv, lib, sources, ... }:

stdenv.mkDerivation {
  pname = sources.erlang-lsp.pname;
  version = sources.erlang-lsp.version;
  src = sources.erlang-lsp.src;
  
  # Отключаем стандартную unpack фазу
  dontUnpack = true;
  dontBuild = true;
  
  installPhase = ''
    mkdir -p $out/bin
    
    # Извлекаем исполняемый файл из архива напрямую
    tar -xzf $src -C $out/bin
    
    # Делаем исполняемым
    chmod +x $out/bin/elp
  '';
  
  meta = with lib; {
    description = "Erlang Language Platform (ELP) - Language Server for Erlang";
    homepage = "https://github.com/WhatsApp/erlang-language-platform";
    license = licenses.asl20;
    maintainers = [];
    platforms = [ "x86_64-linux" ];
  };
}
