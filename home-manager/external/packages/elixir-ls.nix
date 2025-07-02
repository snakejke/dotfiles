{ stdenv, unzip, lib, sources, ... }:

stdenv.mkDerivation {
  pname = sources.elixir-ls.pname;
  version = sources.elixir-ls.version;
  src = sources.elixir-ls.src;
  
  nativeBuildInputs = [ unzip ];
  
  unpackPhase = ''
    unzip $src
  '';
  
  dontBuild = true;
  
  installPhase = ''
    mkdir -p $out/release
    
    # Копируем все содержимое архива в release папку
    cp -r * $out/release/
    
    # Делаем скрипты исполняемыми
    # chmod +x $out/release/language_server.sh
    # chmod +x $out/release/debugger.sh
    
    # Создаем символические ссылки в bin для удобства
    mkdir -p $out/bin
    ln -s $out/release/language_server.sh $out/bin/elixir-ls
    # ln -s $out/release/debugger.sh $out/bin/elixir-ls-debugger
  '';
  
  meta = with lib; {
    description = "A frontend-independent IDE \"smartness\" server for Elixir";
    homepage = "https://github.com/elixir-lsp/elixir-ls";
    license = licenses.asl20;
    maintainers = [];
    platforms = platforms.unix;
  };
}
