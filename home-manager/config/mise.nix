{
  programs.mise = {
    enable = true;
    
    globalConfig = {
      settings = {
        npm.bun = true;
      };
      tools = {
        babashka = "latest";
        elixir = "latest";
        erlang = "latest";
        rebar = "latest";
        leiningen = "latest";
        clojure = "latest";
        go = "latest";
        bun = "latest";
        node = "latest"; 
        uv = "latest";
        "npm:@google/gemini-cli" = "latest";
        "npm:@qwen-code/qwen-code" = "latest";
        "pipx:jedi-language-server" = "latest";
        "pipx:my-cookies" = "latest";
        "pipx:ruff" = "latest";
        "pipx:semgrep" = "latest";
        "pipx:streamlink" = "latest";
        "pipx:docx2txt" = "latest";
      };
      tasks."update:hex" = {
      description = "Обновляет Hex и Rebar для Elixir";
      run = [
        "mix local.hex --force"
        "mix local.rebar --force"
      ];
      };
    };
   };
}
