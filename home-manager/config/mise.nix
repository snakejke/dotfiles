{
  programs.mise = {
    enable = true;
    
    globalConfig = {
      settings = {
        npm.bun = true;
        disable_tools = [ "java" ];
        idiomatic_version_file_enable_tools = [];
        aqua.github_attestations = false; # wait fix <2026-02-26 Thu>
      };
      tools = {
        rust = {
          # version = "stable";
          version = "1.94.0";
          profile = "default"; # rustc, cargo, rustfmt, clippy
          components = "rust-analyzer,rust-src,llvm-tools";
        };
        codex = "latest";
        babashka = "latest";
        jbang = "latest";
        # claude = "latest";
        "asdf:asdf-community/asdf-nim" = "latest";
        elixir = "latest";
        erlang = "latest";
        rebar = "latest";
        leiningen = "latest";
        clojure = "latest";
        go = "latest";
        bun = "latest";
        # node = "22"; 
        node = "latest"; 
        uv = "latest";
        spring-boot = "latest";
        github-cli = "latest";
        # dotnet = "latest";
        # ollama = "latest";
        just = "latest";
        "github:arnetheduck/nph" = "latest";
        coursier = "latest";
        # "cargo:https://github.com/aws/amazon-q-developer-cli" = { 
        #   version = "latest";
        #   crate = "crates/chat-cli";
        # };
        "npm:prettier" = "latest";
        "npm:@google/gemini-cli" = "latest";
        "npm:@qwen-code/qwen-code" = "latest";
        "pipx:jedi-language-server" = "latest";
        "pipx:ty" = "latest";
        "pipx:my-cookies" = "latest";
        "pipx:sqlfluff" = "latest";
        "pipx:huggingface_hub" = {
          version = "latest";
          extras = "cli";
        };
        # "pipx:huggingface_hub" = {
        #   version = "latest";
        #   uvx = false;  # булево значение, не строка
        #   pipx_args = "--include-deps";
        # };
        # "pipx:open-webui" = "0.6.33";
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
