{
  programs.dbeaver = {
    enable = true;
    # userHome = "-Duser.home=/home/snake/.local/share/dbeaver";  # Указываем нужный путь
    maxHeapSize = "1024m";  # Оставляем стандартный размер или изменяем по желанию
  };
}
