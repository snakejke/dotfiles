/* Патч добавляет прозрачность для терминала. */
#define ALPHA_PATCH 1

/* Позволяет использовать разные значения прозрачности для сфокусированного и несфокусированного окон. */
#define ALPHA_FOCUS_HIGHLIGHT_PATCH 0

/* Добавляет градиентную прозрачность, зависит от патча alpha. */
#define ALPHA_GRADIENT_PATCH 0

/* Позволяет задавать начальный размер окна в пикселях. */
#define ANYGEOMETRY_PATCH 1

/* Позволяет изменять размер окна до любого пиксельного размера. */
#define ANYSIZE_PATCH 0

/* Упрощённый вариант anysize, который меняет только подсказки по изменению размера. */
#define ANYSIZE_SIMPLE_PATCH 0

/* Рисует фоновое изображение вместо стандартного цвета фона. */
#define BACKGROUND_IMAGE_PATCH 0

/* Добавляет возможность перезагружать фоновое изображение. */
#define BACKGROUND_IMAGE_RELOAD_PATCH 0

/* Добавляет поддержку мигающего курсора. */
#define BLINKING_CURSOR_PATCH 1

/* Делает жирный текст просто жирным, не меняя его цвет. */
#define BOLD_IS_NOT_BRIGHT_PATCH 1

/* Добавляет специальный рендеринг для линий, блоков и символов Брайля. */
#define BOXDRAW_PATCH 1

/* Заставляет st помещать выделенный текст в буфер обмена CLIPBOARD. */
#define CLIPBOARD_PATCH 1

/* Позволяет изменять размер st, не обрезая текст при увеличении окна. */
#define COLUMNS_PATCH 1

/* Позволяет выделить и скопировать последний отображённый URL. */
#define COPYURL_PATCH 0

/* Позволяет выделить и скопировать последний URL, а также подсвечивает его. */
#define COPYURL_HIGHLIGHT_SELECTED_URLS_PATCH 0

/* Добавляет поддержку последовательностей CSI 22 и 23 для сохранения и восстановления заголовка окна. */
#define CSI_22_23_PATCH 1

/* Позволяет установить курсор по умолчанию при использовании значения 0. */
#define DEFAULT_CURSOR_PATCH 0

/* Desktop entry patch - .desktop файл для графических лаунчеров */
#define DESKTOPENTRY_PATCH 1

/* Возвращает BS при нажатии Backspace и DEL при нажатии Delete. */
#define DELKEY_PATCH 0

/* Добавляет опцию глобального отключения жирных шрифтов. */
#define DISABLE_BOLD_FONTS_PATCH 0

/* Добавляет опцию глобального отключения курсивных шрифтов. */
#define DISABLE_ITALIC_FONTS_PATCH 0

/* Добавляет опцию глобального отключения обычных шрифтов. */
#define DISABLE_ROMAN_FONTS_PATCH 0

/* Позволяет перетаскивать файл в терминал, чтобы получить его путь. */
#define DRAG_AND_DROP_PATCH 0

/* Делает цвет курсора инверсным к цвету текущей ячейки. */
#define DYNAMIC_CURSOR_COLOR_PATCH 0

/* Вариант anysize, который добавляет только динамический отступ. */
#define DYNAMIC_PADDING_PATCH 0

/* Позволяет читать и записывать экран st через пайп. */
#define EXTERNALPIPE_PATCH 0

/* Улучшает патч externalpipe, предотвращая сброс обработчика сигнала и добавляя перенаправление вывода. */
#define EXTERNALPIPEIN_PATCH 0

/* Позволяет приложениям командной строки использовать все расширенные сочетания клавиш. */
#define FIXKEYBOARDINPUT_PATCH 0

/* Позволяет добавлять запасной шрифт для недостающих символов. */
#define FONT2_PATCH 1

/* Добавляет возможность переключаться в полноэкранный режим. */
#define FULLSCREEN_PATCH 0

/* Скрывает курсор при нажатии клавиши и показывает его при движении мыши. */
#define HIDECURSOR_PATCH 0

/* Скрывает курсор терминала, когда окно теряет фокус. */
#define HIDE_TERMINAL_CURSOR_PATCH 0

/* Добавляет сочетание клавиш для инвертирования цветовой схемы. */
#define INVERT_PATCH 0

/* Позволяет вводить символы Unicode с помощью dmenu. */
#define ISO14755_PATCH 0

/* Добавляет возможность выделять текст с помощью сочетаний клавиш. */
#define KEYBOARDSELECT_PATCH 0

/* Добавляет поддержку лигатур с использованием библиотеки Harfbuzz. */
#define LIGATURES_PATCH 1

/* Заставляет терминал отображать только стандартные цвета, игнорируя цвета приложения. */
#define MONOCHROME_PATCH 0

/* Устанавливает свойство _NET_WM_ICON, считывая иконку из файла .png. */
#define NETWMICON_PATCH 0

/* Устанавливает свойство _NET_WM_ICON, считывая иконку из файла .ff. */
#define NETWMICON_FF_PATCH 0

/* Устанавливает свойство _NET_WM_ICON с жёстко закодированной иконкой. */
#define NETWMICON_LEGACY_PATCH 0

/* Позволяет открывать новый терминал с той же текущей рабочей директорией. */
#define NEWTERM_PATCH 1

/* Устанавливает свойство _MOTIF_WM_HINTS, чтобы окно отображалось без декораций. */
#define NO_WINDOW_DECORATIONS_PATCH 0

/* Открывает содержимое буфера обмена в браузере. */
#define OPENCOPIED_PATCH 0

/* Открывает выделенный текст с помощью xdg-open. */
#define OPEN_SELECTED_TEXT_PATCH 0

/* Позволяет открывать URL-адреса по клику. */
#define OPENURLONCLICK_PATCH 0

/* Позволяет получать текущую рабочую директорию через последовательность OSC 7. */
#define OSC7_PATCH 0

/* Позволяет переходить между строками ввода с помощью последовательности OSC 133. */
#define OSC133_PATCH 0

/* Позволяет изменять размер окна, заворачивая текст. */
#define REFLOW_PATCH 0

/* Позволяет задавать размер рамки относительно ширины ячейки. */
#define RELATIVEBORDER_PATCH 0

/* Позволяет по клику правой кнопкой мыши отправлять выделенный текст в программу-обработчик. */
#define RIGHTCLICKTOPLUMB_PATCH 1

/* Добавляет прокрутку истории вывода. */
#define SCROLLBACK_PATCH 1

/* Добавляет прокрутку истории вывода с помощью мыши. */
#define SCROLLBACK_MOUSE_PATCH 0

/* Добавляет прокрутку истории вывода колесом мыши вне режима ALTSCREEN. */
#define SCROLLBACK_MOUSE_ALTSCREEN_PATCH 1

/* Позволяет задать отдельные цвета для выделенного текста. */
#define SELECTION_COLORS_PATCH 0

/* Патч с одним буфером для отображения изображений. */
#define SINGLE_DRAWABLE_BUFFER_PATCH 0

/* Добавляет поддержку графики SIXEL. */
#define SIXEL_PATCH 1

/* Позволяет встраивать клиентские приложения в окно st. */
#define ST_EMBEDDER_PATCH 0

/* Использует инвертированные цвета для выделения, когда цвета фона и текста одинаковы. */
#define SPOILER_PATCH 0

/* Изменяет форму курсора на стандартную, когда программа подписывается на события мыши. */
#define SWAPMOUSE_PATCH 1

/* Добавляет поддержку синхронизированных обновлений для устранения мерцания курсора. */
#define SYNC_PATCH 1

/* Использует курсор из вашей темы вместо стандартного X-курсора. */
#define THEMED_CURSOR_PATCH 1

/* Добавляет поддержку специальных подчёркиваний. */
#define UNDERCURL_PATCH 1

/* Добавляет прокрутку колесом мыши без клавиш-модификаторов. */
#define UNIVERSCROLL_PATCH 0

/* Использует XftFontMatch вместо FcFontMatch для улучшения отрисовки шрифтов. */
#define USE_XFTFONTMATCH_PATCH 0

/* Вертикально центрирует строки, если задан больший масштаб символов. */
#define VERTCENTER_PATCH 0

/* Кратковременно инвертирует содержимое окна при сигнале звонка. */
#define VISUALBELL_1_PATCH 0

/* Добавляет поддержку изображений w3m. */
#define W3M_PATCH 0

/* Добавляет корректный рендеринг широких символов. */
#define WIDE_GLYPHS_PATCH 0

/* Временный патч для исправления слишком широких интервалов в Google Variable Fonts. */
#define WIDE_GLYPH_SPACING_PATCH 0

/* Позволяет указать начальный рабочий каталог для st. */
#define WORKINGDIR_PATCH 0

/* Добавляет возможность настраивать st через Xresources. */
#define XRESOURCES_PATCH 1

/* Добавляет возможность перезагружать конфигурацию Xresources. */
#define XRESOURCES_RELOAD_PATCH 1
