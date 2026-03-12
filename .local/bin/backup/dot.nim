## dotfiles.nim
## Управление дотфайлами: сбор в ~/.local/dotfiles и деплой на новую машину.
##
## Использование:
##   ./dotfiles collect  [--dry-run]  -- копирует файлы с системы в dotfiles_dir
##   ./dotfiles deploy   [--dry-run]  -- разворачивает файлы из dotfiles_dir по местам
##   ./dotfiles status                -- показывает что изменилось

import std/[os, strutils, strformat, terminal, parseopt, posix, osproc, streams, times]

## =============================================================================
## Типы и базовые локации
## =============================================================================

type
  Location = enum
    locHome    ## ~/  (включая .config/, .gnupg/, .ssh/ и т.п.)
    locRoot    ## /   (системные конфиги /etc/ и т.п.)

type
  EntryKind = enum
    ekFile     ## Обычный файл
    ekDir      ## Директория (рекурсивное копирование)

type
  Entry = tuple
    path: string    ## Относительный путь — он же путь внутри dotfiles_dir
    loc:  Location  ## Откуда/куда этот файл на системе
    kind: EntryKind ## Файл или директория
    mode: int       ## Unix-права (0o644, 0o600, 0o755 и т.п.)

## =============================================================================
## Исключения для ~/.local/bin/ (аналог .rsync-filter из Ansible)
## =============================================================================

let defaultBinExcludes* = @[
  "cabal-*",
  "exercism",
  "ghcup",
  "kiro-*",
  "q",
  "qchat",
  "metals",
  "google-java-format.jar",
  "mise",
  "rebar3",
  "rtimer",
  "acli",
  "stack-*",
  "uv",
  "uvx",
  "hlint",
  "noisetorch",
  "__erlang_ls",
  "elp",
  "org.eclipse.lemminx-uber.jar",
  "*/supervise",
  "*/*/supervise",
]

## =============================================================================
## Конфигурация: списки файлов
## =============================================================================

const userConfigs: seq[Entry] = @[
  # Emacs
  ("emacs/early-init.el",                                           locHome, ekFile, 0o444),
  ("emacs/init.el",                                                 locHome, ekFile, 0o644),
  ("emacs/init.org",                                                locHome, ekFile, 0o644),
  ("emacs/init-dev.el",                                             locHome, ekFile, 0o644),
  ("emacs/dict/english-words.txt",                                  locHome, ekFile, 0o644),
  ("emacs/lisp",                                                    locHome, ekDir, 0o755),
  ("emacs/templates",                                               locHome, ekDir, 0o755),

  # Zsh
  ("zsh/.zprofile",                                                 locHome, ekFile, 0o644),
  ("zsh/.zshenv",                                                   locHome, ekFile, 0o644),
  ("zsh/.zshrc",                                                    locHome, ekFile, 0o644),
  ("zsh/aliases",                                                   locHome, ekFile, 0o644),
  ("zsh/fun.zsh",                                                   locHome, ekFile, 0o644),

  # Mpv
  ("mpv/mpv.conf",                                                  locHome, ekFile, 0o644),
  ("mpv/input.conf",                                                locHome, ekFile, 0o644),
  ("mpv/patcher.py",                                                locHome, ekFile, 0o644),

  # Nvim
  ("nvim/init.lua",                                                 locHome, ekFile, 0o644),

  # Ranger
  ("ranger/rc.conf",                                                locHome, ekFile, 0o644),
  ("ranger/rifle.conf",                                             locHome, ekFile, 0o644),
  ("ranger/scope.sh",                                               locHome, ekFile, 0o755),

  # Wireplumber
  ("wireplumber/wireplumber.conf.d/80-disable-logind.conf",         locHome, ekFile, 0o644),

  # Прочие
  ("mimeapps.list",                                                 locHome, ekFile, 0o644),
  ("fontconfig",                                                    locHome, ekDir, 0o755),
  ("picom",                                                         locHome, ekDir, 0o755),
  ("pipewire",                                                      locHome, ekDir, 0o755),

  # FVWM
  ("fvwm",                                                          locHome, ekDir, 0o755),

  # Home Manager
  ("home-manager",                                                  locHome, ekDir, 0o755),

  # LF
  ("lf",                                                            locHome, ekDir, 0o755),

  # Notmuch
  ("notmuch",                                                       locHome, ekDir, 0o755),

  # SX
  ("sx",                                                            locHome, ekDir, 0o755),

  # Xsettingsd
  ("xsettingsd",                                                    locHome, ekDir, 0o755),

  # ~/.local/*
  (".local/bin",                                                    locHome, ekDir, 0o755),
  (".local/rawservices",                                            locHome, ekDir, 0o755),

  # ~/.local/ansible_main (в dotfiles хранится как ansible_main/)
  #("ansible_main",                                                  locHome, ekDir, 0o755),
]

const securityConfigs: seq[Entry] = @[
  (".gnupg/common.conf",    locHome, ekFile, 0o600),
  (".gnupg/gpg-agent.conf", locHome, ekFile, 0o600),
]

const systemConfigs: seq[Entry] = @[
  ("etc/unbound/unbound.conf",               locRoot, ekFile, 0o644),
  ("etc/dnscrypt-proxy/dnscrypt-proxy.toml", locRoot, ekFile, 0o644),
  ("etc/logrotate.conf",                     locRoot, ekFile, 0o644),
  ("etc/logrotate.d/rsyslog",                locRoot, ekFile, 0o644),
  ("etc/logrotate.d/additional-logs",        locRoot, ekFile, 0o644),
  ("etc/pam.d/login",                        locRoot, ekFile, 0o644),
  ("etc/dhcpcd.conf",                        locRoot, ekFile, 0o644),
  ("etc/rsyslog.conf",                       locRoot, ekFile, 0o644),
  ("etc/sysctl.conf",                        locRoot, ekFile, 0o644),
  ("etc/resolv.conf",                        locRoot, ekFile, 0o644),
  ("etc/nftables.conf",                      locRoot, ekFile, 0o644),
  ("etc/iptables/iptables.rules",            locRoot, ekFile, 0o644),
  ("etc/X11/xorg.conf",                      locRoot, ekFile, 0o644),
  ("etc/X11/Xwrapper.config",                locRoot, ekFile, 0o644),
  ("etc/X11/xorg.conf.d/30-keyboard.conf",   locRoot, ekFile, 0o644),
  ("etc/X11/xorg.conf.d/50-mouse-acceleration.conf", locRoot, ekFile, 0o644),
  ("etc/sv/agetty-tty1/conf",                locRoot, ekFile, 0o644),
  ("etc/sv/amneziavpn/run",                  locRoot, ekFile, 0o755),
  ("etc/sv/nix-daemon/run",                  locRoot, ekFile, 0o755),
  ("etc/sv/nixgpu/run",                      locRoot, ekFile, 0o755),
  ("etc/sv/runsvdir-snake/run",              locRoot, ekFile, 0o755),
  ("etc/sv/determinate-nixd/run",            locRoot, ekFile, 0o755),
  ("etc/sv/smtpd_bypass/run",                locRoot, ekFile, 0o755),
  ("etc/sv/smtpd_bypass/finish",             locRoot, ekFile, 0o755),
  ("etc/smtpd/smtpd.conf",                   locRoot, ekFile, 0o644),
  ("etc/docker/daemon.json",                 locRoot, ekFile, 0o644),
  ("etc/dracut.conf.d/boot.conf",            locRoot, ekFile, 0o644),
  ("etc/sudoers.d/00-wheel",                 locRoot, ekFile, 0o644),
  ("etc/sudoers.d/01-nopass",                locRoot, ekFile, 0o644),
  ("etc/xbps.d/10-ignore.conf",              locRoot, ekFile, 0o644),
  ("etc/zsh/zshenv",                         locRoot, ekFile, 0o644),
]

const allEntries: seq[Entry] = userConfigs & securityConfigs & systemConfigs

type
  SensitiveEntry = tuple
    srcRel: string    ## Относительный путь в системе (как в ~/.config/...)
    dstRel: string    ## Относительный путь в dotfiles_dir
    mode: int         ## Права на расшифрованный файл на системе

type
  CompareResult = enum
    crEqual
    crDifferent
    crUnknown

const sensitiveEntries: seq[SensitiveEntry] = @[
  ("zsh/.zhistory", "zsh/.zhistory.gpg", 0o644),
  ("zsh/.z",        "zsh/.z.gpg",        0o644),
]

# =============================================================================
# Вычисление путей
# =============================================================================

proc realHome(): string =
  ## Домашняя директория реального пользователя.
  ## Если запущено через sudo, SUDO_USER указывает на исходного пользователя.
  let uid = getuid()
  if uid == 0:
    let sudoUser = getEnv("SUDO_USER")
    if sudoUser.len > 0:
      return "/home" / sudoUser
  result = getHomeDir()

proc sudoUserName(): string =
  ## Если запущено через sudo, вернёт имя исходного пользователя.
  result = getEnv("SUDO_USER")

proc sudoUserIds(): tuple[ok: bool, uid: Uid, gid: Gid] =
  ## UID/GID пользователя из SUDO_USER.
  let name = sudoUserName()
  if name.len == 0:
    return (false, 0.Uid, 0.Gid)
  let pw = getpwnam(name.cstring)
  if pw == nil:
    return (false, 0.Uid, 0.Gid)
  return (true, pw.pw_uid, pw.pw_gid)

proc dotfilesDir(): string =
  let override = getEnv("DOT_DOTFILESDIR")
  if override.len > 0:
    return override
  return realHome() / ".local/dotfiles"

proc rootPrefix(): string =
  ## Префикс для системного корня (по умолчанию "/").
  ## Полезно для тестов/песочницы: DOT_ROOT_PREFIX=/tmp/fakeroot
  let override = getEnv("DOT_ROOT_PREFIX")
  if override.len > 0:
    return override
  return "/"

proc getOriginalPath(e: Entry): string =
  ## Абсолютный путь файла на живой системе.
  ## Для locHome: если путь не начинается с точки, добавляем .config/
  case e.loc
  of locHome:
    if e.path == "ansible_main" or e.path.startsWith("ansible_main/"):
      return realHome() / ".local" / e.path
    if e.path.startsWith("etc/") or e.path.startsWith("."):
      return realHome() / e.path
    else:
      return realHome() / ".config" / e.path
  of locRoot: return rootPrefix() / e.path

proc getStoredPath(e: Entry): string =
  ## Путь файла внутри dotfiles_dir.
  return dotfilesDir() / e.path

proc getSensitiveOriginalPath(se: SensitiveEntry): string =
  ## Абсолютный путь исходного чувствительного файла на системе.
  ## Храним их в ~/.config/<...>
  return realHome() / ".config" / se.srcRel

proc getSensitiveStoredPath(se: SensitiveEntry): string =
  ## Абсолютный путь шифрованного файла в dotfiles_dir.
  return dotfilesDir() / se.dstRel

# =============================================================================
# Утилиты вывода
# =============================================================================

proc ok(msg: string)   = stdout.styledWriteLine(fgGreen,  "  ✓ ", resetStyle, msg)
proc info(msg: string) = stdout.styledWriteLine(fgCyan,   "  → ", resetStyle, msg)
proc warn(msg: string) = stdout.styledWriteLine(fgYellow, "  ! ", resetStyle, msg)
proc err(msg: string)  = stderr.styledWriteLine(fgRed,    "  ✗ ", resetStyle, msg)

proc header(msg: string) =
  echo ""
  stdout.styledWriteLine(styleBright, msg)
  echo "─".repeat(50)

# =============================================================================
# Glob-паттерны (простая реализация для * и ?)
# =============================================================================

proc matchPattern(pattern, text: string): bool =
  ## Простая реализация glob-паттернов (* и ?).
  ## Для сложных случаев вроде */supervise работает рекурсивно.
  if pattern == "*": return true
  if pattern == text: return true
  
  # Обработка */ и **/
  if pattern.contains("*/"):
    let parts = pattern.split("*/")
    if parts.len == 2:
      # Паттерн вида "*/supervise" — проверяем последний компонент
      if parts[0] == "":
        let textParts = text.split('/')
        if textParts.len >= 1:
          return matchPattern(parts[1], textParts[^1])
      # Паттерн вида "foo/*/bar" — более сложный случай
      elif parts[0].len > 0:
        if text.startsWith(parts[0]):
          let rest = text[parts[0].len..^1]
          if rest.startsWith('/'):
            return matchPattern(parts[1], rest[1..^1])
      return false
  
  # Обработка * в середине
  if pattern.contains('*'):
    let parts = pattern.split('*')
    if parts.len == 2:
      return text.startsWith(parts[0]) and text.endsWith(parts[1])
  
  # Обработка ?
  if pattern.contains('?'):
    if pattern.len != text.len: return false
    for i, c in pattern:
      if c != '?' and c != text[i]: return false
    return true
  
  return false

# =============================================================================
# Загрузка исключений из .rsync-filter
# =============================================================================

proc loadBinExcludes(dotfilesDir: string): seq[string] =
  ## Загружает исключения из .rsync-filter если файл существует.
  ## Иначе возвращает defaultBinExcludes.
  let filterPath = dotfilesDir / ".rsync-filter"
  if fileExists(filterPath):
    result = @[]
    try:
      for line in lines(filterPath):
        let trimmed = line.strip()
        if trimmed.startsWith("- "):
          result.add(trimmed[2..^1])
      if result.len > 0:
        info(&"Загружено {result.len} правил из .rsync-filter")
    except OSError as e:
      warn(&"Не удалось прочитать .rsync-filter: {e.msg}")
      result = defaultBinExcludes
  else:
    result = defaultBinExcludes

proc shouldExclude(filename: string, excludes: seq[string]): bool =
  ## Проверяет, попадает ли файл под одно из исключений.
  let basename = filename.splitPath.tail
  for pattern in excludes:
    if matchPattern(pattern, filename) or matchPattern(pattern, basename):
      return true
  return false

# =============================================================================
# Ядро: файловые операции
# =============================================================================

proc setPerms(path: string, mode: int): bool =
  ## Устанавливает права на файл/директорию. Возвращает true при успехе.
  chmod(path.cstring, mode.Mode) == 0

proc getFilePermissions(path: string): int =
  ## Получает текущие права файла.
  when defined(posix):
    var st: Stat
    if stat(path.cstring, st) != 0:
      return -1
    return int(st.st_mode and 0o777.Mode)
  else:
    try:
      let perms = std/os.getFilePermissions(path)
      result = 0
      if fpUserRead in perms: result = result or 0o400
      if fpUserWrite in perms: result = result or 0o200
      if fpUserExec in perms: result = result or 0o100
      if fpGroupRead in perms: result = result or 0o040
      if fpGroupWrite in perms: result = result or 0o020
      if fpGroupExec in perms: result = result or 0o010
      if fpOthersRead in perms: result = result or 0o004
      if fpOthersWrite in perms: result = result or 0o002
      if fpOthersExec in perms: result = result or 0o001
    except CatchableError:
      result = -1

proc filesIdentical(a, b: string): bool =
  ## Побайтовое сравнение файлов.
  if not fileExists(a) or not fileExists(b): return false
  try:
    readFile(a) == readFile(b)
  except CatchableError:
    return false

proc runCmd(cmd: string, args: seq[string]): tuple[code: int, output: string] =
  ## Запуск внешней команды без shell. stdout+stderr в output.
  try:
    let p = startProcess(cmd, args = args, options = {poUsePath, poStdErrToStdOut})
    let output = p.outputStream.readAll()
    let code = p.waitForExit()
    p.close()
    return (code, output)
  except OSError as e:
    return (127, e.msg)

proc gpgExists(): bool =
  return findExe("gpg").len > 0

proc gpgRecipient(): string =
  ## Идентификатор получателя для gpg --encrypt (email/KEYID).
  result = getEnv("DOT_GPG_RECIPIENT")
  if result.len == 0:
    result = getEnv("DOT_GPG_KEY_ID")

# proc runGpg(args: seq[string]): tuple[code: int, output: string] =
#   ## Запускает gpg максимально "как у пользователя":
#   ## - если мы root под sudo, запускаем gpg от имени SUDO_USER (-H выставит HOME)
#   ## - иначе запускаем gpg напрямую
#   if getuid() == 0 and sudoUserName().len > 0 and findExe("sudo").len > 0:
#     return runCmd("sudo", @["-u", sudoUserName(), "-H", "gpg"] & args)
#   return runCmd("gpg", args)

proc runGpg(args: seq[string]): tuple[code: int, output: string] =
  ## Если мы root (запущено через sudo), используем runuser чтобы выполнить
  ## gpg от имени реального пользователя — точно так же, как это делал ansible.
  ## runuser запускает команду с полным окружением пользователя, поэтому
  ## XDG_RUNTIME_DIR, HOME и сокет gpg-agent будут правильными автоматически.
  if getuid() == 0 and sudoUserName().len > 0:
    let userName = sudoUserName()
    # runuser -u <user> -- gpg <args>
    return runCmd("runuser", @["-u", userName, "--", "gpg"] & args)
  
  # Обычный запуск без sudo — ничего не меняем
  return runCmd("gpg", args)  

proc encryptSensitive(srcPlain, dstGpg: string, dryRun: bool): bool =
  if dryRun:
    return true
  if not gpgExists():
    err("gpg не найден в PATH — пропускаю шифрование чувствительных файлов")
    return false
  try:
    createDir(parentDir(dstGpg))
  except OSError as e:
    err(&"не удалось создать директорию для {dstGpg}: {e.msg}")
    return false

  var args = @["--batch", "--yes", "--output", dstGpg, "--encrypt"]
  let recipient = gpgRecipient()
  if recipient.len > 0:
    args.add(@["--recipient", recipient])
  else:
    args.add("--default-recipient-self")
  args.add(srcPlain)

  let (code, output) = runGpg(args)
  if code != 0:
    if recipient.len == 0:
      err("gpg encrypt failed без получателя. Задай DOT_GPG_RECIPIENT (или настроить default-recipient в gpg).")
    err(&"gpg encrypt failed ({code}): {output.strip()}")
    return false
  discard setPerms(dstGpg, 0o600)
  return true

proc decryptSensitive(srcGpg, dstPlain: string, mode: int, dryRun: bool): bool =
  if dryRun:
    return true
  if not gpgExists():
    err("gpg не найден в PATH — пропускаю расшифровку чувствительных файлов")
    return false
  try:
    createDir(parentDir(dstPlain))
  except OSError as e:
    err(&"не удалось создать директорию для {dstPlain}: {e.msg}")
    return false

  let (code, output) = runGpg(@[
    "--batch", "--yes",
    "--output", dstPlain,
    "--decrypt",
    srcGpg,
  ])
  if code != 0:
    err(&"gpg decrypt failed ({code}): {output.strip()}")
    return false
  discard setPerms(dstPlain, mode)
  return true

proc compareSensitive(storedGpg, originalPlain: string): tuple[res: CompareResult, detail: string] =
  ## Сравнивает содержимое, расшифровав storedGpg во временный файл.
  ## Возвращает crUnknown если сравнение невозможно (нет gpg/ключей/прав).
  if not gpgExists():
    return (crUnknown, "gpg не найден в PATH")
  let tmpPath = getTempDir() / ("dotfiles_sensitive_" & $getCurrentProcessId() & "_" & $int(epochTime()) & ".tmp")

  defer:
    try: removeFile(tmpPath)
    except OSError: discard

  let (code, output) = runGpg(@[
    "--batch", "--yes",
    "--output", tmpPath,
    "--decrypt",
    storedGpg,
  ])
  if code != 0:
    let lines = output.strip().splitLines()
    let msg = (if lines.len > 0: lines[0] else: "gpg decrypt failed").strip()
    return (crUnknown, &"{msg} (code={code})")
  if filesIdentical(tmpPath, originalPlain):
    return (crEqual, "")
  return (crDifferent, "")

proc chownTree(path: string, uid: Uid, gid: Gid) =
  ## Рекурсивно меняет владельца/группу (best-effort).
  if not dirExists(path):
    return
  discard chown(path.cstring, uid, gid)
  for p in walkDirRec(path, yieldFilter = {pcFile, pcDir}):
    discard chown(p.cstring, uid, gid)

proc copyFileSafe(src, dst: string, mode: int, dryRun: bool): bool =
  ## Копирует файл с обработкой ошибок. Возвращает true при успехе.
  if dryRun:
    return true
  
  try:
    createDir(parentDir(dst))
    copyFile(src, dst)
    if not setPerms(dst, mode):
      warn(&"не удалось установить права {mode:o} на {dst}")
    return true
  except OSError as e:
    err(&"ошибка копирования {src} → {dst}: {e.msg}")
    return false

proc copyDirRecursive(srcDir, dstDir: string, mode: int, excludes: seq[string], dryRun: bool) =
  ## Рекурсивно копирует директорию с применением исключений.
  if not dirExists(srcDir):
    warn(&"директория не найдена: {srcDir}")
    return
  
  if dryRun:
    info(&"[dry-run] {srcDir} → {dstDir}")
    return
  
  try:
    createDir(dstDir)
    if not setPerms(dstDir, mode):
      warn(&"не удалось установить права {mode:o} на {dstDir}")
    
    for kind, path in walkDir(srcDir, checkDir = true):
      let relPath = relativePath(path, srcDir)
      
      # Проверяем исключения
      if shouldExclude(relPath, excludes):
        info(&"пропущено (исключение): {relPath}")
        continue
      
      let dstPath = dstDir / relPath
      
      case kind
      of pcFile:
        # Для директорий права `mode` применяются к директориям,
        # а для файлов сохраняем исходные права (как rsync -a).
        let srcMode = getFilePermissions(path)
        let fileMode = if srcMode == -1: 0o644 else: srcMode
        discard copyFileSafe(path, dstPath, fileMode, dryRun)
      of pcDir:
        copyDirRecursive(path, dstPath, mode, excludes, dryRun)
      of pcLinkToFile, pcLinkToDir:
        info(&"пропущена ссылка: {path}")
  except OSError as e:
    err(&"ошибка копирования директории {srcDir}: {e.msg}")

proc copyEntry(e: Entry, src, dst: string, excludes: seq[string], dryRun = false) =
  ## Копирует один файл или директорию.
  case e.kind
  of ekFile:
    if not fileExists(src):
      warn(&"не найден: {src}")
      return

    if filesIdentical(src, dst):
      ok(&"без изменений: {e.path}")
      return

    if dryRun:
      info(&"[dry-run] {src} → {dst}")
      return

    if copyFileSafe(src, dst, e.mode, dryRun):
      ok(&"скопирован: {e.path}")
    else:
      err(&"не удалось скопировать: {e.path}")
  
  of ekDir:
    if not dirExists(src):
      warn(&"директория не найдена: {src}")
      return
    copyDirRecursive(src, dst, e.mode, excludes, dryRun)
    if not dryRun:
      ok(&"скопирована директория: {e.path}")

# =============================================================================
# Проверка прав
# =============================================================================

proc checkPerms(path: string, expected: int): bool =
  ## Проверяет, совпадают ли права с ожидаемыми.
  let actual = getFilePermissions(path)
  if actual == -1:
    return false
  result = (actual == expected)
  if not result:
    warn(&"права не совпадают: ожидалось {expected:o}, фактически {actual:o}")

# =============================================================================
# Команды
# =============================================================================

proc cmdCollect(dryRun = false) =
  header(if dryRun: "Collect [dry-run]" else: "Collect")
  
  let dfDir = dotfilesDir()
  if not dryRun:
    try:
      createDir(dfDir)
    except OSError as e:
      err(&"не удалось создать {dfDir}: {e.msg}")
      return
  
  # Загружаем исключения
  let excludes = loadBinExcludes(dfDir)
  
  for e in allEntries:
    # Пропускаем файлы из ~/.local/bin/ которые в исключениях
    if e.path.startsWith(".local/bin/"):
      let basename = e.path.splitPath.tail
      if shouldExclude(basename, excludes):
        info(&"пропущено (исключение): {e.path}")
        continue
    
    copyEntry(e,
      src = getOriginalPath(e),
      dst = getStoredPath(e),
      excludes = excludes,
      dryRun = dryRun)

  var didChown = false
  # Если collect запускался через sudo — до запуска gpg приводим владельца
  # и заранее создаём директории назначения для *.gpg, чтобы избежать TOCTOU:
  # root создаёт директорию → runuser(gpg) не может в неё писать.
  if not dryRun and getuid() == 0:
    let (okIds, uid, gid) = sudoUserIds()
    if okIds:
      for se in sensitiveEntries:
        try:
          createDir(parentDir(getSensitiveStoredPath(se)))
        except OSError:
          discard
      chownTree(dfDir, uid, gid)
      didChown = true
    else:
      warn("не удалось определить UID/GID из SUDO_USER — dotfiles могут остаться root-owned")

  # Чувствительные файлы: шифруем в dotfiles_dir как *.gpg
  for se in sensitiveEntries:
    let srcPlain = getSensitiveOriginalPath(se)
    let dstGpg = getSensitiveStoredPath(se)
    if not fileExists(srcPlain):
      warn(&"не найден: {srcPlain}")
      continue
    if dryRun:
      info(&"[dry-run] gpg encrypt {srcPlain} → {dstGpg}")
      continue
    if encryptSensitive(srcPlain, dstGpg, dryRun):
      ok(&"зашифрован: {se.dstRel}")
    else:
      err(&"не удалось зашифровать: {se.srcRel}")

  echo ""
  info(&"Готово. Не забудь сделать git commit в {dfDir}")

  # На всякий случай: если не удалось сделать ранний chown, пробуем в конце.
  if not dryRun and getuid() == 0 and not didChown:
    let (okIds, uid, gid) = sudoUserIds()
    if okIds:
      chownTree(dfDir, uid, gid)

proc cmdDeploy(dryRun = false) =
  header(if dryRun: "Deploy [dry-run]" else: "Deploy")

  let isRoot = getuid() == 0
  let rootPref = rootPrefix()
  let allowRootPaths = isRoot or rootPref != "/"
  if not allowRootPaths:
    warn("Системные конфиги (/etc/) будут пропущены — запусти с sudo для полного деплоя")

  # Загружаем исключения
  let excludes = loadBinExcludes(dotfilesDir())
  
  for e in allEntries:
    if e.loc == locRoot and not allowRootPaths:
      info(&"пропущен (нет sudo): {e.path}")
      continue

    # Пропускаем файлы из ~/.local/bin/ которые в исключениях
    if e.path.startsWith(".local/bin/"):
      let basename = e.path.splitPath.tail
      if shouldExclude(basename, excludes):
        info(&"пропущено (исключение): {e.path}")
        continue

    copyEntry(e,
      src = getStoredPath(e),
      dst = getOriginalPath(e),
      excludes = excludes,
      dryRun = dryRun)

  # Чувствительные файлы: расшифровываем из *.gpg обратно в ~/.config/...
  for se in sensitiveEntries:
    let srcGpg = getSensitiveStoredPath(se)
    let dstPlain = getSensitiveOriginalPath(se)
    if not fileExists(srcGpg):
      warn(&"отсутствует в dotfiles: {se.dstRel}")
      continue
    if dryRun:
      info(&"[dry-run] gpg decrypt {srcGpg} → {dstPlain}")
      continue
    if decryptSensitive(srcGpg, dstPlain, se.mode, dryRun):
      ok(&"расшифрован: {se.srcRel}")
    else:
      err(&"не удалось расшифровать: {se.dstRel}")

  echo ""
  ok("Деплой завершён.")

proc cmdStatus() =
  header("Status")
  var changed = 0
  var permIssues = 0

  # Загружаем исключения
  let excludes = loadBinExcludes(dotfilesDir())
  
  for e in allEntries:
    # Пропускаем файлы из ~/.local/bin/ которые в исключениях
    if e.path.startsWith(".local/bin/"):
      let basename = e.path.splitPath.tail
      if shouldExclude(basename, excludes):
        continue
    
    let stored   = getStoredPath(e)
    let original = getOriginalPath(e)

    let storedExists =
      if e.kind == ekDir: dirExists(stored) else: fileExists(stored)
    let originalExists =
      if e.kind == ekDir: dirExists(original) else: fileExists(original)

    if not storedExists:
      warn(&"отсутствует в dotfiles: {e.path}")
      inc changed
    elif not originalExists:
      warn(&"отсутствует на системе: {original}")
      inc changed
    else:
      # Проверяем содержимое (только файлы)
      if e.kind == ekFile:
        if not filesIdentical(stored, original):
          info(&"изменён: {e.path}")
          inc changed
        else:
          ok(&"актуален: {e.path}")
      else:
        ok(&"актуален: {e.path}")
      
      # Проверяем права
      if not checkPerms(original, e.mode):
        inc permIssues

  # Чувствительные файлы (хранятся в dotfiles_dir как *.gpg)
  for se in sensitiveEntries:
    let storedGpg = getSensitiveStoredPath(se)
    let originalPlain = getSensitiveOriginalPath(se)
    if not fileExists(storedGpg):
      warn(&"отсутствует в dotfiles: {se.dstRel}")
      inc changed
    elif not fileExists(originalPlain):
      warn(&"отсутствует на системе: {originalPlain}")
      inc changed
    else:
      let (res, detail) = compareSensitive(storedGpg, originalPlain)
      case res
      of crEqual:
        ok(&"актуален: {se.srcRel}")
      of crDifferent:
        info(&"изменён: {se.srcRel}")
        inc changed
      of crUnknown:
        warn(&"не удалось проверить: {se.srcRel} ({detail})")
      if not checkPerms(originalPlain, se.mode):
        inc permIssues

  echo ""
  if changed == 0 and permIssues == 0:
    ok("Всё синхронизировано.")
  else:
    if changed > 0: warn(&"Изменений: {changed}")
    if permIssues > 0: warn(&"Проблем с правами: {permIssues}")

# =============================================================================
# Точка входа с parseopt
# =============================================================================

proc usage() =
  echo """
Использование: dotfiles <команда> [опции]

Команды:
  collect    скопировать файлы с системы в ~/.local/dotfiles
  deploy     развернуть файлы из ~/.local/dotfiles на систему
  status     показать что изменилось

Опции:
  -n, --dry-run    не выполнять изменения, только показать что будет сделано
  -h, --help       показать эту справку

Примеры:
  ./dotfiles collect --dry-run
  sudo ./dotfiles deploy
  ./dotfiles status

Переменные окружения (полезно для тестов/песочницы):
  DOT_DOTFILESDIR   переопределить путь к dotfiles_dir
  DOT_ROOT_PREFIX   переопределить системный корень (по умолчанию "/")
  DOT_GPG_RECIPIENT получатель для gpg (email/KEYID) для шифрования sensitive-файлов
"""

when isMainModule:
  var
    command = ""
    dryRun = false
    showHelp = false
  
  # Парсинг аргументов через parseopt
  var p = initOptParser(commandLineParams())
  for kind, key, val in p.getopt():
    case kind
    of cmdArgument:
      if command == "":
        command = key
      else:
        err(&"Неизвестный аргумент: {key}")
        usage()
        quit(1)
    of cmdLongOption, cmdShortOption:
      case key
      of "dry-run", "n":
        dryRun = true
      of "help", "h":
        showHelp = true
      else:
        err(&"Неизвестная опция: {key}")
        usage()
        quit(1)
    of cmdEnd:
      discard
  
  if showHelp or command == "":
    usage()
    if command == "":
      quit(0)
    else:
      quit(1)
  
  case command
  of "collect": cmdCollect(dryRun)
  of "deploy":  cmdDeploy(dryRun)
  of "status":  cmdStatus()
  else:
    err(&"Неизвестная команда: {command}")
    usage()
    quit(1)
