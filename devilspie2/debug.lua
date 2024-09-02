local window_name = get_window_name()
local app_name = get_application_name()

debug_print("Application: " .. app_name)
debug_print("Window: " .. window_name)

if window_name == "Mozilla Firefox" then
  set_window_geometry(50, 100, 800, 600)
end

if window_name == "st" then
  set_window_geometry(53, 202, 1816, 770)
end

if app_name == "mpv" then
  set_window_geometry(156, 61, 1564, 878)
end

if app_name == "emacs" or app_name == "emacs-30-0-50" then
  set_window_geometry(149, 93, 1579, 836)
end

if window_name == "passwords.kdbx [Locked] - KeePassXC" then
  os.execute("xdotool mousemove 1208 645 click 1")
end
