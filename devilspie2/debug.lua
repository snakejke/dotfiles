debug_print("Application: " .. get_application_name())
debug_print("Window: " .. get_window_name());

if (get_window_name() == "Mozilla Firefox") then
  set_window_geometry(50, 100, 800, 600);
end

if (get_window_name() == "st") then
  set_window_geometry(131, 214, 1630, 600);
end

if (get_application_name() == "emacs") then
  set_window_geometry(142, 127, 1612, 899);
end

if (get_window_name() == "passwords.kdbx [Locked] - KeePassXC") then
  os.execute("xdotool mousemove 1208 645 click 1");
end


-- if (get_window_name() == "Mozilla Firefox") then
--   set_window_geometry(50, 100, 800, 600);
-- end

