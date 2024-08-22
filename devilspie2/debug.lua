debug_print("Application: " .. get_application_name())
debug_print("Window: " .. get_window_name());

if (get_window_name() == "Mozilla Firefox") then
  set_window_geometry(50, 100, 800, 600);
end

if (get_window_name() == "st") then
  set_window_geometry(53, 202, 1816, 770);
end

if (get_application_name() == "emacs") then
  set_window_geometry(149, 93, 1579, 836);
end

if (get_window_name() == "passwords.kdbx [Locked] - KeePassXC") then
  os.execute("xdotool mousemove 1208 645 click 1");
end


-- if (get_window_name() == "Mozilla Firefox") then
--   set_window_geometry(50, 100, 800, 600);
-- end

