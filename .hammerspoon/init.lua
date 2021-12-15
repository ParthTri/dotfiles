hs.loadSpoon("SpoonInstall")
hyper = {"ctrl", "shift", "cmd", "alt"}

hs.hotkey.bindSpec( {hyper, "y"}, hs.toggleConsole)
hs.hotkey.bindSpec( {hyper, "r"}, hs.reload)

spoon.SpoonInstall:andUse("MiroWindowsManager")
hs.window.animationDuration = 0
spoon.MiroWindowsManager:bindHotkeys({
  up = {hyper, "Up"},
  right = {hyper, "Right"},
  down = {hyper, "Down"},
  left = {hyper, "Left"},
  fullscreen = {hyper, "Space"}
})

spoon.SpoonInstall:andUse("WindowScreenLeftAndRight")
spoon.WindowScreenLeftAndRight:bindHotkeys({
      screen_left = {hyper, "h"},
      screen_right = {hyper, "l"},
})

hs.hotkey.bind("ctrl", "k", function ()
		   local win = hs.window.focusedWindow()
		   local apps = hs.window.allWindows()
		   local curr = 1

		   for i = 1, #apps do
		      if win == apps[i] then curr = i end
		   end

		   if curr == #apps then curr = 0 end
		   print (apps[curr + 1])
		   apps[curr + 1]:focus()
		   print (hs.window.focusedWindow())
end )

hs.hotkey.bind("ctrl", "j", function ()
		   local win = hs.window.focusedWindow()
		   local apps = hs.window.allWindows()
		   local curr = 1

		   for i = 1, #apps do
		      if win == apps[i] then curr = i end
		   end
		   if curr == 1 then curr = #apps end

		   apps[curr - 1]:focus() 
		   print (hs.window.focusedWindow())
end )

