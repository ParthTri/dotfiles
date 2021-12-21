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

hs.hotkey.bind("alt", "k", function ()
		   local win = hs.window.focusedWindow()
		   local apps = hs.window.allWindows()
		   local curr = 1

		   for i = 1, #apps - 1 do
		      if apps[i]:title() == "*dashboard*" then
			 table.remove(apps, i)
		      end
		   end
		   
		   for i = 1, #apps - 1 do
		      if win == apps[i] then curr = i end
		   end

		   print (curr, apps[curr])
		   print (curr + 1, apps[curr + 1])
		   print (#apps)

		   if curr == #apps then curr = 0 end
		   print (curr)
		   apps[curr + 1]:focus()
end )

hs.hotkey.bind("alt", "j", function ()
		   local win = hs.window.focusedWindow()
		   local apps = hs.window.allWindows()
		   local curr = 1

		   for i = 1, #apps - 1 do
		      if apps[i]:title() == "*dashboard*" then
			 table.remove(apps, i)
		      end
		   end

		   for i = 1, #apps - 1 do
		      if win == apps[i] then curr = i end
		   end

		   print (curr, apps[curr])
		   print (curr - 1, apps[curr - 1])

		   if curr == 1 then curr = #apps end

		   apps[curr - 1]:focus() 
end )

