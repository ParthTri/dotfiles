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
