spaces = require("hs._asm.undocumented.spaces")
hhtwm = require("hhtwm")

hhtwm.filters = {
  { app = 'Finder', tile = false },
  { app = 'Hammerspoon', title = 'Hammerspoon Console', tile = false } 
}

hhtwm.screenMargin = { top = 15, bottom = 15, left = 15, right = 15 }
hhtwm.margin = 12
hhtwm.setLayout("main-left")

print (hhtwm.getLayout())
