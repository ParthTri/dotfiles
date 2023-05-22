local gears = require("gears")
local awful = require("awful")
local wibox = require("wibox")
local beautiful = require("beautiful")

local xresources	= require("beautiful.xresources")
local dpi					= xresources.apply_dpi

Theme = {}

Theme.font				= "JetBrains Mono Nerd Font Mono"

Theme.fg_normal		= "#DCD7BA"
Theme.fg_focused	= "#C8C093"
Theme.bg					= "#0F1F28"

-- Accents
Theme.samuraiRed		= "#E82424"
Theme.roninYellow		= "#FF9E3B"
Theme.crystalBlue		= "#7E9CD8"
Theme.springGreen		= "#98BB6C"
Theme.surimiOrange	= "#FFA065"
Theme.carpYellow		= "#E6C384"
Theme.winterGreen		= "#2B3328"
Theme.oniViolet			= "#957FB8"
Theme.katanaGray		= "#717C7C"

-- Border
Theme.useless_gap		= dpi(2)
Theme.border_width	= dpi(1)

-- Taglist Theme
Theme.taglist_fg_focus		= Theme.bg
Theme.taglist_bg_focus		= Theme.roninYellow
Theme.taglist_fg_occupied = Theme.katanaGray

-- Date
local myTextDate = wibox.widget.textclock("%A, %d %b %Y ")
myTextDate.font	= Theme.font

-- Time
local myTextClock = wibox.widget.textclock(" %H:%M ")
myTextClock.font	= Theme.font
myTextClock.fg_color = Theme.springGreen

-- Battery

-- Volume

-- Background 

-- Bar
local tasklist_buttons = gears.table.join(
	awful.button({ }, 1, function (c)
		if c == client.focus then
			c.minimized = true
		else
			c:emit_signal(
			"request::activate",
			"tasklist",
			{raise = true}
			)
		end
	end),
	awful.button({ }, 3, function()
		awful.menu.client_list({ theme = { width = 250 } })
	end),
	awful.button({ }, 4, function ()
		awful.client.focus.byidx(1)
	end),
	awful.button({ }, 5, function ()
		awful.client.focus.byidx(-1)
end))

awful.screen.connect_for_each_screen(function(s)
	-- Each screen has its own tag table.
	awful.tag({ "1", "2", "3", "4", "5", "6" }, s, awful.layout.layouts[2])

	-- Create a promptbox for each screen
	s.mypromptbox = awful.widget.prompt()

	-- Create a taglist widget
	s.mytaglist = awful.widget.taglist {
		screen  = s,
		filter  = awful.widget.taglist.filter.all
	}

	-- Create a tasklist widget
	s.mytasklist = awful.widget.tasklist {
		screen  = s,
		filter  = awful.widget.tasklist.filter.currenttags,
		buttons = tasklist_buttons,
	}

	-- Create the wibox
	s.mywibox = awful.wibar({ position = "top", screen = s })

	-- Add widgets to the wibox
	s.mywibox:setup {
			layout = wibox.layout.align.horizontal,
			{ -- Left widgets
			layout = wibox.layout.fixed.horizontal,
			s.mytaglist
		},
		{ -- Center Widgets
			layout = wibox.layout.fixed.horizontal,
		},
		{ -- Right widgets
			layout = wibox.layout.fixed.horizontal,
			wibox.widget.systray(),
			myTextClock,
			myTextDate,
			spacing = 5
		}
	}
end)

return Theme
