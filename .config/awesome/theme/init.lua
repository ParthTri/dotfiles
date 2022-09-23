local gears = require("gears")
local awful = require("awful")
local wibox = require("wibox")
local beautiful = require("beautiful")

local xresources	= require("beautiful.xresources")
local dpi					= xresources.apply_dpi

Theme = {}

Theme.font = "FiraCode Nerd Font"

Theme.fg_normal		= "#DCD7BA"
Theme.fg_focused	= "#C8C093"
Theme.bg					= "#0F1F28"

-- Accents
Theme.samuraiRed		= "#E82424"
Theme.roninYellow		= "#FF9E3B"
Theme.crystalBlue		= "#7E9CD8"
Theme.springGreen		= "#98BB6C"
Theme.surimiOrange	= "#FFA065"

-- Border
Theme.useless_gap		= dpi(2)
Theme.border_width	= dpi(1)
Theme.border_normal = "#1e3440"
Theme.border_focus  = Theme.crystalBlue
Theme.border_marked = "#2c3040"

-- Taglist Theme
Theme.taglist_fg_focus = Theme.bg
Theme.taglist_bg_focus = Theme.roninYellow

-- Clock
local mytextclock = wibox.widget.textclock()
mytextclock.font = Theme.font

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

	-- We need one layoutbox per screen.
	s.mylayoutbox = awful.widget.layoutbox(s)
	s.mylayoutbox:buttons(gears.table.join(
		awful.button({ }, 1, function () awful.layout.inc( 1) end),
		awful.button({ }, 3, function () awful.layout.inc(-1) end),
		awful.button({ }, 4, function () awful.layout.inc( 1) end),
		awful.button({ }, 5, function () awful.layout.inc(-1) end)))
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
			s.mytaglist,
			s.mypromptbox,
		},
		nil,
		{ -- Right widgets
			layout = wibox.layout.fixed.horizontal,
			wibox.widget.systray(),
			mytextclock,
			s.mylayoutbox,
		}
	}
end)

return Theme
