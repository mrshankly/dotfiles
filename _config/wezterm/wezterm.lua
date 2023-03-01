local wezterm = require("wezterm")

local background = "#ebdbb2"
local foreground = "#665c54"

local active_background = "#7c6f64"
local active_foreground = "#fbf1c7"

local inactive_background = "#d5c4a1"
local inactive_foreground = "#665c54"

local hover_background = "#bdae93"
local hover_foreground = foreground

return {
	font = wezterm.font("JetBrains Mono NL"),
	font_size = 13.0,

	enable_scroll_bar = true,
	scrollback_lines = 100000,

	hide_tab_bar_if_only_one_tab = true,

	color_scheme = "Gruvbox Light",
	colors = {
		scrollbar_thumb = inactive_background,
		tab_bar = {
			active_tab = {
				bg_color = active_background,
				fg_color = active_foreground,
			},
			inactive_tab = {
				bg_color = inactive_background,
				fg_color = inactive_foreground,
			},
			inactive_tab_hover = {
				bg_color = hover_background,
				fg_color = hover_foreground,
			},
			new_tab = {
				bg_color = background,
				fg_color = foreground,
			},
			new_tab_hover = {
				bg_color = hover_background,
				fg_color = hover_foreground,
			},
		},
	},

	window_frame = {
		font = wezterm.font({ family = "JetBrains Mono NL", weight = "Bold" }),
		font_size = 11.0,
		active_titlebar_bg = background,
		inactive_titlebar_bg = background,
	},
}
