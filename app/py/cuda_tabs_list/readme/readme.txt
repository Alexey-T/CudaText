Plugin for CudaText.
Shows "Tabs" panel in the side-panel, with a list of all opened editor-tabs.

Features:
- The list is visually grouped by editor groups (e.g., "Group 1", "Floating 1").
- Click a tab in the list to focus it.
- Drag-and-drop tabs to reorder them, even between different groups.
- Context menu (right-click) for closing tabs or copying file paths.
- Updates automatically when a file is opened, closed, or its title changes.


Here is a description for each of the configuration options in cuda_tabs_list.ini.

[op]
These are the main operational settings for the plugin.
	• show_index_group=1
		○ Shows the editor group index (e.g., 1:, 2:, or f1: for floating groups) before the tab name.
		○ Set to 1 to show the group index, or 0 to hide it.
	• show_index_tab=1
		○ Shows the tab's position number within its group (e.g., 1., 2.) before the tab name.
		○ Set to 1 to show the tab index, or 0 to hide it.
	• show_index_aligned=0
		○ If show_index_tab is enabled, this will right-align the tab numbers by adding padding. This makes 1. and 10. line up vertically.
		○ Set to 1 to align the numbers, or 0 for default (left) alignment.
	• font_name=default
		○ Specifies the font family used in the tab list and the filter input box.
		○ Keep as default to use the system UI font, or enter a specific font name like Consolas.
	• font_size=9
		○ Sets the font size for the list and filter box.
		○ Set to a number representing the font's point size.
	• auto_scroll_speed=1
		○ Controls how fast the list scrolls (in items per tick) when you drag a tab to the top or bottom edge.
		○ Set to a small number like 1 or 2 for slow scrolling, or a larger number like 10 for fast scrolling.


[columns]
These settings control the optional columns in the list.
	• width_name=170
		○ The width of the main column that shows the tab/file name.
		○ Set to a positive number (e.g., 170) for pixels, 0 to auto-stretch, or a negative number for a percentage.
	• width_folder=0
		○ The width of the "Folder" column (if enabled).
		○ Set to a positive number for pixels, 0 to auto-stretch, or a negative number for a percentage.
	• width_lexer=80
		○ The width of the "Lexer" column (if enabled), which shows the file's syntax type (e.g., "Python").
		○ Set to a positive number for pixels, 0 to auto-stretch, or a negative number for a percentage.
	• show_folder=0
		○ Toggles the visibility of the "Folder" column, which shows the file's parent directory.
		○ Set to 1 to show this column, or 0 to hide it.
	• show_lexer=0
		○ Toggles the visibility of the "Lexer" column, which shows the file's type.
		○ Set to 1 to show this column, or 0 to hide it.

Authors:
- Alexey Torgashin (CudaText)
- Badr Elmers, https://github.com/badrelmers
License: MIT
