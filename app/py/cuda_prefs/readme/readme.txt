Plugin for CudaText.
Gives command "Options Editor Lite" - dialog for configuring all options in CudaText (they are listed in default.json, and are read/parsed from there by plugin).

Options are grouped by sections, which are read from default.json.
By default saves options to user.json (Scope: User), but Lexer scope can be chosen to set lexer-specific config.

Options are shown in a table, after clicking an option you can see its current value and scope.

- For boolean opts, checkbox is shown to change.
- For string/number opts, input is shown to change.
- For opts with limited count of values, combobox is used to choose one variant.
- For font-name opts, combobox is shown (it lists special OS-dependent "default" font and OS fonts).

Clicking on the list header displays a menu to choose visible columns and configure widths.


Authors: 
  Initial version: Andrey Kvichanskiy (kvichans, at forum/github)
  Lite version:    halfbrained        (https://github.com/halfbrained)
License: MIT
