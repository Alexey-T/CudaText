Plugin for CudaText.
Gives command "Options Editor" - dialog for configuring all options in CudaText (they are listed in default.json, and are read/parsed from there by plugin).

Options are grouped by sections, which are read from default.json.
By default saves options to user.json, but checkbox "For Lexer" allows to choose lexer-specific config.

Options are shown in a table, after clicking an option you can see its default value, and change current value (or press "Reset" to change to default value).

- For boolean opts, checkbox is shown to change.
- For string/number opts, input is shown to change.
- For opts with limited count of values, combobox is used to choose one variant.
- For font-name opts, combobox is shown (it lists special OS-dependent "default" font and OS fonts).

Dialog gives "=" hamburger button with advanvced commands. For ex, command to create HTML report for options. And you can toggle option "Instant filtering" there, so you don't need to press Enter in the filter field.


Author: Andrey Kvichanskiy (kvichans, at forum/github)
License: MIT
