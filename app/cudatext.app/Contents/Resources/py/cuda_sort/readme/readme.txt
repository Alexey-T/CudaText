Plugin for CudaText.
Gives commands + dialog (with sorting options) to sort lines.

Gives also other commands: 
- Reverse lines
- Shuffle lines
- Extract duplicate lines
- Extract duplicate lines, ignore case
- Extract unique lines
- Remove duplicate lines
- Remove duplicate lines + origins
- Remove adjacent duplicate lines
- Remove blank lines
- Remove adjacent blank lines
- Ini file: sort sections + keys
- Ini file: sort sections without keys
- Sort e-mail list by domain - sorts entire file as list of e-mails, first sorts by domain after "@", then by name before "@"

To allow handle all text without selection, set the option: call plugin config by "Options / Settings-plugins / Sort / Config", and change/add option in .ini file:
[op]
allow_all=1


Author: Alexey (CudaText)
License: MIT
