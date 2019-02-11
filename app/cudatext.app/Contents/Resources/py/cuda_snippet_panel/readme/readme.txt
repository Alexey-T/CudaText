plugin for CudaText.
gives command "Plugins - Snippets Panel" to show panel, in sidebar, similar to Clips panel from SynWrite editor. this panel shows drop-down list of folders, which contain several "clips"/"snippets". you can double-click snippets to insert them into text (multi-carets are supported).

preinstalled folders: 
- Arrows
- Currency symbols
- Greek alphabet (lower)
- Greek alphabet (upper)
- HTML - Arrows
- HTML - Color names
- HTML - Color names+values
- HTML - Letters
- HTML - Math symbols
- HTML - Special characters
- Math symbols
- Quote selection
- Special characters

snippet folders are searched in 2 places:
- folder "clips" in plugin folder (preinstalled)
- folder [CudaText]/data/clips, which is absent by default, for custom user folders.

each snippet folder can contain one or more .txt files, in UTF-8 (no BOM) or UTF-16 (with BOM) encoding. files have snippet per line, in the form "name=value" or simply "name" (if value missed, it equals to name).
each snippet can be simple short string, or string with ${sel} macro to replace selected text. this allows to quote currently selected text by calling snippets from "Quote selection" folder.


author: Alexey T. (CudaText)
license: MIT
