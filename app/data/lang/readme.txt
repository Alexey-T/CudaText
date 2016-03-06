how to make translation zip package:
- make LangName.ini, test it, use utf-8
- write your contact in 1st commented lines in ini
- make install.inf with such text:

[info]
title=LangName translation (by AuthorName)
type=cudatext-data
subdir=lang

- make zip file "translation.LangName.zip" with files LangName.ini, install.inf
- publish zip at Cudatext forum
- package will be in Addon Manager
