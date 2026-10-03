currently cudatext has weak place. on non-windows os. 
on windows: rendering is using ExtTextOutW (it is build in in os). no layers. direct calls of win32 api ExtTextOutW. 
so on windows, rendering is very fast.
on non-windows: rendering is using Lazarus' ExtTextOutW which has layers in LCL. so rendering is slow.

if you enable cudatext option "log_timing":true, it shows time of one frame rendeding. time is much bigger on non-windows.
especially its slow on macOS coz LCL has more heavy laeyrs there. maybe.

----------------
Plan. todo.
rework rendering in ATSynEdit.

current approach in ATSynEdit: we call DoPaintLine, which calls LCL ExtTextOut for each colored token/element.
for each colored token, ATSynEdit first setups the Canvas's FontColor/FontSize/FontStyles.

----------------
planned approach. part1.
instead of painting one token via ExtTextOut, collect token info
(font size, font color, font styles bold/italic, text, canvas X:Y pos) in a list.
so DoPaintLine must not textout, it must only collect tokens in the list TokensList.
after all DoPaintLine's for all visible lines ended, we may now render the collected list.
render it not like now: 1st token, 2nd token, etc, no, do rendering of the equal-colored-sized-styled
tokens in batch.
so, find first used style in TokensList, and do textout of all tokens with this style.
next, find next used style in TokensList, and do textout. etc.
so canvas prepearing runs only 1 for each uniq font-style.
its much faster.

--------------
part2.
make TokensList the collection of smaller lists PartsList, where each PartsList has only fragments 
with the same style.
in PartsList[i], we need only fields X, Y, Text;
and FontColor, FontStyle are now props of PartsList[i].
this will allow to faster find equal styles.
eg. TokensList has list of item[0] (all tokens with some first style), item[1] (all tokens with some next style) etc.
now rendering must loop over TokensList, prepare canvas 1 time for TokensList[i], do faster textout
of all items in TokensList[i].

---------------
part3.
change/add ATSynEdit units: 

- atsynedit_canvasproc_text_windows.pas
- atsynedit_canvasproc_text_gtk2.pas
- atsynedit_canvasproc_text_gtk3.pas
- atsynedit_canvasproc_text_qt5.pas
- atsynedit_canvasproc_text_qt6.pas
- atsynedit_canvasproc_text_cocoa.pas

for each unit:
a) add there procedure (very low level for widgetset) to setup canvas. (same name across all units)
b) add there proc (very low level for widgetset, e.g. Gtk3) to textout one string at given X:Y (again same name across all units).


Alexey Torgashin, 2026/10
