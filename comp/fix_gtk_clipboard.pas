unit fix_gtk_clipboard;

{$mode objfpc}{$H+}

interface

uses
  gtk2, gdk2, Clipbrd;

implementation

var
  c: PGtkClipboard;
  t: string;

finalization
  c := gtk_clipboard_get(GDK_SELECTION_CLIPBOARD);
  t := Clipboard.AsText;
  gtk_clipboard_set_text(c, PChar(t), Length(t));
  gtk_clipboard_store(c);
end.
