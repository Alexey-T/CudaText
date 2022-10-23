//License: seems Public Domain, it's copy/pasted from Lazarus forum
unit fix_gtk_clipboard;

{$mode objfpc}{$H+}

interface

procedure FixClipboardFinalization;

implementation

uses
  gtk2, gdk2, Clipbrd;

procedure FixClipboardFinalization;
var
  c: PGtkClipboard;
  t: string;
begin
  c := gtk_clipboard_get(GDK_SELECTION_CLIPBOARD);
  t := Clipboard.AsText;
  gtk_clipboard_set_text(c, PChar(t), Length(t));
  gtk_clipboard_store(c);
end;

end.
