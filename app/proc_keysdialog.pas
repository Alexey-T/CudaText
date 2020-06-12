(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
unit proc_keysdialog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Forms,
  ATSynEdit_Keymap,
  proc_globdata,
  proc_cmd,
  formkeys;

function DoDialogHotkeys(AKeymap: TATKeymap; ACmd: integer; const ALexerName: string): boolean;
function DoDialogHotkeys(AKeymap: TATKeymap; const AModuleAndMethod: string; const ALexerName: string): boolean;


implementation

function DoDialogHotkeys(AKeymap: TATKeymap; ACmd: integer; const ALexerName: string): boolean;
var
  Form: TfmKeys;
  StrId: string;
  bForLexer: boolean;
  n: integer;
begin
  Result:= false;
  if not AppCommandHasConfigurableHotkey(ACmd) then exit;

  n:= AKeymap.IndexOf(ACmd);
  if n<0 then exit;

  StrId:= DoOps_CommandCode_To_HotkeyStringId(ACmd);

  Form:= TfmKeys.Create(nil);
  try
    Form.Caption:= Form.Caption+' - '+AKeymap[n].Name;
    Form.LexerName:= ALexerName;
    Form.CommandCode:= ACmd;
    Form.Keymap:= AKeymap;
    Form.Keys1:= AKeymap[n].Keys1;
    Form.Keys2:= AKeymap[n].Keys2;

    Result:= Form.ShowModal=mrOk;
    if Result then
    begin
      AKeymap[n].Keys1:= Form.Keys1;
      AKeymap[n].Keys2:= Form.Keys2;
      bForLexer:= Form.chkForLexer.Checked;

      DoOps_SaveKeyItem(AKeymap[n], StrId, ALexerName, bForLexer);
    end;
  finally
    Form.Free
  end;
end;


function DoDialogHotkeys(AKeymap: TATKeymap;
  const AModuleAndMethod: string;
  const ALexerName: string): boolean;
var
  N: integer;
begin
  Result:= false;
  N:= CommandPlugins_GetIndexFromModuleAndMethod(AModuleAndMethod);
  if N<0 then exit;

  Result:= DoDialogHotkeys(AKeymap, N+cmdFirstPluginCommand, ALexerName);
end;


end.

