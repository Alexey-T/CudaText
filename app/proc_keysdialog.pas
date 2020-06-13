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
  CmdIndex, i: integer;
  Map: TATKeymap;
begin
  Result:= false;
  if not AppCommandHasConfigurableHotkey(ACmd) then exit;

  CmdIndex:= AKeymap.IndexOf(ACmd);
  if CmdIndex<0 then exit;

  StrId:= DoOps_CommandCode_To_HotkeyStringId(ACmd);

  Form:= TfmKeys.Create(nil);
  try
    Form.Caption:= Form.Caption+': '+AKeymap[CmdIndex].Name;
    Form.LexerName:= ALexerName;
    Form.CommandCode:= ACmd;
    Form.Keymap:= AKeymap;
    Form.Keys1:= AKeymap[CmdIndex].Keys1;
    Form.Keys2:= AKeymap[CmdIndex].Keys2;
    Form.chkForLexer.Enabled:= ALexerName<>'?';

    Result:= Form.ShowModal=mrOk;
    if Result then
    begin
      bForLexer:= Form.chkForLexer.Checked;
      if bForLexer then
      begin
        //apply to caller keymap
        AKeymap[CmdIndex].Keys1:= Form.Keys1;
        AKeymap[CmdIndex].Keys2:= Form.Keys2;
        AKeymap[CmdIndex].LexerSpecific:= true;

        //save to 'keys nn.json'
        DoOps_SaveKeyItem(AKeymap[CmdIndex], StrId, ALexerName, true);
      end
      else
      begin
        //apply to caller keymap
        AKeymap[CmdIndex].Keys1:= Form.Keys1;
        AKeymap[CmdIndex].Keys2:= Form.Keys2;
        AKeymap[CmdIndex].LexerSpecific:= false;

        //apply to main keymap
        AppKeymapMain[CmdIndex].Keys1:= Form.Keys1;
        AppKeymapMain[CmdIndex].Keys2:= Form.Keys2;

        //apply to all lexer keymaps
        for i:= 0 to AppKeymapLexers.Count-1 do
        begin
          Map:= TATKeymap(AppKeymapLexers.Objects[i]);
          if not Map[CmdIndex].LexerSpecific then
          begin
            Map[CmdIndex].Keys1:= Form.Keys1;
            Map[CmdIndex].Keys2:= Form.Keys2;
          end;
        end;

        //delete in 'keys nn.json'
        DoOps_DeleteKeyItem(AKeymap[CmdIndex], StrId, ALexerName, true);

        //save to keys.json
        DoOps_SaveKeyItem(AppKeymapMain[CmdIndex], StrId, '', false);
      end;
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
  N:= CommandPlugins_GetIndexFromModuleAndMethod(AModuleAndMethod);
  if N>=0 then
    Result:= DoDialogHotkeys(AKeymap, N+cmdFirstPluginCommand, ALexerName)
  else
    Result:= false;
end;


end.

