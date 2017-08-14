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
  StrUtils,
  proc_globdata,
  proc_cmd,
  formkeys;

function DoDialogHotkeys(ACmd: integer; const ALexerName: string): boolean;
function DoDialogHotkeys(const AModuleAndMethod: string; const ALexerName: string): boolean;


implementation

function DoDialogHotkeys(ACmd: integer; const ALexerName: string): boolean;
var
  n: integer;
  Form: TfmKeys;
  StrId: string;
begin
  Result:= false;
  if (ACmd>=cmdFirstLexerCommand) and
     (ACmd<=cmdLastLexerCommand) then exit;

  n:= AppKeymap.IndexOf(ACmd);
  if n<0 then exit;

  //number (usual ACmd) or
  //'module,proc' (plugin)
  StrId:= IntToStr(AppKeymap[n].Command);

  if (ACmd>=cmdFirstPluginCommand) and
     (ACmd<=cmdLastPluginCommand) then
    with FPluginsCmd[ACmd-cmdFirstPluginCommand] do
      StrId:= ItemModule+','+ItemProc+IfThen(ItemProcParam<>'', ','+ItemProcParam);

  Form:= TfmKeys.Create(nil);
  try
    DoLocalize_FormKeys(Form);
    Form.Caption:= Form.Caption+' - '+AppKeymap[n].Name;
    Form.LexerName:= ALexerName;
    Form.CommandCode:= ACmd;
    Form.Keys1:= AppKeymap[n].Keys1;
    Form.Keys2:= AppKeymap[n].Keys2;

    Result:= Form.ShowModal=mrOk;
    if Result then
    begin
      AppKeymap[n].Keys1:= Form.Keys1;
      AppKeymap[n].Keys2:= Form.Keys2;
      DoOps_SaveKeyItem(AppKeymap[n], StrId,
        ALexerName, Form.chkForLexer.Checked);
    end;
  finally
    Form.Free
  end;
end;


function DoDialogHotkeys(const AModuleAndMethod: string;
  const ALexerName: string): boolean;
var
  N: integer;
begin
  Result:= false;
  N:= CommandPlugins_GetIndexFromModuleAndMethod(AModuleAndMethod);
  if N<0 then exit;

  Result:= DoDialogHotkeys(N+cmdFirstPluginCommand, ALexerName);
end;


end.

