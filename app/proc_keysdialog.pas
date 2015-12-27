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

function DoDialogHotkeys(ACmd: integer): boolean;
function DoDialogHotkeys(const AModuleAndMethod: string): boolean;


implementation

function DoDialogHotkeys(ACmd: integer): boolean;
var
  n: integer;
  StrId: string;
begin
  Result:= false;
  if (ACmd>=cmdFirstLexerCommand) and
     (ACmd<=cmdLastLexerCommand) then exit;

  n:= keymap.IndexOf(ACmd);
  if n<0 then exit;

  //number (usual ACmd) or
  //'module,proc' (plugin)
  StrId:= IntToStr(keymap[n].Command);

  if (ACmd>=cmdFirstPluginCommand) and
     (ACmd<=cmdLastPluginCommand) then
    with FPluginsCmd[ACmd-cmdFirstPluginCommand] do
      StrId:= ItemModule+','+ItemProc+IfThen(ItemProcParam<>'', ','+ItemProcParam);

  with TfmKeys.Create(nil) do
  try
    Caption:= 'Keys - '+keymap[n].Name;
    Keys1:= keymap[n].Keys1;
    Keys2:= keymap[n].Keys2;

    Result:= ShowModal=mrOk;
    if Result then
    begin
      keymap[n].Keys1:= Keys1;
      keymap[n].Keys2:= Keys2;
      DoSaveKeyItem(keymap[n], StrId);
    end;
  finally
    Free
  end;
end;


function DoDialogHotkeys(const AModuleAndMethod: string): boolean;
var
  N: integer;
begin
  Result:= false;
  N:= CommandPlugins_GetIndexFromModuleAndMethod(AModuleAndMethod);
  if N<0 then exit;

  Result:= DoDialogHotkeys(N+cmdFirstPluginCommand);
end;

end.

