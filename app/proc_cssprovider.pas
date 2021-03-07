(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
unit proc_cssprovider;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes,
  ATSynEdit_Cmp_CSS,
  ATSynEdit_Cmp_CSS_Provider;

type
  TATCssPythonProvider = class(TATCssProvider)
  public
    procedure GetProps(L: TStringList); override;
    procedure GetValues(const AProp: string; L: TStringList); override;
  end;

implementation

uses
  PythonEngine,
  proc_appvariant,
  proc_py;

{ TATCssPythonProvider }

procedure TATCssPythonProvider.GetProps(L: TStringList);
begin
  GetValues('', L);
end;

procedure TATCssPythonProvider.GetValues(const AProp: string; L: TStringList);
var
  FParamObjs: array[0..0] of PPyObject;
  FParamNames: array[0..0] of string;
  Obj: PPyObject;
  NCount, i: integer;
begin
  L.Clear;

  FParamObjs[0]:= AppVariantToPyObject(AppVariant(AProp));
  FParamNames[0]:= 'name';

  Obj:= AppPython.RunModuleFunction('cudax_css', 'get_data', FParamObjs, FParamNames);
  if Assigned(Obj) then
    with AppPython.Engine do
    begin
      if PyList_Check(Obj) then
      begin
        NCount:= PyList_Size(Obj);
        for i:= 0 to NCount-1 do
          L.Add(PyObjectAsString(PyList_GetItem(Obj, i)));
      end;
      Py_DECREF(Obj);
    end;
end;

end.

