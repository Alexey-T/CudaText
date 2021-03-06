(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
{$ifdef nn}begin end;{$endif}

type
  TATCssPythonProvider = class(TATCssProvider)
  public
    procedure GetProps(L: TStringList); override;
    procedure GetValues(const AProp: string; L: TStringList); override;
  end;

{ TATCssPythonProvider }

procedure TATCssPythonProvider.GetProps(L: TStringList);
var
  FParamObjs: array of PPyObject;
  FParamNames: array of string;
  Obj: PPyObject;
  NCount, i: integer;
begin
  L.Clear;

  SetLength(FParamObjs, 0);
  SetLength(FParamNames, 0);

  Obj:= AppPython.RunModuleFunction('cudax_css', 'get_props', FParamObjs, FParamNames);
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

procedure TATCssPythonProvider.GetValues(const AProp: string; L: TStringList);
var
  FParamObjs: array of PPyObject;
  FParamNames: array of string;
  Obj: PPyObject;
  NCount, i: integer;
begin
  L.Clear;

  SetLength(FParamObjs, 1);
  SetLength(FParamNames, 1);
  FParamObjs[0]:= AppVariantToPyObject(AppVariant(AProp));
  FParamNames[0]:= 'name';

  Obj:= AppPython.RunModuleFunction('cudax_css', 'get_values', FParamObjs, FParamNames);
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

