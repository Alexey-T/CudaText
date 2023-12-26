(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
unit proc_appvariant;

{$mode objfpc}{$H+}
{$ScopedEnums on}

interface

uses
  SysUtils, Types,
  PythonEngine,
  proc_str;

type
  TAppVariantTypeId = (
    Null,
    Bool,
    Int,
    Str,
    Tuple,
    Dict
    );

  TAppVariantItemTypeId = (
    Bool,
    Int,
    Str,
    Rect
    );

  TAppVariantItem = record
    Typ: TAppVariantItemTypeId;
    KeyName: string[15]; //enough len for Py API events
    case integer of
      0: (Bool: boolean);
      1: (Int: Int64);
      2: (Rect: TRect);
      3: (Str: string[15]);
  end;

  TAppVariant = record
    Typ: TAppVariantTypeId;
    Str: string;
    Items: array of TAppVariantItem;
    case integer of
      0: (Bool: boolean);
      1: (Int: Int64);
  end;

  TAppVariantArray = array of TAppVariant;

var
  AppVariantNil: TAppVariant;

function AppVariant(Value: boolean): TAppVariant; inline;
function AppVariant(const Value: Int64): TAppVariant; inline;
function AppVariant(const Value: string): TAppVariant; inline;
function AppVariant(const Value: array of integer): TAppVariant;
function AppVariantToString(const V: TAppVariant; AndQuote: boolean=true): string;
function AppVariantArrayToString(const V: TAppVariantArray): string;

procedure AppVariantInitializePython;
function AppVariantToPyObject(const V: TAppVariant): PPyObject;
function AppVariantArrayToPyObject(const V: TAppVariantArray): PPyObject;

implementation

var
  FEngine: TPythonEngine = nil;

procedure AppVariantInitializePython;
begin
  FEngine:= GetPythonEngine;
end;

function AppVariant(Value: boolean): TAppVariant;
begin
  Result:= Default(TAppVariant);
  Result.Typ:= TAppVariantTypeId.Bool;
  Result.Bool:= Value;
end;

function AppVariant(const Value: Int64): TAppVariant;
begin
  Result:= Default(TAppVariant);
  Result.Typ:= TAppVariantTypeId.Int;
  Result.Int:= Value;
end;

function AppVariant(const Value: string): TAppVariant;
begin
  Result:= Default(TAppVariant);
  Result.Typ:= TAppVariantTypeId.Str;
  Result.Str:= Value;
end;

function AppVariant(const Value: array of integer): TAppVariant;
var
  i: integer;
begin
  Result:= Default(TAppVariant);
  Result.Typ:= TAppVariantTypeId.Tuple;
  SetLength(Result.Items, Length(Value));
  for i:= 0 to Length(Value)-1 do
  begin
    Result.Items[i].Typ:= TAppVariantItemTypeId.Int;
    Result.Items[i].Int:= Value[i];
  end;
end;

function AppVariantItemToString(const V: TAppVariantItem): string;
begin
  case V.Typ of
    TAppVariantItemTypeId.Bool:
      begin
        if V.Bool then
          Result:= 'True'
        else
          Result:= 'False';
      end;

    TAppVariantItemTypeId.Int:
      Result:= IntToStr(V.Int);

    TAppVariantItemTypeId.Rect:
      Result:= Format('(%d,%d,%d,%d)', [V.Rect.Left, V.Rect.Top, V.Rect.Right, V.Rect.Bottom]);

    TAppVariantItemTypeId.Str:
      Result:= SStringToPythonString(V.Str);

    else
      raise Exception.Create('Unhandled type in AppVariantItemToString');
  end;
end;

function AppVariantToString(const V: TAppVariant; AndQuote: boolean=true): string;
var
  i: integer;
begin
  case V.Typ of
    TAppVariantTypeId.Null:
      raise Exception.Create('Nil value in AppVariantToString');

    TAppVariantTypeId.Int:
      Result:= IntToStr(V.Int);

    TAppVariantTypeId.Str:
      Result:= SStringToPythonString(V.Str, AndQuote);

    TAppVariantTypeId.Bool:
      begin
        if V.Bool then
          Result:= 'True'
        else
          Result:= 'False';
      end;

    TAppVariantTypeId.Dict:
      begin
        Result:= '{';
        for i:= 0 to Length(V.Items)-1 do
          Result+= '"'+V.Items[i].KeyName+'":'+AppVariantItemToString(V.Items[i])+',';
        Result+= '}';
      end;

    TAppVariantTypeId.Tuple:
      begin
        Result:= '(';
        for i:= 0 to Length(V.Items)-1 do
          Result+= AppVariantItemToString(V.Items[i])+',';
        Result+= ')';
      end;

    else
      raise Exception.Create('Unhandled type in AppVariantToString');
  end;
end;

function AppVariantArrayToString(const V: TAppVariantArray): string;
var
  i: integer;
begin
  Result:= '';
  for i:= 0 to Length(V)-1 do
    Result+= AppVariantToString(V[i])+',';
  if Result<>'' then
    SetLength(Result, Length(Result)-1);
end;

function AppVariantItemToPyObject(const V: TAppVariantItem): PPyObject;
begin
  with FEngine do
    case V.Typ of
      TAppVariantItemTypeId.Bool:
        Result:= PyBool_FromLong(Ord(V.Bool));
      TAppVariantItemTypeId.Int:
        Result:= PyLong_FromLongLong(V.Int);
      TAppVariantItemTypeId.Str:
        Result:= PyUnicodeFromString(V.Str);
      TAppVariantItemTypeId.Rect:
        Result:= Py_BuildValue('(iiii)', V.Rect.Left, V.Rect.Top, V.Rect.Right, V.Rect.Bottom);
      else
        raise Exception.Create('Unhandled item in AppVariantItemToPyObject');
    end;
end;

function AppVariantToPyObject(const V: TAppVariant): PPyObject;
var
  NLen, i: integer;
begin
  with FEngine do
    case V.Typ of
      TAppVariantTypeId.Null:
        raise Exception.Create('Nil type in AppVariantToPyObject');

      TAppVariantTypeId.Int:
        Result:= PyLong_FromLongLong(V.Int);

      TAppVariantTypeId.Str:
        Result:= PyUnicodeFromString(V.Str);

      TAppVariantTypeId.Bool:
        Result:= PyBool_FromLong(Ord(V.Bool));

      TAppVariantTypeId.Dict:
        begin
          Result:= PyDict_New();
          NLen:= Length(V.Items);
          for i:= 0 to NLen-1 do
            PyDict_SetItemString(Result,
              PChar(string(V.Items[i].KeyName)),
              AppVariantItemToPyObject(V.Items[i])
              );
        end;

      TAppVariantTypeId.Tuple:
        begin
          NLen:= Length(V.Items);
          Result:= PyTuple_New(NLen);
          for i:= 0 to NLen-1 do
            PyTuple_SetItem(Result, i,
              AppVariantItemToPyObject(V.Items[i])
              );
        end;

      else
        raise Exception.Create('Unhandled type in AppVariantToPyObject');
    end;
end;

function AppVariantArrayToPyObject(const V: TAppVariantArray): PPyObject;
var
  i: integer;
begin
  with FEngine do
  begin
    Result:= PyTuple_New(Length(V));
    for i:= 0 to Length(V)-1 do
      PyTuple_SetItem(Result, i, AppVariantToPyObject(V[i]));
  end;
end;

{
var
  n: integer;
  }

initialization

  FillChar(AppVariantNil, SizeOf(AppVariantNil), 0);
  AppVariantNil.Typ:= TAppVariantTypeId.Null;

  {
  n:= SizeOf(TAppVariant);
  if n>0 then ;
  }

end.
