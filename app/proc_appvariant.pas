(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
unit proc_appvariant;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Types, Math,
  PythonEngine,
  proc_str;

type
  TAppVariantTypeId = (
    avrNil,
    avrBool,
    avrInt,
    avrStr,
    avrTuple,
    avrDict
    );

  TAppVariantItemTypeId = (
    avdBool,
    avdInt,
    avdStr,
    avdRect
    );

  TAppVariantItem = record
    KeyName: string[15];
    case Typ: TAppVariantItemTypeId of
      avdBool: (Bool: boolean);
      avdInt: (Int: Int64);
      avdRect: (Rect: TRect);
      avdStr: (Str: string[10]);
  end;

  TAppVariant = record
    case Typ: TAppVariantTypeId of
      avrBool: (Bool: boolean);
      avrInt: (Int: Int64);
      avrStr: (Str: string[100]);
      avrDict: (Len: integer; Items: array[0..6] of TAppVariantItem);
  end;

  TAppVariantArray = array of TAppVariant;

const
  AppVariantNil: TAppVariant = (
    Typ: avrNil;
    Int: 0
    );

function AppVariant(Value: boolean): TAppVariant; inline;
function AppVariant(const Value: Int64): TAppVariant; inline;
function AppVariant(const Value: string): TAppVariant; inline;
function AppVariant(const Value: array of integer): TAppVariant;
function AppVariantToString(const V: TAppVariant): string;
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
  FillChar(Result, SizeOf(Result), 0);
  Result.Typ:= avrBool;
  Result.Bool:= Value;
end;

function AppVariant(const Value: Int64): TAppVariant;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.Typ:= avrInt;
  Result.Int:= Value;
end;

function AppVariant(const Value: string): TAppVariant;
begin
  FillChar(Result, SizeOf(Result), 0);
  if Length(Value)>SizeOf(Result.Str)-1 then
    raise Exception.Create('Too long str in AppVariant');
  Result.Typ:= avrStr;
  Result.Str:= Value;
end;

function AppVariant(const Value: array of integer): TAppVariant;
var
  i: integer;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.Typ:= avrTuple;
  Result.Len:= Length(Value);
  for i:= 0 to Min(Length(Value), Length(Result.Items))-1 do
  begin
    Result.Items[i].Typ:= avdInt;
    Result.Items[i].Int:= Value[i];
  end;
end;

function AppVariantItemToString(const V: TAppVariantItem): string;
begin
  case V.Typ of
    avdBool:
      begin
        if V.Bool then
          Result:= 'True'
        else
          Result:= 'False';
      end;

    avdInt:
      Result:= IntToStr(V.Int);

    avdRect:
      Result:= Format('(%d,%d,%d,%d)', [V.Rect.Left, V.Rect.Top, V.Rect.Right, V.Rect.Bottom]);

    avdStr:
      Result:= SStringToPythonString(V.Str);

    else
      raise Exception.Create('Unhandled type in AppVariantItemToString');
  end;
end;

function AppVariantToString(const V: TAppVariant): string;
var
  i: integer;
begin
  case V.Typ of
    avrNil:
      raise Exception.Create('Nil value in AppVariantToString');

    avrInt:
      Result:= IntToStr(V.Int);

    avrStr:
      Result:= SStringToPythonString(V.Str);

    avrBool:
      begin
        if V.Bool then
          Result:= 'True'
        else
          Result:= 'False';
      end;

    avrDict:
      begin
        Result:= '{';
        for i:= 0 to V.Len-1 do
          Result+= '"'+V.Items[i].KeyName+'":'+AppVariantItemToString(V.Items[i])+',';
        Result+= '}';
      end;

    avrTuple:
      begin
        Result:= '(';
        for i:= 0 to V.Len-1 do
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
      avdBool:
        Result:= PyBool_FromLong(Ord(V.Bool));
      avdInt:
        Result:= PyLong_FromLongLong(V.Int);
      avdStr:
        Result:= PyString_FromString(PChar(string(V.Str)));
      avdRect:
        Result:= Py_BuildValue('(iiii)', V.Rect.Left, V.Rect.Top, V.Rect.Right, V.Rect.Bottom);
      else
        raise Exception.Create('Unhandled item in AppVariantItemToPyObject');
    end;
end;

function AppVariantToPyObject(const V: TAppVariant): PPyObject;
var
  i: integer;
begin
  with FEngine do
    case V.Typ of
      avrNil:
        raise Exception.Create('Nil type in AppVariantToPyObject');

      avrInt:
        Result:= PyLong_FromLongLong(V.Int);

      avrStr:
        Result:= PyString_FromString(PChar(string(V.Str)));

      avrBool:
        Result:= PyBool_FromLong(Ord(V.Bool));

      avrDict:
        begin
          Result:= PyDict_New();
          for i:= 0 to V.Len-1 do
            PyDict_SetItemString(Result,
              PChar(string(V.Items[i].KeyName)),
              AppVariantItemToPyObject(V.Items[i])
              );
        end;

      avrTuple:
        begin
          Result:= PyTuple_New(V.Len);
          for i:= 0 to V.Len-1 do
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

end.
