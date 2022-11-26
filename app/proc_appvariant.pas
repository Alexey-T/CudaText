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
  SysUtils, Types,
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
  Result.Typ:= avrBool;
  Result.Bool:= Value;
end;

function AppVariant(const Value: Int64): TAppVariant;
begin
  Result:= Default(TAppVariant);
  Result.Typ:= avrInt;
  Result.Int:= Value;
end;

function AppVariant(const Value: string): TAppVariant;
begin
  Result:= Default(TAppVariant);
  Result.Typ:= avrStr;
  Result.Str:= Value;
end;

function AppVariant(const Value: array of integer): TAppVariant;
var
  i: integer;
begin
  Result:= Default(TAppVariant);
  Result.Typ:= avrTuple;
  SetLength(Result.Items, Length(Value));
  for i:= 0 to Length(Value)-1 do
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

function AppVariantToString(const V: TAppVariant; AndQuote: boolean=true): string;
var
  i: integer;
begin
  case V.Typ of
    avrNil:
      raise Exception.Create('Nil value in AppVariantToString');

    avrInt:
      Result:= IntToStr(V.Int);

    avrStr:
      Result:= SStringToPythonString(V.Str, AndQuote);

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
        for i:= 0 to Length(V.Items)-1 do
          Result+= '"'+V.Items[i].KeyName+'":'+AppVariantItemToString(V.Items[i])+',';
        Result+= '}';
      end;

    avrTuple:
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
      avdBool:
        Result:= PyBool_FromLong(Ord(V.Bool));
      avdInt:
        Result:= PyLong_FromLongLong(V.Int);
      avdStr:
        Result:= PyUnicodeFromString(V.Str);
      avdRect:
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
      avrNil:
        raise Exception.Create('Nil type in AppVariantToPyObject');

      avrInt:
        Result:= PyLong_FromLongLong(V.Int);

      avrStr:
        Result:= PyUnicodeFromString(V.Str);

      avrBool:
        Result:= PyBool_FromLong(Ord(V.Bool));

      avrDict:
        begin
          Result:= PyDict_New();
          NLen:= Length(V.Items);
          for i:= 0 to NLen-1 do
            PyDict_SetItemString(Result,
              PChar(string(V.Items[i].KeyName)),
              AppVariantItemToPyObject(V.Items[i])
              );
        end;

      avrTuple:
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
  AppVariantNil.Typ:= avrNil;

  {
  n:= SizeOf(TAppVariant);
  if n>0 then ;
  }

end.
