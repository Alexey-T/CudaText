unit proc_json_ex;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, at__jsonConf, at__fpjson;

type

  TJSONConfigEx = class(TJSONConfig)
  public
    procedure SetModified(AValue: Boolean);
    function GetJsonObj: TJSONObject;
  end;

  function IsJsonObjEqual(const AObj1: TJSONData; const AObj2: TJSONData; const AKeysToIgnore: array of UTF8String): Boolean;
  function CloneJsonObj(const AObj: TJSONData; const AKeysToIgnore: array of UTF8String): TJSONData;

implementation

function IsStrInArr(const AStr: UTF8String; const AArr: array of UTF8String): boolean;
var
  i: integer;
begin
  for i := 0 to length(AArr)-1 do
    if AStr = AArr[i] then
      Exit(true);
  Result := false;
end;

procedure TJSONConfigEx.SetModified(AValue: Boolean);
begin
  FModified := AValue;
end;

function TJSONConfigEx.GetJsonObj: TJSONObject;
begin
  Result := FJSON;
end;

function IsJsonObjEqual(const AObj1: TJSONData; const AObj2: TJSONData; const AKeysToIgnore: array of UTF8String): Boolean;
var
  i: integer;
  skip: boolean;
  obj1, obj2: TJSONObject;
  num1, num2: TJSONNumber;
begin
  if AObj1.JSONType <> AObj2.JSONType then
    Exit(false);

  Result := true;

  case AObj1.JSONType of
    jtObject:
      begin
        obj1 := AObj1 as TJSONObject;
        obj2 := AObj2 as TJSONObject;
        if obj1.Count <> obj2.Count then
          Exit(false);

        for i := 0 to obj1.Count-1 do
        begin
          if obj1.Names[i] <> obj2.Names[i] then
            Exit(false);

          skip := IsStrInArr(obj1.Names[i], AKeysToIgnore);
          if not skip then
            if not IsJsonObjEqual(obj1.Items[i], obj2.Items[i], AKeysToIgnore) then
              Exit(false);
        end;
      end;

    jtArray:
      begin
        if AObj1.Count <> AObj2.Count then
          Exit(false);

        for i := 0 to AObj1.Count-1 do
        begin
          if not IsJsonObjEqual(AObj1.Items[i], AObj2.Items[i], AKeysToIgnore) then
            Exit(false);
        end;
      end;

    jtNumber:
      begin
        num1 := AObj1 as TJSONNumber;
        num2 := AObj2 as TJSONNumber;
        if num1.NumberType <> num2.NumberType then
          Exit(false);

        case num1.NumberType of
          ntInteger:
            Result := num1.AsInteger = num2.AsInteger;
          ntInt64:
            Result := num1.AsInt64 = num2.AsInt64;
          ntQWord:
            Result := num1.AsQWord = num2.AsQWord;
          ntFloat:
            Result := num1.AsFloat = num2.AsFloat;
        end;
      end;

    jtString:
      Result := AObj1.AsString = AObj2.AsString;

    jtBoolean:
      Result := AObj1.AsBoolean = AObj2.AsBoolean;

  end; // case

end;

function CloneJsonObj(const AObj: TJSONData; const AKeysToIgnore: array of UTF8String): TJSONData;
var
  i: integer;
  skip: boolean;
  obj: TJSONObject;
  resObj: TJSONObject;
  resArr: TJSONArray;
begin
  Result := nil;

  case AObj.JSONType of
    jtObject:
      begin
        resObj := TJSONObject.Create;
        obj := AObj as TJSONObject;

        for i := 0 to obj.Count-1 do
        begin
          skip := IsStrInArr(obj.Names[i], AKeysToIgnore);
          if not skip then
            resObj.Add(obj.Names[i], CloneJsonObj(obj.Items[i], AKeysToIgnore))
          else
            resObj.Add(obj.Names[i], TJSONNull.Create);
        end;

        Result := resObj;
      end;

    jtArray:
      begin
        resArr := TJSONArray.Create;
        for i := 0 to AObj.Count-1 do
        begin
          resArr.Add(CloneJsonObj(AObj.Items[i], AKeysToIgnore));
        end;

        Result := resArr;
      end;

    jtNumber,
    jtString,
    jtBoolean,
    jtNull:
      Result := AObj.Clone;

  end; // case

end;

end.

