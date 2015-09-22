{
    This file is part of the Free Component Library

    Implementation of TJSONConfig class
    Copyright (c) 2007 Michael Van Canneyt michael@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{
  TJSONConfig enables applications to use JSON files for storing their
  configuration data
}

{$IFDEF FPC}
{$MODE objfpc}
{$H+}
{$ENDIF}

unit jsonConf;

interface

uses
  SysUtils, Classes, fpjson, jsonscanner,jsonparser;


type
  EJSONConfigError = class(Exception);

(* ********************************************************************
   "APath" is the path and name of a value: A JSON configuration file 
   is hierachical. "/" is the path delimiter, the part after the last 
   "/" is the name of the value. The path components will be mapped 
   to nested JSON objects, with the name equal to the part. In practice 
   this means that "/my/path/value" will be written as:
   { 
     "my" : {
       "path" : {
         "value" : Value
       }
     }
   }
   ******************************************************************** *)

  { TJSONConfig }

  TJSONConfig = class(TComponent)
  private
    FFilename: String;
    FFormatIndentSize: Integer;
    FFormatoptions: TFormatOptions;
    FFormatted: Boolean;
    FKey: TJSONObject;
    procedure DoSetFilename(const AFilename: String; ForceReload: Boolean);
    procedure SetFilename(const AFilename: String);
    Function StripSlash(Const P : UnicodeString) : UnicodeString;
  protected
    FJSON: TJSONObject;
    FModified: Boolean;
    procedure Loaded; override;
    function FindPath(Const APath: UnicodeString; AllowCreate : Boolean) : TJSONObject;
    function FindObject(Const APath: UnicodeString; AllowCreate : Boolean) : TJSONObject;
    function FindObject(Const APath: UnicodeString; AllowCreate : Boolean;Out ElName : UnicodeString) : TJSONObject;
    function FindElement(Const APath: UnicodeString; CreateParent : Boolean; AllowObject : Boolean = False) : TJSONData;
    function FindElement(Const APath: UnicodeString; CreateParent : Boolean; out AParent : TJSONObject; Out ElName : UnicodeString; AllowObject : Boolean = False) : TJSONData;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    Procedure Reload;
    procedure Clear;
    procedure Flush;    // Writes the JSON file
    procedure OpenKey(const aPath: UnicodeString; AllowCreate : Boolean);
    procedure CloseKey;
    procedure ResetKey;
    Procedure EnumSubKeys(Const APath : UnicodeString; List : TStrings);
    Procedure EnumValues(Const APath : UnicodeString; List : TStrings);

    function  GetValue(const APath: UnicodeString; const ADefault: UnicodeString): UnicodeString; overload;
    function  GetValue(const APath: UnicodeString; ADefault: Integer): Integer; overload;
    function  GetValue(const APath: UnicodeString; ADefault: Int64): Int64; overload;
    function  GetValue(const APath: UnicodeString; ADefault: Boolean): Boolean; overload;
    function  GetValue(const APath: UnicodeString; ADefault: Double): Double; overload;
    Function GetValue(const APath: UnicodeString; AValue: TStrings; Const ADefault: String) : Boolean; overload;
    Function GetValue(const APath: UnicodeString; AValue: TStrings; Const ADefault: TStrings): Boolean; overload;
    procedure SetValue(const APath: UnicodeString; const AValue: UnicodeString); overload;
    procedure SetValue(const APath: UnicodeString; AValue: Integer); overload;
    procedure SetValue(const APath: UnicodeString; AValue: Int64); overload;
    procedure SetValue(const APath: UnicodeString; AValue: Boolean); overload;
    procedure SetValue(const APath: UnicodeString; AValue: Double); overload;
    procedure SetValue(const APath: UnicodeString; AValue: TStrings; AsObject : Boolean = False); overload;

    procedure SetDeleteValue(const APath: UnicodeString; const AValue, DefValue: UnicodeString); overload;
    procedure SetDeleteValue(const APath: UnicodeString; AValue, DefValue: Integer); overload;
    procedure SetDeleteValue(const APath: UnicodeString; AValue, DefValue: Int64); overload;
    procedure SetDeleteValue(const APath: UnicodeString; AValue, DefValue: Boolean); overload;

    procedure DeletePath(const APath: UnicodeString);
    procedure DeleteValue(const APath: UnicodeString);
    property Modified: Boolean read FModified;
  published
    Property Filename: String read FFilename write SetFilename;
    Property Formatted : Boolean Read FFormatted Write FFormatted;
    Property FormatOptions : TFormatOptions Read FFormatoptions Write FFormatOptions Default DefaultFormat;
    Property FormatIndentsize : Integer Read FFormatIndentSize Write FFormatIndentSize Default DefaultIndentSize;
  end;


// ===================================================================

implementation

Resourcestring
  SErrInvalidJSONFile = '"%s" is not a valid JSON configuration file.';
  SErrCouldNotOpenKey = 'Could not open key "%s".';

constructor TJSONConfig.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FJSON:=TJSONObject.Create;
  FKey:=FJSON;
  FFormatOptions:=DefaultFormat;
  FFormatIndentsize:=DefaultIndentSize;
end;

destructor TJSONConfig.Destroy;
begin
  if Assigned(FJSON) then
    begin
    Flush;
    FreeANdNil(FJSON);
    end;
  inherited Destroy;
end;

procedure TJSONConfig.Clear;
begin
  FJSON.Clear;
  FKey:=FJSON;
end;

procedure TJSONConfig.Flush;

Var
  F : Text;
  S : TJSONStringType;
  
begin
  if Modified then
    begin
    AssignFile(F,FileName);
    Rewrite(F);
    Try
      if Formatted then
        S:=FJSON.FormatJSON(Formatoptions,FormatIndentSize)
      else
        S:=FJSON.AsJSON;
      Writeln(F,S);  
    Finally
      CloseFile(F);
    end;
    FModified := False;
    end;
end;


function TJSONConfig.FindObject(const APath: UnicodeString; AllowCreate: Boolean
  ): TJSONObject;

Var
  Dummy : UnicodeString;

begin
  Result:=FindObject(APath,AllowCreate,Dummy);
end;

function TJSONConfig.FindObject(const APath: UnicodeString; AllowCreate: Boolean;
  out ElName: UnicodeString): TJSONObject;

Var
  S,El : UnicodeString;
  P,I : Integer;
  T : TJSonObject;
  
begin
//  Writeln('Looking for : ', APath);
  S:=APath;
  If Pos('/',S)=1 then
    Result:=FJSON
  else
    Result:=FKey;
  Repeat
    P:=Pos('/',S);
    If (P<>0) then
      begin
      // Only real paths, ignore double slash
      If (P<>1) then
        begin
        El:=Copy(S,1,P-1);
        If (Result.Count=0) then
          I:=-1
        else
          I:=Result.IndexOfName(El);
        If (I=-1) then
          // No element with this name.
          begin
          If AllowCreate then
            begin
            // Create new node.
            T:=Result;
            Result:=TJSonObject.Create;
            T.Add(El,Result);
            end
          else
            Result:=Nil
          end
        else
          // Node found, check if it is an object
          begin
          if (Result.Items[i].JSONtype=jtObject) then
            Result:=Result.Objects[el]
          else
            begin
//            Writeln(el,' type wrong');
            If AllowCreate then
              begin
//              Writeln('Creating ',el);
              Result.Delete(I);
              T:=Result;
              Result:=TJSonObject.Create;
              T.Add(El,Result);
              end
            else
              Result:=Nil
            end;
          end;
        end;
      Delete(S,1,P);
      end;
  Until (P=0) or (Result=Nil);
  ElName:=S;
end;

function TJSONConfig.FindElement(const APath: UnicodeString; CreateParent: Boolean; AllowObject : Boolean = False): TJSONData;

Var
  O : TJSONObject;
  ElName : UnicodeString;
  
begin
  Result:=FindElement(APath,CreateParent,O,ElName,AllowObject);
end;

function TJSONConfig.FindElement(const APath: UnicodeString;
  CreateParent: Boolean; out AParent: TJSONObject; out ElName: UnicodeString;
  AllowObject : Boolean = False): TJSONData;

Var
  I : Integer;

begin
  Result:=Nil;
  Aparent:=FindObject(APath,CreateParent,ElName);
  If Assigned(Aparent) then
    begin
//    Writeln('Found parent, looking for element:',elName);
    I:=AParent.IndexOfName(ElName);
//    Writeln('Element index is',I);
    If (I<>-1) And ((AParent.items[I].JSONType<>jtObject) or AllowObject) then
      Result:=AParent.Items[i];
    end;
//  Writeln('Find ',aPath,' in "',FJSON.AsJSOn,'" : ',Elname,' : ',Result<>NIl);
end;


function TJSONConfig.GetValue(const APath: UnicodeString; const ADefault: UnicodeString): UnicodeString;

var
  El : TJSONData;
  
begin
  El:=FindElement(StripSlash(APath),False);
  If Assigned(El) then
    Result:=UTF8Decode(El.AsString)
  else
    Result:=ADefault;
end;

function TJSONConfig.GetValue(const APath: UnicodeString; ADefault: Integer): Integer;
var
  El : TJSONData;
  
begin
  El:=FindElement(StripSlash(APath),False);
  If Not Assigned(el) then
    Result:=ADefault
  else if (el is TJSONNumber) then
    Result:=El.AsInteger
  else
    Result:=StrToIntDef(El.AsString,ADefault);
end;

function TJSONConfig.GetValue(const APath: UnicodeString; ADefault: Int64): Int64;
var
  El : TJSONData;

begin
  El:=FindElement(StripSlash(APath),False);
  If Not Assigned(el) then
    Result:=ADefault
  else if (el is TJSONNumber) then
    Result:=El.AsInt64
  else
    Result:=StrToInt64Def(El.AsString,ADefault);
end;

function TJSONConfig.GetValue(const APath: UnicodeString; ADefault: Boolean): Boolean;

var
  El : TJSONData;
  
begin
  El:=FindElement(StripSlash(APath),False);
  If Not Assigned(el) then
    Result:=ADefault
  else if (el is TJSONBoolean) then
    Result:=El.AsBoolean
  else
    Result:=StrToBoolDef(El.AsString,ADefault);
end;

function TJSONConfig.GetValue(const APath: UnicodeString; ADefault: Double): Double;

var
  El : TJSONData;

begin
  El:=FindElement(StripSlash(APath),False);
  If Not Assigned(el) then
    Result:=ADefault
  else if (el is TJSONNumber) then
    Result:=El.AsFloat
  else
    Result:=StrToFloatDef(El.AsString,ADefault);
end;

function TJSONConfig.GetValue(const APath: UnicodeString; AValue: TStrings;
  const ADefault: String): Boolean;
var
  El : TJSONData;
  D : TJSONEnum;

begin
  AValue.Clear;
  El:=FindElement(StripSlash(APath),False,True);
  Result:=Assigned(el);
  If Not Result then
    begin
    AValue.Text:=ADefault;
    exit;
    end;
  Case El.JSONType of
    jtArray:
      For D in El do
        if D.Value.JSONType in ActualValueJSONTypes then
          AValue.Add(D.Value.AsString);
    jtObject:
      For D in El do
        if D.Value.JSONType in ActualValueJSONTypes then
          AValue.Add(D.Key+'='+D.Value.AsString);
  else
    AValue.Text:=EL.AsString
  end;

end;

function TJSONConfig.GetValue(const APath: UnicodeString; AValue: TStrings;
  const ADefault: TStrings): Boolean;
begin
  Result:=GetValue(APath,AValue,'');
  If Not Result then
    AValue.Assign(ADefault);
end;


procedure TJSONConfig.SetValue(const APath: UnicodeString; const AValue: UnicodeString);

var
  El : TJSONData;
  ElName : UnicodeString;
  O : TJSONObject;
  I : integer;
  
begin
  El:=FindElement(StripSlash(APath),True,O,ElName);
  if Assigned(El) and (El.JSONType<>jtString) then
    begin
    I:=O.IndexOfName(elName);
    O.Delete(i);
    El:=Nil;
    end;
  If Not Assigned(el) then
    begin
    El:=TJSONString.Create(UTF8encode(AValue));
    O.Add(ElName,El);
    end
  else
    El.AsString:=UTF8Encode(AValue);
  FModified:=True;
end;

procedure TJSONConfig.SetDeleteValue(const APath: UnicodeString; const AValue, DefValue: UnicodeString);
begin
  if AValue = DefValue then
    DeleteValue(APath)
  else
    SetValue(APath, AValue);
end;

procedure TJSONConfig.SetValue(const APath: UnicodeString; AValue: Integer);

var
  El : TJSONData;
  ElName : UnicodeString;
  O : TJSONObject;
  I : integer;

begin
  El:=FindElement(StripSlash(APath),True,O,ElName);
  if Assigned(El) and (Not (El is TJSONIntegerNumber)) then
    begin
    I:=O.IndexOfName(elName);
    If (I<>-1) then // Normally not needed...
      O.Delete(i);
    El:=Nil;
    end;
  If Not Assigned(el) then
    begin
    El:=TJSONIntegerNumber.Create(AValue);
    O.Add(ElName,El);
    end
  else
    El.AsInteger:=AValue;
  FModified:=True;
end;

procedure TJSONConfig.SetValue(const APath: UnicodeString; AValue: Int64);

var
  El : TJSONData;
  ElName : UnicodeString;
  O : TJSONObject;
  I : integer;

begin
  El:=FindElement(StripSlash(APath),True,O,ElName);
  if Assigned(El) and (Not (El is TJSONInt64Number)) then
    begin
    I:=O.IndexOfName(elName);
    If (I<>-1) then // Normally not needed...
      O.Delete(i);
    El:=Nil;
    end;
  If Not Assigned(el) then
    begin
    El:=TJSONInt64Number.Create(AValue);
    O.Add(ElName,El);
    end
  else
    El.AsInt64:=AValue;
  FModified:=True;
end;

procedure TJSONConfig.SetDeleteValue(const APath: UnicodeString; AValue,
  DefValue: Integer);
begin
  if AValue = DefValue then
    DeleteValue(APath)
  else
    SetValue(APath, AValue);
end;

procedure TJSONConfig.SetDeleteValue(const APath: UnicodeString; AValue,
  DefValue: Int64);
begin
  if AValue = DefValue then
    DeleteValue(APath)
  else
    SetValue(APath, AValue);
end;

procedure TJSONConfig.SetValue(const APath: UnicodeString; AValue: Boolean);

var
  El : TJSONData;
  ElName : UnicodeString;
  O : TJSONObject;
  I : integer;

begin
  El:=FindElement(StripSlash(APath),True,O,ElName);
  if Assigned(El) and (el.JSONType<>jtBoolean) then
    begin
    I:=O.IndexOfName(elName);
    O.Delete(i);
    El:=Nil;
    end;
  If Not Assigned(el) then
    begin
    El:=TJSONBoolean.Create(AValue);
    O.Add(ElName,El);
    end
  else
    El.AsBoolean:=AValue;
  FModified:=True;
end;

procedure TJSONConfig.SetValue(const APath: UnicodeString; AValue: Double);

var
  El : TJSONData;
  ElName : UnicodeString;
  O : TJSONObject;
  I : integer;

begin
  El:=FindElement(StripSlash(APath),True,O,ElName);
  if Assigned(El) and (Not (El is TJSONFloatNumber)) then
    begin
    I:=O.IndexOfName(elName);
    O.Delete(i);
    El:=Nil;
    end;
  If Not Assigned(el) then
    begin
    El:=TJSONFloatNumber.Create(AValue);
    O.Add(ElName,El);
    end
  else
    El.AsFloat:=AValue;
  FModified:=True;
end;

procedure TJSONConfig.SetValue(const APath: UnicodeString; AValue: TStrings; AsObject : Boolean = False);
var
  El : TJSONData;
  ElName : UnicodeString;
  O : TJSONObject;
  I : integer;
  A : TJSONArray;
  N,V : String;
  DoDelete: Boolean;

begin
  El:=FindElement(StripSlash(APath),True,O,ElName,True);
  if Assigned(El) then
    begin
    if AsObject then
      DoDelete:=(Not (El is TJSONObject))
    else
      DoDelete:=(Not (El is TJSONArray));
    if DoDelete then
      begin
      I:=O.IndexOfName(elName);
      O.Delete(i);
      El:=Nil;
      end;
    end;
  If Not Assigned(el) then
    begin
    if AsObject then
      El:=TJSONObject.Create
    else
      El:=TJSONArray.Create;
    O.Add(ElName,El);
    end;
  if Not AsObject then
    begin
    A:=El as TJSONArray;
    A.Clear;
    For N in Avalue do
      A.Add(N);
    end
  else
    begin
    O:=El as TJSONObject;
    For I:=0 to AValue.Count-1 do
      begin
      AValue.GetNameValue(I,N,V);
      O.Add(N,V);
      end;
    end;
  FModified:=True;
end;

procedure TJSONConfig.SetDeleteValue(const APath: UnicodeString; AValue,
  DefValue: Boolean);
begin
  if AValue = DefValue then
    DeleteValue(APath)
  else
    SetValue(APath,AValue);
end;

procedure TJSONConfig.DeletePath(const APath: UnicodeString);

Var
  P : String;
  L : integer;
  Node : TJSONObject;
  ElName : UnicodeString;
  
begin
  P:=StripSlash(APath);
  L:=Length(P);
  If (L>0) then
    begin
    Node := FindObject(P,False,ElName);
    If Assigned(Node) then
      begin
      L:=Node.IndexOfName(ElName);
      If (L<>-1) then
        Node.Delete(L);
      end;
    end;
end;

procedure TJSONConfig.DeleteValue(const APath: UnicodeString);

begin
  DeletePath(APath);
end;

procedure TJSONConfig.Reload;

begin
  if Length(Filename) > 0 then
    DoSetFilename(Filename,True);
end;
procedure TJSONConfig.Loaded;
begin
  inherited Loaded;
  Reload;
end;

function TJSONConfig.FindPath(const APath: UnicodeString; AllowCreate: Boolean
  ): TJSONObject;
  
Var
  P : UnicodeString;
  L : Integer;
  
begin
  P:=APath;
  L:=Length(P);
  If (L=0) or (P[L]<>'/') then
    P:=P+'/';
  Result:=FindObject(P,AllowCreate);
end;

procedure TJSONConfig.DoSetFilename(const AFilename: String; ForceReload: Boolean);

Var
  P : TJSONParser;
  J : TJSONData;
  F : TFileStream;
  
begin
  if (not ForceReload) and (FFilename = AFilename) then
    exit;
  FFilename := AFilename;

  if csLoading in ComponentState then
    exit;

  Flush;
  If Not FileExists(AFileName) then
    Clear
  else
    begin
    F:=TFileStream.Create(AFileName,fmopenRead);
    try
      P:=TJSONParser.Create(F,[joUTF8,joComments]);
      try
        J:=P.Parse;
        If (J is TJSONObject) then
          begin
          FreeAndNil(FJSON);
          FJSON:=J as TJSONObject;
          FKey:=FJSON;
          end
        else
          Raise EJSONConfigError.CreateFmt(SErrInvalidJSONFile,[AFileName]);
      finally
        P.Free;
      end;
    finally
      F.Free;
    end;
    end;
end;

procedure TJSONConfig.SetFilename(const AFilename: String);
begin
  DoSetFilename(AFilename, False);
end;

function TJSONConfig.StripSlash(const P: UnicodeString): UnicodeString;

Var
  L : Integer;

begin
  L:=Length(P);
  If (L>0) and (P[l]='/') then
    Result:=Copy(P,1,L-1)
  else
    Result:=P;
end;


procedure TJSONConfig.CloseKey;
begin
  ResetKey;
end;

procedure TJSONConfig.OpenKey(const aPath: UnicodeString; AllowCreate: Boolean);

Var
  P : String;
  L : Integer;
begin
  P:=APath;
  L:=Length(P);
  If (L=0) then
    FKey:=FJSON
  else
    begin
    if (P[L]<>'/') then
      P:=P+'/';
    FKey:=FindObject(P,AllowCreate);
    If (FKey=Nil) Then
      Raise EJSONConfigError.CreateFmt(SErrCouldNotOpenKey,[APath]);
    end;
end;

procedure TJSONConfig.ResetKey;
begin
  FKey:=FJSON;
end;

procedure TJSONConfig.EnumSubKeys(const APath: UnicodeString; List: TStrings);

Var
  AKey : TJSONObject;
  I : Integer;
  
begin
  AKey:=FindPath(APath,False);
  If Assigned(AKey) then
    begin
    For I:=0 to AKey.Count-1 do
      If AKey.Items[i] is TJSONObject then
        List.Add(AKey.Names[i]);
    end;
end;

procedure TJSONConfig.EnumValues(const APath: UnicodeString; List: TStrings);

Var
  AKey : TJSONObject;
  I : Integer;

begin
  AKey:=FindPath(APath,False);
  If Assigned(AKey) then
    begin
    For I:=0 to AKey.Count-1 do
      If Not (AKey.Items[i] is TJSONObject) then
        List.Add(AKey.Names[i]);
    end;
end;


end.
