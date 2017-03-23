{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2016 by Michael Van Canneyt

    Converter unit to convert JSON object to object pascal classes.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit fpjsontopas;

// TODO : Array of Array LoadFromJSON/SaveToJSON

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser;

Type
  EJSONToPascal = Class(EJSON);

  { TPropertyMapItem }
  TPropertyMapItem = Class(TCollectionItem)
  private
    FGenerated: Boolean;
    FJSONType: TJSONType;
    FParentTypeName: String;
    FPath: String;
    FPropertyName: String;
    FSkipType: Boolean;
    FTypeName: String;
  Public
    Procedure Assign(Source: TPersistent); override;
    Property Generated : Boolean Read FGenerated;
  Published
    Property Path : String Read FPath Write FPath;
    Property TypeName : String Read FTypeName Write FTypeName;
    Property ParentTypeName : String Read FParentTypeName Write FParentTypeName;
    Property PropertyName : String Read FPropertyName Write FPropertyName;
    Property JSONType : TJSONType Read FJSONType write FJSONType;
    // Set this to true if no class/array should be generated
    Property SkipType : Boolean Read FSkipType Write FSkipType;
  end;

  TPropertyMap = Class(TCollection)
  private
    function GetM(Aindex : Integer): TPropertyMapItem;
    procedure SetM(Aindex : Integer; AValue: TPropertyMapItem);
  Public
    Function AddPath(Const APath,ATypeName : String) : TPropertyMapItem;
    Function IndexOfPath(Const APath : String) : Integer;
    Function FindPath(Const APath : String) : TPropertyMapItem;
    Property Map[Aindex : Integer] : TPropertyMapItem Read GetM Write SetM; Default;
  end;

  { TJSONToPascal }
  TJSONToPascalOption = (jpoUseSetter,jpoGenerateLoad,jpoUnknownLoadPropsError,jpoDelphiJSON, jpoLoadCaseInsensitive,jpoGenerateSave);
  TJSONToPascalOptions = set of TJSONToPascalOption;

  TJSONToPascal = Class(TComponent)
  private
    FExtraUnitNames: String;
    FFieldPrefix: String;
    FIndent : String;
    FActive : Boolean;
    FCode : TStrings;
    FDefaultParentName : String;
    FDestUnitName : String;
    FIndentSize : Integer;
    FJSON : TJSONStringType;
    FJSONData: TJSONData;
    FJSONStream: TStream;
    FObjectConstructorArguments: String;
    FOptions: TJSONToPascalOptions;
    FPropertyMap: TPropertyMap;
    FPropertyTypeSuffix: String;
    FinType : Boolean; //  State
    procedure GenerateSaveFunctionForm(M: TPropertyMapItem);
    function GetObjectConstructorArguments: String;
    function JSONDataName: String;
    procedure MaybeEmitType;
    procedure SetActive(AValue: Boolean);
    procedure SetCode(AValue: TStrings);
    procedure SetJSON(AValue: TJSONStringType);
    procedure SetPropertyMap(AValue: TPropertyMap);
  Protected
    Procedure AddSemiColonToLastLine;
    Procedure Indent;
    Procedure Undent;
    Procedure AddLn(Const Line : String);
    Procedure AddLn(Const Fmt : String; Const Args : Array of const);
    Procedure AddIndented(Const Line : String);
    Procedure AddIndented(Const Fmt : String; Const Args : Array of const);
    Function CreatePropertyMap : TPropertyMap; virtual;
    Function GetJSONData(Out FreeResult : Boolean) : TJSONData; virtual;
    function IsDateTimeValue(const AValue: String): Boolean; virtual;
    Function GetDefaultParentName : String;
    function GetPropertyTypeName(const APath, AName: String; AValue: TJSONData): String; virtual;
    function PathToTypeName(const APath: String): String; virtual;
    function AddToPath(const APath, AName: String): String;
    class function CleanPropertyName(const AName: String): string;
    function GetPropertyName(const APath, AName: String): String;

    // Called for each type
    function  GenerateAssign(IM: TPropertyMapItem; AVarName, AJSONName: String ): String;
    function  GenerateAssignDelphi(IM: TPropertyMapItem; AVarName, AJSONName: String; AddSemiColon : Boolean ): String;
    procedure GenerateCreateArray(M: TPropertyMapItem);
    procedure GenerateSaveArray(M: TPropertyMapItem);
    procedure GenerateCreateObjectfpJSON(M: TPropertyMapItem);
    procedure GenerateLoadJSONDelphi(M: TPropertyMapItem; J: TJSONObject);
    procedure GenerateLoadJSONfpJSON(M: TPropertyMapItem; J: TJSONObject);
    procedure GenerateSaveJSONDelphi(M: TPropertyMapItem; J: TJSONObject);
    procedure GenerateSaveJSONfpJSON(M: TPropertyMapItem; J: TJSONObject);
    Function  GenerateArrayDeclaration(M: TPropertyMapItem; J: TJSONArray) : Boolean; virtual;
    procedure GenerateObjectDeclaration(M: TPropertyMapItem;  J: TJSONObject); virtual;
    procedure GenerateArrayImplementation(M : TPropertyMapItem; J: TJSONArray); virtual;
    procedure GenerateObjectImplementation(M : TPropertyMapItem; J: TJSONObject); virtual;
    // Top level routines
    Function  GetExtraUnitNames : String; virtual;
    Procedure ClearGeneratedTypes;virtual;
    Procedure GenerateInterfaceHeader;virtual;
    procedure GenerateDeclaration(const APath : String; J: TJSONData);  virtual;
    Procedure GenerateImplementationHeader;virtual;
    Procedure GenerateImplementation(const APath: String; J: TJSONData); virtual;
    Procedure GenerateImplementationEnd;virtual;
  Public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
    Procedure Execute;
    // JSON Data to generate code from.
    Property JSONData : TJSONData Read FJSONData Write FJSONData;
    // JSON Data (in stream form) to generate code from. JSONData takes prioroty over this property.
    Property JSONStream : TStream Read FJSONStream Write FJSONStream;
  Published
    // Setting this to true will call execute. Can be used to generate code in the IDE.
    Property Active : Boolean Read FActive Write SetActive;
    // Options to use.
    Property Options : TJSONToPascalOptions Read FOptions Write FOptions;
    // The JSON to use. JSONData/JSONStream take priority over this property.
    Property JSON : TJSONStringType Read FJSON Write SetJSON;
    // This string
    Property Code : TStrings Read FCode Write SetCode;
    // Type information for generated types. After Execute, this will contain generated/detected types for all properties.
    Property PropertyMap : TPropertyMap Read FPropertyMap Write SetPropertyMap;
    // Generated unit name.
    Property DestUnitName : String Read FDestUnitName Write FDestUnitName;
    // Default Parent class name when declaring objects. Can be overridden per property.
    Property DefaultParentName: String Read FDefaultParentName Write FDefaultParentName;
    // Indent size
    Property IndentSize : Integer Read FIndentSize Write FIndentSize default 2;
    // These units (comma separated list) will be added to the interface uses clause.
    Property ExtraUnitNames : String Read FExtraUnitNames Write FExtraUnitNames;
    // This will be suffixed to an object/array type name when the propert map is constructed.
    Property PropertyTypeSuffix : String Read FPropertyTypeSuffix Write FPropertyTypeSuffix;
    // First letter for field name.
    Property FieldPrefix : String Read FFieldPrefix Write FFieldPrefix;
    // What are the arguments to a constructor ? This property is inserted literally in the code between ().
    Property ObjectConstructorArguments : String Read FObjectConstructorArguments Write FObjectConstructorArguments;
  end;



implementation

{$IFDEF VER2_6_4}
Const
  StructuredJSONTypes  = [jtArray,jtObject];
{$ENDIF}

{ TPropertyMap }

function TPropertyMap.GetM(Aindex : Integer): TPropertyMapItem;
begin
  Result:=Items[AIndex] as TPropertyMapItem;
end;

procedure TPropertyMap.SetM(Aindex : Integer; AValue: TPropertyMapItem);
begin
  Items[AIndex]:=AValue;
end;

function TPropertyMap.AddPath(const APath, ATypeName: String): TPropertyMapItem;
begin
  Result:=Add as TPropertyMapItem;
  Result.Path:=APath;
  Result.TypeName:=ATypeName;
end;

function TPropertyMap.IndexOfPath(const APath: String): Integer;
begin
  Result:=Count-1;
  While (Result>=0) and (GetM(Result).Path<>APath) do
    Dec(Result);
end;

function TPropertyMap.FindPath(const APath: String): TPropertyMapItem;

Var
  I : Integer;

begin
  I:=IndexOfPath(APath);
  If I=-1 then
    Result:=Nil
  else
    Result:=GetM(I);
end;

{ TJSONToPascal }

class function TJSONToPascal.CleanPropertyName(const AName: String): string;

Const
   KW=';absolute;and;array;asm;begin;case;const;constructor;destructor;div;do;'+
       'downto;else;end;file;for;function;goto;if;implementation;in;inherited;'+
       'inline;interface;label;mod;nil;not;object;of;on;operator;or;packed;'+
       'procedure;program;record;reintroduce;repeat;self;set;shl;shr;string;then;'+
       'to;type;unit;until;uses;var;while;with;xor;dispose;exit;false;new;true;'+
       'as;class;dispinterface;except;exports;finalization;finally;initialization;'+
       'inline;is;library;on;out;packed;property;raise;resourcestring;threadvar;try;'+
       'private;published;length;setlength;';
Var
  I : Integer;

begin
  Result:=Aname;
  For I:=Length(Result) downto 1 do
    If Not ((Upcase(Result[i]) in ['_','A'..'Z'])
             or ((I>1) and (Result[i] in (['0'..'9'])))) then
     Delete(Result,i,1);
  if Pos(';'+lowercase(Result)+';',KW)<>0 then
   Result:='_'+Result
end;

procedure TJSONToPascal.SetActive(AValue: Boolean);
begin
  if (FActive=AValue) then Exit;
  if AValue then
    Execute;
end;

procedure TJSONToPascal.SetCode(AValue: TStrings);
begin
  if FCode=AValue then Exit;
  FCode.Assign(AValue);
end;

procedure TJSONToPascal.SetJSON(AValue: TJSONStringType);
begin
  if FJSON=AValue then Exit;
  FJSON:=AValue;
end;

procedure TJSONToPascal.SetPropertyMap(AValue: TPropertyMap);
begin
  if FPropertyMap=AValue then Exit;
  FPropertyMap.Assign(AValue);
end;

procedure TJSONToPascal.AddSemiColonToLastLine;

Var
  I : Integer;

begin
  I:=FCode.Count-1;
  FCode[I]:=FCode[I]+';'
end;

procedure TJSONToPascal.Indent;
begin
  FIndent:=Findent+StringOfChar(' ',FIndentSize);
end;

procedure TJSONToPascal.Undent;

Var
  L : Integer;

begin
  L:=Length(FIndent);
  Dec(L,FIndentSize);
  if L<0 then L:=0;
  FIndent:=Copy(FIndent,1,L);
end;

procedure TJSONToPascal.AddLn(const Line: String);
begin
  FCode.Add(FIndent+Line);
end;

procedure TJSONToPascal.AddLn(const Fmt: String; const Args: array of const);
begin
  AddLn(Format(Fmt,Args));
end;

procedure TJSONToPascal.AddIndented(const Line: String);
begin
  Indent;
  AddLn(Line);
  Undent;
end;

procedure TJSONToPascal.AddIndented(const Fmt: String;
  const Args: array of const);
begin
  Indent;
  AddLn(Fmt,Args);
  Undent;
end;

constructor TJSONToPascal.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCode:=TStringList.Create;
  FPropertyMap:=CreatePropertyMap;
  FIndentSize:=2;
  FFieldPrefix:='F';
end;

destructor TJSONToPascal.Destroy;
begin
  FreeAndNil(FCode);
  FreeAndNil(FPropertyMap);
  inherited Destroy;
end;

function TJSONToPascal.CreatePropertyMap: TPropertyMap;

begin
  Result:=TPropertyMap.Create(TPropertyMapItem);
end;

function TJSONToPascal.GetJSONData(out FreeResult: Boolean): TJSONData;

Var
  D : TJSONData;

begin
  FreeResult:=not Assigned(FJSONData);
  if Not FreeResult then
    Exit(FJSONData);
  Result:=Nil;
  If Assigned(JSONStream) then
    D:=GetJSON(JSONStream)
  else if (JSON<>'') then
    D:=GetJSON(JSON)
  else
    Raise EJSONToPascal.Create('Need one of JSONObject, JSONStream or JSON to be set');
  If Not (D.JSONType in [jtObject,jtArray]) then
    begin
    FreeAndNil(D);
    Raise EJSONToPascal.Create('Provided JSONStream or JSON is not a JSON Object or array');
    end;
  Result:=D;
end;

function TJSONToPascal.GetExtraUnitNames: String;
begin
  Result:=FExtraUnitNames;
end;

procedure TJSONToPascal.ClearGeneratedTypes;

Var
  I : integer;

begin
  For i:=FPropertyMap.Count-1 downto 0 do
    if FPropertyMap[i].Generated then
      FPropertyMap.Delete(I);
end;

procedure TJSONToPascal.GenerateInterfaceHeader;

Var
  S: string;
begin
  AddLn('unit %s;',[DestUnitName]);
  Addln('');
  Addln('interface');
  Addln('');
  S:=Trim(GetExtraUnitNames);
  if (S<>'') and (S[1]<>',') then
    S:=', '+S;
  if jpoDelphiJSON in Options then
    S:='JSON'+S
  else
    S:='fpJSON'+S;
  S:='SysUtils, Classes, '+S;
  Addln('uses %s;',[s]);
  Addln('');
end;


function TJSONToPascal.PathToTypeName(const APath: String): String;

begin
  Result:=StringReplace(Apath,'.','',[rfReplaceAll]);
  Result:=StringReplace(Result,'[0]','Item',[rfReplaceAll]);
  Result:=StringReplace(Result,'[]','Item',[rfReplaceAll]);
  if Result='' then
    Result:='TMyObject'
  else
    Result:='T'+Result+PropertyTypeSuffix;
end;

function TJSONToPascal.IsDateTimeValue(const AValue: String): Boolean;

Var
  D : TDateTime;

begin
  Result:=TryStrToDate(AValue,D);
  if Not Result then
    Result:=TryStrToTime(AValue,D);
  if Not Result then
    Result:=TryStrToDateTime(AValue,D);
end;

function TJSONToPascal.GetDefaultParentName: String;
begin
  Result:=FDefaultParentName;
  if Result='' then
    Result:='TObject';
end;

Resourcestring
  SErrCannotDetermineType = 'Cannot determine type for %s : Not in type map';
  SErrCannotDeterminePropertyType = 'Cannot determine property type for %s';
  SErrCannotGenerateArrayDeclaration = 'Cannot generate array declaration from empty array at "%s"';

function TJSONToPascal.GetPropertyTypeName(const APath, AName: String; AValue: TJSONData): String;

Var
  M : TPropertyMapItem;
  IP : String;

begin
  Case AValue.JSONType of
    jtBoolean : Result:='Boolean';
    jtNull : Result:='Boolean';
    jtNumber :
      Case TJSONNumber(AValue).NumberType of
        ntFloat : Result:='Double';
        ntInt64 : Result:='Int64';
        ntInteger : Result:='Integer';
      end;
    jtString :
      if not IsDateTimeValue(AValue.AsString) then
        Result:='String'
      else
        Result:='TDateTime';
    jtArray:
      begin
      IP:=AddToPath(APath,AName);
      M:=FPropertyMap.FindPath(IP);
      If (M=Nil) then
        raise EJSONToPascal.CreateFmt(SErrCannotDetermineType, [IP]);
      if M.TypeName='' then
        M.TypeName:='Array of '+GetPropertyTypeName(AddToPath(APath,AName)+'[0]','Item',TJSONArray(AValue)[0]);
      Result:=M.TypeName;
      end;
    jtObject :
      begin
      M:=FPropertyMap.FindPath(AddToPath(APath,AName));
      If (M=Nil) then // Can happen in case of [ [ {} ] ]
        M:=FPropertyMap.AddPath(AddToPath(APath,AName),'');
//        Raise EJSONToPascal.CreateFmt('Cannot determine type for %s.%s : Not in type map',[APath,AName]);
      if M.TypeName='' then
        M.TypeName:=PathToTypeName(AddToPath(APath,AName));
      if M.ParentTypeName='' then
         M.ParentTypeName:=GetDefaultParentName;
      Result:=M.TypeName;
      end;
  end;
end;

function TJSONToPascal.GetPropertyName(const APath, AName: String): String;

begin
  Result:=CleanPropertyName(AName);
end;

function TJSONToPascal.JSONDataName: String;

begin
  if jpoDelphiJSON in options then
    Result:='TJSONValue'
  else
    Result:='TJSONData';
end;

function TJSONToPascal.GenerateArrayDeclaration(M: TPropertyMapItem;
  J: TJSONArray): Boolean;

Var
  IP : String;
  IM : TPropertyMapItem;
  B : Boolean;

begin
  Result:=False;
  IP:=AddToPath(M.Path,'[0]');
  IM:=FPropertyMap.FindPath(IP);
  AddLn('%s = Array of %s;',[M.TypeName,IM.TypeName]);
  B:=([jpoGenerateLoad,jpoGenerateSave] * options)<>[];
  if B then
    begin
    Undent;
    AddLn('');
    end;
  if jpoGenerateLoad in options then
    AddLn('Function Create%s(AJSON : %s) : %s;',[M.TypeName,JSONDataName,M.TypeName]);
  if jpoGenerateSave in options then
    begin
    AddLn('Procedure Save%sToJSON(AnArray : %s; AJSONArray : TJSONArray); overload;',[M.TypeName,M.TypeName]);
    AddLn('Function Save%sToJSON(AnArray : %s) : TJSONArray; overload;',[M.TypeName,M.TypeName]);
    end;
  AddLn('');
  if B then
    begin
    Indent;
    FinType:=False;
    Result:=True;
    end;
end;

procedure TJSONToPascal.GenerateObjectDeclaration(M : TPropertyMapItem; J: TJSONObject);

Var
  E : TJSONEnum;
  IM :  TPropertyMapItem;
  IP, FRN,FWN : String;
  HaveObj : Boolean;

begin
  HaveObj:=False;
  Addln('');
  AddLn('{ -----------------------------------------------------------------------');
  Addln('  '+M.TypeName);
  AddLn('  -----------------------------------------------------------------------}');
  Addln('');
  AddLn('%s = class(%s)',[M.TypeName,M.ParentTypeName]);
  Addln('Private');
  Indent;
  For E in J do
    begin
    IM:=FPropertyMap.FindPath(AddToPath(M.Path,E.Key));
    If IM=Nil then
      begin
      IM:=FPropertyMap.Add as TPropertyMapItem;
      IM.Path:=AddToPath(M.Path,E.Key);
      IM.FGenerated:=True;
      end;
    if IM.TypeName='' then
      IM.TypeName:=GetPropertyTypeName(M.Path,E.Key,E.Value);
    if IM.PropertyName='' then
      IM.PropertyName:=GetPropertyName(M.Path,E.Key);
    IM.JSONType:=E.Value.JSONtype;
    AddLn('F%s : %s;',[IM.PropertyName,IM.TypeName]);
    HaveObj:=HaveObj or (IM.JSONType=jtObject);
    end;
  Undent;
  if jpoUseSetter in Options then
    begin
    Addln('Protected');
    Indent;
    For E in J do
      begin
      IM:=FPropertyMap.FindPath(AddToPath(M.Path,E.Key));
      If IM=Nil then
        raise EJSONToPascal.CreateFmt(SErrCannotDeterminePropertyType, [AddToPath(M.Path, E.Key)]);
      FRN:=FieldPrefix+IM.PropertyName;
      AddLn('Procedure Set%s(AValue : %s); virtual;',[IM.PropertyName,IM.TypeName]);
      end;
    Undent;
    end;
  Addln('Public');
  Indent;
  if HaveObj then
    AddLn('Destructor Destroy; override;');
  if jpoGenerateLoad in options then
    begin
    AddLn('Constructor CreateFromJSON(AJSON : %s); virtual;',[JSONDataName]);
    AddLn('Procedure LoadFromJSON(AJSON : %s); virtual;',[JSONDataName]);
    end;
  if jpoGenerateSave in options then
    begin
    AddLn('Function SaveToJSON : TJSONObject; overload;');
    AddLn('Procedure SaveToJSON(AJSON : TJSONObject); overload; virtual;');
    end;

  For E in J do
    begin
    IP:=AddToPath(M.Path,E.Key);
    IM:=FPropertyMap.FindPath(IP);
    If IM=Nil then
      raise EJSONToPascal.CreateFmt(SErrCannotDeterminePropertyType, [IP]);
    FRN:=FieldPrefix+IM.PropertyName;
    if jpoUseSetter in Options then
      FWN:='Set'+IM.PropertyName
    else
      FWN:=FRN;
    AddLn('Property %s : %s Read %s Write %s;',[IM.PropertyName,IM.TypeName,FRN, FWN]);
    end;
  Undent;
  AddLn('end;');
end;

function TJSONToPascal.AddToPath(const APath, AName: String): String;

begin
  Result:=APath;
  if (AName<>'') then
    begin
    if (Result<>'') and (AName[1]<>'[') then
      Result:=Result+'.';
    Result:=Result+AName;
    end;
end;

procedure TJSONToPascal.MaybeEmitType;

begin
  if FinType then exit;
  Undent;
  AddLn('Type');
  Indent;
  FinType:=True;
end;

procedure TJSONToPascal.GenerateDeclaration(const APath: String;J: TJSONData);

Var
  M :  TPropertyMapItem;
  O : TJSONEnum;
  IP : String;

begin
  AddLn('');
  MaybeEmitType;
  M:=FPropertyMap.FindPath(APath);
  If M=Nil then
    begin
    M:=FPropertyMap.Add as TPropertyMapItem;
    M.Path:=APath;
    M.FGenerated:=True;
    end
  else if M.SkipType then
    exit;
  if (M.TypeName='') then
    if J.JSONType in StructuredJSONtypes then
      M.TypeName:=PathToTypeName(APath)
    else
      M.TypeName:=GetPropertyTypeName(APath,'',J);
  M.JSONType:=J.JSONType;
  if J is TJSONArray then
    begin
    M.ParentTypeName:='';
    if J.Count=0 then
      raise EJSONToPascal.CreateFmt(SErrCannotGenerateArrayDeclaration, [APath]);
    IP:=AddToPath(M.Path,'[0]');
    GenerateDeclaration(IP,J.Items[0]);
    MaybeEmitType;
    GenerateArrayDeclaration(M,TJSONarray(J));
    end
  else if J is TJSONObject then
    begin
    For O in TJSONOBject(J) do
      begin
      IP:=AddToPath(APath,O.Key);
      GenerateDeclaration(IP,O.Value);
      end;
    M.ParentTypeName:=GetDefaultParentName;
    MaybeEmitType;
    GenerateObjectDeclaration(M,TJSONObject(J));
    end;
end;

procedure TJSONToPascal.GenerateImplementationHeader;
begin
  Addln('');
  Addln('implementation');
  Addln('');
end;

procedure TJSONToPascal.GenerateArrayImplementation(M : TPropertyMapItem; J: TJSONArray);

Var
  IM : TPropertyMapItem;
  P : String;

begin
  P:=AddToPath(M.Path,'[0]');
  IM:=FPropertyMap.FindPath(P);
  if J.Items[0] is TJSONObject then
    GenerateObjectImplementation(IM,J.Items[0] as TJSONObject)
  else if J.Items[0] is TJSONArray then
    GenerateArrayImplementation(IM,J.Items[0] as TJSONArray);
  if jpoGenerateLoad in Options then
    GenerateCreateArray(M);
  if jpoGenerateSave in Options then
    GenerateSaveArray(M)
  // Do nothing yet
end;

procedure TJSONToPascal.GenerateCreateArray(M : TPropertyMapItem);

Var
  IP : String;
  IM : TPropertyMapItem;

begin
  IP:=AddToPath(M.Path,'[0]');
  IM:=FPropertyMap.FindPath(IP);
  AddLn('');
  AddLn('Function Create%s(AJSON : %s) : %s;',[M.TypeName,JSONDataName,M.TypeName]);
  AddLn('');
  AddLn('var');
  AddIndented('I : integer;');
  if (jpoDelphiJSON in Options) then
    AddIndented('A : TJSONArray;');
  AddLn('');
  AddLn('begin');
  Indent;
  if not (jpoDelphiJSON in Options) then
    begin
    AddLn('SetLength(Result,AJSON.Count);');
    AddLn('For I:=0 to AJSON.Count-1 do');
    AddIndented(GenerateAssign(IM,'Result[i]','AJSON.Items[i]'));
    end
  else
    begin
    AddLn('A:=AJSON as TJSONArray;');
    AddLn('SetLength(Result,A.Count);');
    AddLn('For I:=0 to A.Count-1 do');
    AddIndented(GenerateAssignDelphi(IM,'Result[i]','A.Items[i]',True));
    end;
  Undent;
  Addln('End;');
  AddLn('');
end;

procedure TJSONToPascal.GenerateSaveArray(M : TPropertyMapItem);

Var
  IP : String;
  IM : TPropertyMapItem;

begin
  IP:=AddToPath(M.Path,'[0]');
  IM:=FPropertyMap.FindPath(IP);
  AddLn('');
  AddLn('Function Save%sToJSON(AnArray : %s) : TJSONArray;',[M.TypeName,M.TypeName]);
  AddLn('begin');
  Indent;
  Addln('Result:=TJSONArray.Create;');
  Addln('Try');
  AddIndented('Save%sToJSON(AnArray,Result);',[M.TypeName]);
  Addln('Except');
  Indent;
  Addln('FreeAndNil(Result);');
  Addln('Raise;');
  Undent;
  Addln('end;');
  Undent;
  Addln('end;');
  AddLn('');
  AddLn('');
  AddLn('Procedure Save%sToJSON(AnArray : %s; AJSONArray : TJSONArray);',[M.TypeName,M.TypeName]);
  AddLn('');
  AddLn('var');
  AddIndented('I : integer;');
  AddLn('');
  AddLn('begin');
  Indent;
  AddLn('For I:=0 to Length(AnArray)-1 do');
  Case IM.JSONType of
    jtObject : AddIndented('AJSONArray.Add(AnArray[i].SaveToJSON);');
    jtArray :  AddIndented('AJSONArray.Add(Save%sToJSON(AnArray[i]));',[IM.TypeName]);
  else
    AddIndented('AJSONArray.Add(AnArray[i]);');
  end;
  Undent;
  Addln('end;');
  AddLn('');
end;

function TJSONToPascal.GetObjectConstructorArguments: String;

begin
  Result:=ObjectConstructorArguments
end;

procedure TJSONToPascal.GenerateCreateObjectfpJSON(M : TPropertyMapItem);

Var
  IP : String;
  IM : TPropertyMapItem;

begin
  IP:=AddToPath(M.Path,'[0]');
  IM:=FPropertyMap.FindPath(IP);
  AddLn('');
  Indent;
  AddLn('Function CreateObject%s(AnObject : TJSONData) : %s;',[M.TypeName,M.TypeName]);
  AddLn('');
  AddLn('begin');
  Indent;
  AddLn('Result:='+M.TypeName+'.Create('+GetObjectConstructorArguments+');');
  AddLn('Result.LoadFromJSON(AnObject);');
  Undent;
  Addln('End;');
  Undent;
  AddLn('');
end;

procedure TJSONToPascal.GenerateLoadJSONDelphi(M: TPropertyMapItem;
  J: TJSONObject);
Var
  IM :  TPropertyMapItem;
  E : TJSONEnum;
  P,K : String;
  SElse : String;

begin
  AddLn('Procedure %s.LoadFromJSON(AJSON : TJSONValue);',[M.TypeName]);
  Addln('');
  Addln('var');
  AddIndented('P : TJSONPair;');
  AddIndented('O : TJSONObject;');
  AddIndented('PN : String;');
  Addln('');
  Addln('begin');
  Indent;
  if (jpoUnknownLoadPropsError in options) then
    begin
    Addln('if not (AJSON is TJSONObject) then');
    AddIndented('Raise EJSONException.CreateFmt(''"%s" : Cannot load from : "%s"'',[ClassName,AJSON.ClassName]);');
    end
  else
    Addln('if not (AJSON is TJSONObject) then exit;');
  Addln('O:=AJSON as TJSONObject;');
  Addln('for P in O do');
  Indent;
  Addln('begin');
  if jpoLoadCaseInsensitive in Options then
    Addln('PN:=LowerCase(P.JSONString.Value);')
  else
    Addln('PN:=P.JSONString.Value;');
  SElse:='';
  For E in J do
    begin
    P:=AddToPath(M.Path,E.Key);
    IM:=FPropertyMap.FindPath(P);
    If IM=Nil then
      raise EJSONToPascal.CreateFmt(SErrCannotDeterminePropertyType, [P]);
    K:=E.Key;
    If jpoLoadCaseInsensitive in Options then
      K:=LowerCase(K);
    Addln(SElse+'If (PN=''%s'') then',[K]);
    IM.JSONType:=E.Value.JSONType;
    AddIndented(GenerateAssignDelphi(IM,IM.PropertyName,'P.JSONValue',False));
    if SElse='' then
      SElse:='else '
    end;
  if (jpoUnknownLoadPropsError in options) then
    begin
    Addln('else');
    AddIndented('Raise EJSONException.CreateFmt(''"%s" : Unknown property : "%s"'',[ClassName,PN]);');
    end
  else
    AddSemiColonToLastLine;
  Addln('end;'); // For loop
  Undent;
  Undent;
  Addln('end;');
end;

function TJSONToPascal.GenerateAssign(IM: TPropertyMapItem; AVarName, AJSONName: String): String;

Var
  T : String;
  C : Boolean;

begin
  T:='';
  Case LowerCase(IM.TypeName) of
    'boolean' : T:='AsBoolean';
    'string'  : T:='AsString';
    'double'  : T:='AsFloat';
    'integer' : T:='AsInteger';
    'int64'   : T:='AsInt64';
    'qword'   : T:='AsQWord';
  else
    if IM.JSONType=jtArray then
      Result:=Format('%s:=Create%s(%s);',[AVarName,IM.TypeName,AJSONName])
    else if IM.JSONType=jtObject then
      Result:=Format('%s:=%s.CreateFromJSON(%s);',[AVarName,IM.TypeName,AJSONName])
    else
      Result:=Format('Raise EJSON.CreateFmt(''"%%s": Cannot handle property of type "%%s"''),[ClassName,''%s'']);',[IM.TypeName]);
  end;
  if T<>'' then
    Result:=Format('%s:=%s.%s;',[AVarName,AJSONName,T]);
end;

function TJSONToPascal.GenerateAssignDelphi(IM: TPropertyMapItem; AVarName,
  AJSONName: String; AddSemiColon: Boolean): String;

Var
  T : String;

begin
  T:='';
  Case LowerCase(IM.TypeName) of
    'boolean' : T:='Boolean';
    'string'  : T:='String';
    'double'  : T:='Double';
    'integer' : T:='Integer';
    'int64'   : T:='Int64';
    'qword'   : T:='Int64';
  else
    if IM.JSONType=jtArray then
      Result:=Format('%s:=Create%s(%s)',[AVarName,IM.TypeName,AJSONName])
    else if IM.JSONType=jtObject then
      Result:=Format('%s:=%s.CreateFromJSON(%s)',[AVarName,IM.TypeName,AJSONName])
    else
      Result:=Format('Raise EJSON.CreateFmt(''"%%s": Cannot handle property of type "%%s"''),[ClassName,''%s'']);',[IM.TypeName]);
  end;
  if T<>'' then
    Result:=Format('%s:=%s.GetValue<%s>',[AVarName,AJSONName,T]);
  If AddSemicolon then
    Result:=Result+';'
end;

procedure TJSONToPascal.GenerateLoadJSONfpJSON(M : TPropertyMapItem; J: TJSONObject);

Var
  IM :  TPropertyMapItem;
  E : TJSONEnum;
  P : String;

begin
  AddLn('Procedure %s.LoadFromJSON(AJSON : TJSONData);',[M.TypeName]);
  Addln('');
  Addln('var');
  AddIndented('E : TJSONEnum;');
  Addln('');
  Addln('begin');
  Indent;
  Addln('for E in AJSON do');
  Indent;
  Addln('begin');
  if jpoLoadCaseInsensitive in Options then
    Addln('case lowercase(E.Key) of')
  else
    Addln('case E.Key of');
  For E in J do
    begin
    P:=AddToPath(M.Path,E.Key);
    IM:=FPropertyMap.FindPath(P);
    If IM=Nil then
      raise EJSONToPascal.CreateFmt(SErrCannotDeterminePropertyType, [P]);
    if jpoLoadCaseInsensitive in Options then
      Addln('''%s'':',[LowerCase(E.Key)])
    else
      Addln('''%s'':',[E.Key]);
    IM.JSONType:=E.Value.JSONType;
    AddIndented(GenerateAssign(IM,IM.PropertyName,'E.Value'));
    end;
  if (jpoUnknownLoadPropsError in options) then
    begin
    Addln('else');
    AddIndented('Raise EJSON.CreateFmt(''"%s" : Unknown property : "%s"'',[ClassName,E.Key]);');
    end;
  Addln('end;'); // Case
  Addln('end;'); // For loop
  Undent;
  Undent;
  Addln('end;');
end;

procedure TJSONToPascal.GenerateSaveFunctionForm(M: TPropertyMapItem);

begin
  AddLn('Function  %s.SaveToJSON : TJSONObject;',[M.TypeName]);
  AddLn('begin');
  Indent;
  AddLn('Result:=TJSONObject.Create;');
  AddLn('Try');
  AddIndented('SaveToJSON(Result);');
  AddLn('except');
  Indent;
    Addln('FreeAndNil(Result);');
    AddLn('Raise;');
  Undent;
  AddLn('end;');
  Undent;
  AddLn('end;');
  AddLn('');
end;

procedure TJSONToPascal.GenerateSaveJSONDelphi(M: TPropertyMapItem;  J: TJSONObject);

Var
  IM :  TPropertyMapItem;
  E : TJSONEnum;
  T,P : String;
  B,C : Boolean; // B : Indent called. C : Need to create value

begin
  GenerateSaveFunctionForm(M);
  AddLn('');
  AddLn('Procedure %s.SaveToJSON(AJSON : TJSONObject);',[M.TypeName]);
  Addln('');
  Addln('begin');
  Indent;
  For E in J do
    begin
    B:=False;
    C:=True;
    P:=AddToPath(M.Path,E.Key);
    IM:=FPropertyMap.FindPath(P);
    If IM=Nil then
      raise EJSONToPascal.CreateFmt(SErrCannotDeterminePropertyType, [P]);
    Case LowerCase(IM.TypeName) of
      'boolean' : T:='Boolean';
      'string'  : T:='String';
      'double'  : T:='Number';
      'integer' : T:='Number';
      'int64'   : T:='Number';
      'qword'   : T:='Number';
    else
      C:=False;
      if IM.JSONType=jtArray then
        T:=Format('Save%sToJSON(%s)',[IM.TypeName,IM.PropertyName])
      else if IM.JSONType=jtObject then
        begin
        Addln('If Assigned(%s) then',[IM.PropertyName]);
        T:=Format('%s.SaveToJSON',[IM.PropertyName]);
        B:=True; // Indent called
        Indent;
        end;
    end;
    if C then
      T:='TJSON'+T+'.Create('+IM.PropertyName+')';
    if (T<>'') then
      AddLn('AJSON.AddPair(''%s'',%s);',[E.Key,T]);
    if B then
      Undent;
    end;
  Undent;
  Addln('end;');
end;

procedure TJSONToPascal.GenerateSaveJSONfpJSON(M: TPropertyMapItem; J: TJSONObject);

Var
  IM :  TPropertyMapItem;
  E : TJSONEnum;
  T,P : String;
  B : Boolean;

begin
  GenerateSaveFunctionForm(M);
  AddLn('');
  AddLn('Procedure %s.SaveToJSON(AJSON : TJSONObject);',[M.TypeName]);
  Addln('');
  Addln('begin');
  Indent;
  For E in J do
    begin
    B:=False;
    P:=AddToPath(M.Path,E.Key);
    IM:=FPropertyMap.FindPath(P);
    If IM=Nil then
      raise EJSONToPascal.CreateFmt(SErrCannotDeterminePropertyType, [P]);
    Case LowerCase(IM.TypeName) of
      'boolean' : T:=IM.PropertyName;
      'string'  : T:=IM.PropertyName;
      'double'  : T:=IM.PropertyName;
      'integer' : T:=IM.PropertyName;
      'int64'   : T:=IM.PropertyName;
      'qword'   : T:=IM.PropertyName;
    else
      if IM.JSONType=jtArray then
        t:=Format('Save%sToJSON(%s)',[IM.TypeName,IM.PropertyName])
      else if IM.JSONType=jtObject then
        begin
        Addln('If Assigned(%s) then',[IM.PropertyName]);
        T:=Format('%s.SaveToJSON',[IM.PropertyName]);
        B:=True; // Indent called
        Indent;
        end;
    end;
    if (T<>'') then
      AddLn('AJSON.Add(''%s'',%s);',[E.Key,T]);
    if B then
      Undent;
    end;
  Undent;
  Addln('end;');
end;

procedure TJSONToPascal.GenerateObjectImplementation(M : TPropertyMapItem; J: TJSONObject);

Var
  IM :  TPropertyMapItem;
  E : TJSONEnum;
  P,FRN : String;
  HaveObj : Boolean;

begin
  HaveObj:=False;
  For E in J do
    begin
    P:=AddToPath(M.Path,E.Key);
    IM:=FPropertyMap.FindPath(P);
    If IM<>Nil then
      HaveObj:=HaveObj or (IM.JSONType=jtObject);
    end;
  Addln('');
  AddLn('{ -----------------------------------------------------------------------');
  Addln('  '+M.TypeName);
  AddLn('  -----------------------------------------------------------------------}');
  Addln('');
  if HaveObj then
    begin
    AddLn('Destructor %s.Destroy;',[M.TypeName]);
    Addln('');
    Addln('begin');
    Indent;
    For E in J do
      begin
      P:=AddToPath(M.Path,E.Key);
      IM:=FPropertyMap.FindPath(P);
      If (IM<>Nil) and (IM.JSONType=jtObject) then
        AddLn('FreeAndNil('+FieldPrefix+IM.PropertyName+');');
      end;
    Addln('inherited;');
    Undent;
    Addln('end;');
    Addln('');
    end;
  Addln('');
  if jpoUseSetter in Options then
    For E in J do
      begin
      P:=AddToPath(M.Path,E.Key);
      IM:=FPropertyMap.FindPath(P);
      If IM=Nil then
        raise EJSONToPascal.CreateFmt(SErrCannotDeterminePropertyType, [P]);
      FRN:=FieldPrefix+IM.PropertyName;
      AddLn('Procedure %s.Set%s(AValue : %s);',[M.TypeName,IM.PropertyName,IM.TypeName]);
      Addln('');
      Addln('begin');
      Indent;
      AddLn('if ('+FieldPrefix+IM.PropertyName+'=AValue) then exit;');
      If IM.JSONType=jtObject then
        AddLn('FreeAndNil('+FieldPrefix+IM.PropertyName+');');
      AddLn(FieldPrefix+IM.PropertyName+':=AValue;');
      Undent;
      Addln('end;');
      Addln('');
      end;
  if jpoGenerateLoad in Options then
    begin
    AddLn('Constructor %s.CreateFromJSON(AJSON : %s);',[M.TypeName,JSONDataName]);
    Addln('');
    Addln('begin');
    Indent;
    AddLn('Create(%s);',[GetObjectConstructorArguments]);
    AddLn('LoadFromJSON(AJSON);');
    Undent;
    Addln('end;');
    Addln('');
    if jpoDelphiJSON in options then
      GenerateLoadJSONDelphi(M,J)
    else
      GenerateLoadJSONfpJSON(M,J);
    end;
  if jpoGenerateSave in Options then
    if jpoDelphiJSON in options then
      GenerateSaveJSONDelphi(M,J)
    else
      GenerateSaveJSONfpJSON(M,J);
end;

procedure TJSONToPascal.GenerateImplementation(const APath: String; J: TJSONData);

Var
  M ,IM :  TPropertyMapItem;
  O : TJSONEnum;
  P : String;

begin
  Addln('');
  M:=FPropertyMap.FindPath(APath);
  if M.SkipType then
    exit;
  if J is TJSONArray then
    GenerateArrayImplementation(M,TJSONarray(J))
  else if J is TJSONObject then
    begin
    For O in TJSONOBject(J) do
      begin
      P:=AddToPath(APath,O.Key);
      IM:=FPropertyMap.FindPath(P);
      If (O.Value.JSONType in StructuredJSONTypes) then
        GenerateImplementation(P,O.Value);
      end;
    GenerateObjectImplementation(M,TJSONObject(J));
    end;
  Addln('');
end;

procedure TJSONToPascal.GenerateImplementationEnd;
begin
  Addln('end.');
end;

procedure TJSONToPascal.Execute;

Var
  J : TJSONData;
  DoFree : Boolean;

begin
  J:=Nil;
  DoFree:=False;
  Factive:=True;
  try
    ClearGeneratedTypes;
    J:=GetJSONData(DoFree);
    GenerateInterfaceHeader;
    FInType:=False;
    GenerateDeclaration('',J);
    Undent;
    GenerateImplementationHeader;
    GenerateImplementation('',J);
    GenerateImplementationEnd;
  finally
    if DoFree then
      FreeAndNil(J);
    Factive:=False;
  end;
end;

{ TPropertyMapItem }

procedure TPropertyMapItem.Assign(Source: TPersistent);

Var
  M : TPropertyMapItem;

begin
  if Source is TPropertyMapItem then
    begin
    M:=Source as TPropertyMapItem;
    FPath:=M.Path;
    FTypeName:=M.TypeName;
    FParentTypeName:=M.ParentTypeName;
    FGenerated:=M.Generated;
    end
  else
    inherited Assign(Source);
end;

end.

