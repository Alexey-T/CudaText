unit fpjsonrtti;

{$mode objfpc}

interface

uses
  Classes, SysUtils, contnrs, typinfo, fpjson, rttiutils, jsonparser;

Const
  RFC3339DateTimeFormat = 'yyyy"-"mm"-"dd"T"hh":"nn":"ss';
  RFC3339DateTimeFormatMsec = RFC3339DateTimeFormat+'.zzz';
  

Type

  TJSONStreamEvent = Procedure (Sender : TObject; AObject : TObject; JSON : TJSONObject) of object;
  TJSONPropertyEvent = Procedure (Sender : TObject; AObject : TObject; Info : PPropInfo; var Res : TJSONData) of object;

  TJSONStreamOption = (jsoStreamChildren,         // If set, children will be streamed in 'Children' Property
                       jsoEnumeratedAsInteger,    // Write enumerated as integer. Default is string.
                       jsoSetAsString,            // Write Set as a string. Default is an array.
                       jsoSetEnumeratedAsInteger, // Write enumerateds in set array as integers.
                       jsoSetBrackets,            // Use brackets when creating set as array
                       jsoComponentsInline,       // Always stream components inline. Default is to stream name, unless csSubcomponent in ComponentStyle
                       jsoTStringsAsArray,        // Stream TStrings as an array of strings. Associated objects are not streamed.
                       jsoTStringsAsObject,       // Stream TStrings as an object : string = { object }
                       jsoDateTimeAsString,       // Format a TDateTime value as a string
                       jsoUseFormatString,        // Use FormatString when creating JSON strings.
                       jsoCheckEmptyDateTime,     // If TDateTime value is empty and jsoDateTimeAsString is used, 0 date returns empty string
                       jsoLegacyDateTime,         // Set this to enable old date/time formatting. Current behaviour is to save date/time as a ISO 9601 value.
                       jsoLowerPropertyNames,     // Set this to force lowercase names when streaming to JSON.
                       jsoStreamTList             // Set this to assume that TList contains a list of TObjects. Use with care!
                       );  
  TJSONStreamOptions = Set of TJSONStreamOption;

  TJSONFiler = Class(TComponent)
  Protected
    Procedure Error(Const Msg : String);
    Procedure Error(Const FMT : String;  Args : Array of const);
  end;

  { TJSONStreamer }

  TJSONStreamer = Class(TJSONFiler)
  private
    FAfterStreamObject: TJSONStreamEvent;
    FBeforeStreamObject: TJSONStreamEvent;
    FChildProperty: String;
    FDateTimeFormat: String;
    FOnStreamProperty: TJSONPropertyEvent;
    FOptions: TJSONStreamOptions;
    function GetChildProperty: String;
    function IsChildStored: boolean;
    function StreamChildren(AComp: TComponent): TJSONArray;
  protected
    function StreamClassProperty(Const AObject: TObject): TJSONData; virtual;
    Function StreamProperty(Const AObject : TObject; Const PropertyName : String) : TJSONData;
    Function StreamProperty(Const AObject : TObject; PropertyInfo : PPropInfo) : TJSONData;
    Function FormatDateProp(const DateTime : TDateTime) : TJSONString;
  Public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy;override;
    //
    // Basic functions
    //
    // Use RTTI to stream object.
    // If AObject is of type TStrings or TCollection, special treatment occurs:
    // TStrings results in { Strings: [S,S,S] } or { Strings: { "S1" : O1, "S2" : O2 }} depending on Options.
    // Collection results in { Items: [I,I,I] }
    Function ObjectToJSON(Const AObject : TObject) : TJSONObject;
    // Stream a collection - always returns an array
    function StreamCollection(Const ACollection: TCollection): TJSONArray;
    // Stream an objectlist - always returns an array
    function StreamObjectList(Const AnObjectList: TObjectList): TJSONArray;
    // Stream a List - always returns an array
    function StreamTList(Const AList: TList): TJSONArray;
    // Stream a TStrings instance as an array
    function StreamTStringsArray(Const AStrings: TStrings): TJSONArray;
    // Stream a TStrings instance as an object
    function StreamTStringsObject(Const AStrings: TStrings): TJSONObject;
    // Stream a TStrings instance. Takes into account Options.
    function StreamTStrings(Const AStrings: TStrings): TJSONData;
    // Stream a variant as JSON.
    function StreamVariant(const Data: Variant): TJSONData; virtual;
    //
    // Some utility functions.
    //
    // Call ObjectToJSON and convert result to JSON String.
    Function ObjectToJSONString(AObject : TObject) : TJSONStringType;
    // Convert TSTrings to JSON string with array or Object.
    Function StringsToJSON(Const Strings : TStrings; AsObject : Boolean = False) : TJSONStringType;
    // Convert collection to JSON string
    Function CollectionToJSON(Const ACollection : TCollection) : TJSONStringType;
    // Convert variant to JSON String
    Function VariantToJSON(Const Data : Variant) : TJSONStringType;
  Published
    // Format used when formatting DateTime values. Only used in conjunction with jsoDateTimeToString
    Property DateTimeFormat : String Read FDateTimeFormat Write FDateTimeFormat;
    // Options to use when streaming
    Property Options : TJSONStreamOptions Read FOptions Write FOptions;
    // Called before streaming an object with ObjectToJSON
    Property BeforeStreamObject : TJSONStreamEvent Read FBeforeStreamObject Write FBeforeStreamObject;
    // Called After streaming an object with ObjectToJSON
    Property AfterStreamObject : TJSONStreamEvent Read FAfterStreamObject Write FAfterStreamObject;
    // Called whenever a property was streamed. If Res is nil on return, no property is added.
    Property OnStreamProperty : TJSONPropertyEvent Read FOnStreamProperty Write FOnStreamProperty;
    // Property name to use when streaming child components. Default is "Children"
    Property ChildProperty : String Read GetChildProperty Write FChildProperty Stored IsChildStored;
  end;

  { TJSONDeStreamer }
  TJSONRestorePropertyEvent = Procedure (Sender : TObject; AObject : TObject; Info : PPropInfo; AValue : TJSONData; Var Handled : Boolean) of object;
  TJSONPropertyErrorEvent = Procedure (Sender : TObject; AObject : TObject; Info : PPropInfo; AValue : TJSONData; Error : Exception; Var Continue : Boolean) of object;
  TJSONGetObjectEvent = Procedure (Sender : TOBject; AObject : TObject; Info : PPropInfo; AData : TJSONObject; DataName : TJSONStringType; Var AValue : TObject);
  TJSONDestreamOption = (jdoCaseInsensitive,jdoIgnorePropertyErrors);
  TJSONDestreamOptions = set of TJSONDestreamOption;

  TJSONDeStreamer = Class(TJSONFiler)
  private
    FAfterReadObject: TJSONStreamEvent;
    FBeforeReadObject: TJSONStreamEvent;
    FDateTimeFormat: String;
    FOnGetObject: TJSONGetObjectEvent;
    FOnPropError: TJSONpropertyErrorEvent;
    FOnRestoreProp: TJSONRestorePropertyEvent;
    FCaseInsensitive : Boolean;
    FOptions: TJSONDestreamOptions;
    procedure DeStreamClassProperty(AObject: TObject; PropInfo: PPropInfo; PropData: TJSONData);
    function GetCaseInsensitive: Boolean;
    procedure SetCaseInsensitive(AValue: Boolean);
  protected
    // Try to parse a date.
    Function ExtractDateTime(S : String): TDateTime;
    function GetObject(AInstance : TObject; const APropName: TJSONStringType; D: TJSONObject; PropInfo: PPropInfo): TObject;
    procedure DoRestoreProperty(AObject: TObject; PropInfo: PPropInfo;  PropData: TJSONData); virtual;
    Function ObjectFromString(Const JSON : TJSONStringType) : TJSONData; virtual;
    procedure RestoreProperty(AObject: TObject; PropInfo: PPropInfo; PropData: TJSONData);
  Public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
    // Convert JSON object to properties of AObject
    Procedure JSONToObject(Const JSON : TJSONStringType; AObject : TObject);
    Procedure JSONToObject(Const JSON : TJSONObject; AObject : TObject);
    // Convert JSON object/array to collection.
    Procedure JSONToCollection(Const JSON : TJSONStringType; ACollection : TCollection);
    Procedure JSONToCollection(Const JSON : TJSONData; ACollection : TCollection);
    // Convert JSON array/object/string to TStrings
    Procedure JSONToStrings(Const JSON : TJSONStringType; AStrings : TSTrings);
    Procedure JSONToStrings(Const JSON : TJSONData; AStrings : TSTrings);
    // Convert JSON data to a variant. Supports simple data types and arrays.
    Function JSONToVariant(Data: TJSONData): Variant;
    Function JSONToVariant(Data: TJSONStringType): Variant;
    // Triggered at the start of each call to JSONToObject
    Property BeforeReadObject : TJSONStreamEvent Read FBeforeReadObject Write FBeforeReadObject;
    // Triggered at the end of each call to JSONToObject (not if exception happens)
    Property AfterReadObject : TJSONStreamEvent Read FAfterReadObject Write FAfterReadObject;
    // Called when a property will be restored. If 'Handled' is True on return, property is considered restored.
    Property OnRestoreProperty : TJSONRestorePropertyEvent Read FOnRestoreProp Write FOnRestoreProp;
    // Called when an error occurs when restoring a property. If Continue is False on return, exception is re-raised.
    Property OnPropertyError : TJSONpropertyErrorEvent Read FOnPropError Write FOnPropError;
    // Called when a object-typed property must be restored, and the property is Nil. Must return an instance for the property.
    // Published Properties of the instance will be further restored with available data.
    Property OngetObject : TJSONGetObjectEvent Read FOnGetObject Write FOnGetObject;
    // JSON is by definition case sensitive. Should properties be looked up case-insentive ?
    Property CaseInsensitive : Boolean Read GetCaseInsensitive Write SetCaseInsensitive ; deprecated;
    // DateTime format. If not set, RFC3339DateTimeFormat is assumed.
    // If set, it will be used as an argument to ScanDateTime. If that fails, StrToDateTime is used.
    Property DateTimeFormat : String Read FDateTimeFormat Write FDateTimeFormat;
    // Options overning the behaviour
    Property Options : TJSONDestreamOptions Read FOptions Write FOptions;
  end;

  EJSONRTTI = Class(Exception);


implementation

uses dateutils, variants, rtlconsts;

ResourceString
  SErrUnknownPropertyKind     = 'Unknown property kind for property : "%s"';
  SErrUnsupportedPropertyKind = 'Unsupported property kind for property: "%s"';
  SErrUnsupportedVariantType  = 'Unsupported variant type : %d';
  SErrUnsupportedArrayType    = 'JSON array cannot be streamed to object of class "%s"';
  SErrUnsupportedJSONType     = 'Cannot destream object from JSON data of type "%s"';
  SErrUnsupportedCollectionType = 'Unsupported JSON type for collections: "%s"';
  SErrUnsupportedCollectionItemType = 'Array element %d is not a valid type for a collection item: "%s"';
  SErrUnsupportedStringsItemType = 'Array element %d is not a valid type for a stringlist item: "%s"';
  SErrUnsupportedStringsType = 'Unsupported JSON type for stringlists: "%s"';
  SErrUnsupportedStringsObjectType = 'Object Element %s is not a valid type for a stringlist object: "%s"';
  SErrUnSupportedEnumDataType = 'Unsupported JSON type for enumerated property "%s" : "%s"';
  SErrUnsupportedVariantJSONType = 'Unsupported JSON type for variant value : "%s"';
  SErrUnsupportedObjectData = 'Unsupported JSON type for object property: "%s"';

{ TStreamChildrenHelper }

Type
  TSet = set of 0..31; // Used to (de)stream set properties.

  TStreamChildrenHelper = Class
  Private
   FChildren : TJSONArray;
   FStreamer:TJSONStreamer;
   procedure StreamChild(AChild: TComponent);
  public
    Function StreamChildren(AComponent : TComponent; AStreamer : TJSONStreamer): TJSONArray;
  end;

  THackComponent = Class(TComponent);

{ TJSONDeStreamer }

function TJSONDeStreamer.ObjectFromString(const JSON: TJSONStringType): TJSONData;

begin
  With TJSONParser.Create(JSON) do
    try
      Result:=Parse;
    finally
      Free;
    end;
end;

constructor TJSONDeStreamer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TJSONDeStreamer.Destroy;
begin
  inherited Destroy;
end;

procedure TJSONDeStreamer.JSONToObject(const JSON: TJSONStringType;
  AObject: TObject);

Var
  D : TJSONData;

begin
  D:=ObjectFromString(JSON);
  try
    If D.JSONType=jtObject then
      JSONToObject(D as TJSONObject,AObject)
    else if D.JSONType=jtArray then
      begin
      If AObject is TStrings then
        JSONToStrings(D,AObject as TSTrings)
      else if AObject is TCollection then
        JSONTOCollection(D,AObject as TCollection)
      else
        Error(SErrUnsupportedArrayType,[AObject.ClassName])
      end
    else if (D.JSONType=jtString) and (AObject is TStrings) then
      JSONToStrings(D,AObject as TStrings)
    else
      Error(SErrUnsupportedJSONType,[JSONTypeName(D.JSONType)]);
  finally
    FreeAndNil(D);
  end;
end;

function TJSONDeStreamer.JSONToVariant(Data: TJSONData): Variant;

Var
  I : integer;

begin
  Case Data.JSONType of
    jtNumber :
      Case TJSONNumber(Data).NumberType of
        ntFloat   : Result:=Data.AsFloat;
        ntInteger : Result:=Data.AsInteger;
        ntInt64   : Result:=Data.Asint64;
        ntQWord   : Result:=Data.AsQWord;
      end;
    jtString :
      Result:=Data.AsString;
    jtBoolean:
      Result:=Data.AsBoolean;
    jtNull:
      Result:=Null;
    jtArray :
      begin
      Result:=VarArrayCreate([0,Data.Count-1],varVariant);
      For I:=0 to Data.Count-1 do
        Result[i]:=JSONToVariant(Data.Items[i]);
      end;
  else
    Error(SErrUnsupportedVariantJSONType,[GetEnumName(TypeInfo(TJSONType),Ord(Data.JSONType))]);
  end;
end;

function TJSONDeStreamer.JSONToVariant(Data: TJSONStringType): Variant;

Var
  D : TJSONData;

begin
  D:=ObjectFromString(Data);
  try
    Result:=JSONToVariant(D);
  finally
    D.Free;
  end;
end;

procedure TJSONDeStreamer.DeStreamClassProperty(AObject : TObject;PropInfo : PPropInfo; PropData : TJSONData);

Var
  O : TObject;

begin
  O:=GetObjectProp(AObject,PropInfo);
  If O is TStrings then
    JSONToStrings(PropData,O as TStrings)
  else if (O is TCollection) then
    JSONToCollection(PropData,O as TCollection)
  else
    begin
    If (O=Nil) then
      begin
      If (PropData.JSONType=jtString) then
        O:=GetObject(AObject,PropData.AsString,Nil,PropInfo)
      else if (PropData.JSONType=jtObject) then
        O:=GetObject(AObject,'',PropData as TJSONObject,PropInfo)
      else
        Error(SErrUnsupportedObjectData,[JsonTypeName(PropData.JSONType){GetEnumName(TypeInfo(TJSONType),Ord(PropData.JSONType))}]);
      SetObjectProp(AObject,PropInfo,O);
      end;
    If (O<>Nil) and (PropData.JSONType=jtObject) then
      JSONToObject(PropData as TJSONObject,O);
    end;
end;

function TJSONDeStreamer.GetCaseInsensitive: Boolean;
begin
  Result:=jdoCaseInsensitive in Options;
end;

procedure TJSONDeStreamer.SetCaseInsensitive(AValue: Boolean);
begin
  if AValue then
    Include(Foptions,jdoCaseInsensitive)
  else
    Exclude(Foptions,jdoCaseInsensitive);
end;

function TJSONDeStreamer.ExtractDateTime(S: String): TDateTime;

Var
  Fmt : String;
  E,fmtSpecified : Boolean;

begin
  E:=False;
  FMT:=DateTimeFormat;
  fmtSpecified:=Fmt<>'';
  if Not fmtSpecified then
    FMT:=RFC3339DateTimeFormat;
  Try
    // No TryScanDateTime
    Result:=ScanDatetime(FMT,S);
  except
    if fmtSpecified then
      Raise
    else
      E:=True;
  end;
  if E then
    if not TryStrToDateTime(S,Result) then
      if not TryStrToDate(S,Result) then
        if not TryStrToTime(S,Result) then
          Raise EConvertError.CreateFmt(SInvalidDateTime,[S]);
//  ExtractDateTime(PropData.AsString)
end;

procedure TJSONDeStreamer.RestoreProperty(AObject : TObject;PropInfo : PPropInfo; PropData : TJSONData);

Var
  B : Boolean;

begin
  try
    B:=Not Assigned(FOnRestoreProp);
    If Not B then
      begin
      FOnRestoreProp(Self,AObject,PropInfo,PropData,B);
      If B then
        exit;
      end;
    DoRestoreProperty(AObject,PropInfo,PropData);
  except
    On E : Exception do
      If Assigned(FOnPropError) then
        begin
        B:=False;
        FOnPropError(Self,AObject,PropInfo,PropData,E,B);
        If Not B then
          Raise;
        end
      else if Not (jdoIgnorePropertyErrors in Options) then
        Raise;
  end;
end;

procedure TJSONDeStreamer.DoRestoreProperty(AObject : TObject;PropInfo : PPropInfo; PropData : TJSONData);

Var
  PI : PPropInfo;
  TI : PTypeInfo;
  I,J,S : Integer;
  D : Double;
  A : TJSONArray;
  JS : TJSONStringType;
begin
  PI:=PropInfo;
  TI:=PropInfo^.PropType;
  case TI^.Kind of
    tkUnknown :
      Error(SErrUnknownPropertyKind,[PI^.Name]);
    tkInteger :
      SetOrdProp(AObject,PI,PropData.AsInteger);
    tkInt64 :
      SetOrdProp(AObject,PI,PropData.AsInt64);
    tkEnumeration :
      begin
      if (PropData.JSONType=jtNumber) then
        I:=PropData.AsInteger
      else if PropData.JSONType=jtString then
        I:=GetEnumValue(TI,PropData.AsString)
      else
        Error(SErrUnSupportedEnumDataType,[PI^.Name,GetEnumName(TypeInfo(TJSONType),Ord(PropData.JSONType))]);
      SetOrdProp(AObject,PI,I);
      end;
    tkFloat :
      begin
      if (TI=TypeInfo(TDateTime)) and (PropData.JSONType=jtString) then
        SetFloatProp(AObject,PI,ExtractDateTime(PropData.AsString))
      else
        SetFloatProp(AObject,PI,PropData.AsFloat)
      end;
    tkSet :
      If PropData.JSONType=jtString then
        SetSetProp(AObject,PI,PropData.AsString)
      else if (PropData.JSONType=jtArray) then
        begin
        A:=PropData as TJSONArray;
        TI:=GetTypeData(TI)^.CompType;
        S:=0;
        For I:=0 to A.Count-1 do
          begin
          if A.types[i]=jtNumber then
            J:=A.Integers[i]
          else
            J:=GetEnumValue(TI,A.strings[i]);
          TSet(S):=TSet(S)+[j];
          end;
        SetOrdProp(AObject,PI,S);
        end;
    tkChar:
      begin
      JS:=PropData.AsString;
      If (JS<>'') then
        SetOrdProp(AObject,PI,Ord(JS[1]));
      end;
    tkSString,
    tkLString,
    tkAString:
      SetStrProp(AObject,PI,PropData.AsString);
    tkWString :
      SetWideStrProp(AObject,PI,PropData.AsUnicodeString);
    tkVariant:
      SetVariantProp(AObject,PI,JSONToVariant(PropData));
    tkClass:
      DeStreamClassProperty(AObject,PI,PropData);
    tkWChar :
      begin
      JS:=PropData.asString;
      If (JS<>'') then
        SetOrdProp(AObject,PI,Ord(JS[1]));
      end;
    tkBool :
      SetOrdProp(AObject,PI,Ord(PropData.AsBoolean));
    tkQWord :
      SetOrdProp(AObject,PI,Trunc(PropData.AsFloat));
    tkObject,
    tkArray,
    tkRecord,
    tkInterface,
    tkDynArray,
    tkInterfaceRaw,
    tkProcVar,
    tkMethod :
      Error(SErrUnsupportedPropertyKind,[PI^.Name]);
    tkUString :
      SetUnicodeStrProp(AObject,PI,PropData.AsUnicodeString);
    tkUChar:
      begin
      JS:=PropData.asString;
      If (JS<>'') then
        SetOrdProp(AObject,PI,Ord(JS[1]));
      end;
  end;
end;

procedure TJSONDeStreamer.JSONToObject(const JSON: TJSONObject; AObject: TObject
  );
Var
  I,J : Integer;
  PIL : TPropInfoList;

begin
  If Assigned(FBeforeReadObject) then
    FBeforeReadObject(Self,AObject,JSON);
  If (AObject is TStrings) then
    JSONToStrings(JSON,AObject as TStrings)
  else If (AObject is TCollection) then
    JSONToCollection(JSON, AObject as TCollection)
  else
    begin
    Pil:=TPropInfoList.Create(AObject,tkProperties);
    try
      For I:=0 to PIL.Count-1 do
        begin
        J:=JSON.IndexOfName(Pil.Items[i]^.Name,(jdoCaseInsensitive in Options));
        If (J<>-1) then
          RestoreProperty(AObject,PIL.Items[i],JSON.Items[J]);
        end;
    finally
      FreeAndNil(PIL);
    end;
    end;
  If Assigned(FAfterReadObject) then
    FAfterReadObject(Self,AObject,JSON)
end;

procedure TJSONDeStreamer.JSONToCollection(const JSON: TJSONStringType;
  ACollection: TCollection);
Var
  D : TJSONData;

begin
  D:=ObjectFromString(JSON);
  try
    JSONToCollection(D,ACollection);
  finally
    D.Free;
  end;
end;

procedure TJSONDeStreamer.JSONToCollection(const JSON: TJSONData;
  ACollection: TCollection);

Var
  I : integer;
  A : TJSONArray;
  O : TJSONObject;

begin
  If (JSON.JSONType=jtArray) then
    A:=JSON As TJSONArray
  else if JSON.JSONType=jtObject then
    A:=(JSON as TJSONObject).Arrays['Items']
  else
    Error(SErrUnsupportedCollectionType,[JSONTypeName(JSON.JSONType)]);
  ACollection.Clear;
  For I:=0 to A.Count-1 do
    If (A.Types[i]<>jtObject) then
      Error(SErrUnsupportedCollectionItemType,[I,JSONTypeName(A.Types[I])])
    else
      JSONToObject(A.Objects[i],ACollection.Add);
end;

procedure TJSONDeStreamer.JSONToStrings(const JSON: TJSONStringType;
  AStrings: TSTrings);
Var
  D : TJSONData;

begin
  D:=ObjectFromString(JSON);
  try
    JSONToStrings(D,AStrings);
  finally
    D.Free;
  end;
end;

function TJSONDeStreamer.GetObject(AInstance: TObject;
  const APropName: TJSONStringType; D: TJSONObject; PropInfo: PPropInfo
  ): TObject;

Var
  C : TClass;

begin
  Result:=Nil;
  If Assigned(FOnGetObject) then
    FOnGetObject(Self,AInstance,PropInfo,D,APropName,Result);
  If (Result=Nil) and (AInstance is TComponent) and Assigned(PropInfo) then
     begin
     C:=GetTypeData(Propinfo^.PropType)^.ClassType;
     If C.InheritsFrom(TComponent) then
       Result:=TComponentClass(C).Create(TComponent(AInstance));
     end;
end;

procedure TJSONDeStreamer.JSONToStrings(const JSON: TJSONData;
  AStrings: TSTrings);

Var
  O  : TJSONObject;
  D  : TJSONData;
  I  : Integer;
  IO : TObject;
  N  : TJSONStringType;

begin
  Case JSON.JSONType of
    jtString:
      AStrings.Text:=JSON.AsString;
    jtArray:
      begin
      AStrings.Clear;
      For I:=0 to JSON.Count-1 do
        begin
        if not (JSON.Items[i].JSONType=jtString) then
          Error(SErrUnsupportedStringsItemType,[i,JSONTypeName(JSON.Items[i].JSONType)]);
        AStrings.Add(JSON.Items[i].AsString);
        end;
      end;
    jtObject:
      begin
      O:=JSON As TJSONObject;
      If (O.Count=1) and (O.Names[0]='Strings') and (O.Items[0].JSONType=jtArray) then
        JSONToStrings(O.Items[0],AStrings)
      else
        begin
        AStrings.Clear;
        For I:=0 to O.Count-1 do
          begin
          D:=O.Items[i];
          N:=O.Names[i];
          If D.JSONType=jtNull then
            IO:=Nil
          else if D.JSONType=jtObject then
            IO:=GetObject(AStrings,N,TJSONOBject(D),Nil)
          else
            Error(SErrUnsupportedStringsObjectType,[D,JSONTypeName(D.JSONType)]);
          AStrings.AddObject(O.Names[i],IO);
          end;
        end;
      end;
  else
    Error(SErrUnsupportedStringsType,[JSONTypeName(JSON.JSONType)]);
  end;
end;

Procedure TStreamChildrenHelper.StreamChild(AChild : TComponent);

begin
  FChildren.Add(FStreamer.ObjectToJSON(AChild));
end;

Function TStreamChildrenHelper.StreamChildren(AComponent : TComponent; AStreamer : TJSONStreamer): TJSONArray;

begin
  FStreamer:=AStreamer;
  Result:=TJSONArray.Create;
  try
    FChildren:=Result;
    THackComponent(AComponent).GetChildren(@StreamChild,AComponent);
  except
    FreeAndNil(Result);
    Raise;
  end;
end;

{ TJSONFiler }

procedure TJSONFiler.Error(Const Msg: String);
begin
  Raise EJSONRTTI.Create(Name+' : '+Msg);
end;

procedure TJSONFiler.Error(Const FMT: String; Args: array of const);
begin
  Raise EJSONRTTI.CreateFmt(Name+' : '+FMT,Args);
end;

{ TJSONStreamer }

constructor TJSONStreamer.Create(AOwner: TComponent);
begin
  Inherited;
end;

destructor TJSONStreamer.Destroy;
begin
  Inherited;
end;


Function TJSONStreamer.StreamChildren(AComp : TComponent) : TJSONArray;

begin
  With TStreamChildrenHelper.Create do
    try
      Result:=StreamChildren(AComp,Self);
    finally
      Free;
    end;
end;

function TJSONStreamer.GetChildProperty: String;
begin
  Result:=FChildProperty;
  If (Result='') then
    Result:='Children';
end;

function TJSONStreamer.IsChildStored: boolean;
begin
  Result:=(GetChildProperty<>'Children');
end;

function TJSONStreamer.ObjectToJSON(Const AObject: TObject): TJSONObject;

Var
  PIL : TPropInfoList;
  PD : TJSONData;
  I : Integer;

begin
  Result:=Nil;
  If (AObject=Nil) then
    Exit;
  Result:=TJSONObject.Create;
  try
    If Assigned(FBeforeStreamObject) then
      FBeforeStreamObject(Self,AObject,Result);
    If AObject is TStrings then
      Result.Add('Strings',StreamTStrings(Tstrings(AObject)))
    else If AObject is TCollection then
      Result.Add('Items',StreamCollection(TCollection(AObject)))
    else If AObject is TObjectList then
      Result.Add('Objects',StreamObjectList(TObjectList(AObject)))
    else if (jsoStreamTlist in Options) and (AObject is TList) then
      Result := TJSONObject(StreamTList(TList(AObject)))
    else
      begin
      PIL:=TPropInfoList.Create(AObject,tkProperties);
      try
        For I:=0 to PIL.Count-1 do
          begin
          PD:=StreamProperty(AObject,PIL.Items[i]);
            If (PD<>Nil) then begin
              if jsoLowerPropertyNames in Options then
                Result.Add(LowerCase(PIL.Items[I]^.Name),PD)
              else
            Result.Add(PIL.Items[I]^.Name,PD);
          end;
          end;
      finally
        FReeAndNil(Pil);
      end;
      If (jsoStreamChildren in Options) and (AObject is TComponent) then
        Result.Add(ChildProperty,StreamChildren(TComponent(AObject)));
      If Assigned(FAfterStreamObject) then
        FAfterStreamObject(Self,AObject,Result);
      end;
  except
    FreeAndNil(Result);
    Raise;
  end;
end;

function TJSONStreamer.StreamProperty(Const AObject: TObject; Const PropertyName : String): TJSONData;

begin
  Result:=StreamProperty(AObject,GetPropInfo(AObject,PropertyName));
end;

Function TJSONStreamer.StreamVariant(Const Data : Variant): TJSONData;

Var
  A : TJSONArray;
  I : Integer;

begin
  Result:=Nil;
  If VarIsArray(Data) then
    begin
    A:=TJSONArray.Create;
    try
      For I:=VarArrayLowBound(Data,1) to VarArrayHighBound(Data,1) do
        A.Add(StreamVariant(Data[i]));
    except
      FreeAndNil(A);
      Raise;
    end;
    Exit(A);
    end;
  If VarIsEmpty(Data) or VarisNull(Data) or (Data=UnAssigned) then
    Exit(TJSONNull.Create);
  Case VarType(Data) of
    varshortint,
    varbyte,
    varword,
    varsmallint,
    varinteger :
      Result:=TJSONIntegerNumber.Create(Data);
    varlongword,
    varint64 :
      Result:=TJSONInt64Number.Create(Data);
    vardecimal,
    varqword,
    varsingle,
    vardouble,
    varCurrency :
      Result:=TJSONFloatNumber.Create(Data);
    varString,
    varolestr :
      Result:=TJSONString.Create(Data);
    varboolean :
      Result:=TJSONBoolean.Create(Data);
    varDate :
      if jsoDateTimeAsString in Options then
        Result:=FormatDateProp(Data)
      else
        Result:=TJSONFloatNumber.Create(Data);
  else
    Error(SErrUnsupportedVariantType,[VarType(Data)])
  end;
end;

function TJSONStreamer.ObjectToJSONString(AObject: TObject): TJSONStringType;

Var
  O : TJSONData;

begin
  O:=ObjectToJSON(AObject);
  try
    if (jsoUseFormatString in Options) then
      Result:=O.FormatJSON()
    else
      Result:=O.AsJSON;
  finally
    FreeAndNil(O);
  end;
end;

function TJSONStreamer.StringsToJSON(Const Strings: TStrings; AsObject: Boolean = False): TJSONStringType;

Var
  D : TJSONData;

begin
  If ASObject then
    D:=StreamTSTringsObject(Strings)
  else
    D:=StreamTStringsArray(Strings);
  try
    if (jsoUseFormatString in Options) then
      Result:=D.FormatJSON
    else
      Result:=D.AsJSON;
  finally
    FreeAndNil(D);
  end;
end;

function TJSONStreamer.CollectionToJSON(const ACollection: TCollection
  ): TJSONStringType;

Var
  D : TJSONArray;

begin
  D:=StreamCollection(ACollection);
  try
    if (jsoUseFormatString in Options) then
      Result:=D.FormatJSON()
    else
    Result:=D.AsJSON;
  finally
    FreeAndNil(D);
  end;
end;

function TJSONStreamer.VariantToJSON(const Data: Variant): TJSONStringType;

Var
  D : TJSONData;

begin
  D:=StreamVariant(Data);
  try
    if (jsoUseFormatString in Options) then
      Result:=D.FormatJSON()
    else
      Result:=D.AsJSON;
  finally
    FreeAndNil(D);
  end;
end;

function TJSONStreamer.StreamTList(const AList: TList): TJSONArray;
var
  I : Integer;
  o : TJSONObject;
begin
  Result:=TJSONArray.Create;
  try
    for I:=0 to AList.Count-1 do begin
      o := ObjectToJSON(TObject(AList.Items[i]));
      if Assigned(o) then
        Result.Add(o);
    end;
  except
    FreeAndNil(Result);
    Raise;
  end;
end;

Function TJSONStreamer.StreamTStringsArray(Const AStrings : TStrings) : TJSONArray;

Var
  I : Integer;

begin
  Result:=TJSONArray.Create;
  try
    For I:=0 to AStrings.Count-1 do
      Result.Add(AStrings[i]);
  except
    FreeAndNil(Result);
    Raise;
  end;
end;

function TJSONStreamer.StreamTStringsObject(Const AStrings: TStrings): TJSONObject;

Var
  I : Integer;
  O : TJSONData;

begin
  Result:=TJSONObject.Create;
  try
    For I:=0 to AStrings.Count-1 do
      begin
      O:=ObjectToJSON(AStrings.Objects[i]);
      If O=Nil then
        O:=TJSONNull.Create;
      Result.Add(AStrings[i],O);
      end;
  except
    FreeAndNil(Result);
    Raise;
  end;
end;

function TJSONStreamer.StreamTStrings(Const AStrings: TStrings): TJSONData;
begin
  If jsoTStringsAsArray in Options then
    Result:=StreamTStringsArray(AStrings)
  else If jsoTStringsAsObject in Options then
    Result:=StreamTStringsObject(AStrings)
  else
    Result:=TJSONString.Create(AStrings.Text);
end;


Function TJSONStreamer.StreamCollection(Const ACollection : TCollection) : TJSONArray;

Var
  I : Integer;

begin
  Result:=TJSONArray.Create;
  try
    For I:=0 to ACollection.Count-1 do
      Result.Add(ObjectToJSON(ACollection.Items[i]));
  except
    FreeAndNil(Result);
    Raise;
  end;
end;

function TJSONStreamer.StreamObjectList(const AnObjectList: TObjectList): TJSONArray;
Var
  I : Integer;

begin
  if not Assigned(AnObjectList) then
    Result:=Nil;
  Result:=TJSONArray.Create;
  try
    For I:=0 to AnObjectList.Count-1 do
      Result.Add(ObjectToJSON(AnObjectList.Items[i]));
  except
    FreeAndNil(Result);
    Raise;
  end;
end;

function TJSONStreamer.StreamClassProperty(const AObject: TObject): TJSONData;

Var
  C : TCollection;
  I : integer;

begin
  Result:=Nil;
  If (AObject=Nil) then
    Result:=TJSONNull.Create()
  else if (AObject is TComponent) then
    begin
    if (csSubComponent in TComponent(AObject).ComponentStyle) or (jsoComponentsInline in Options) then
      Result:=ObjectToJSON(AObject)
    else
      Result:=TJSONString.Create(TComponent(AObject).Name);
    end
  else if (AObject is TStrings) then
    Result:=StreamTStrings(TStrings(AObject))
  else if (AObject is TCollection) then
    Result:=StreamCollection(TCollection(Aobject))
  else If AObject is TObjectList then
    Result:=StreamObjectList(TObjectList(AObject))
  else // Normally, this is only TPersistent.
    Result:=ObjectToJSON(AObject);
end;

function TJSONStreamer.StreamProperty(Const AObject: TObject; PropertyInfo: PPropInfo): TJSONData;

Var
  PI : PPropInfo;
  PT : PTypeInfo;
  S,I : integer;

begin
  Result:=Nil;
  PI:=PropertyInfo;
  PT:=PI^.PropType;
  Case PT^.Kind of
    tkUnknown :
      Error(SErrUnknownPropertyKind,[PI^.Name]);
    tkInteger :
      Result:=TJSONIntegerNumber.Create(GetOrdProp(AObject,PI));
    tkEnumeration :
      if jsoEnumeratedAsInteger in Options then
        Result:=TJSONIntegerNumber.Create(GetOrdProp(AObject,PI))
      else
        Result:=TJSONString.Create(GetEnumName(PT,GetOrdProp(AObject,PI)));
    tkFloat :
      if (PT=TypeInfo(TDateTime)) and (jsoDateTimeAsString in Options) then
        Result:=FormatDateProp(GetFloatProp(AObject,PI))
      else
        Result:=TJSONFloatNumber.Create(GetFloatProp(AObject,PI));
    tkSet :
      If jsoSetAsString in Options then
        Result:=TJSONString.Create(GetSetProp(AObject,PI,jsoSetBrackets in Options))
      else
        begin
        PT:=GetTypeData(PT)^.CompType;
        S:=GetOrdProp(AObject,PI);
        Result:=TJSONArray.Create;
        try
          for i:=0 to 31 do
            if (i in TSet(S)) then
              if jsoSetEnumeratedAsInteger in Options then
                TJSONArray(Result).Add(i)
              else
                TJSONArray(Result).Add(GetEnumName(PT, i));
        except
          FreeAndNil(Result);
          Raise;
        end;
        end;
    tkChar:
      Result:=TJSONString.Create(Char(GetOrdProp(AObject,PI)));
    tkSString,
    tkLString,
    tkAString:
      Result:=TJSONString.Create(GetStrProp(AObject,PI));
    tkWString :
      Result:=TJSONString.Create(GetWideStrProp(AObject,PI));
    tkVariant:
      Result:=StreamVariant(GetVariantProp(AObject,PI));
    tkClass:
      Result:=StreamClassProperty(GetObjectProp(AObject,PI));
    tkWChar :
      Result:=TJSONString.Create(WideChar(GetOrdProp(AObject,PI)));
    tkBool :
      Result:=TJSONBoolean.Create(GetOrdProp(AObject,PropertyInfo)<>0);
    tkInt64 :
      Result:=TJSONInt64Number.Create(GetOrdProp(AObject,PropertyInfo));
    tkQWord :
      Result:=TJSONFloatNumber.Create(GetOrdProp(AObject,PropertyInfo));
    tkObject :
      Result:=ObjectToJSON(GetObjectProp(AObject,PropertyInfo));
    tkArray,
    tkRecord,
    tkInterface,
    tkDynArray,
    tkInterfaceRaw,
    tkProcVar,
    tkMethod :
      Error(SErrUnsupportedPropertyKind,[PI^.Name]);
    tkUString :
      Result:=TJSONString.Create(GetWideStrProp(AObject,PI));
    tkUChar:
      Result:=TJSONString.Create(UnicodeChar(GetOrdProp(AObject,PI)));
  end;
  If Assigned(FOnStreamProperty) then
    FOnStreamProperty(Self,AObject,PI,Result);
end;

function TJSONStreamer.FormatDateProp(Const DateTime: TDateTime): TJSONString;

Var
  S: String;

begin
  if (jsoCheckEmptyDateTime in Options) and (DateTime=0) then
    S:=''
  else if (DateTimeFormat<>'') then
    S:=FormatDateTime(DateTimeFormat,DateTime)
  else if (jsoLegacyDateTime in options) then  
    begin
    if Frac(DateTime)=0 then
      S:=DateToStr(DateTime)
    else if Trunc(DateTime)=0 then
      S:=TimeToStr(DateTime)
    else
      S:=DateTimeToStr(DateTime);
    end
  else
    S:=FormatDateTime(RFC3339DateTimeFormat,DateTime);
     
  Result:=TJSONString.Create(S);
end;

end.

