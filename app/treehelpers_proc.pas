(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
unit TreeHelpers_Proc;

{$mode ObjFPC}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  Classes, SysUtils,
  ATSynEdit,
  ATSynEdit_fgl;

type
  { TATTreeHelperRecord }

  PATTreeHelperRecord = ^TATTreeHelperRecord;
  TATTreeHelperRecord = record
    X1, Y1, X2, Y2: integer;
    Level: integer;
    Icon: integer;
    Title: string[39];
    class operator =(const A, B: TATTreeHelperRecord): boolean;
  end;

  { TATTreeHelperRecords }

  TATTreeHelperRecords = class(specialize TFPGList<TATTreeHelperRecord>)
  private
    function GetItemPtr(N: integer): PATTreeHelperRecord;
  public
    property ItemPtr[N: integer]: PATTreeHelperRecord read GetItemPtr;
  end;

procedure TreeHelperInPascal(Ed: TATSynEdit; const ALexer: string;
  Data: TATTreeHelperRecords);

implementation

procedure TreeHelperInPascal(Ed: TATSynEdit; const ALexer: string;
  Data: TATTreeHelperRecords);
begin
  Data.Clear;
  case ALexer of
    'Markdown':
      begin
        //TreeHelperForMarkdown(Ed, Data);
      end;
  end;
end;

{ TATTreeHelperRecords }

function TATTreeHelperRecords.GetItemPtr(N: integer): PATTreeHelperRecord;
begin
  Result:= PATTreeHelperRecord(InternalGet(N));
end;


{ TATTreeHelperRecord }

class operator TATTreeHelperRecord.=(const A, B: TATTreeHelperRecord): boolean;
begin
  Result:= false;
end;

end.

