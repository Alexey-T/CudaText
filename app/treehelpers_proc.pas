(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
unit TreeHelpers_Proc;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils,
  ATSynEdit,
  TreeHelpers_Base;

function TreeHelperInPascal(Ed: TATSynEdit; const ALexer: string;
  Data: TATTreeHelperRecords): boolean;


implementation

uses
  TreeHelper_Markdown,
  TreeHelper_MediaWiki,
  TreeHelper_Ini,
  TreeHelper_reST,
  TreeHelper_Textile,
  TreeHelper_WikidPad;

//--------------------------------------------------------------
function TreeHelperInPascal(Ed: TATSynEdit; const ALexer: string;
  Data: TATTreeHelperRecords): boolean;
begin
  Result:= false;
  Data.Clear;
  case ALexer of
    'Ini files ^':
      begin
        Result:= true;
        TTreeHelperIni.GetHeaders(Ed, Data);
      end;
    'Markdown':
      begin
        Result:= true;
        TTreeHelperMarkdown.GetHeaders(Ed, Data);
      end;
    'MediaWiki':
      begin
        Result:= true;
        TTreeHelperMediawiki.GetHeaders(Ed, Data);
      end;
    'reStructuredText':
      begin
        Result:= true;
        TTreeHelperRest.GetHeaders(Ed, Data);
      end;
    'WikidPad',
    'WikidPad ^':
      begin
        Result:= true;
        TTreeHelperWikidpad.GetHeaders(Ed, Data);
      end;
    'Textile':
      begin
        Result:= true;
        TTreeHelperTextile.GetHeaders(Ed, Data);
      end;
  end;
end;


end.

