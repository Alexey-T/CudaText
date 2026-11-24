{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Markdown emoji shortcode registry.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit Markdown.Emoji;

{$mode ObjFPC}{$H+}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.SysUtils, System.Contnrs,
{$ELSE}
  SysUtils, Contnrs,
{$ENDIF}
  Markdown.Utils,
  Markdown.InlineText;

Type
  // Consulted for a name that is not registered. Return False to leave the shortcode alone.
  TEmojiLookupEvent = function(const aName : TMarkdownString; out aReplacement : TMarkdownString) : Boolean of object;

  { TEmojiRegistry }

  // Replacements for :name: shortcodes. Using this unit makes the inline processor consult it.
  TEmojiRegistry = Class(TObject)
  Private
    Class Var
      FNames : TFPStringHashTable;
      FMarkup : TFPStringHashTable;
      FOnLookup : TEmojiLookupEvent;
      FOnLookupMarkup : TEmojiLookupEvent;
    class function GetCount: Integer; static;
    class function GetMarkupCount: Integer; static;
    class function GetNameChars: TSysCharSet; static;
    class procedure SetNameChars(const aValue: TSysCharSet); static;
  Public
    class constructor Init;
    class destructor Done;
    // Register the replacement text for :aName:. An already registered name is replaced.
    class procedure Register(const aName, aReplacement : TMarkdownString);
    // Register replacement markup for :aName:, emitted without escaping.
    class procedure RegisterMarkup(const aName, aMarkup : TMarkdownString);
    // Forget a registered name, text or markup.
    class procedure UnRegister(const aName : TMarkdownString);
    // Replacement for aName, from the registered names or from OnLookup.
    class function Find(const aName : TMarkdownString; out aReplacement : TMarkdownString) : Boolean;
    // Replacement markup for aName, from the registered names or from OnLookupMarkup.
    class function FindMarkup(const aName : TMarkdownString; out aMarkup : TMarkdownString) : Boolean;
    // Register the names in markdown.emoji.inc.
    class procedure RegisterDefaults;
    // Forget all registered names.
    class procedure Clear;
    // Number of names registered as text.
    class property Count : Integer Read GetCount;
    // Number of names registered as markup.
    class property MarkupCount : Integer Read GetMarkupCount;
    // Consulted when a name is not registered, so open ended sets need no registration.
    class property OnLookup : TEmojiLookupEvent Read FOnLookup Write FOnLookup;
    // Same, for markup. Consulted after OnLookup.
    class property OnLookupMarkup : TEmojiLookupEvent Read FOnLookupMarkup Write FOnLookupMarkup;
    // Characters allowed in a name.
    class property NameChars : TSysCharSet Read GetNameChars Write SetNameChars;
  end;

implementation

Type
  TEmojiDefault = Record
    Name : TMarkdownString;
    Text : TMarkdownString;
  end;

{$i markdown.emoji.inc}

{ TEmojiRegistry }

class constructor TEmojiRegistry.Init;

begin
  FNames:=TFPStringHashTable.Create;
  FMarkup:=TFPStringHashTable.Create;
end;


class destructor TEmojiRegistry.Done;

begin
  FreeAndNil(FNames);
  FreeAndNil(FMarkup);
end;


class function TEmojiRegistry.GetCount: Integer;

begin
  Result:=FNames.Count;
end;


class function TEmojiRegistry.GetMarkupCount: Integer;

begin
  Result:=FMarkup.Count;
end;


class function TEmojiRegistry.GetNameChars: TSysCharSet;

begin
  Result:=TInlineTextProcessor.EmojiNameChars;
end;


class procedure TEmojiRegistry.SetNameChars(const aValue: TSysCharSet);

begin
  TInlineTextProcessor.EmojiNameChars:=aValue;
end;


class procedure TEmojiRegistry.Register(const aName, aReplacement : TMarkdownString);

begin
  if (aName='') or (aReplacement='') then
    Exit;
  // A name is text or markup, not both.
  FMarkup.Delete(LowerCase(aName));
  FNames.Items[LowerCase(aName)]:=aReplacement;
end;


class procedure TEmojiRegistry.RegisterMarkup(const aName, aMarkup : TMarkdownString);

begin
  if (aName='') or (aMarkup='') then
    Exit;
  FNames.Delete(LowerCase(aName));
  FMarkup.Items[LowerCase(aName)]:=aMarkup;
end;


class procedure TEmojiRegistry.UnRegister(const aName : TMarkdownString);

begin
  FNames.Delete(LowerCase(aName));
  FMarkup.Delete(LowerCase(aName));
end;


class function TEmojiRegistry.Find(const aName : TMarkdownString; out aReplacement : TMarkdownString) : Boolean;

begin
  aReplacement:=FNames.Items[LowerCase(aName)];
  Result:=aReplacement<>'';
  if (not Result) and Assigned(FOnLookup) then
    Result:=FOnLookup(aName,aReplacement) and (aReplacement<>'');
end;


class function TEmojiRegistry.FindMarkup(const aName : TMarkdownString; out aMarkup : TMarkdownString) : Boolean;

begin
  aMarkup:=FMarkup.Items[LowerCase(aName)];
  Result:=aMarkup<>'';
  if (not Result) and Assigned(FOnLookupMarkup) then
    Result:=FOnLookupMarkup(aName,aMarkup) and (aMarkup<>'');
end;


class procedure TEmojiRegistry.RegisterDefaults;

Var
  I : Integer;

begin
  For I:=1 to DefaultEmojiCount do
    Register(DefaultEmoji[I].Name,DefaultEmoji[I].Text);
end;


class procedure TEmojiRegistry.Clear;

begin
  FNames.Clear;
  FMarkup.Clear;
end;


// Emoji resolver for the inline processor: registered text first, then registered markup.
function ResolveEmoji(const aName : TMarkdownString; out aReplacement : TMarkdownString; out aMarkup : Boolean) : Boolean;

begin
  aMarkup:=False;
  Result:=TEmojiRegistry.Find(aName,aReplacement);
  if not Result then
    begin
    Result:=TEmojiRegistry.FindMarkup(aName,aReplacement);
    aMarkup:=Result;
    end;
end;


initialization
  TInlineTextProcessor.EmojiResolver:=@ResolveEmoji;

finalization
  TInlineTextProcessor.EmojiResolver:=Nil;
end.
