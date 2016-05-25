{****************************************}
{                                        }
{  ATFileNotif for Lazarus               }
{  Copyright (C) Alexey Torgashin        }
{  http://uvviewsoft.com                 }
{                                        }
{****************************************}
{
Example of usage:
  procedure TFormMain.NotifyFile;
  begin
    with ATFileNotif1 do
    begin
      Timer.Enabled:= False;
      Timer.Interval:= 1000;
      FileName:= edFileName.Text;
      Timer.Enabled:= True;
    end;
  end;
}

{$mode objfpc}
{$BOOLEVAL OFF} //Short boolean evaluation.

unit ATFileNotif;

interface

uses
  SysUtils, Classes, ExtCtrls, LazFileUtils;

type
  TATFileSimpleRec = record
    FExist: Boolean;
    FSize: Int64;
    FAge: Longint;
  end;

type
  TATFileNotif = class(TComponent)
  private
    { Private declarations }
    FFileName: string;
    FFileRec: TATFileSimpleRec;
    FTimer: TTimer;
    FTimerBusy: Boolean;
    FOnChanged: TNotifyEvent;
    procedure SetFileName(const AValue: string);
    procedure TimerTimer(Sender: TObject);
    procedure DoChanged;
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    property FileName: string read FFileName write SetFileName;
    property Timer: TTimer read FTimer;
  published
    { Published declarations }
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  end;


implementation

{ Helper functions }

procedure FGetFileRec(const FileName: string; var Rec: TATFileSimpleRec);
begin
  FillChar(Rec, SizeOf(Rec), 0);
  Rec.FExist:= FileExistsUTF8(FileName);
  Rec.FSize:= FileSizeUtf8(FileName);
  Rec.FAge:= FileAgeUTF8(FileName);
end;

function FFileChanged(const FileName: string; var OldRec: TATFileSimpleRec): Boolean;
var
  NewRec: TATFileSimpleRec;
begin
  FGetFileRec(FileName, NewRec);

  Result:=
    (OldRec.FExist <> NewRec.FExist) or
    (OldRec.FSize <> NewRec.FSize) or
    (OldRec.FAge <> NewRec.FAge);

  if Result then
    Move(NewRec, OldRec, SizeOf(NewRec));
end;


{ TATFileNotif }

constructor TATFileNotif.Create(AOwner: TComponent);
begin
  inherited;
  FFileName:= '';
  FillChar(FFileRec, SizeOf(FFileRec), 0);
  FTimer:= TTimer.Create(Self);
  with FTimer do
  begin
    Enabled:= False;
    Interval:= 1000;
    OnTimer:= @TimerTimer;
  end;
  FTimerBusy:= False;
end;

procedure TATFileNotif.SetFileName(const AValue: string);
var
  En: Boolean;
begin
  En:= FTimer.Enabled;
  FTimer.Enabled:= False;

  FFileName:= AValue;
  FGetFileRec(FFileName, FFileRec);
  if (FFileName <> '') and (not FFileRec.FExist) then
    raise Exception.Create('File to watch doesn''t exist');

  FTimer.Enabled:= En;
end;

procedure TATFileNotif.TimerTimer(Sender: TObject);
begin
  if not FTimerBusy then
    try
      FTimerBusy:= True;
      if FFileChanged(FFileName, FFileRec) then
        DoChanged;
    finally
      FTimerBusy:= False;
    end;
end;

procedure TATFileNotif.DoChanged;
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

end.
