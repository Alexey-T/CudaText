{ TAboutScrollText and TAboutBox Component License

  Copyright (C) 2014 Gordon Bamber minesadorada@charcodelvalle.com

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}
unit AboutScrolltextunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, Controls, Dialogs, Forms, Graphics, LResources, SysUtils,
  ExtCtrls, StdCtrls, StrUtils,Buttons;
const
  C_DEFAULTLICENSEFORMWIDTH = 500;
  C_DEFAULTLICENSEFORMWIDTH_LINUX = C_DEFAULTLICENSEFORMWIDTH + 100;
  C_DEFAULTLICENSEFORMHEIGHT = 400;
  C_DEFAULTLICENSEFORMHEIGHT_LINUX = C_DEFAULTLICENSEFORMHEIGHT + 50;

type
  TLicenseType = (abNone, abGPL, abLGPL, abMIT, abModifiedGPL, abProprietry);

  TAboutScrollText = class(TGraphicControl)
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override; // Constructor must be public
    destructor Destroy; override; // Destructor must be public

  published
  end;

{For i8n if required}
resourcestring
  rs_Componentname='Component name';
  rs_About='About';
  rs_License='License';
  rs_By='By';
  rs_For='For';
  rs_DatafileMissing='Resource datafile license.lrs is missing';
  rs_LicenseTextError='There is something wrong with the Licence text';
  rs_AboutBoxError = 'Subcomponent TAboutBox Error';


implementation

constructor TAboutScrollText.Create(AOwner: TComponent);
begin
  // Inherit default properties
  inherited Create(AOwner);
end;

destructor TAboutScrollText.Destroy;
begin
  inherited Destroy;
end;

end.
