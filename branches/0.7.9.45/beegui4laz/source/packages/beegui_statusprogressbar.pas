{
    Copyright (c) 2006 Andrew Filinsky and Melchiorre Caruso

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
}

{   Contains:

    TStatusProgressBar class.

    Modifyed:

    v1.0.3 build 0020 - 2006/11/25 by Melchiorre Caruso.
}

unit BeeGui_StatusProgressBar;

interface

uses
  Classes,
  ComCtrls,
  SysUtils,
  LResources;

type
  TStatusProgressBar = class (TStatusBar)
  private
    FProgressBar: TProgressBar;
    // ---
    procedure SetProgressBar(Value: TProgressBar);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure DrawProgressBar(PanelIndex: Integer);
  published
    property ProgressBar: TProgressBar read FProgressBar write SetProgressBar default nil;
  end;

  { Register }

  procedure Register;

implementation

  { TStatusProgressBar }

  constructor TStatusProgressBar.Create(TheOwner: TComponent);
  begin
    inherited Create(TheOwner);
  end;

  destructor TStatusProgressBar.Destroy;
  begin
    inherited Destroy;
  end;

  procedure TStatusProgressBar.SetProgressBar(Value: TProgressBar);
  begin
    FProgressBar := Value;
  end;
  
  procedure TStatusProgressBar.DrawProgressBar(PanelIndex: integer);
  var
    i: integer;
    LeftPos: integer;
  begin
    {$IFDEF WINDOWS}
      Self.Panels[PanelIndex].Bevel := pbNone;
    {$ELSE}
      Self.Panels[PanelIndex].Bevel := pbLowered;
    {$ENDIF}
    LeftPos := Self.Left + Self.BorderWidth;
    for i := 0 to PanelIndex - 1 do
    begin
      Inc(LeftPos, Self.Panels[i].Width + 2);
    end;
    // ---
    if Panels.Count > 0 then
    begin
      {$IFDEF WINDOWS}
        FProgressBar.Height := Self.Height - 2;
        FProgressBar.Width  := Self.Panels[PanelIndex].Width - 18;
        // ---
        FProgressBar.Top    := Self.Top + 2;
        FProgressBar.Left   := LeftPos;
      {$ELSE}
        FProgressBar.Height := Self.Height - 3;
        FProgressBar.Width  := Self.Panels[PanelIndex].Width - 2;
        // ---
        FProgressBar.Top    := Self.Top + 2;
        FProgressBar.Left   := LeftPos;
      {$ENDIF}
    end;
  end;
  
  { Register }

  procedure Register;
  begin
    RegisterComponents('BeeGui', [TStatusProgressBar]);
  end;
  
initialization

 {$I beegui_statusprogressbar.lrs }
  
end.

