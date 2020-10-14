unit WaptSearchEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type

  { TWaptSearchEdit }

  TWaptSearchEdit = class(TEdit)
  private
     FEmbeddedImage: TImage;
     FImagePanel: TPanel;
     FSearchIconSize: TConstraintSize;
     FSearchIconSpacingLeft: Integer;
     FSearchIconIsHidden: Boolean;
     procedure SetUpEdit;
     procedure SetUpPanel;
     procedure SetUpImage;
     procedure SetDefault;
     procedure SetSearchIconSize(Value: TConstraintSize);
     procedure SetSearchIconSpacingLeft(Value: Integer);
     function GetSearchIconVisible: Boolean;
     procedure SetSearchIconVisible(Value: Boolean);
     function GetOnSearchIconClick: TNotifyEvent;
     procedure SetOnSearchIconClick(Value: TNotifyEvent);
     function GetFontSize: Integer;
     procedure HideIconForText;

  protected
      procedure SetParent(NewParent: TWinControl); override;
      procedure DoSetBounds(ALeft, ATop, AWidth, AHeight: integer); override;

  public
     constructor Create(AOwner: TComponent); override;
     procedure TextChanged; override;
     procedure EnabledChanged; override;

  published
      property SearchIconSize: TConstraintSize read FSearchIconSize write SetSearchIconSize;
      property SearchIconSpacingLeft: Integer read FSearchIconSpacingLeft write SetSearchIconSpacingLeft;
      property SearchIconVisible: Boolean read GetSearchIconVisible write SetSearchIconVisible;
      property OnSearchIconClick: TNotifyEvent read GetOnSearchIconClick write SetOnSearchIconClick;

  end;

procedure Register;

implementation
{$R icons.rc}

procedure Register;
begin
  RegisterComponents('Wapt',[TWaptSearchEdit]);
end;

procedure TWaptSearchEdit.SetSearchIconSize(Value: TConstraintSize);
begin
  if FSearchIconSize = Value then Exit;
  FSearchIconSize := Value;
  DoSetBounds(Left, Top, Width, Height);
end;

procedure TWaptSearchEdit.SetSearchIconSpacingLeft(Value: Integer);
begin
  if FSearchIconSpacingLeft = Value then Exit;
  FSearchIconSpacingLeft := Value;
  if FSearchIconSpacingLeft >= 0 then
    FImagePanel.Color := clNone
  else
    FImagePanel.Color := FImagePanel.Parent.Color;
  DoSetBounds(Left, Top, Width, Height);
end;

function TWaptSearchEdit.GetSearchIconVisible: Boolean;
begin
  Result := FImagePanel.Visible;
end;

procedure TWaptSearchEdit.SetSearchIconVisible(Value: Boolean);
begin
  FImagePanel.Visible := Value;
end;

function TWaptSearchEdit.GetOnSearchIconClick: TNotifyEvent;
begin
  Result := FEmbeddedImage.OnClick;
end;

procedure TWaptSearchEdit.SetOnSearchIconClick(Value: TNotifyEvent);
begin
  FEmbeddedImage.OnClick := Value;
end;

function TWaptSearchEdit.GetFontSize: Integer;
var
  c: TBitmap;
  char_len: Integer;
begin
  if not (csDestroyingHandle in ControlState) then
  begin
    c := TBitmap.Create;
    c.Canvas.Font.Assign(self.Font);
    if length(self.Text) = 0 then
    begin
      Result := 0;
      Exit;
    end;
    char_len := c.Canvas.TextWidth(self.Text) div length(self.Text);
    Result := c.Canvas.TextWidth(self.Text) + char_len;
  end;
end;

procedure TWaptSearchEdit.HideIconForText;
var
  max_width : Integer;
begin
  if (not (csDestroyingHandle in ControlState)) and (FSearchIconSpacingLeft >= 0) then
  begin
    max_width := Width - FSearchIconSize - FSearchIconSpacingLeft;
    if (GetFontSize >= max_width) and GetSearchIconVisible then
    begin
      SetSearchIconVisible(false);
      FSearchIconIsHidden := true;
    end
    else if (GetFontSize < max_width) and FSearchIconIsHidden then
    begin
      SetSearchIconVisible(true);
      FSearchIconIsHidden := false;
    end;
  end;
end;

procedure TWaptSearchEdit.SetParent(NewParent: TWinControl);
begin
  inherited SetParent(NewParent);
  FImagePanel.Parent := NewParent;
  FEmbeddedImage.Parent := FImagePanel;
end;

procedure TWaptSearchEdit.DoSetBounds(ALeft, ATop, AWidth, AHeight: integer);
begin
  inherited DoSetBounds(ALeft, ATop, AWidth, AHeight);

  if FImagePanel = nil then Exit;
  FImagePanel.BorderSpacing.Right := FSearchIconSpacingLeft;
  FImagePanel.SetBounds(0, 0, FSearchIconSize, FSearchIconSize);

  if FEmbeddedImage = nil then Exit;
  FEmbeddedImage.SetBounds(0, 0,  FSearchIconSize, FSearchIconSize);
end;

procedure TWaptSearchEdit.SetUpEdit;
begin
  TextHint := 'Search keywords';
  Text := '';
  ControlStyle := ControlStyle - [csSetCaption];
end;

procedure TWaptSearchEdit.SetUpPanel;
begin
  FImagePanel.ControlStyle := FImagePanel.ControlStyle + [csNoDesignSelectable];
  FImagePanel.AutoSize := false;
  FImagePanel.Visible := true;
  FImagePanel.BevelOuter := bvNone;
  FImagePanel.BevelInner := bvNone;
  FImagePanel.Color := clNone;
  FImagePanel.Align := alNone;
  FImagePanel.Anchors := [akRight, akTop];
  FImagePanel.AnchorSide[akRight].Side := asrRight;
  FImagePanel.AnchorSide[akRight].Control := self;
  FImagePanel.AnchorSide[akTop].Side := asrCenter;
  FImagePanel.AnchorSide[akTop].Control := self;
end;

procedure TWaptSearchEdit.SetUpImage;
begin
  FEmbeddedImage.ControlStyle := FEmbeddedImage.ControlStyle + [csNoDesignSelectable];
  FEmbeddedImage.Picture.Create;
  FEmbeddedImage.Picture.LoadFromResourceName(HINSTANCE,'SEARCH_ICON',TPortableNetworkGraphic);
  FEmbeddedImage.stretch := true;
  FEmbeddedImage.Visible := true;
  FEmbeddedImage.Cursor := crHandPoint;
  FEmbeddedImage.Align := alNone;
  FEmbeddedImage.Anchors := [];
  FSearchIconIsHidden := false;
end;

procedure TWaptSearchEdit.SetDefault;
begin
  FSearchIconSize := 15;
  Width := 130;
  Height := 24;
  FSearchIconSpacingLeft := 5;
end;

constructor TWaptSearchEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetDefault;
  SetUpEdit;

  if FImagePanel = nil then
  begin
    FImagePanel := TPanel.Create(self);
    SetUpPanel;
  end;

  if FEmbeddedImage = nil then
  begin
    FEmbeddedImage := TImage.Create(FImagePanel);
    SetUpImage;
  end;
end;

procedure TWaptSearchEdit.TextChanged;
begin
  inherited TextChanged;
  HideIconForText;
end;

procedure TWaptSearchEdit.EnabledChanged;
begin
  inherited EnabledChanged;
  if self.Enabled = False then
    FImagePanel.Color := clDefault
  else
  begin
      if FSearchIconSpacingLeft >= 0 then
        FImagePanel.Color := clNone
      else
        FImagePanel.Color := FImagePanel.Parent.Color;
  end;
end;

initialization
  {$I waptsearchedit.lrs}

end.
