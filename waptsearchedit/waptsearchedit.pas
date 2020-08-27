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
     FSearchIconSpacingLeft: TConstraintSize;
     procedure SetUpEdit;
     procedure SetUpPanel;
     procedure SetUpImage;
     procedure SetDefault;
     procedure SetSearchIconSize(Value: TConstraintSize);
     procedure SetSearchIconSpacingLeft(Value: TConstraintSize);
     function GetSearchIconVisible: Boolean;
     procedure SetSearchIconVisible(Value: Boolean);


  protected
      procedure SetParent(NewParent: TWinControl); override;
      procedure DoSetBounds(ALeft, ATop, AWidth, AHeight: integer); override;

  public
     constructor Create(AOwner: TComponent); override;

  published
      property EmbeddedImage: TImage read FEmbeddedImage write FEmbeddedImage;
      property ImagePanel: TPanel read FImagePanel write FImagePanel;
      property SearchIconSize: TConstraintSize read FSearchIconSize write SetSearchIconSize;
      property SearchIconSpacingLeft: TConstraintSize read FSearchIconSpacingLeft write SetSearchIconSpacingLeft;
      property SearchIconVisible: Boolean read GetSearchIconVisible write SetSearchIconVisible;

  end;

procedure Register;

implementation

{$R icons.rc}

procedure Register;
begin
  RegisterComponents('Standard',[TWaptSearchEdit]);
end;

procedure TWaptSearchEdit.SetSearchIconSize(Value: TConstraintSize);
begin
  if FSearchIconSize = Value then exit;
  FSearchIconSize := Value;
  DoSetBounds(Left, Top, Width, Height);
end;

procedure TWaptSearchEdit.SetSearchIconSpacingLeft(Value: TConstraintSize);
begin
  if FSearchIconSpacingLeft = Value then exit;
  FSearchIconSpacingLeft := Value;
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

procedure TWaptSearchEdit.SetParent(NewParent: TWinControl);
begin
  inherited SetParent(NewParent);
  FImagePanel.Parent := NewParent;
  FEmbeddedImage.Parent := FImagePanel;
end;

procedure TWaptSearchEdit.DoSetBounds(ALeft, ATop, AWidth, AHeight: integer);
begin
  inherited DoSetBounds(ALeft, ATop, AWidth, AHeight);

  if FEmbeddedImage = nil then exit;
  FEmbeddedImage.SetBounds(ALeft, ATop,  FSearchIconSize, FSearchIconSize);

  if FImagePanel = nil then exit;
  FImagePanel.SetBounds(ALeft + AWidth - FSearchIconSpacingLeft - FSearchIconSize, ATop + (height div 2 - SearchIconSize div 2) - 1, FImagePanel.Width, FImagePanel.Height);

  FImagePanel.Visible := (Width > (FSearchIconSize * 2 + 5));
end;

procedure TWaptSearchEdit.SetUpEdit;
begin
  self.SetBounds(self.Top, self.Left, 130, 24);
end;

procedure TWaptSearchEdit.SetUpPanel;
begin
  FImagePanel.ControlStyle := FImagePanel.ControlStyle + [csNoDesignSelectable];
  FImagePanel.AutoSize := true;
  FImagePanel.Visible := true;
end;

procedure TWaptSearchEdit.SetUpImage;
begin
  FEmbeddedImage.ControlStyle := FEmbeddedImage.ControlStyle + [csNoDesignSelectable];
  FEmbeddedImage.Picture.Create;
  FEmbeddedImage.Picture.LoadFromResourceName(HINSTANCE,'SEARCH_ICON',TPortableNetworkGraphic);
  FEmbeddedImage.stretch := true;
  FEmbeddedImage.Visible := true;
end;

procedure TWaptSearchEdit.SetDefault;
begin
  FSearchIconSize := 15;
  width := 130;
  FSearchIconSpacingLeft := 5;
end;

constructor TWaptSearchEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetDefault;

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


end.
