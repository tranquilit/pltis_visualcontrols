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
     FSearchIconSize: Integer;
     procedure SetSearchIconSize(Value: Integer);

  protected
      procedure SetParent(NewParent: TWinControl); override;
      procedure DoSetBounds(ALeft, ATop, AWidth, AHeight: integer); override;
      procedure SetUpEdit;
      procedure SetUpPanel;
      procedure SetUpImage;

  public
     constructor Create(AOwner: TComponent); override;

  published
      property EmbeddedImage: TImage read FEmbeddedImage write FEmbeddedImage;
      property ImagePanel: TPanel read FImagePanel write FImagePanel;
      property SearchIconSize: Integer read FSearchIconSize write SetSearchIconSize default 15;

  end;

procedure Register;

var
  default_x : Integer = 134;
  default_y : Integer = 24;
  FSearchIconSize: Integer = 15;

implementation

{$R icons.rc}

procedure Register;
begin
  RegisterComponents('Standard',[TWaptSearchEdit]);
end;

procedure TWaptSearchEdit.SetSearchIconSize(Value: Integer);
begin
  if FSearchIconSize = Value then exit;
  FSearchIconSize := Value;
  DoSetBounds(Left, Top, Width, Height);
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
  FImagePanel.SetBounds(ALeft + AWidth - 5 - FSearchIconSize, ATop + 3, FImagePanel.Width, FImagePanel.Height);
end;

procedure TWaptSearchEdit.SetUpEdit;
begin
  self.SetBounds(self.Top, self.Left, default_x, default_y);
end;

procedure TWaptSearchEdit.SetUpPanel;
begin
  FImagePanel.ControlStyle := FImagePanel.ControlStyle + [csNoDesignSelectable];
  FImagePanel.AutoSize := true;
end;

procedure TWaptSearchEdit.SetUpImage;
begin
  FEmbeddedImage.ControlStyle := FEmbeddedImage.ControlStyle + [csNoDesignSelectable];
  FEmbeddedImage.Picture.Create;
  FEmbeddedImage.Picture.LoadFromResourceName(HINSTANCE,'SEARCH_ICON',TPortableNetworkGraphic);
  FEmbeddedImage.stretch := true;
end;

constructor TWaptSearchEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetUpEdit;

  if FImagePanel = nil then
  begin
    FImagePanel := TPanel.Create(self);
    SetUpPanel;
  end;

  if FEmbeddedImage<>nil then exit;
  FEmbeddedImage := TImage.Create(FImagePanel);
  SetUpImage;
end;


end.
