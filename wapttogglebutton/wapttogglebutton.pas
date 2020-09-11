unit WaptToggleButton;

{$mode objfpc}{$H+}

interface

uses
  Classes, LResources, SysUtils, Controls, Graphics, Buttons, ExtCtrls, Dialogs;

type

  { TWaptToggleButton }

  TWaptToggleButton = class(TCustomSpeedButton)
  private
    FImage1: TButtonGlyph;
    FImage2: TButtonGlyph;
    FClickBox: TPaintBox;
    function getImageOne: TBitmap;
    procedure setImageOne(Value: TBitmap);
    function getImageTwo: TBitmap;
    procedure setImageTwo(Value: TBitmap);
    procedure SetBoxDefault(Box: TPaintBox);
    procedure InterMouseEnter(Sender: TObject);
    procedure InterMouseLeave(Sender: TObject);

  protected
    procedure SetParent(NewParent: TWinControl); override;
    procedure DoSetBounds(ALeft, ATop, AWidth, AHeight: integer); override;
    procedure Loaded; override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;

  public
    ToggleState: Boolean;
    procedure OnClickPan(Sender: TObject);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetButtonState(TState: Boolean);

  published
    property ImageState1: TBitmap read getImageOne write setImageOne;
    property ImageState2: TBitmap read getImageTwo write setImageTwo;
    property Action;
    property Align;
    property AllowAllUp;
    property Anchors;
    property AutoSize;
    property BidiMode;
    property BorderSpacing;
    property Constraints;
    property Caption;
    property Color;
    property Down;
    property Enabled;
    property Flat;
    property Font;
    property GroupIndex;
    property ImageWidth;
    property Layout;
    property Margin;
    property Spacing;
    property Transparent;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnPaint;
    property OnResize;
    property OnChangeBounds;
    property ShowCaption;
    property ShowHint;
    property ParentBidiMode;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Wapt',[TWaptToggleButton]);
end;

{ TWaptToggleButton }

procedure TWaptToggleButton.OnClickPan(Sender: TObject);
begin
  ToggleState := (not ToggleState);
  SetButtonState(ToggleState);
  if Assigned(OnClick) then OnClick(Sender);
end;

procedure TWaptToggleButton.Loaded;
begin
  inherited Loaded;
  Glyph := FImage1.Glyph;

  if FClickBox = nil then
  begin
    FClickBox := TPaintBox.Create(self);
    SetBoxDefault(FClickBox);
    SetParent(self.Parent);
    DoSetBounds(Left, Top, Width, Height);
  end;
end;

procedure TWaptToggleButton.MouseEnter;
begin
  inherited MouseEnter;
end;

procedure TWaptToggleButton.MouseLeave;
begin
  inherited MouseLeave;
end;

function TWaptToggleButton.getImageOne: TBitmap;
begin
  Result := FImage1.Glyph;
end;

procedure TWaptToggleButton.setImageOne(Value: TBitmap);
begin
  FImage1.Glyph := Value;
  Glyph := Value;
  Invalidate;
end;

function TWaptToggleButton.getImageTwo: TBitmap;
begin
  Result := FImage2.Glyph;
end;

procedure TWaptToggleButton.setImageTwo(Value: TBitmap);
begin
  FImage2.Glyph := Value;
  Invalidate;
end;

procedure TWaptToggleButton.SetBoxDefault(Box: TPaintBox);
begin
  Box.Caption := '';
  Box.Color := clDefault;
  Box.Cursor := crDefault;
  Box.ControlStyle := Box.ControlStyle + [csNoDesignSelectable];
  Box.OnClick := @OnClickPan;
  Box.OnMouseEnter := @InterMouseEnter;
  Box.OnMouseleave := @InterMouseLeave;
end;

procedure TWaptToggleButton.InterMouseEnter(Sender: TObject);
begin
  MouseEnter;
end;

procedure TWaptToggleButton.InterMouseLeave(Sender: TObject);
begin
  MouseLeave;
end;

procedure TWaptToggleButton.SetParent(NewParent: TWinControl);
begin
  inherited SetParent(NewParent);

  if FClickBox = nil then exit;
  FClickBox.Parent := NewParent;
end;

procedure TWaptToggleButton.DoSetBounds(ALeft, ATop, AWidth, AHeight: integer);
begin
  inherited DoSetBounds(ALeft, ATop, AWidth, AHeight);

  if FClickBox = nil then Exit;
  FClickBox.SetBounds(ALeft, ATop, AWidth, AHeight);
end;

constructor TWaptToggleButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ToggleState := false;
  FImage1 := TButtonGlyph.Create;
  FImage2 := TButtonGlyph.Create;
end;

destructor TWaptToggleButton.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FImage1);
  FreeAndNil(FImage2);
end;

procedure TWaptToggleButton.SetButtonState(TState: Boolean);
begin
    if TState then
    Glyph := FImage2.Glyph
  else
    Glyph := FImage1.Glyph;
end;

initialization
  {$I wapttogglebutton.lrs}


end.
