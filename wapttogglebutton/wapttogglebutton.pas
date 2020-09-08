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
    ToggleState: Boolean;
    FClickPan: TPaintBox;
    function getImageOne: TBitmap;
    procedure setImageOne(Value: TBitmap);
    function getImageTwo: TBitmap;
    procedure setImageTwo(Value: TBitmap);
    procedure SetPanInvisible(Pan: TPaintBox);

  protected
    procedure SetParent(NewParent: TWinControl); override;
    procedure DoSetBounds(ALeft, ATop, AWidth, AHeight: integer); override;
    procedure Loaded; override;

  public
    procedure OnClickPan(Sender: TObject);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

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
  if ToggleState then
    Glyph := FImage2.Glyph
  else
    Glyph := FImage1.Glyph;
  if Assigned(OnClick) then OnClick(Sender);
end;

procedure TWaptToggleButton.Loaded;
begin
  inherited Loaded;
  Glyph := FImage1.Glyph;

  if FClickPan = nil then
  begin
    FClickPan := TPaintBox.Create(self.Parent);
    SetPanInvisible(FClickPan);
    FClickPan.OnClick := @OnClickPan;
    SetParent(self.Parent);
    ShowMessage('T1');
    DoSetBounds(Left, Top, Width, Height);
  end;
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

procedure TWaptToggleButton.SetPanInvisible(Pan: TPaintBox);
begin
  Pan.Caption := '';
 // Pan.BevelOuter := bvNone;
  //Pan.BevelInner := bvNone;
  Pan.Color := clDefault;
  Pan.Cursor := crDefault;
 // PAN.Color := clRed;
  //Pan.ControlStyle := Pan.ControlStyle + [csNoDesignSelectable];
end;

procedure TWaptToggleButton.SetParent(NewParent: TWinControl);
begin
  inherited SetParent(NewParent);

  if FClickPan = nil then exit;
  FClickPan.Parent := NewParent;
end;

procedure TWaptToggleButton.DoSetBounds(ALeft, ATop, AWidth, AHeight: integer);
begin
  inherited DoSetBounds(ALeft, ATop, AWidth, AHeight);

  if FClickPan = nil then Exit;
  ShowMessage('T2');
  FClickPan.SetBounds(ALeft, ATop, AWidth, AHeight);
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

initialization
  {$I wapttogglebutton.lrs}


end.
