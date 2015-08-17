unit LazGLCanvas;

{$MODE DELPHI}

interface

uses
  Classes, Graphics, GraphType, SysUtils, Types;

type

  TLazGLCanvas = class(TCanvas)
  protected
    procedure DoMoveTo(AX, AY: Integer); override;
    procedure DoLineTo(AX, AY: Integer); override;
    procedure DoLine(AX1, AY1, AX2, AY2: Integer); override;
    procedure DrawAsTexture(const ARect: TRect; const ABitmap: TBitmap);
    procedure SetPixel(X, Y: Integer; AColor: TColor); override;
  public
    function TextExtent(const AText: String): TSize; override;
    procedure Draw(AX, AY: Integer; AGraphic: TGraphic); override;
    procedure Ellipse(AX1, AY1, AX2, AY2: Integer); override;
    procedure FillRect(const ARect: TRect); override;
    procedure Polyline(APoints: Types.PPoint; ACount: Integer); override;
    procedure Polygon(APoints: Types.PPoint; ACount: Integer; AWinding: Boolean = False); override;
    procedure Rectangle(AX1, AY1, AX2, AY2: Integer); override;
    procedure StretchDraw(const ARect: TRect; AGraphic: TGraphic); override;
    procedure TextOut(AX, AY: Integer; const AText: String); override;
    procedure DrawFocusRect(const ARect: TRect); override;
    procedure Frame3D(var ARect: TRect; const AFrameWidth: Integer;
      const AStyle: TGraphicsBevelCut); override;
    procedure FrameRect(const ARect: TRect); override;
    procedure Arc(AX1, AY1, AX2, AY2, AAngle16Deg, AAngle16DegLength: Integer);
      override; overload;
    procedure Arc(AX1, AY1, AX2, AY2, SX, SY, EX, EY: Integer); override; overload;
    procedure RadialPie(AX1, AY1, AX2, AY2, AAngle16Deg, AAngle16DegLength: Integer); override;
    procedure Pie(AX1, AY1, AX2, AY2, SX, SY, EX, EY: Integer); override;
    procedure RoundRect(X1, Y1, X2, Y2: Integer; RX, RY: Integer); override;
  end;

implementation

uses GL, GraphMath, LazGL, LazGLTexture, LCLType, Math;

procedure TLazGLCanvas.DrawAsTexture(const ARect: TRect; const ABitmap: TBitmap);
var
  LIndex: Integer;
  LTexture: TLazTextureGL;
begin
  if GTextures.Find(ABitmap, LIndex) then begin
    LTexture := GTextures.Data[LIndex];
  end else begin
    LTexture := TLazTextureGL.Create(ABitmap);
  end;
  lglDrawTexture(LTexture.TextureId, ARect);
end;

procedure TLazGLCanvas.SetPixel(X, Y: Integer; AColor: TColor);
begin
  if lglSetColor(AColor) then begin
    glBegin(GL_POINTS);
    glVertex2i(X, Y);
    glEnd();
  end;
end;

procedure TLazGLCanvas.StretchDraw(const ARect: TRect; AGraphic: TGraphic);
var
  LBitmap: TBitmap;
  LTemp: TBitmap;
begin
  if Assigned(AGraphic) then begin
    LBitmap := AGraphic as TBitmap;

    if (LBitmap.Transparent and (LBitmap.Canvas.Font.Quality <> fqNonAntialiased)) or
      (ARect.Left < 0) or (ARect.Top < 0) then begin
      DrawAsTexture(ARect, LBitmap);
    end else begin
      if LBitmap.Transparent and (LBitmap.PixelFormat <> pf32bit) then begin
        LTemp := lglBitmapCreate32(LBitmap);
        try
          lglDrawBitmap(LTemp, ARect);
        finally
          FreeAndNil(LTemp);
        end;
      end else begin
        lglDrawBitmap(LBitmap, ARect);
      end;
    end;
  end;
end;

procedure TLazGLCanvas.Draw(AX, AY: Integer; AGraphic: TGraphic);
begin
  if Assigned(AGraphic) then begin
    StretchDraw(Bounds(AX, AY, AGraphic.Width, AGraphic.Height), AGraphic);
  end;
end;

procedure TLazGLCanvas.DoMoveTo(AX, AY: Integer);
begin
  // NOTHING TODO HERE
end;

procedure TLazGLCanvas.DoLineTo(AX, AY: Integer);
begin
  if lglSetPen(Pen) then begin
    lglDrawLine(FPenPos.X, FPenPos.Y, AX, AY);
    FPenPos.X := AX;
    FPenPos.Y := AY;
  end;
end;

procedure TLazGLCanvas.DoLine(AX1, AY1, AX2, AY2: Integer);
begin
  if lglSetPen(Pen) then begin
    lglDrawLine(AX1, AY1, AX2, AY2);
  end;
end;

procedure TLazGLCanvas.FillRect(const ARect: TRect);
begin
  if lglSetBrush(Brush) then begin
    lglDrawRectangle(GL_TRIANGLE_FAN, ARect);
  end;
end;

procedure TLazGLCanvas.Rectangle(AX1, AY1, AX2, AY2: Integer);
var
  LRect: TRect;
begin
  LRect := Rect(AX1, AY1, AX2, AY2);
  FillRect(LRect);
  if lglSetPen(Pen) then begin
    lglDrawRectangle(GL_LINE_LOOP, LRect);
  end;
end;

function TLazGLCanvas.TextExtent(const AText: String): TSize;
var
  LBitmap: TBitmap;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.Canvas.Font := Font;
    Result := LBitmap.Canvas.TextExtent(AText);
  finally
    FreeAndNil(LBitmap);
  end;
end;

procedure TLazGLCanvas.TextOut(AX, AY: Integer; const AText: String);
var
  LSize: TSize;
  LBitmap: TBitmap;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.Canvas.Brush := Brush;
    LBitmap.Canvas.Pen := Pen;
    LBitmap.Canvas.Font := Font;
    LBitmap.Canvas.Font.Quality := fqNonAntialiased;
    if LBitmap.Canvas.Brush.Style = bsClear then begin
      LBitmap.Transparent := True;
      LBitmap.TransparentColor := DefaultTransparentColor;
      LBitmap.Canvas.Brush.Color := DefaultTransparentColor;
    end;
    LSize := LBitmap.Canvas.TextExtent(AText);
    LBitmap.PixelFormat := pf32bit;
    LBitmap.SetSize(LSize.cx, LSize.cy);
    LBitmap.Canvas.TextOut(0, 0, AText);
    lglBitmapColorKey(LBitmap, DefaultTransparentColor);
    Draw(AX, AY, LBitmap);
  finally
    FreeAndNil(LBitmap);
  end;
end;

procedure TLazGLCanvas.Polyline(APoints: Types.PPoint; ACount: Integer);
begin
  if lglSetPen(Pen) then begin
    lglDrawPolyline(GL_LINE_STRIP, APoints, ACount);
  end;
end;

procedure TLazGLCanvas.Polygon(APoints: Types.PPoint; ACount: Integer; AWinding: Boolean = False);
begin
  // TODO: AWinding support.
  if lglSetBrush(Brush) then begin
    lglDrawPolyline(GL_POLYGON, APoints, ACount);
  end;
  if lglSetPen(Pen) then begin
    lglDrawPolyline(GL_LINE_LOOP, APoints, ACount);
  end;
end;

procedure TLazGLCanvas.Ellipse(AX1, AY1, AX2, AY2: Integer);
begin
  if lglSetBrush(Brush) then begin
    lglDrawEllipse(GL_TRIANGLE_FAN, Rect(AX1, AY1, AX2, AY2));
  end;
  if lglSetPen(Pen) then begin
    lglDrawEllipse(GL_LINE_STRIP, Rect(AX1, AY1, AX2, AY2));
  end;
end;

procedure TLazGLCanvas.DrawFocusRect(const ARect: TRect);
begin
  lglDrawFocusRect(ARect);
end;

procedure TLazGLCanvas.Frame3D(var ARect: TRect; const AFrameWidth: Integer;
  const AStyle: TGraphicsBevelCut);
begin
  Pen.Width := 1;
  case AStyle of
    bvLowered: begin
      inherited Frame3D(ARect, clBtnShadow, clBtnHighlight, AFrameWidth);
    end;
    bvRaised: begin
      inherited Frame3D(ARect, clBtnHighlight, clBtnShadow, AFrameWidth);
    end;
  end;
end;

procedure TLazGLCanvas.FrameRect(const ARect: TRect);
begin
  if lglSetBrush(Brush) then begin
    lglDrawRectangle(GL_LINE_LOOP, ARect);
  end;
end;

procedure TLazGLCanvas.Arc(AX1, AY1, AX2, AY2, AAngle16Deg, AAngle16DegLength: Integer);
begin
  lglDrawArc(GL_LINE_STRIP, Rect(AX1, AY1, AX2, AY2), AAngle16Deg, AAngle16DegLength);
end;

procedure TLazGLCanvas.Arc(AX1, AY1, AX2, AY2, SX, SY, EX, EY: Integer);
var
  LRect: TRect;
begin
  LRect := Rect(AX1, AY1, AX2, AY2);
  SX := Round(EccentricAngle(Point(SX, SY), LRect));
  EX := Round(EccentricAngle(Point(EX, EY), LRect));
  Arc(AX1, AY1, AX2, AY2, SX, EX - SX);
end;

procedure TLazGLCanvas.RadialPie(AX1, AY1, AX2, AY2, AAngle16Deg, AAngle16DegLength: Integer);
begin
  if lglSetBrush(Brush) then begin
    lglDrawArc(GL_TRIANGLE_FAN, Rect(AX1, AY1, AX2, AY2), AAngle16Deg,
      AAngle16DegLength);
  end;
  if lglSetPen(Pen) then begin
    lglDrawArc(GL_LINE_LOOP, Rect(AX1, AY1, AX2, AY2), AAngle16Deg,
      AAngle16DegLength);
  end;
end;

procedure TLazGLCanvas.Pie(AX1, AY1, AX2, AY2, SX, SY, EX, EY: Integer);
var
  LRect: TRect;
begin
  LRect := Rect(AX1, AY1, AX2, AY2);
  SX := Round(EccentricAngle(Point(SX, SY), LRect));
  EX := Round(EccentricAngle(Point(EX, EY), LRect));
  RadialPie(AX1, AY1, AX2, AY2, SX, EX - SX);
end;

procedure TLazGLCanvas.RoundRect(X1, Y1, X2, Y2: Integer; RX, RY: Integer);
begin

end;

end.















