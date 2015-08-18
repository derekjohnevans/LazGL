(*
Copyright (C) 2015 Derek John Evans

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
*)

unit LazGL;

{$MODE DELPHI}

interface

uses
  GL, Graphics, LCLType, SysUtils, Types;

const

  DefaultTransparentColor = clFuchsia;

type

  GlazFloat = Single;

procedure lglBitmapColorKey(const ABitmap: TBitmap; const ATransparentColor: TColorRef);

function lglBitmapCreate32(const ABitmap: TBitmap): TBitmap;

function lglSetColor(AColor: TColor): Boolean;
function lglSetPen(APen: TPen): Boolean;
function lglSetBrush(ABrush: TBrush): Boolean;
function lglGetPixelFormat(const APixelFormat: TPixelFormat): Integer;

procedure lglDrawLine(const AX1, AY1, AX2, AY2: Integer);
procedure lglDrawRectangle(const AMode: GLenum; const ARect: TRect);
procedure lglDrawFocusRect(const ARect: TRect);
procedure lglDrawArc(const AMode: GLenum; const ARect: TRect;
  AAngle16Deg, AAngle16DegLength: Integer);
procedure lglRoundRect(const AMode: GLenum; const ARect: TRect; const X, Y: Integer);
procedure lglDrawEllipse(const AMode: GLenum; const ARect: TRect);
procedure lglDrawPolyline(const AMode: GLenum; const APoints: PPoint; const ACount: Integer);
procedure lglDrawBitmap(const ABitmap: TBitmap; const ARect: TRect);
procedure lglDrawTexture(const ATexture: GLuint; const ARect: TRect);
procedure lglClearOrtho2D(const ARect: TRect; const AColor: TColor);

implementation

uses GLext, GLU, GraphMath;

procedure lglBitmapColorKey(const ABitmap: TBitmap; const ATransparentColor: TColorRef);
var
  LIndex: PtrUInt;
  LPixel: PByte;
begin
  if ABitmap.PixelFormat = pf32bit then begin
    LPixel := ABitmap.RawImage.Data;
    LIndex := 0;
    while LIndex < ABitmap.RawImage.DataSize do begin
      if (PDWord(LPixel)^ and clWhite) = ATransparentColor then begin
        LPixel[3] := 0;
      end else begin
        LPixel[3] := 255;
      end;
      LPixel += 4;
      LIndex += 4;
    end;
  end;
end;

function lglBitmapCreate32(const ABitmap: TBitmap): TBitmap;
begin
  Result := TBitmap.Create;
  try
    Result.PixelFormat := pf32bit;
    Result.SetSize(ABitmap.Width, ABitmap.Height);
    Result.Canvas.Brush.Color := DefaultTransparentColor;
    Result.Canvas.FillRect(0, 0, ABitmap.Width, ABitmap.Height);
    Result.Canvas.Draw(0, 0, ABitmap);
    lglBitmapColorKey(Result, DefaultTransparentColor);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function lglSetColor(AColor: TColor): Boolean;
begin
  Result := AColor <> clNone;
  if Result then begin
    AColor := ColorToRGB(AColor);
    glColor3ub(Red(AColor), Green(AColor), Blue(AColor));
  end;
end;

function lglSetPen(APen: TPen): Boolean;
begin
  glLineWidth(APen.Width);
  Result := lglSetColor(APen.Color);
end;

function lglSetBrush(ABrush: TBrush): Boolean;
begin
  Result := (ABrush.Style <> bsClear) and lglSetColor(ABrush.Color);
end;

function lglGetPixelFormat(const APixelFormat: TPixelFormat): Integer;
begin
  case APixelFormat of
    pf24bit: begin
      Result := GL_BGR;
    end;
    pf32bit: begin
      Result := GL_BGRA;
    end;
  end;
end;

procedure lglVertex(const APoint: TPoint);
begin
  glVertex2iv(@APoint);
end;

procedure lglDrawLine(const AX1, AY1, AX2, AY2: Integer);
begin
  glBegin(GL_LINES);
  glVertex2i(AX1, AY1);
  glVertex2i(AX2, AY2);
  glEnd;
end;

procedure lglDrawRectangle(const AMode: GLenum; const ARect: TRect);
begin
  glBegin(AMode);
  glVertex2f(ARect.Left + 1, ARect.Top + 1);
  glVertex2f(ARect.Left + 1, ARect.Bottom);
  glVertex2f(ARect.Right, ARect.Bottom);
  glVertex2f(ARect.Right, ARect.Top + 1);
  glEnd;
end;

procedure lglDrawFocusRect(const ARect: TRect);
begin
  glLineWidth(1);
  glEnable(GL_COLOR_LOGIC_OP);
  glLogicOp(GL_XOR);
  lglSetColor(clRed);
  lglDrawRectangle(GL_LINE_LOOP, ARect);
  glDisable(GL_COLOR_LOGIC_OP);
end;

procedure lglDrawEllipse(const AMode: GLenum; const ARect: TRect);
var
  LIndex: Integer;
begin
  glBegin(AMode);
  for LIndex := 0 to 360 do begin
    lglVertex(RadialPoint(LIndex * 16, ARect));
  end;
  glEnd;
end;

procedure lglVertexArc(const ARect: TRect; AAngle16Deg, AAngle16DegLength: Integer);

begin
  if AAngle16DegLength > 0 then begin
    while AAngle16DegLength > 0 do begin
      AAngle16DegLength -= 16;
      AAngle16Deg += 16;
      lglVertex(RadialPoint(AAngle16Deg, ARect));
    end;
  end else begin
    while AAngle16DegLength < 0 do begin
      AAngle16DegLength += 16;
      AAngle16Deg -= 16;
      lglVertex(RadialPoint(AAngle16Deg, ARect));
    end;
  end;
end;

procedure lglDrawArc(const AMode: GLenum; const ARect: TRect;
  AAngle16Deg, AAngle16DegLength: Integer);
begin
  glBegin(AMode);
  lglVertexArc(ARect, AAngle16Deg, AAngle16DegLength);
  glEnd;
end;

procedure lglRoundRect(const AMode: GLenum; const ARect: TRect; const X, Y: Integer);
begin
  glBegin(AMode);
  lglVertexArc(Bounds(ARect.Left, ARect.Top, X, Y), 90 * 16, 90 * 16);
  lglVertexArc(Bounds(ARect.Left, ARect.Bottom - Y, X, Y), 180 * 16, 90 * 16);
  lglVertexArc(Bounds(ARect.Right - X, ARect.Bottom - Y, X, Y), -90 * 16, 90 * 16);
  lglVertexArc(Bounds(ARect.Right - X, ARect.Top, X, Y), 0, 90 * 16);
  glEnd;
end;

procedure lglDrawPolyline(const AMode: GLenum; const APoints: Types.PPoint; const ACount: Integer);
var
  LIndex: Integer;
begin
  glBegin(AMode);
  for LIndex := 0 to ACount - 1 do begin
    glVertex2iv(@APoints[LIndex]);
  end;
  glEnd;
end;

procedure lglDrawBitmap(const ABitmap: TBitmap; const ARect: TRect);
begin
  if ABitmap.PixelFormat = pf32bit then begin
    glEnable(GL_BLEND);
  end;
  glPixelZoom((ARect.Right - ARect.Left) / ABitmap.Width, -(ARect.Bottom - ARect.Top) /
    ABitmap.Height);
  glRasterPos2i(ARect.Left, ARect.Top);
  glDrawPixels(ABitmap.Width, ABitmap.Height, lglGetPixelFormat(ABitmap.PixelFormat),
    GL_UNSIGNED_BYTE, ABitmap.RawImage.Data);
  glDisable(GL_BLEND);
end;

procedure lglDrawTexture(const ATexture: GLuint; const ARect: TRect);
begin
  glBindTexture(GL_TEXTURE_2D, ATexture);
  glEnable(GL_TEXTURE_2D);
  glEnable(GL_BLEND);
  glBegin(GL_TRIANGLE_FAN);
  try
    glColor3f(1, 1, 1);
    glTexCoord2f(0, 0);
    glVertex2f(ARect.Left, ARect.Top);
    glTexCoord2f(0, 1);
    glVertex2f(ARect.Left, ARect.Bottom);
    glTexCoord2f(1, 1);
    glVertex2f(ARect.Right, ARect.Bottom);
    glTexCoord2f(1, 0);
    glVertex2f(ARect.Right, ARect.Top);
  finally
    glEnd;
    glDisable(GL_TEXTURE_2D);
    glDisable(GL_BLEND);
  end;
end;

procedure lglClearOrtho2D(const ARect: TRect; const AColor: TColor);
begin
  glClearColor(Red(AColor) / 255, Green(AColor) / 255, Blue(AColor) / 255, 1.0);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  glViewport(ARect.Left, ARect.Top, ARect.Right - ARect.Left, ARect.Bottom - ARect.Top);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  gluOrtho2D(ARect.Left, ARect.Right, ARect.Bottom, ARect.Top);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  //glEnable(GL_LINE_SMOOTH);
  //glHint(GL_LINE_SMOOTH_HINT, GL_NICEST);
  //glFrontFace(GL_CCW);
  //glEnable(GL_CULL_FACE);
end;

end.
