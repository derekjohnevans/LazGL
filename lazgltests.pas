unit LazGLTests;

{$MODE DELPHI}

interface

uses
  Classes, Controls;

procedure CustomControlTest(const ACustomControl: TCustomControl);

implementation

uses customdrawn_common, customdrawndrawers, FileUtil, Graphics, GraphUtil, Types;

// These variables are created below in initialization.
var
  GPicture1, GPicture2: TPicture;
  GCustom: TCDDrawerCommon;

procedure TestDrawWindow(const ACanvas: TCanvas; var ARect: TRect; const ATitle: String);
begin
  ACanvas.Pen.Width := 1;
  DrawGradientWindow(ACanvas, ARect, 21, clDkGray);
  ACanvas.Font.Color := clWhite;
  ACanvas.Font.Height := 17;
  ACanvas.Font.Name := 'Arial';
  ACanvas.Brush.Style := bsClear;
  ACanvas.TextOut(ARect.Left + 4, ARect.Top + 2, ATitle);
  ARect.Top += 22;
  ARect.Left += 1;
  ARect.Bottom -= 1;
  DrawVerticalGradient(ACanvas, ARect, clBlack, clBtnShadow);
end;

procedure TestDrawPixels(const ACanvas: TCanvas; ARect: TRect);
var
  LIndex: Integer;
begin
  TestDrawWindow(ACanvas, ARect, 'Pixels');
  RandSeed := 0;
  for LIndex := 1 to 500 do begin
    ACanvas.Pixels[ARect.Left + Random(ARect.Right - ARect.Left), ARect.Top +
      Random(ARect.Bottom - ARect.Top)] := clYellow;
  end;
end;

procedure TestDrawLines(const ACanvas: TCanvas; ARect: TRect);
var
  LIndex: Integer;
begin
  TestDrawWindow(ACanvas, ARect, 'Lines');
  RandSeed := 0;
  ACanvas.MoveTo((ARect.Left + ARect.Right) div 2, (ARect.Top + ARect.Bottom) div 2);
  for LIndex := 1 to 50 do begin
    ACanvas.LineTo(ARect.Left + Random(ARect.Right - ARect.Left), ARect.Top +
      Random(ARect.Bottom - ARect.Top));
  end;
end;

procedure TestStretchDraw(const ACanvas: TCanvas; ARect: TRect);
begin
  TestDrawWindow(ACanvas, ARect, 'Stretch');
  ACanvas.StretchDraw(ARect, GPicture1.Bitmap);
end;

procedure TestEllipse(const ACanvas: TCanvas; ARect: TRect);
begin
  TestDrawWindow(ACanvas, ARect, 'Ellipse');
  InflateRect(ARect, -8, -8);
  ACanvas.Brush.Color := clRed;
  ACanvas.Pen.Color := clYellow;
  ACanvas.Pen.Width := 1;
  ACanvas.Ellipse(ARect);
end;

procedure TestRectangle(const ACanvas: TCanvas; ARect: TRect);
begin
  TestDrawWindow(ACanvas, ARect, 'Rectangle');
  InflateRect(ARect, -8, -8);
  ACanvas.Brush.Color := clPurple;
  ACanvas.Pen.Color := clRed;
  ACanvas.Pen.Width := 2;
  ACanvas.Rectangle(ARect);
end;

procedure TestFont(const ACanvas: TCanvas; ARect: TRect);
begin
  TestDrawWindow(ACanvas, ARect, 'Fonts');
  InflateRect(ARect, -8, -8);
  ACanvas.Font.Name := 'Wingdings';
  ACanvas.Brush.Style := bsClear;
  ACanvas.Font.Color := clBlue;
  ACanvas.Font.Height := 70;
  ACanvas.TextOut(ARect.Left + 32, ARect.Top, '[');
  ACanvas.Font.Height := 48;
  ACanvas.Font.Color := clWhite;
  ACanvas.Brush.Color := clRed;
  ACanvas.TextOut(ARect.Left, ARect.Top, 'A');
  ACanvas.Brush.Style := bsClear;
  ACanvas.Font.Color := clSkyBlue;
  ACanvas.TextOut(ARect.Left + 16, ARect.Top, 'h');
end;

procedure TestPolyLine(const ACanvas: TCanvas; ARect: TRect);

  function P(X, Y: Single): TPoint;
  begin
    Result := Point(Round(ARect.Left + (ARect.Right - ARect.Left) * X),
      Round(ARect.Top + (ARect.Bottom - ARect.Top) * Y));
  end;

begin
  TestDrawWindow(ACanvas, ARect, 'PolyLine');
  InflateRect(ARect, -8, -8);
  ACanvas.Pen.Color := clYellow;
  ACanvas.Pen.Width := 3;
  ACanvas.Polyline([P(0.5, 0), P(0, 0.5), P(0.5, 1), P(1, 0.5), P(0.5, 0),
    P(0.5, 0.5), P(0.3, 0.5), P(0.5, 0.7), P(0.7, 0.5), P(0.5, 0.5)]);
end;

procedure TestPolygon(const ACanvas: TCanvas; ARect: TRect);

  function P(X, Y: Single): TPoint;
  begin
    Result := Point(Round(ARect.Left + (ARect.Right - ARect.Left) * X),
      Round(ARect.Top + (ARect.Bottom - ARect.Top) * Y));
  end;

begin
  TestDrawWindow(ACanvas, ARect, 'Polygon');
  InflateRect(ARect, -8, -8);
  ACanvas.Pen.Color := clWhite;
  ACanvas.Pen.Width := 5;
  ACanvas.Brush.Color := clRed;
  ACanvas.Polygon([P(0.5, 0), P(0, 0.5), P(0.5, 1), P(1, 0.5), P(0.5, 0),
    P(0.5, 0.5), P(0.3, 0.5), P(0.5, 0.7), P(0.7, 0.5), P(0.5, 0.5)]);
end;

procedure TestSunkenFrame(const ACanvas: TCanvas; ARect: TRect);
begin
  TestDrawWindow(ACanvas, ARect, 'Sunken');
  InflateRect(ARect, -8, -8);
  GCustom.DrawSunkenFrame(ACanvas, ARect.TopLeft, Size(ARect));
end;

procedure TestRaisedFrame(const ACanvas: TCanvas; ARect: TRect);
begin
  TestDrawWindow(ACanvas, ARect, 'Raised');
  InflateRect(ARect, -8, -8);
  GCustom.DrawRaisedFrame(ACanvas, ARect.TopLeft, Size(ARect));
end;

procedure TestButtonWithArrow(const ACanvas: TCanvas; ARect: TRect);
begin
  TestDrawWindow(ACanvas, ARect, 'Button');
  InflateRect(ARect, -8, -8);
  GCustom.DrawButtonWithArrow(ACanvas, ARect.TopLeft, Size(ARect), [csfEnabled]);
end;

procedure TestSmallCloseButton(const ACanvas: TCanvas; ARect: TRect);
begin
  TestDrawWindow(ACanvas, ARect, 'Close');
  InflateRect(ARect, -8, -8);
  GCustom.DrawSmallCloseButton(ACanvas, ARect.TopLeft);
end;

procedure TestArc(const ACanvas: TCanvas; ARect: TRect);
begin
  TestDrawWindow(ACanvas, ARect, 'Arc');
  InflateRect(ARect, -8, -8);
  ACanvas.Arc(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom,
    ARect.Right, ARect.Top, ARect.Left, ARect.Bottom);
end;

procedure TestPie(const ACanvas: TCanvas; ARect: TRect);
begin
  TestDrawWindow(ACanvas, ARect, 'Pie');
  InflateRect(ARect, -8, -8);
  ACanvas.Pen.Color := clRed;
  ACanvas.Brush.Color := clBlue;
  ACanvas.Pie(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom,
    ARect.Right, ARect.Top, ARect.Left, ARect.Bottom);
end;

procedure TestRoundRect(const ACanvas: TCanvas; ARect: TRect);
begin
  TestDrawWindow(ACanvas, ARect, 'RoundRect');
  InflateRect(ARect, -8, -8);
  ACanvas.Pen.Color := clRed;
  ACanvas.Brush.Color := clBlue;
  ACanvas.RoundRect(ARect, 20, 20);
end;

procedure TextOut3D(const ACanvas: TCanvas; const X, Y: Integer; const AString: String);
begin
  ACanvas.Brush.Style := bsClear;
  ACanvas.Font.Color := clBlack;
  ACanvas.TextOut(X + 2, Y + 2, AString);
  ACanvas.Font.Color := clWhite;
  ACanvas.TextOut(X, Y, AString);
end;

type
  TProcTest = procedure(const ACanvas: TCanvas; ARect: TRect);

var
  GTests: array[0..14] of TProcTest = (TestStretchDraw, TestDrawPixels,
    TestDrawLines, TestEllipse, TestPolyLine, TestPolygon, TestRectangle,
    TestSunkenFrame, TestRaisedFrame, TestFont, TestButtonWithArrow,
    TestSmallCloseButton, TestArc, TestPie, TestRoundRect);

procedure CustomControlTest(const ACustomControl: TCustomControl);
var
  LLeft: Integer;
  LRect, LClient: TRect;
  LTest: TProcTest;
  LString: String;

begin
  with ACustomControl do begin
    LRect := ClientRect;

    Canvas.StretchDraw(LRect, GPicture2.Bitmap);

    Canvas.Frame3D(LRect, clBtnShadow, clBtnHighlight, 4);
    Canvas.Frame3D(LRect, clBtnHighlight, clBtnShadow, 4);

    LClient := Bounds(LRect.Left + 3, LRect.Top + 3, 100, 100);
    LLeft := LClient.Left;

    for LTest in GTests do begin
      LTest(ACustomControl.Canvas, LClient);
      OffsetRect(LClient, LClient.Right - LClient.Left + 8, 0);
      if LClient.Right > LRect.Right then begin
        LClient := Bounds(LLeft, LClient.Top, 100, 100);
        OffsetRect(LClient, 0, LClient.Bottom - LClient.Top + 8);
      end;
    end;

    Canvas.Font.Height := 48;
    Canvas.Font.Name := 'Impact';
    LString := ACustomControl.Canvas.ClassName;
    TextOut3D(ACustomControl.Canvas, (ClientWidth - Canvas.TextWidth(LString)) div 2,
      LRect.Bottom - Canvas.TextHeight(LString) - 8, LString);
  end;
end;

initialization

  GCustom := TCDDrawerCommon.Create;

  GPicture1 := TPicture.Create;
  GPicture2 := TPicture.Create;

  GPicture1.LoadFromFile(ProgramDirectory + '1.jpg');
  GPicture2.LoadFromFile(ProgramDirectory + '2.jpg');

end.
