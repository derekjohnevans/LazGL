unit LazGLControl;

{$MODE DELPHI}

interface

uses
  Classes, Controls, Graphics, LCLType, SysUtils;

type

  TLazGLControl = class(TCustomControl)
  private
    FOldCanvas: TCanvas;
    FControl: TControl;
  private
    procedure DoOnPaint(ASender: TObject);
  protected
    procedure PaintWindow(DC: HDC); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses LazGL, LazGLCanvas, OpenGLContext;

constructor TLazGLControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if not (csDesigning in ComponentState) then begin
    FOldCanvas := Canvas;
    Canvas := TLazGLCanvas.Create;
    FControl := TOpenGLControl.Create(Self);
    FControl.Enabled := False; // Drops mouse clicks to parent.
    FControl.Align := alClient;
    FControl.Parent := Self;
    TOpenGLControl(FControl).OnPaint := DoOnPaint;
  end;
  Color := clBtnFace;
end;

destructor TLazGLControl.Destroy;
begin
  if not (csDesigning in ComponentState) then begin
    Canvas.Free;
    Canvas := FOldCanvas;
  end;
  inherited Destroy;
end;

procedure TLazGLControl.PaintWindow(DC: HDC);
begin
  if csDesigning in ComponentState then begin
    inherited PaintWindow(DC);
  end;
end;

procedure TLazGLControl.DoOnPaint(ASender: TObject);
begin
  lglClearOrtho2D(FControl.ClientRect, ColorToRGB(Color));
  inherited Paint;
  TOpenGLControl(FControl).SwapBuffers;
end;

end.
