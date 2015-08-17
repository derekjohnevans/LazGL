unit UnitMain;

{$mode objfpc}{$H+}

interface

uses
  ComCtrls, Controls, ExtCtrls, Forms,
  LazGLControl, LazGLTests;

type

  { TFormMain }

  TFormMain = class(TForm)
    PanelClient: TPanel;
    StatusBar: TStatusBar;
    procedure FormCreate(Sender: TObject);
    procedure PanelClientResize(Sender: TObject);
  private
    { private declarations }
    FPanel1: TCustomControl;
    FPanel2: TLazGLControl;
    procedure DoOnPaint2(ASender: TObject);
    procedure DoOnPaint1(ASender: TObject);
  public
    { public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

procedure TFormMain.FormCreate(Sender: TObject);
begin
  Caption := Application.Title;
  Width := 900;
  Height := 500;

  FPanel1 := TCustomControl.Create(Self);
  FPanel1.Parent := PanelClient;
  FPanel1.OnPaint := @DoOnPaint1;

  FPanel2 := TLazGLControl.Create(Self);
  FPanel2.Parent := PanelClient;
  FPanel2.OnPaint := @DoOnPaint2;
end;

procedure TFormMain.PanelClientResize(Sender: TObject);
begin
  FPanel1.SetBounds(0, 0, PanelClient.ClientWidth div 2, PanelClient.ClientHeight);
  FPanel2.SetBounds(PanelClient.ClientWidth div 2, 0, PanelClient.ClientWidth div
    2, PanelClient.ClientHeight);
end;

procedure TFormMain.DoOnPaint1(ASender: TObject);
begin
  CustomControlTest(FPanel1);
end;

procedure TFormMain.DoOnPaint2(ASender: TObject);
begin
  CustomControlTest(FPanel2);
end;

end.
