unit LazGLTexture;

{$MODE objfpc}{$H+}

interface

uses
  Classes, fgl, Graphics, SysUtils;

type

  TLazTextureGL = class(IFPObserver)
  strict private
    FBitmap: TBitmap;
    FTexture: Integer;
  public
    procedure FPOObservedChanged(ASender: TObject; AOperation: TFPObservedOperation;
      AData: Pointer);
  public
    constructor Create(const ABitmap: TBitmap);
    destructor Destroy; override;
  public
    procedure Release;
    procedure Generate(const ABitmap: TBitmap);
    procedure FromBitmap(const ABitmap: TBitmap);
  public
    property TextureId: Integer read FTexture;
  end;

  TTextures = specialize TFPGMap<Pointer, TLazTextureGL>;

var
  GTextures: TTextures;

implementation

uses  GL, GLU, LazGL, LCLType;

constructor TLazTextureGL.Create(const ABitmap: TBitmap);
begin
  inherited Create;
  Assert(Assigned(ABitmap));
  ABitmap.FPOAttachObserver(Self);
  FBitmap := ABitmap;
  GTextures.Add(ABitmap, Self);
  FromBitmap(ABitmap);
end;

destructor TLazTextureGL.Destroy;
begin
  Release;
  GTextures.Remove(FBitmap);
  inherited Destroy;
end;

procedure TLazTextureGL.FPOObservedChanged(ASender: TObject; AOperation: TFPObservedOperation;
  AData: Pointer);
begin
  if AOperation = ooFree then
  begin
    Free;
  end;
end;

procedure TLazTextureGL.Release;
begin
  if FTexture <> 0 then
  begin
    glDeleteTextures(1, @FTexture);
    FTexture := 0;
  end;
end;

procedure TLazTextureGL.Generate(const ABitmap: TBitmap);
begin
  Release;
  glGenTextures(1, @FTexture);
  glBindTexture(GL_TEXTURE_2D, FTexture);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
  gluBuild2DMipmaps(GL_TEXTURE_2D, GL_RGBA, ABitmap.Width, ABitmap.Height,
    lglGetPixelFormat(ABitmap.PixelFormat), GL_UNSIGNED_BYTE, ABitmap.RawImage.Data);
end;

procedure TLazTextureGL.FromBitmap(const ABitmap: TBitmap);
var
  LBitmap: TBitmap;
begin
  if ABitmap.Transparent and (ABitmap.PixelFormat <> pf32bit) then
  begin
    LBitmap := lglBitmapCreate32(ABitmap);
    try
      Generate(LBitmap);
    finally
      FreeAndNil(LBitmap);
    end;
  end else begin
    Generate(ABitmap);
  end;
end;


initialization

  GTextures := TTextures.Create;

end.

