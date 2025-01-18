unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  Forms, Math, Types, Menus, ExtCtrls, ComCtrls, Commdlg,
  Img32, Img32.Panels, Img32.Text, Dialogs;

type
  TForm1 = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Exit1: TMenuItem;
    StatusBar1: TStatusBar;
    FontDialog1: TFontDialog;
    Font1: TMenuItem;
    N1: TMenuItem;
    mnuAlignLeft: TMenuItem;
    N2: TMenuItem;
    mnuAlignCenter: TMenuItem;
    mnuAlignRight: TMenuItem;
    mnuAlignJustified: TMenuItem;
    mnuAlignTop: TMenuItem;
    mnuAlignMiddle: TMenuItem;
    mnuAlignBottom: TMenuItem;
    N3: TMenuItem;
    procedure Exit1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure Font1Click(Sender: TObject);
    procedure mnuAlignJustifiedClick(Sender: TObject);
    procedure ImgPanelMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  protected
    procedure DrawChunkEvent(chunk: TTextChunk; const chunkRec: TRectD);
  public
    chunkedText: TChunkedText;
    pageMetrics: TPageTextMetrics;
    imgPanel: TImage32Panel;

    regularCache: TFontCache;
    italicCache: TFontCache;
    boldCache: TFontCache;
    boldItalCache: TFontCache;
    monoSpaceCache: TFontCache;

    procedure Draw;
    procedure ResetPanelImage(color: TColor32);
    function LoadFontFamily(const logFont: TLogFont): TLoadFontResult;
  end;

var
  essay: UnicodeString;
  Form1: TForm1;
  currentFont: TLogFont;

implementation

{$R *.dfm}
{$R text2.res}
{$I Img32.inc}

uses
  Img32.Draw, Img32.Vector, Img32.Extra;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

// MyVerySimpleChunkifyTextProc parses a string of text and breaks it up
// into chunks that will sensibly word-wrap. It also applies different fonts
// and styles as instructed by a very simple (custom) text markup.
procedure MyVerySimpleChunkifyTextProc(const text: UnicodeString;
  chunkedText: TChunkedText;
  fontNormal, fontItalic, fontBold, fontBoldItalic, fontMonoSpace: TFontCache);
var
  len: integer;
  including: Boolean;
  p, p2, pEnd: PWideChar;
  s: UnicodeString;
  styles: TFontStyles;
  mono: Boolean;
  font: TFontCache;
begin
  if not Assigned(fontNormal) then Exit;

  if not Assigned(fontBoldItalic) then
  begin
    if Assigned(fontBold) then fontBoldItalic := fontBold
    else if Assigned(fontItalic) then fontBoldItalic := fontItalic
    else fontBoldItalic := fontNormal;
  end;
  if not Assigned(fontItalic) then fontItalic := fontNormal;
  if not Assigned(fontBold) then fontBold := fontNormal;

  font := fontNormal;
  styles := [];
  mono := false;
  p := PWideChar(text);
  pEnd := p;
  Inc(pEnd, Length(text));
  while p < pEnd do
  begin

    // nb: the very simple text markup doesn't currently handle escape chars
    if (p^ = '<') then
    begin
      inc(p);
      including := p^ <> '/';
      if including then
      begin
        if p^ = 'm' then mono := true
        else if p^ = 'i' then Include(styles, fsItalic)
        else if p^ = 'b' then Include(styles, fsBold)
        else Break; // assume an error
      end else
      begin
        inc(p);
        if p^ = 'm' then mono := false
        else if p^ = 'i' then Exclude(styles, fsItalic)
        else if p^ = 'b' then Exclude(styles, fsBold)
        else Break; // assume an error
      end;
      inc(p);
      if (p^ <> '>') then break;
      inc(p);

      if mono then font := fontMonoSpace
      else if styles = [fsBold, fsItalic] then font := fontBoldItalic
      else if styles = [fsBold] then font := fontBold
      else if styles = [fsItalic] then font := fontItalic
      else font := fontNormal;
      Continue;
    end;

    if (p^ <= #32) then
    begin
      if (p^ = #32) then chunkedText.AddSpace(font)
      else if (p^ = #10) then chunkedText.AddNewline(font);
      inc(p);
    end else
    begin
      p2 := p;
      inc(p);
      while (p < pEnd) and (p^ > #32) and (p^ <> '<') do inc(p);
      len := p - p2;
      SetLength(s, len);
      Move(p2^, s[1], len * SizeOf(WideChar));
      if fsBold in Styles then
      begin
        // for basic underline support ...
        font.Underlined := true;
        chunkedText.AddTextChunk(font, s, clMaroon32);
        font.Underlined := false;
      end else if fsItalic in Styles then
        chunkedText.AddTextChunk(font, s, clNavy32)
      else
        chunkedText.AddTextChunk(font, s, clBlack32);
    end;
  end;
end;
//------------------------------------------------------------------------------

function DoFontDialog(var logFont: TLogFont): Boolean;
var
  cf: TChooseFont;
begin
  FillChar(cf, sizeof(cf),0);
  cf.lStructSize := sizeof(cf);
  cf.hWndOwner := 0;
  FontSizeToFontHeight(logFont.lfHeight);
  cf.lpLogFont := @logFont;
  cf.nSizeMin := 8;
  cf.nSizeMax := 16;
  cf.Flags := CF_FORCEFONTEXIST or CF_EFFECTS or
    CF_TTONLY or CF_INITTOLOGFONTSTRUCT;
  cf.nFontType := REGULAR_FONTTYPE;
  result := ChooseFont(cf);
  FontHeightToFontSize(logFont.lfHeight);
end;
//------------------------------------------------------------------------------

function Utf8TextResourceToUnicodeString(resId: integer;
  const resType: string): UnicodeString;
var
  rs: TResourceStream;
  sl: TStringList;
{$ifndef unicode}
  srcLen, dstLen, bomLen: integer;
  utf8Str: string;
{$endif}
begin
  rs := TResourceStream.CreateFromID(hInstance, resId, PChar(resType));
  sl := TStringList.Create;
  try
{$ifdef unicode}
    sl.LoadFromStream(rs, TEncoding.UTF8);
    Result := sl.Text;
{$else}
    Result := '';
    srcLen := rs.Size;
    if srcLen < 3 then Exit;
    SetLength(utf8Str, 3);
    rs.Read(utf8Str[1], 3);
    if (utf8Str[1] = #$EF) and
      (utf8Str[2] = #$BB) and (utf8Str[3] = #$BF) then
        bomLen := 3 else
        bomLen := 0;
    srcLen := srcLen - bomLen;
    SetLength(utf8Str, srcLen);
    rs.Position := bomLen;
    rs.Read(utf8Str[1], srcLen);
    dstLen := MultiByteToWideChar(CP_UTF8, 0, PChar(@utf8Str[1]), -1, nil , 0);
    SetLength(Result, dstLen);
    MultiByteToWideChar(CP_UTF8, 0,
      PChar(@utf8Str[1]), srcLen, PWideChar(Result), dstLen);
{$endif}
  finally
    sl.Free;
    rs.Free;
  end;
end;
//------------------------------------------------------------------------------

const
  DefaultLogfont: TLogFont = (
    lfHeight: 12;
    lfWeight: FW_NORMAL;
    lfCharSet: DEFAULT_CHARSET;
    lfOutPrecision: OUT_OUTLINE_PRECIS;
    lfClipPrecision: CLIP_DEFAULT_PRECIS;
    lfQuality: PROOF_QUALITY;
    lfPitchAndFamily: DEFAULT_PITCH or FF_DONTCARE;
    lfFaceName: 'Arial'#0);

procedure TForm1.FormCreate(Sender: TObject);
var
  monoFR: TFontReader;
begin
  currentFont := DefaultLogfont;
  LoadFontFamily(currentFont);
  FontManager.LoadFontReader('Segoe UI Emoji');
  monoFR := FontManager.LoadFontReader('Courier New Bold');
  monoSpaceCache := TFontCache.Create(monoFR, GetFontPixelHeight(currentFont.lfHeight));

  essay := Utf8TextResourceToUnicodeString(1, 'TEXT');

  chunkedText := TChunkedText.Create;
  MyVerySimpleChunkifyTextProc(essay, chunkedText,
    regularCache, italicCache, boldCache, boldItalCache, monoSpaceCache);

  //for i := 4 to 14 do chunkedText[i].backColor := clYellow32;
  chunkedText.OnDrawChunk := DrawChunkEvent;

  StatusBar1.Panels[0].Text := Format(' %s, %d', [currentFont.lfFaceName, currentFont.lfHeight]);
  StatusBar1.Panels[1].Text := 'Clicking on a word will wavy underline it :)';

  imgPanel := TImage32Panel.Create(self);
  imgPanel.Parent := self;
  imgPanel.align := alClient;
  imgPanel.AllowFileDrop := false;
  imgPanel.AllowZoom := false;
  imgPanel.OnMouseDown := ImgPanelMouseDown;
end;
//------------------------------------------------------------------------------

procedure TForm1.FormDestroy(Sender: TObject);
begin
  if (italicCache <> regularCache) then
    italicCache.free;
  if (boldCache <> regularCache) then
    boldCache.free;
  if (boldItalCache <> regularCache) then
    boldItalCache.free;

  FreeAndNil(regularCache);
  FreeAndNil(monoSpaceCache);
  chunkedText.Free;
end;
//------------------------------------------------------------------------------

procedure TForm1.FormResize(Sender: TObject);
begin
  if not (csDestroying in ComponentState) then Draw;
end;
//------------------------------------------------------------------------------

procedure TForm1.Font1Click(Sender: TObject);
var
  tmpLogFont: TLogFont;
begin
  tmpLogFont := currentFont;
  if not DoFontDialog(tmpLogFont) then Exit;
  if LoadFontFamily(tmpLogFont) = lfrInvalid then Exit;
  currentFont := tmpLogFont;

  StatusBar1.Panels[0].Text :=
    Format(' %s, %d', [currentFont.lfFaceName, currentFont.lfHeight]);
  chunkedText.Clear;
  MyVerySimpleChunkifyTextProc(essay, chunkedText,
    regularCache, italicCache, boldCache, boldItalCache, monoSpaceCache);
  Draw;
end;
//------------------------------------------------------------------------------

procedure TForm1.Exit1Click(Sender: TObject);
begin
  Close;
end;
//------------------------------------------------------------------------------

function TForm1.LoadFontFamily(const logFont: TLogFont): TLoadFontResult;
var
  frf: TFontReaderFamily;
  fontHeight: double;
  fontFaceName: string;
begin
  // LoadFontReaderFamily gets the available TFontReader objects for
  // the various styles associated with a specific font face name.
  // The function will return false if not fonts match the given face name.
  // When the function returns true, at least frf.regularFR will be assigned.
  // TFontReader objects for the styled fonts may, or may not be assigned.
  fontFaceName := logFont.lfFaceName;
  Result := FontManager.LoadFontReaderFamily(fontFaceName, frf);
  if Result = lfrInvalid then Exit;

  // convert the device independant font point size into the pixel height
  fontHeight := GetFontPixelHeight(logFont.lfHeight);

  if Assigned(regularCache) then
  begin
    regularCache.FontReader := frf.regularFR;
    regularCache.FontHeight := fontHeight;
  end
  else
    regularCache := TFontCache.Create(frf.regularFR, fontHeight);

  if not Assigned(frf.boldFR) then FreeAndNil(boldCache)
  else if Assigned(boldCache) then
  begin
    boldCache.FontReader := frf.boldFR;
    boldCache.FontHeight := fontHeight;
  end
  else
    boldCache := TFontCache.Create(frf.boldFR, fontHeight);

  if not Assigned(frf.italicFR) then FreeAndNil(italicCache)
  else if Assigned(italicCache) then
  begin
    italicCache.FontReader := frf.italicFR;
    italicCache.FontHeight := fontHeight;
  end
  else
    italicCache := TFontCache.Create(frf.italicFR, fontHeight);

  if not Assigned(frf.boldItalicFR) then FreeAndNil(boldItalCache)
  else if Assigned(boldItalCache) then
  begin
    boldItalCache.FontReader := frf.boldItalicFR;
    boldItalCache.FontHeight := fontHeight;
  end
  else
    boldItalCache := TFontCache.Create(frf.boldItalicFR, fontHeight);
end;
//------------------------------------------------------------------------------

procedure TForm1.mnuAlignJustifiedClick(Sender: TObject);
begin
  Draw;
end;

//------------------------------------------------------------------------------

procedure TForm1.Draw;
var
  rec: TRect;
  align: TTextAlign;
  valign: TTextVAlign;
const
  margin = 40;
begin
  if mnuAlignLeft.Checked then align := taLeft
  else if mnuAlignCenter.Checked then align := taCenter
  else if mnuAlignRight.Checked then align := taRight
  else align := taJustify;

  if mnuAlignTop.Checked then valign := tvaTop
  else if mnuAlignMiddle.Checked then valign := tvaMiddle
  else valign := tvaBottom;

  ResetPanelImage($FFFDFDFD);
  rec := Rect(margin, margin,
    imgPanel.Image.Width -margin, imgPanel.Image.Height - margin);

  // draw chunked essay (and update pageMetrics) ...
  imgPanel.Image.Clear;
  pageMetrics := chunkedText.DrawText(imgPanel.Image, rec, align, valign, 0);
  imgPanel.Invalidate;
end;
//------------------------------------------------------------------------------

procedure TForm1.ResetPanelImage(color: TColor32);
var
  w,h: integer;
  rec: TRect;
begin
  rec := imgPanel.InnerClientRect;
  RectWidthHeight(rec, w,h);
  imgPanel.Image.SetSize(w, h);
end;
//------------------------------------------------------------------------------

procedure TForm1.ImgPanelMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  chunkIdx, chunkChrOffset: integer;
  chunk: TTextChunk;
  ulp: TPathD;
begin
  // first, remove any prior wavy underlines :)
  Draw;

  // now get the chunk under the click point ...
  if not chunkedText.GetChunkAndGlyphOffsetAtPt(pageMetrics,
    imgPanel.ClientToImage(img32.Vector.Point(X,Y)), chunkIdx, chunkChrOffset) then
      Exit;

  // and draw a green wavy line under that chunk ...
  chunk := chunkedText[chunkIdx];
  ulp := regularCache.GetUnderlineOutline(chunk.left,
    chunk.left + chunk.width, chunk.top + chunk.ascent, InvalidD, true);
  DrawPolygon(imgPanel.Image, ulp, frNonZero, clGreen32);
end;
//------------------------------------------------------------------------------

procedure TForm1.DrawChunkEvent(chunk: TTextChunk; const chunkRec: TRectD);
begin
  // usually filling behind chunks is done by setting the chunk's fillColor
  // property, but this event demonstrates that it can also be used for this
  // purpose (and numerous other purposes) ...
  if (chunk.index > 3) and (chunk.index < 17) then
    imgPanel.Image.FillRect(Rect(chunkRec), $33FFFF00);
end;
//------------------------------------------------------------------------------

end.
