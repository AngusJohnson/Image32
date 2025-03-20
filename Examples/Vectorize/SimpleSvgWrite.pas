unit SimpleSvgWrite;

interface

{$I Img32.inc}

uses
  Classes, SysUtils, Math, Img32, Img32.Vector;

type

  PTextInfo = ^TTextInfo;
{$IFDEF RECORD_METHODS}
  TTextInfo = record
{$ELSE}
  TTextInfo = object
{$ENDIF}
    x,y : integer;
    text: string;
    fontSize: integer;
    fontColor: TColor32;
    constructor Create(atext: string; _x, _y: integer;
      afontsize: integer = 12; afontcolor: TColor32 = clBlack32);
  end;

  PPolyInfo = ^TPolyInfo;
  TPolyInfo = record
    paths     : TPathsD;
    BrushClr  : Cardinal;
    PenClr    : Cardinal;
    PenWidth  : double;
    IsOpen    : Boolean;
  end;

  TSimpleSvgWriter = class
  private
    fFillRule   : TFillRule;
    fPolyInfos  : TList;
    fTextInfos  : TList;
    function GetBounds: TRectD;
  public
    constructor Create(fillRule: TFillRule);
    destructor Destroy; override;
    procedure AddPaths(const paths: TPathsD; isOpen: Boolean;
      brushColor, penColor: Cardinal; penWidth: double);
    procedure AddText(text: string; x,y: integer;
      fontSize: integer = 14; fontClr: TColor32 = clBlack32);
    function SaveToFile(const filename: string;
      maxWidth: integer = 0; maxHeight: integer = 0;
      margin: integer = 20): Boolean;
    procedure ClearPaths;
    procedure ClearText;
    procedure ClearAll;
  end;

implementation

const
  MaxRect: TRectD  = (left: MaxDouble;
    Top: MaxDouble; Right: -MaxDouble; Bottom: -MaxDouble);

  leftSpaces = '  ';
  leftSpacesMul2 = leftSpaces + leftSpaces;
  svg_header: string =
    '<?xml version="1.0" standalone="no"?>'#10 +
    '<!DOCTYPE svg PUBLIC "-/W3C/DTD SVG 1.0/EN"'#10 +
    '"http://www.w3.org/TR/2001/REC-SVG-20010904/DTD/svg10.dtd">'#10#10 +
    '<svg width="%dpx" height="%dpx" viewBox="0 0 %0:d %1:d" ' +
    'version="1.1" xmlns="http://www.w3.org/2000/svg">';
  svg_path_format: string = '%s"'#10 +
    leftSpacesMul2 + 'style="fill:%s;' +
    ' fill-opacity:%1.2f; fill-rule:%s;'#10 + leftSpacesMul2 +
    'stroke:%s; stroke-opacity:%1.2f; stroke-width:%1.2f;"/>';
  svg_path_format2: string = '%s"'#10 +
    leftSpacesMul2 + 'style="fill:none; stroke:%s; ' +
    'stroke-opacity:%1.2f; stroke-width:%1.2f;"/>';

function ColorToHtml(color: Cardinal): string;
begin
  Result := Format('#%6.6x', [color and $FFFFFF]);
end;

function GetAlpha(clr: Cardinal): double;
begin
  Result := (clr shr 24) / 255;
end;

constructor TTextInfo.Create(atext: string; _x, _y: integer;
  afontsize: integer; afontcolor: TColor32);
begin
  self.x := _x;
  self.y := _y;
  self.text := text;
  self.fontSize := afontsize;
  self.fontColor := afontcolor;
end;

constructor TSimpleSvgWriter.Create(fillRule: TFillRule);
begin
  fPolyInfos := TList.Create;
  fTextInfos := TList.Create;
  fFillRule := fillRule;
end;

destructor TSimpleSvgWriter.Destroy;
begin
  ClearAll;
  fPolyInfos.Free;
  fTextInfos.Free;
  inherited;
end;

procedure TSimpleSvgWriter.AddPaths(const paths: TPathsD; isOpen: Boolean;
  brushColor, penColor: Cardinal; penWidth: double);
var
  pi: PPolyInfo;
begin
  new(pi);
  pi.paths := Copy(paths, 0, Length(paths));
  pi.BrushClr := brushColor;
  pi.PenClr   := penColor;
  pi.PenWidth := penWidth;
  pi.IsOpen := isOpen;
  fPolyInfos.Add(pi);
end;

procedure TSimpleSvgWriter.AddText(text: string; x,y: integer;
  fontSize: integer; fontClr: TColor32);
var
  ti: PTextInfo;
begin
  new(ti);
  ti.x := x;
  ti.y := y;
  ti.text := text;
  ti.fontSize := fontSize;
  ti.fontColor := fontClr;
  fTextInfos.Add(ti);
end;

function TSimpleSvgWriter.GetBounds: TRectD;
var
  i: integer;
  bounds: TRectD;
begin
  Result := MaxRect;
  for i := 0 to fPolyInfos.Count -1 do
    with PPolyInfo(fPolyInfos[i])^ do
    begin
      bounds := GetBoundsD(paths);
      if (bounds.left < Result.Left) then Result.Left := bounds.Left;
      if (bounds.right> Result.Right) then Result.Right := bounds.Right;
      if (bounds.top < Result.Top) then Result.Top := bounds.Top;
      if (bounds.bottom > Result.Bottom) then Result.Bottom := bounds.Bottom;
    end;
end;

procedure TSimpleSvgWriter.ClearPaths;
var
  i: integer;
begin
  for i := 0 to fPolyInfos.Count -1 do
    Dispose(PPolyInfo(fPolyInfos[i]));
  fPolyInfos.Clear;
end;

procedure TSimpleSvgWriter.ClearText;
var
  i: integer;
begin
  for i := 0 to fTextInfos.Count -1 do
    Dispose(PTextInfo(fTextInfos[i]));
  fTextInfos.Clear;
end;

procedure TSimpleSvgWriter.ClearAll;
begin
  ClearText;
  ClearPaths;
end;

function TSimpleSvgWriter.SaveToFile(const filename: string;
  maxWidth: integer = 0; maxHeight: integer = 0; margin: integer = 20): Boolean;
var
  i,j,k: integer;
  bounds: TRectD;
  scale: double;
  offsetX, offsetY: integer;
  s: string;
const
  fillRuleStr: array[boolean] of string = ('evenodd', 'nonzero');
begin
  Result := false;
  if (margin < 20) then margin := 20;
  bounds := GetBounds;
  if bounds.IsEmpty then Exit;

  scale := 1.0;
  if (maxWidth > 0) and (maxHeight > 0) then
    scale := 1.0 / Max((bounds.right - bounds.left) /
      (maxWidth - margin * 2), (bounds.bottom - bounds.top) /
      (maxHeight - margin * 2));

  offsetX := margin - Round(bounds.left * scale);
  offsetY := margin - Round(bounds.top * scale);

  with TStringList.Create do
  try
    if (maxWidth <= 0) or (maxHeight <= 0) then
       Add(Format(svg_header,
       [Round(bounds.right - bounds.left) + margin * 2,
        Round(bounds.bottom - bounds.top) + margin * 2]))
    else
      Add(Format(svg_header, [maxWidth, maxHeight]));

    for i := 0 to fPolyInfos.Count -1 do
      with PPolyInfo(fPolyInfos[i])^ do
      begin
        s := leftSpaces +'<path d="';
        for j := 0 to High(paths) do
        begin
          if Length(paths[j]) < 2 then Continue;
          if not IsOpen and (Length(paths[j]) < 3) then Continue;
          s := s + Format('M %1.2f %1.2f L ',
            [paths[j][0].x * scale + offsetX,
            paths[j][0].y * scale + offsetY]);
          for k := 1 to High(paths[j]) do
          begin
            if k mod 6 = 0 then
            begin
              Add(s);
              s := leftSpacesMul2;
            end;
            s := s + Format('%1.2f %1.2f ',
              [paths[j][k].x * scale + offsetX,
              paths[j][k].y * scale + offsetY]);
          end;
          if not IsOpen then s := s + 'Z';

          if (j = High(paths)) then Break;
          if (s <> leftSpacesMul2) then Add(s);
          s := leftSpacesMul2;
        end;

        if not IsOpen then
          Add(Format(svg_path_format,
            [s, ColorToHtml(BrushClr), GetAlpha(BrushClr),
            fillRuleStr[fFillRule = frNonZero],
            ColorToHtml(PenClr), GetAlpha(PenClr), PenWidth]))
        else
          Add(Format(svg_path_format2,
            [s, ColorToHtml(PenClr), GetAlpha(PenClr), PenWidth]));
        s := leftSpacesMul2;
      end;

    for i := 0 to fTextInfos.Count -1 do
      with PTextInfo(fTextInfos[i])^ do
      begin
        Add(Format(
          '<g font-family="Verdana" font-style="normal" ' +
          'font-weight="normal" font-size="%d" fill="%s">' +
          '<text x="%1.2f" y="%1.2f">%s</text></g>'#10,
          [fontSize, ColorToHtml(fontColor),
          x * scale + margin, y * scale + margin, text]));
      end;
    Add('</svg>');
    SaveToFile(filename);
    Result := true;
  finally
    free;
  end;
end;


end.
