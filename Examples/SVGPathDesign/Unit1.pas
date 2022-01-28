unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, ShellApi,
  Forms, Math, Types, Menus, ExtCtrls, ComCtrls, StdCtrls, Dialogs,
  Img32, Img32.Layers, Img32.Draw, Img32.SVG.PathDesign;

type

  //----------------------------------------------------------------------
  //----------------------------------------------------------------------

  TMainForm = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Exit1: TMenuItem;
    SaveDialog1: TSaveDialog;
    PopupMenu1: TPopupMenu;
    mnuDeleteSeg2: TMenuItem;
    OpenDialog1: TOpenDialog;
    Action1: TMenuItem;
    mnuReverseArc2: TMenuItem;
    mnuRotate: TMenuItem;
    mnuDeleteSeg: TMenuItem;
    N1: TMenuItem;
    mnuRotate2: TMenuItem;
    mnuReverseArc: TMenuItem;
    pnlBottom: TPanel;
    N4: TMenuItem;
    mnuA: TMenuItem;
    mnuC: TMenuItem;
    mnuQ: TMenuItem;
    mnuL: TMenuItem;
    mnuH: TMenuItem;
    mnuV: TMenuItem;
    mnuS: TMenuItem;
    mnuT: TMenuItem;
    N5: TMenuItem;
    N3: TMenuItem;
    N6: TMenuItem;
    N7: TMenuItem;
    AppendSegment1: TMenuItem;
    mnuVert2: TMenuItem;
    MnuHorz2: TMenuItem;
    mnuLine2: TMenuItem;
    N8: TMenuItem;
    mnuTSpline2: TMenuItem;
    mnuQuad2: TMenuItem;
    N9: TMenuItem;
    mnuSpline2: TMenuItem;
    mnuCubic2: TMenuItem;
    N10: TMenuItem;
    mnuArc2: TMenuItem;
    N11: TMenuItem;
    mnuScaleToFit: TMenuItem;
    memo1: TRichEdit;
    N2: TMenuItem;
    N13: TMenuItem;
    Open1: TMenuItem;
    N12: TMenuItem;
    OpenDialog2: TOpenDialog;
    N14: TMenuItem;
    mnuZClose: TMenuItem;
    N15: TMenuItem;
    mnuZClose2: TMenuItem;
    procedure mnuExitClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MainFormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MainFormMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ReverseArcClick(Sender: TObject);
    procedure mnuDeleteClick(Sender: TObject);
    procedure mnuRotateClick(Sender: TObject);
    procedure mnuTClick(Sender: TObject);
    procedure Memo1Change(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure mnuScaleToFitClick(Sender: TObject);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Open1Click(Sender: TObject);
    procedure memo1KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure memo1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    layeredImg32  : TLayeredImage32;
    svgPathLayer  : TSvgPathLayer;
    clickedLayer  : TLayer32;
    targetLayer   : TPathSegLayer;
    buttonGroup   : TGroupLayer32;
    rotateGroup   : TGroupLayer32;
    clickPoint    : TPoint;
    resizing      : Boolean;
    MemoKeyPressed    : Boolean;
    procedure SetTargetLayer(layer: TLayer32);
    procedure UpdateMemo;
    procedure ClearMemoBolding;
  protected
    procedure WMERASEBKGND(var message: TMessage); message WM_ERASEBKGND;
  public
  end;

var
  MainForm: TMainForm;
  decimalPrec: integer = 0;
  relativeCoords: Boolean = true;

implementation

{$R *.dfm}

{$R cursors.res}

uses
  Img32.Fmt.PNG, Img32.Fmt.JPG, Img32.Fmt.SVG,
  Img32.Vector, Img32.Extra, Img32.SVG.Core, Img32.SVG.Path;


//------------------------------------------------------------------------------
// TMainForm methods
//------------------------------------------------------------------------------

procedure TMainForm.FormCreate(Sender: TObject);
begin
  layeredImg32 := TLayeredImage32.Create;
  layeredImg32.SetSize(ClientWidth, ClientHeight - pnlBottom.Height);

  //add a hatched background design layer (drawn in FormResize).
  layeredImg32.AddLayer(TLayer32);

  //get ready to load an SVG-DPath
  svgPathLayer := layeredImg32.AddLayer(TSvgPathLayer) as TSvgPathLayer;
end;
//------------------------------------------------------------------------------

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  layeredImg32.Free;
end;
//------------------------------------------------------------------------------

procedure TMainForm.Open1Click(Sender: TObject);
var
  rec       : TRect;
const
  borderSize = 50;
begin
  if not OpenDialog1.Execute then Exit;

  SetTargetLayer(nil);

  rec := layeredImg32.Bounds;
  img32.Vector.InflateRect(rec, -borderSize, -borderSize*2);

  ////////////////////////////////////////////////
  svgPathLayer.LoadPathsFromFile(OpenDialog1.FileName, rec, 0);
  ////////////////////////////////////////////////
  UpdateMemo;
end;
//------------------------------------------------------------------------------

procedure TMainForm.Memo1Change(Sender: TObject);
var
  rec       : TRect;
const
  borderSize = 50;
begin
  if MemoKeyPressed then Exit;
  SetTargetLayer(nil);

  rec := layeredImg32.Bounds;
  img32.Vector.InflateRect(rec, -borderSize, -borderSize);

  ////////////////////////////////////////////////
  svgPathLayer.LoadPath(memo1.Lines.Text, rec, 0);
  ////////////////////////////////////////////////
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TMainForm.mnuExitClick(Sender: TObject);
begin
  if Assigned(targetLayer) then
  begin
    MemoKeyPressed := false;
    SetTargetLayer(nil); //deselect the active control
  end else
    Close;
end;
//------------------------------------------------------------------------------

procedure TMainForm.memo1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Shift * [ssAlt, ssCtrl] <> [] then
  begin
    MemoKeyPressed := false;
    Exit;
  end;
  case key of
    Ord('Z'), VK_SPACE, VK_RETURN: MemoKeyPressed := false;
    else MemoKeyPressed := true;
  end;
end;
//------------------------------------------------------------------------------

procedure TMainForm.memo1KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if MemoKeyPressed or (Shift * [ssAlt, ssCtrl] <> []) then Exit;
  case key of
    Ord('Z'), VK_SPACE, VK_RETURN: Memo1Change(nil);
  end;
end;
//------------------------------------------------------------------------------

procedure TMainForm.WMERASEBKGND(var message: TMessage);
begin
  //don't erase because we're mostly doing partial form paints
  message.Result := 1;
end;
//------------------------------------------------------------------------------

procedure TMainForm.FormPaint(Sender: TObject);
var
  updateRect: TRect;
begin
  //layeredImg32.GetMergedImage optionally returns the portion of
  //the image that's changed since the previous GetMergedImage call.
  //Painting only this changed region significantly speeds up drawing.
  with layeredImg32.GetMergedImage(false, updateRect) do
  begin
    if not IsEmptyRect(updateRect) then
      CopyToDc(updateRect, updateRect, Canvas.Handle)
    else if resizing then
      CopyToDc(Canvas.Handle); //ie do a full repaint
  end;
end;
//------------------------------------------------------------------------------

procedure TMainForm.SetTargetLayer(layer: TLayer32);
begin
  FreeAndNil(buttonGroup);
  FreeAndNil(rotateGroup);
  Invalidate;

  if Assigned(targetLayer) then
    targetLayer.Focused := false;
  clickedLayer := nil;
  targetLayer := nil;
  if not assigned(layer) or
    not (layer is TPathSegLayer) then
  begin
    ClearMemoBolding;
    Exit;
  end;

  clickedLayer := layer;
  targetLayer := layer as TPathSegLayer;
  with targetLayer do
  begin
    buttonGroup := CreateBtnGroup;
    Focused := true;
    UpdateMemo;
  end;
end;
//------------------------------------------------------------------------------

procedure TMainForm.MainFormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  clickPoint := Types.Point(X,Y);
  clickedLayer := layeredImg32.GetLayerAt(clickPoint);
  MemoKeyPressed := false;

  if not Assigned(clickedLayer) then          //no target
  begin
    FreeAndNil(buttonGroup);
    FreeAndNil(rotateGroup);
    if Assigned(targetLayer) then
    begin
      targetLayer.Focused := false;
      targetLayer := nil;
      ClearMemoBolding;
    end;
  end
  else if (clickedLayer is TPathSegLayer) and
    (clickedLayer <> targetLayer) then
      SetTargetLayer(clickedLayer);
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TMainForm.MainFormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  dx, dy: integer;
  pt: TPoint;
  layer: TLayer32;
begin
  pt := Types.Point(X,Y);

  if not (ssLeft in Shift) then
  begin
    //not moving anything so just update the cursor
    layer := layeredImg32.GetLayerAt(pt);
    if Assigned(layer) then
      Cursor := layer.CursorId else
      Cursor := crDefault;
    Exit;
  end;

  if not Assigned(clickedLayer) then Exit;
  dx := pt.X - clickPoint.X;
  dy := pt.Y - clickPoint.Y;

  //if moving a designer button fails TestUpdateBtnGroup() then Exit
  if (clickedLayer is TButtonDesignerLayer32) and assigned(targetLayer) then
    if not targetLayer.TestUpdateBtnGroup(clickedLayer, dx, dy) then Exit;

  if clickedLayer is TButtonDesignerLayer32 then
  begin
    //OK, we're moving a designer or rotation button
    if not assigned(targetLayer) then Exit;

    clickedLayer.Offset(dx, dy);
    clickPoint := OffsetPoint(clickPoint, dx, dy);

    if (clickedLayer.Parent is TRotatingGroupLayer32) then
      targetLayer.UpdateRotateBtnGroup(clickedLayer) else
      targetLayer.UpdateBtnGroup(clickedLayer);

    //Updating clickPoint when the control button is an Arc rect button can be
    //problematic because these buttons are offset from the rect. As a result
    //this can produce 'button bouncing' when the rect is being reflected.
    //So bypass updating clickPoint for vert and horz Arc rect adjustments
    if (targetLayer is TSvgASegLayer) and
      (TButtonDesignerLayer32(clickedLayer).BtnIdx in [2,4]) then
      clickPoint := Point(clickedLayer.MidPoint);
  end
  else if Assigned(clickedLayer) and (clickedLayer is TPathSegLayer) then
  begin
    clickPoint := OffsetPoint(clickPoint, dx, dy);
    //move the whole path
    clickedLayer.Parent.Offset(dx, dy);
    //and move the whole button group too
    if assigned(buttonGroup) then buttonGroup.Offset(dx, dy)
    else if assigned(rotateGroup) then rotateGroup.Offset(dx, dy);
  end;
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TMainForm.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  //Updating the richedit memo can be very slow,
  //so we'll avoid calling it in MouseMove
  UpdateMemo;
end;
//------------------------------------------------------------------------------

procedure TMainForm.mnuDeleteClick(Sender: TObject);
var
  layer: TLayer32;
  subPath: TSvgSubPath;
begin
  if not Assigned(targetLayer) then Exit;
  //it's only safe to delete the last segment
  layer := TSubPathLayer(targetLayer.Parent).GetLastSegLayer;
  SetTargetLayer(layer.PrevLayerInGroup);
  subPath := TSubPathLayer(layer.Parent).SubPath;
  subPath.DeleteLastSeg;
  if subPath.Count = 0 then
  begin
    subPath.Parent.DeleteSubPath(subPath);
    layer.Parent.Free; //also frees layer ;)
  end else
    layer.Free;
  UpdateMemo;
end;
//------------------------------------------------------------------------------

procedure TMainForm.mnuRotateClick(Sender: TObject);
begin
  if not Assigned(targetLayer) then Exit;

  if Assigned(rotateGroup) then
  begin
    FreeAndNil(rotateGroup);
    buttonGroup := targetLayer.CreateBtnGroup;
  end else
  begin
    FreeAndNil(buttonGroup);
    rotateGroup := targetLayer.CreateRotateBtnGroup(targetLayer);
  end;
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TMainForm.ReverseArcClick(Sender: TObject);
begin
  if not Assigned(targetLayer) or
    not (targetLayer is TSvgASegLayer) then Exit;
  FreeAndNil(rotateGroup);
  TSvgASegLayer(targetLayer).ReverseArcDirection;
  UpdateMemo;
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TMainForm.FormResize(Sender: TObject);
var
  w,h: integer;
begin
  if csDestroying in ComponentState then Exit;
  //resize layeredImg32
  w := ClientWidth;
  h := ClientHeight - pnlBottom.Height;
  layeredImg32.SetSize(w, h);
  //and resize and repaint the hatched design background layer
  with TLayer32(layeredImg32[0]) do
  begin
    SetSize(w, h);
    HatchBackground(Image);
  end;
  resizing := true;
  UpdateMemo;
end;
//------------------------------------------------------------------------------

procedure TMainForm.mnuScaleToFitClick(Sender: TObject);
begin
  Memo1Change(Sender);
end;
//------------------------------------------------------------------------------

function TextWidth(dc: HDC; const text: string): integer;
var
  size: TSize;
begin
  if not GetTextExtentPoint32(dc, pchar(text), length(text), size) then
    Result := 0 else
    Result := size.cx;
end;
//------------------------------------------------------------------------------

procedure TMainForm.UpdateMemo;
var
  savedOnChange: TNotifyEvent;
  i,j, len, charWidth, cw, maxWidth, startPos, endPos: integer;
  s, txt: string;
  svgPath: TSvgPath;
  targetSeg: TSvgPathSeg;
begin
  if (GetAsyncKeyState(VK_LBUTTON) < 0) then Exit;
  maxWidth := memo1.ClientWidth - GetSystemMetrics(SM_CXVSCROLL) - 2;

  if Assigned(targetLayer) then
    targetSeg := targetLayer.Seg else
    targetSeg := nil;

  savedOnChange := memo1.OnChange;
  memo1.OnChange := nil;
  memo1.Lines.BeginUpdate;
  try
    memo1.lines.Clear;
    startPos := 0; endPos := 0; txt := '';
    svgPath := svgPathLayer.svgPath;
    if Assigned(svgPath) then
      for i := 0 to svgPath.Count -1 do
         with svgPath.Path[i] do
         begin
            s := GetMoveStrDef(true, decimalPrec);
            charWidth := TextWidth(canvas.handle, s);
            txt := txt + s;
            for j := 0 to Count -1 do
            begin
              s := Seg[j].GetStringDef(relativeCoords, decimalPrec);
              len := Length(s);
              cw := TextWidth(canvas.handle, s);
              if Seg[j] = targetSeg then
              begin
                startPos := Length(txt);
                endPos := startPos + len;
              end;

              if charWidth + cw >= maxWidth then
              begin
                txt := txt + #10 + s;
                charWidth := cw;
                if Seg[j] = targetSeg then inc(startPos);
              end else
              begin
                txt := txt+ s;
                charWidth := charWidth + cw;
              end;
            end;
            txt := txt + #10;
          end;

    //copy the SVG string to the memo
    memo1.Text := txt;

    if endPos > 0 then
    begin
      //highlight the target segment's text
      memo1.SelStart := startPos;
      memo1.SelLength := endPos - startPos;
      //memo1.SelAttributes.Style := [TFontStyle.fsBold];
    end;

  finally
    memo1.Lines.EndUpdate;
    memo1.OnChange := savedOnChange;
  end;
end;
//------------------------------------------------------------------------------

procedure TMainForm.ClearMemoBolding;
var
  savedOnChange: TNotifyEvent;
begin
  savedOnChange := memo1.OnChange;
  memo1.OnChange := nil;
  memo1.SelectAll;
  memo1.SelAttributes.Style := [];
  memo1.SelStart := -1;
  memo1.OnChange := savedOnChange;
end;
//------------------------------------------------------------------------------

procedure TMainForm.mnuTClick(Sender: TObject);
var
  i,j   : integer;
  c     : Char;
  s, s2 : string;
  d     : double;
  spl   : TSubPathLayer;
  lastSegLayer: TPathSegLayer;
begin
  //add a new segment to the end of the targetLayer's path
  //unless there is no targetLayer selected.

  lastSegLayer := nil;
  if Assigned(targetLayer) then
  begin
    spl := targetLayer.Parent as TSubPathLayer;
    lastSegLayer := spl.GetLastSegLayer;
  end else
  begin
    spl := nil;
    if svgPathLayer.ChildCount > 0 then
      with svgPathLayer[svgPathLayer.ChildCount -1] as TSubPathLayer do
        lastSegLayer := GetLastSegLayer;
  end;

  //create a new segment that's very roughly proportional
  //to the size of the previous segment
  if Assigned(lastSegLayer) then
  begin
    if lastSegLayer.Seg.SegType = stVert then
      d := GetBoundsD(lastSegLayer.Seg.FlatPath).Height else
      d := GetBoundsD(lastSegLayer.Seg.FlatPath).Width;
    d := d/ (3*svgPathLayer.SvgPath.Scale);
  end
  else
    d := 100;

  c := TMenuItem(Sender).Name[4];
  case c of
    'A': s2 := format(' %1.0f,%1.0f 0 0 1 %1.0f,0 ', [d, d*2, d*2]);
    'C': s2 := format(' %1.0f,%1.0f %1.0f,%1.0f, %1.0f,0 ', [d*1.5, d, d*1.5, -d, d*3]);
    'Q': s2 := format(' %1.0f,%1.0f %1.0f,0 ', [d, d*2, d*3]);
    'L': s2 := format(' %1.0f,0', [d*1.5]);
    'H': s2 := format(' %1.0f ', [d*1.5]);
    'V': s2 := format(' %1.0f ', [d*1.5]);
    'S': s2 := format(' %1.0f,%1.0f %1.0f,0 ', [d*1.5, d, d*3]);
    'T': s2 := format(' %1.0f,0 ', [d*1.5]);
    'Z': s2 := '';
    else Exit;
  end;

  s := ''; j := -1;
  for i := 0 to svgPathLayer.ChildCount -1 do
  begin
    s := s + TSubPathLayer(svgPathLayer[i]).GetStringDef(relativeCoords, 2);
    if svgPathLayer[i] = spl then
    begin
      if assigned(lastSegLayer) and (lastSegLayer.Name = c) then
        s := s + s2 else
        s := s + LowerCase(c) + s2;
      j := i;
    end;
    s := s + #10;
  end;

  if not Assigned(spl) then
    s := s + 'M 0,0 ' + LowerCase(c) + s2;

  //set the SVG DPath text
  Memo1.Lines.Text := s;

  //find the new targetLayer
  if (svgPathLayer.ChildCount = 0) then Exit;
  if j < 0 then j := svgPathLayer.ChildCount -1;
  spl := svgPathLayer[j] as TSubPathLayer;
  if spl.ChildCount > 0 then
    SetTargetLayer(spl.GetLastSegLayer);
end;
//------------------------------------------------------------------------------

procedure TMainForm.PopupMenu1Popup(Sender: TObject);
var
  targetSelected: Boolean;
  targetIsArcSeg: Boolean;
begin
  targetSelected := Assigned(targetLayer);
  mnuDeleteSeg.Enabled    := targetSelected;
  mnuDeleteSeg2.Enabled   := targetSelected;

  targetIsArcSeg := targetSelected and (targetLayer is TSvgASegLayer);
  mnuReverseArc2.Enabled  := targetIsArcSeg;
  mnuReverseArc.Enabled   := targetIsArcSeg;
  mnuRotate.Enabled       := targetIsArcSeg;
  mnuRotate2.Enabled      := targetIsArcSeg;
end;
//------------------------------------------------------------------------------

end.
