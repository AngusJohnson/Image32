unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, ShellApi,
  Forms, Math, Types, Menus, ExtCtrls, ComCtrls, StdCtrls, Dialogs,
  Img32, Img32.Layers, Img32.Draw, Img32.SVG.Core, Img32.Svg.PathDesign;

type

  //----------------------------------------------------------------------
  //----------------------------------------------------------------------

  TMainForm = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Exit1: TMenuItem;
    SaveDialog1: TSaveDialog;
    PopupMenu1: TPopupMenu;
    mnuDeleteLayer2: TMenuItem;
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
    mnuScaleToFit2: TMenuItem;
    N12: TMenuItem;
    memo1: TRichEdit;
    procedure mnuExitClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MainFormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MainFormMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ReverseArcClick(Sender: TObject);
    procedure mnuDeleteLayerClick(Sender: TObject);
    procedure mnuRotateClick(Sender: TObject);
    procedure mnuTClick(Sender: TObject);
    procedure Memo1Change(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure mnuScaleToFitClick(Sender: TObject);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    layeredImg32  : TLayeredImage32;
    svgPathLayer  : TSvgPathLayer;
    clickedLayer  : TLayer32;
    targetLayer   : TSegBaseLayer;
    buttonGroup   : TGroupLayer32;
    rotateGroup   : TGroupLayer32;
    clickPoint    : TPoint;
    resizing      : Boolean;
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

implementation

{$R *.dfm}

{$R cursors.res}

uses
  Img32.Fmt.PNG, Img32.Fmt.JPG, Img32.Fmt.SVG,
  Img32.Vector, Img32.Extra;


//------------------------------------------------------------------------------
// TMainForm methods
//------------------------------------------------------------------------------

procedure TMainForm.FormCreate(Sender: TObject);
begin
  layeredImg32 := TLayeredImage32.Create;

  //add a hatched background design layer (drawn in FormResize).
  layeredImg32.AddLayer(TDesignerLayer32);

  //get ready to load an SVG-DPath
  svgPathLayer := layeredImg32.AddLayer(TSvgPathLayer) as TSvgPathLayer;

//  memo1.Lines.Text := 'M 0,0 l 200,0 ';

end;
//------------------------------------------------------------------------------

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  layeredImg32.Free;
end;
//------------------------------------------------------------------------------

procedure TMainForm.Memo1Change(Sender: TObject);
var
  rec       : TRect;
const
  borderSize = 50;
begin
  SetTargetLayer(nil);

  rec := layeredImg32.Bounds;
  img32.Vector.InflateRect(rec, -borderSize, -borderSize);

  ////////////////////////////////////////////////
  svgPathLayer.LoadPath(memo1.Lines.Text, rec, 0);
  ////////////////////////////////////////////////
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TMainForm.FormKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_ESCAPE) and Assigned(targetLayer) then
  begin
    SetTargetLayer(nil); //deselect the active control
    Key := 0;
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
    not (layer is TSegBaseLayer) then
  begin
    ClearMemoBolding;
    Exit;
  end;

  clickedLayer := layer;
  targetLayer := layer as TSegBaseLayer;
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
  else if (clickedLayer is TSegBaseLayer) and
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
    clickPoint.Offset(dx, dy);

    if (clickedLayer.Parent is TRotatingGroupLayer32) then
      targetLayer.UpdateRotateBtnGroup(clickedLayer) else
      targetLayer.UpdateBtnGroup(clickedLayer);
  end
  else if Assigned(clickedLayer) and (clickedLayer is TSegBaseLayer) then
  begin
    clickPoint.Offset(dx, dy);
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

procedure TMainForm.mnuExitClick(Sender: TObject);
begin
  Close;
end;
//------------------------------------------------------------------------------

procedure TMainForm.mnuDeleteLayerClick(Sender: TObject);
var
  layer: TLayer32;
  subPath: TSvgSubPath;
begin
  if not Assigned(targetLayer) then Exit;
  //it's only safe to delete the last segment
  layer := TSvgSubPathLayer(targetLayer.Parent).GetLastSegLayer;
  SetTargetLayer(layer.PrevLayerInGroup);
  subPath := TSvgSubPathLayer(layer.Parent).SubPath;
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
  with TDesignerLayer32(layeredImg32[0]) do
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

procedure TMainForm.UpdateMemo;
var
  savedOnChange: TNotifyEvent;
  leftMouseBtnDown: Boolean;
  i,j, startPos, endPos: integer;
  txt: string;
  svgPath: TSvgPath;
begin
  leftMouseBtnDown := GetAsyncKeyState(VK_LBUTTON) < 0;
  savedOnChange := memo1.OnChange;
  memo1.OnChange := nil;
  memo1.Lines.BeginUpdate;
  try
    memo1.Text := '';
    if Assigned(targetLayer) and not leftMouseBtnDown then
    begin
      startPos := 0; endPos := 0; txt := '';
      svgPath := targetLayer.Seg.Owner;
      for i := 0 to svgPath.Count -1 do
        with svgPath.Path[i] do
        begin
          txt := txt + GetMoveStrDef(true, decimalPrec);
          for j := 0 to Count -1 do
            if Seg[j] = targetLayer.Seg then
            begin
              startPos := Length(txt);
              txt := txt + Seg[j].GetStringDef(true, decimalPrec);
              endPos := Length(txt);
            end
            else
              txt := txt + Seg[j].GetStringDef(true, decimalPrec);
        end;

      //copy the SVG string to the memo
      memo1.Text := txt;

      //highlight the target segment's text
      memo1.SelStart := startPos;
      memo1.SelLength := endPos - startPos;
      memo1.SelAttributes.Style := [TFontStyle.fsBold];
      memo1.SelStart := -1;
    end else
    begin
      memo1.Text := svgPathLayer.svgPath.GetStringDef(true, decimalPrec);
    end;
  finally
    memo1.Lines.EndUpdate;
  end;
  memo1.OnChange := savedOnChange;
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
  C     : Char;
  s, lc : string;
  layer : TSegBaseLayer;
  d     : double;
  spl   : TSvgSubPathLayer;
begin
  //add a new segment to the end of the targetLayer's path
  //unless there is no targetLayer selected.

  C := TMenuItem(Sender).Name[4];

  //draw a new segment that's very roughly proportional
  //to the size of the previous segment

  layer := nil;
  if Assigned(targetLayer) then
    layer := targetLayer
  else if (svgPathLayer.ChildCount > 0) then
    with TGroupLayer32(svgPathLayer[svgPathLayer.ChildCount-1]) do
      Layer := LastChild as TSegBaseLayer;

  if Assigned(layer) then
  begin
    if layer.Seg.SegType = dsVert then
      d := GetBoundsD(layer.Seg.FlatPath).Height else
      d := GetBoundsD(layer.Seg.FlatPath).Width;
    d := d/ (3*svgPathLayer.SvgPath.Scale);
  end else
    d := 100;

  if not Assigned(targetLayer) then
    lc := 'M 0,0 ' + LowerCase(C)
  else if (targetLayer.Name <> C) then
    lc := LowerCase(C)
  else
    lc := '';

  case C of
    'A': s := lc +
      format(' %1.0f,%1.0f 0 0 1 %1.0f,0 ', [d, d*2, d*2]);
    'C': s := lc +
      format(' %1.0f,%1.0f %1.0f,%1.0f, %1.0f,0 ', [d*1.5, d, d*1.5, -d, d*3]);
    'Q': s := lc +
      format(' %1.0f,%1.0f %1.0f,0 ', [d, d*2, d*3]);
    'L': s := lc + format(' %1.0f,0', [d*1.5]);
    'H': s := lc + format(' %1.0f ', [d*1.5]);
    'V': s := lc + format(' %1.0f ', [d*1.5]);
    'S': s := lc + format(' %1.0f,%1.0f %1.0f,0 ', [d*1.5, d, d*3]);
    'T': s := lc +format(' %1.0f,0 ', [d*1.5]);
    else Exit;
  end;

  //set the SVG DPath text
  Memo1.Lines.Text := Memo1.Lines.Text + s;

  //find the new targetLayer
  if (svgPathLayer.ChildCount = 0) then Exit;
  spl := svgPathLayer[svgPathLayer.ChildCount -1] as TSvgSubPathLayer;
  if spl.ChildCount > 0 then
    SetTargetLayer(spl.GetLastSegLayer);
end;
//------------------------------------------------------------------------------

procedure TMainForm.PopupMenu1Popup(Sender: TObject);
begin
  mnuReverseArc2.Enabled :=
    Assigned(targetLayer) and (targetLayer is TSvgASegLayer);
  mnuReverseArc.Enabled := mnuReverseArc2.Enabled;
end;
//------------------------------------------------------------------------------

end.
