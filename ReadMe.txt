
Image32 - 2D graphics library for Delphi Pascal
Latest version: 1.41
Released: 14 February 2020

Copyright © 2019-2020 Angus Johnson
Freeware released under Boost Software License
https://www.boost.org/LICENSE_1_0.txt

Website  : http://www.angusj.com/delphi/image32/Docs/_Body.htm
download : https://sourceforge.net/projects/image32/files/

Recent changes:

Version: 1.41
  CHANGES IN BITMAPPANELS
    MAJOR UPDATE INCLUDING THE REMOVAL OF THE CONFUSING 
      BITMAPPROPERTIES PROPERTY, AND WITH SEVERAL BUGFIXES
  Changes in Image32
    Added TImage32.ScaleToFit method
  Changes in Image32_PNG
    Improved transparency detection
  Changes in Image32_Layers
    Changed and renamed AddNewLayer to AddLayer
    Changed and renamed InsertNewLayer to InsertLayer
  Changes in Image32_SmoothPath
    Renamed DrawSmoothPathOnDesigner to
      DrawSmoothPathCtrlLinesOnDesigner
  Changes in Examples
    Added "Photo" application, an image viewer

Version: 1.40
  Changes in Image32_Extra
    Improved Vectorize function
  Changes in BitmapPanels
    Updated
  Changes in Examples
    Updated RasterToSVG and Vectorize samples
    
Version: 1.39
  Changes in Image32_Layers
    Fixed minor bug introduced in previous update
    
Version: 1.38
  Changes in Image32
    Added TImage32.Copy method 
      (it's the same as CopyBlend with nil blend)
  Changes in Image32_Layers
    TLayeredImage32.Group method now returns the GroupId
    CreateButtonGroup and CreateSizingBtnsGroup functions now 
      return the newly created group's GroupId
    Replace UpdateSizingGroup function with UpdateButtonSizingGroup 
      function (and significantly changed its functionality).
    TLayer.HitTestRegions coordinates are now relative to 
      the layer itself (not the TLayeredImage32 control).
  Changes in Image32_CQ
    Removed TrimPaletteByFraction and overloaded TrimPalette
  Changes in Examples
    Significantly updated SimpleLayers example
    
Version: 1.37
  Changes in Image32
    TImage32.CanPasteFromClipboard is now a class function
  Changes in Image32_Draw      
    Added DrawInvertedLine, DrawInvertedDashedLine
  Changes in Image32_Vector      
    Fixed minor bug in GetDashedPath function
  Changes to Image32_Transform
    Renamed SplineTransformVert to SplineVertTransform
    Renamed SplineTransformHorz to SplineHorzTransform
  Changes in Examples
    Major update to LayersAndTransform; also renamed to Transform
    LayersAndTransform2 removed
      
Version: 1.36
  Changes in Image32
    Renamed TImage32.SetMonochromeColor to TImage32.SetRGB
    Renamed TImage32.ConvertToMask to TImage32.ConvertToBoolMask
    Added TImage32.ConvertToAlphaMask
    Renamed GetMask function to GetBoolMask
    Added GetByteMask function
    Added CompareRgbEx & CompareAlphaEx functions    
  Changes in Image32_CQ
    Renamed CreatePalette to MakePalette because CreateXXXX 
      tends to imply the returned structure will need freeing
    Added MakeAndApplyPalette which is *much* faster than
      MakePalette followed by ApplyPalette
    Added TrimPalette and TrimPaletteByFraction functions
  Changes in Examples
    Updated RasterToSVG utility
