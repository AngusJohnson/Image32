
Image32 - 2D graphics library for Delphi Pascal
Latest version: 1.38
Released: 17 January 2020

Copyright © 2019-2020 Angus Johnson
Freeware released under Boost Software License
https://www.boost.org/LICENSE_1_0.txt

Website  : http://www.angusj.com/delphi/image32/Docs/_Body.htm
download : https://sourceforge.net/projects/image32/files/

Recent changes:

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
  
Version: 1.35
  Changes in Examples
    Added a detailed RasterToSVG utility that converts both  
    monochrome and colored raster images into SVG format
  Changes in Image32
    Added TImage32.ConvertToMonochrome method
  Changes in Image32_Extra
    Exposed VectorizeMask function
  Changes in Image32_CQ
    Added CreatePaletteEx function
    
Version: 1.34
  Changes in Image32
    Fixed a bug when saving images with no supplied path 
    Added optional params to TImage32.ConvertToMask method 
  Changes in Image32_Vector
    minor improvements to the SmoothLine function
  Changes in Image32_CQ
    added optional 'essentialColors' param to CreatePalette function
    added GetIndexNearestPaletteColor function
  Changes in BitmapPanels
    fixed a minor bug with keyboard controls (using Ctrl + numeric)
    
Version: 1.33
  Changes in Image32_Extra
    Moved GetMask function to the Image32 unit.
    Moved CompareRGB, CompareHue & CompareAlpha functions to Image32.
    Much improved Vectorize function (it's now very accurate)
  Changes in Image32
    Added TImage32.ConvertToMask method

Version: 1.32
  Fixed numerous minor compiler bugs for older Delphi versions.
  Changes in Image32_Vector
    Added an extra (optional) parameter to the new SmoothLine function.
    
Version: 1.31
  Changes in Image32_Vector
    Renamed CBezier to FlattenCBezier;
    Renamed CSpline to FlattenCSpline;
    Renamed QBezier to FlattenQBezier;
    Renamed QSpline to FlattenQSpline;
    Added SmoothLine function (see documentation)
  Changes in Image32_Extra
    Minor improvement to the Vectorize function
  Changes in Examples
    Significant update to the Vectorize application
    
Version: 1.30
  Changes in Image32_Vector
    Added RamerDouglasPeucker function
  Changes in Image32_Extra
    Improved Vectorize function    
    
