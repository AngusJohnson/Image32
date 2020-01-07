
Image32 - 2D graphics library for Delphi Pascal
Latest version: 1.36
Released: 5 January 2020

Copyright © 2019-2020 Angus Johnson
Freeware released under Boost Software License
https://www.boost.org/LICENSE_1_0.txt

Website  : http://www.angusj.com/delphi/image32/Docs/_Body.htm
download : https://sourceforge.net/projects/image32/files/

Recent changes:

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
    
Version: 1.29
  Changes in Image32_Extra
    Improved CompareRGB function
    Replaced MaskToPolygons function with Vectorize function
    Bugfixed GetMask function
  Changes in Examples
    Updated Vectorize application
    
Version: 1.28
  Changes in Image32
    Renamed TImage32.CopyFrom method to CopyBlend
  Changes in Image32_MixedPath
    Deleted deprecated TMixedPath class in Image32_MixedPath
    Renamed Image32_MixedPath to Image32_SmoothPath
    Renamed DrawSmoothPathDesigner to DrawSmoothPathOntoDesigner
  Changes in Image32_Extra
    Changed and renamed FloodFillRGB function to CompareRGB
    Changed and renamed FloodFillHue function to CompareHue
    Exposed GetFloodFillMask function
    Added GetMask function
    Added Vectorize function (see documentation)
  Changes in Image32_Draw
    Added ErasePolygon function
    Added DrawBoolMask function
    Added DrawAlphaMask function
  Changes in BitmapPanels
    Added TPanel.BitmapProperties.ScaleType property
    Removed TPanel.BitmapProperties.RawScale property
    Modified TPanel.ClearBitmap (see documentation)
  Changes in Examples
    Added Vectorize application
    
Version: 1.27
  Changes in Image32
    Fixed bug in Color32 function
  Changes in Image32_Vector  
    Renamed AddToPath function to JoinPaths
  Changes in Image32_Text
    Added TFontInfo.MeasureText method
  Changes in BitmapPanels
    Changed TPanel.ClearBitmap pixelFormat parameter default (now pf24bit)
    Added TPanel.BitmapProperties.AutoCenter property (default = True)
  Changes in Image32_Layers
    Renamed TLayeredImage32.GetLayerNamed to FindLayerNamed
  Changes in Examples
    Updated several examples
    
Version: 1.26
  Changes in Image32
    Added IsImageOwner property to TImageList32
  Changes in Image32_Layers
    Fixed several bugs
  Changes in Image32_Vector  
    Added ClosestPointOnLine and ClosestPointOnSegment functions
  Changes in BitmapPanels
    Added OnBeginPaint property to TBitmapProperties
    Added OnKeyDown & OnKeyUp properties to TBitmapProperties
  Changes in Examples
    Added Image,Arrows,Text utility    