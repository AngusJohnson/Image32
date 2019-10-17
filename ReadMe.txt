
Image32 is a 2D graphics library written in Delphi Pascal. It provides an extensive range of image manipulation functions. It also includes a polygon renderer for line and polygon drawing. The renderer supports a range of brush filling options including tiled images, and linear and radial gradient fills.

Version: 1.27
Uploaded: 17 October 2019
Freeware for both open source and commercial applications released under 
Boost Software License - see https://www.boost.org/LICENSE_1_0.txt
Copyright © 2019 Angus Johnson

http://www.angusj.com/delphi/image32.php


Recent changes:

Version: 1.27
  Changes in Image32
    Fixed bug in Color32 function
  Changes in Image32_Vector  
    Renamed AddToPath function to JoinPaths
  Changes in Image32_Text
    Added TFontInfo.MeasureText method
  Changes in BitmapPanels
    TPanel.ClearBitmap pixelFormat parameter now defaults to pf24bit
    Added TPanel.BitmapProperties.AutoCenter property (default = True)
    
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
    
Version: 1.25
  Changes in Image32_Layers
    Renamed TLayer32.GroupIndex property to TLayer32.GroupId
    Renamed TLayer32.GetIdxFirstLayerInGroup to GeFirstInGroupIdx
    Renamed TLayer32.GetIdxLastLayerInGroup to GetLastInGroupIdx
    Added TLayer32.IndexInGroup property
    Merged THitTestLayer32 into TLayer32 class
    Added TDesignerLayer32.DrawLine method
    Added StartButtonGroup function
    Modified AddToButtonGroup function
  Changes in Image32_MixedPath
    Fixed minor bug in TMixedPath.FlattenPath
    Changed TMixedPath.Points property 
    Added TMixedPath.PointTypes property
    Changed TPointType to TMixedType (as there's now also a TSmoothType)
    Added TSmoothPath class to Image32_MixedPath unit
    Added a LayersAndSmoothPaths example
    Added TMixedPathLayer32 & TSmoothPathLayer32 classes
    Added DrawMixedPathDesigner & DrawSmoothPathDesigner functions
  Changes in Image32_Vector  
    Fixed bug in ShortenPath function
    Added RotatePoint function
    Added GetDefaultArrowHeadSize function
    Amended GetAngle function
  Changes in Image32_Text  
    Added GetTextAlongPathOutine function
  Minor bugfix to BitmapPanels
  
Version: 1.24
  Added TMixedPath class in new Image32_MixedPath unit
  Added LayersAndMixedPaths example.
  Added InflateOpenPath and InflateOpenPaths function in Image32_Clipper
  Added GetAngle functions in Image32_Vector
  Renamed PointInPaths function to PointInPolygons in Image32_Vector
  Updated THitTestLayer32 in Image32_Layers
  Modified DrawCSplineDesign and DrawQSplineDesign methods in TDesignerLayer32
  Modified CreateSizingBtnsGroup function in Image32_Layers
  Modified CreateButtonGroup function in Image32_Layers
  Renamed TLayeredImage32.CountLayersInGroup to TLayeredImage32.GroupCount
  Fixed bug in CreatePalette in Image32_CQ (introduced in ver. 1.23)

Version: 1.23
  Added InflatePolygon function in Image32_Clipper
  Added UnionPolygon function in Image32_Clipper
  Added OpenPathToFlatPolygon function in Image32_Vector
  Added PointInPaths function in Image32_Vector
  Added THitTestLayer32 class in Image32_Layers
  Added TButtonDesignerLayer32 class in Image32_Layers
  Added TLayer32.SetBounds procedure 
  Fixed TLayeredImage32.DeleteLayer that broke GroupIndexes
  Added new LayersAndSplines example application
  
Version: 1.22
  Improved TDesignerLayer32 dashed line drawing.
  Changed TImage32.CopyFromDC parameters
  Added ReplaceColor function in Image32_Extra
  Modified EraseColor function in Image32_Extra
  Moved TImageList32 class to Image32 unit
  Added Last property to TImageList32

Version: 1.21
  Added TImage32.Tile method
  Fixed another minor bug in TLayeredImage32.GetMergedImage 
  Added ButtonSize, PenWidth & PenColor properties to TDesignerLayer32.
  Added ReduceColors function to Image32_CQ.
  Added TraceContours function to Image32_Extra.

Version: 1.20
  Significant refactor of TImage32. Removed 5 methods:
    BoxBlur; Emboss; EraseColor; GaussianBlur; HatchBackground
    and made them functions in Image32_Extra unit.
  Added new PencilEffect function to Image32_Extra.
  Fixed bug in TLayeredImage32.GetMergedImage 
  Fixed bug in TLayeredImage32.GetLayerAt
  Fixed minor bug in TImages32.CopyToDC.
  Fixed minor bug in BitmapPanels unit.
  Added an animation example to Examples folder.
