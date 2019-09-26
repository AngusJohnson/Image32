
Image32 is a 2D graphics library written in Delphi Pascal. It provides an extensive range of image manipulation functions. It also includes a polygon renderer for line and polygon drawing. The renderer supports a range of brush filling options including tiled images, and linear and radial gradient fills.

Version: 1.24
Uploaded: 26 September 2019
Freeware for both open source and commercial applications released under 
Boost Software License - see https://www.boost.org/LICENSE_1_0.txt
Copyright © 2019 Angus Johnson

http://www.angusj.com/delphi/image32.php


Recent changes:

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
