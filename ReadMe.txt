
Image32 is a compact 2D graphics library written in Delphi Pascal. It provides an extensive range of image manipulation functions. It also includes a polygon renderer for line and polygon drawing. The renderer supports a range of brush filling options including tiled images, and linear and radial gradient fills.

Version: 1.15
Uploaded: 3 August 2019
Freeware for both open source and commercial applications released under 
Boost Software License - see https://www.boost.org/LICENSE_1_0.txt
Copyright © 2019 Angus Johnson

http://www.angusj.com/delphi/image32.php


Changes since initial release:

Version: 1.15
  Added SplineTransformVert & SplineTransformHorz to Image32_Transform.pas
  Added TLayer32.MidPoint property in Image32_Layers.pas
  Changed TTextAlignV to TTextVAlign in Image32_Text.pas
  Added Alpha() and NoAlpha() functions to Image32.pas
  Fixed a minor bug in color gradient renderers
  Fixed a bug in TImage32.CropTransparentPixels
 
Version: 1.14
  Modified DrawButton function parameters
  Minor code tidy and documentation improvements

Version: 1.13
  Major improvements to Image32_Layers unit
  Modified ProjectiveTransform function

Version: 1.12
  Added OnChange event to TImage32 class
  Added new Image32_Layers unit
  Tidied up Clipboard support which was messy.

Version: 1.11
  Modified DrawShadow function parameters
  Modified DrawDashedLine function parameters
  Renamed BuildDashPath function to GetDashedPath
  Added GetDashedOutLine function

Version: 1.10
  Added Draw3D function
  Added affine and projective transformation functions.
  Added QSpline & CSpline functions
  Added option to insert intermediate colors into gradient renders
  Numerous minor bug fixes
  Significantly improved documentation
  