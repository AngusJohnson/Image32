
Image32 - 2D graphics library for Delphi Pascal
Latest version: 1.55
Released: 8 November 2020

Copyright © 2019-2020 Angus Johnson
Freeware released under Boost Software License
https://www.boost.org/LICENSE_1_0.txt

Documentation : http://www.angusj.com/delphi/image32/Docs/
Download      : https://sourceforge.net/projects/image32/files/

Recent changes:

Version 1.55
  changes in Image32_Ttf
	bugfix DrawAngledText function
  changes in ImagePanels
    minor bugfix
  Image32_Text now deprecated in favour of Image32_Ttf

Version 1.54
  changes in Image32_Ttf
	added 3 methods to TFontreader (Window's platform only):
	  Create(string), Load(string) and  LoadUsingFontHdl methods
	added TGlyphCache.GetAngledTextGlyphs functions
	added DrawAngledText function
	added GetTextGlyphsOnPath function
	removed 3 functions:  GetFacenameAndStyle(), 
	  FilterOnFacename() and FilterOnStyles()    	
  changes in Image32_Text
	renamed GetTextAlongPathOutine() to GetTextGlyphsOnPath()
  changes in Image32_Clipper
	Deleted InflatePolygons and InflateOpenPath functions
	  and replaced with InflatePath and InflatePaths functions
  changes in Image32_Vector
	added esPolygon to TEndStyle enumeration
	('Closed' paths are handled differently polygons.)
  changes in Image32_Extra
	Added GradientColor, MakeDarker and MakeLighter functions
  updated Examples to accommodate recent changes

Version 1.53
  Image32_Ttf
    Renamed TTtfFontReader class to TFontReader class
    Similarly renamed several related records
    Added TFontReader.Weight property
    Fixed bug in TFontReader.FontInfo record where the
      'facename' and 'style' fields could contain misplaced #0's
    Added GetFontFolder & GetInstalledTtfFilenames functions
    Added GetFacenameAndStyle function
    Added FilterOnFacename function
    Added FilterOnStyles function
    Fixed bug in GetMeaningfulDateTime with invalid dates        
    Added TGlyphCache.GetTextWidth method
    Added DrawText functions
    Added DrawTextVertical function    
  Image32_Text
    Move TArrayOfString type definition to Image32 unit
  Image32_Transform
    Updated ProjectiveTransform
  Image32_Vector
    Added Matrix function
    Renamed MatrixAdjoint to MatrixAdjugate and modified it.
    Fixed but in UnionRect function  
  Image32_Bmp:
    Various minor improvements
  Examples
    Updated FMX and FMX2 examples to accommodate the recent
    renaming of TTtfFontReader.
    Fixed several broken examples

Version 1.52
  Changes in Image32
    type TArrayOfPointD deleted (replaced by its former alias TPathD)
    type TArrayOfArrayOfPointD deleted (replaced by its former alias TPathsD)
    DPI function renamed DPIAware
    ScreenPixelsY variable removed
    DpiScale renamed ScreenScale
  Changes in Image32_Draw
    DrawPolygon_ClearType function added
  Changes in Image32_Text
    Most parameters of string type changed to UnicodeString type
    Similarly, parameters of Char type changed to WideChar type
    DrawText_LCD function renamed to DrawText_ClearType
  Changes in Image32_Ttf
    TTtfFontInfo.GetGlyph renamed TTtfFontInfo.GetGlyphInfo
    TGlyphCache.GetChar renamed TGlyphCache.GetCharInfo
    TGlyphCache.GetString renamed TGlyphCache.GetTextGlyphs
    Overloaded TGlyphCache.GetTextGlyphs method now manages
      line wrapping, justification, horizontal and vertical 
      alignments within a specified TRect.
    Added TGlyphCache.GetCharOffsets method
    TGlyphCache.LineGap property replaced with LineHeight property

Version: 1.51
  Modified Image32_Ttf
    Significant updates to TTtfFontReader and also to
    TGlyphManager which has been renamed TGlyphCache
  Minor updates to other units
  Updated FMX demo

Version: 1.50a
  Modified Image32_Ttf
    Added kerning support (enabled by default)

Version: 1.50
  Added Image32_Ttf
    This unit supports cross-platform text rendering 
    by parsing TTF files directly.
  Numerous minor bug-fixes and other improvements.

Version: 1.49
  Fixed a rare bug in Image32_Draw that was 
  introduced in version 1.47

Version: 1.48
  Bugfixes to FMX framework support

Version: 1.47
  Added Image32_FMX
    Provides FMX support (excluding text rendering).
    This unit uses FMX to load and save stored images
    (ie instead of using Image32_BMP, Image32_PNG etc).
  Minor tweaks

Version: 1.46
  Replaced BitmapPanels unit with ImagePanels unit
  Changes in Image32 unit
    ScaleToFit method modified
    ScaleToFitCentered method added
    CopyToDc method overloaded
  
Version: 1.45
  Changes in Image32_Vector
    Renamed the SmoothLine function SmoothToBezier

Version: 1.44
  Changes in BitmapPanels 
    Fixed a significant bug introduced in the previous update :(
  Changes in Image32_Transform and Image32_Vector  
    Moved most matrix code to Image32_Vector
    Added a number of new matrix functions

Version: 1.43
  Changes in Image32_BMP
    Fixed compiler bug affecting Delphi XE4 compiler.
  Changes in BitmapPanels
    Minor changes to ClientToBitmap and BitmapToClient methods
  Examples
    Added missing DialogsEx.res file
    
Version: 1.42
  Changes in Image32
    Fixed a minor bug in TImage32.CopyToDc method
  Changes in BitmapPanels
    Fixed minor bugs in cursor management and with scrollbars
  Changes in Examples
    Minor update to "Photo" application
        
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

