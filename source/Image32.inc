{$IF COMPILERVERSION >= 18}
  {$DEFINE ABSTRACT_CLASSES}     //Delphi 2006 - added abstract classes
  {$DEFINE RECORD_METHODS}       //Delphi 2006 - added records with methods
  {$IF COMPILERVERSION >= 18.5}
    {$IFNDEF DEBUG}
      {$DEFINE INLINE}             //Delphi 2007 - added inlining
    {$ENDIF}  
    {$DEFINE MAINFORMONTASKBAR}  //Delphi 2007 - added TApplication.MainFormOnTaskbar
    {$if CompilerVersion >= 20}  
      {$DEFINE PBYTE}            //Delphi 2009 - added PByte
      {$IF COMPILERVERSION >= 23}
        {$DEFINE UITYPES}          //UITypes uint added in DelphiXE2
      {$IFEND}
    {$IFEND}
  {$IFEND}
{$IFEND}