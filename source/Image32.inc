{$IFDEF FPC}
  {$MODE DELPHI}
  {$DEFINE ABSTRACT_CLASSES}
  {$DEFINE RECORD_METHODS}
  //{$modeSwitch advancedRecords}
  {$DEFINE PBYTE}
  {$DEFINE UITYPES}
  {$IFNDEF DEBUG}
    {$DEFINE INLINE}
  {$ENDIF}
{$ELSE}
  {$IF COMPILERVERSION < 15}
    Your version of Delphi is not supported (requires Delphi ver. 7 or above).
  {$IFEND}
  {$IF COMPILERVERSION >= 17}
    {$IFNDEF DEBUG}
      {$DEFINE INLINE}                 //Delphi 2005 - added inlining
    {$IFEND}  
    {$IF COMPILERVERSION >= 18}
      {$DEFINE ABSTRACT_CLASSES}       //Delphi 2006 - added abstract classes
      {$DEFINE RECORD_METHODS}         //Delphi 2006 - added records with methods
      {$DEFINE REPORTMEMORYLEAKS}      //Delphi 2006 - added ReportMemoryLeaksOnShutdown
      {$WARN SYMBOL_PLATFORM OFF}
      {$DEFINE SETSIZE}                //Delphi 2006 - added TBitmap.SetSize
      {$IF COMPILERVERSION >= 18.5}
        {$DEFINE DELPHI_PNG}           //Delphi 2007 - added PNG support
        {$DEFINE MAINFORMONTASKBAR}    //Delphi 2007 - added TApplication.MainFormOnTaskbar
        {$if CompilerVersion >= 20}
          {$DEFINE PBYTE}              //Delphi 2009 - added PByte
          {$DEFINE EXIT_PARAM}         //Delphi 2009 - added Exit(value)
          {$IF COMPILERVERSION >= 23}
            {$DEFINE UITYPES}          //UITypes unit added in DelphiXE2
          {$IFEND}
        {$IFEND}
      {$IFEND}
    {$IFEND}
  {$IFEND}
{$ENDIF}

