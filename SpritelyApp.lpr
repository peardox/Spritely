{ Program to run the game on desktop (standalone) platforms.
  Can be auto-generated by "castle-engine generate-program". }
program SpritelyApp;

{$ifdef MSWINDOWS} {$apptype GUI} {$endif}

{ This adds icons and version info for Windows,
  automatically created by "castle-engine compile". }
{$ifdef CASTLE_AUTO_GENERATED_RESOURCES} {$R castle-auto-generated-resources.res} {$endif}

uses
  {$ifndef CASTLE_DISABLE_THREADS}
    {$info Thread support enabled.}
    {$ifdef UNIX} CThreads, {$endif}
  {$endif}
  CastleApplicationProperties, CastleLog, CastleWindow, AppInitialization,
  MainGameUnit, multimodel, SpritelyLog;

begin
  { Optionally you can specify here your application version.
    It will appear e.g. in the log and in the --help output.
    Instead of updating this program file, you can also delete it (and remove
    "standalone_source" from the CastleEngineManifest.xml),
    and specify <version> inside CastleEngineManifest.xml.
    In this case, the program file, with appropriate version set,
    will be automatically generated and updated by the build tool. }
  LogHandler := TLogHandler.Create(Application);

  Application.ParseStandardParameters;
  ApplicationProperties.ApplicationName := 'Spritely';
  ApplicationProperties.Caption := 'Spritely';
  ApplicationProperties.Version := '0.1';


  { On standalone, activate log only after parsing command-line options.
    This allows to handle --version and --help command-line parameters
    without any extra output on Unix.
    This also allows to set --log-file from Application.ParseStandardParameters. }
  InitializeLog;
  
  Application.MainWindow.OpenAndRun;
end.
