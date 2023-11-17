program ActoriousServer;

uses
  Vcl.Forms,
  Unit1 in 'Unit1.pas' {ServerContainer: TDataModule},
  Unit2 in 'Unit2.pas' {MainForm},
  ActorInfoService in 'ActorInfoService.pas',
  ActorInfoServiceImplementation in 'ActorInfoServiceImplementation.pas';

{$R *.res}

begin
//  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'ActoriousServer';
  Application.CreateForm(TServerContainer, ServerContainer);
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.











