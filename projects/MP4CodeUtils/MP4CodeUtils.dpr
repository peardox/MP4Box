program MP4CodeUtils;

uses
  System.StartUpCopy,
  FMX.Forms,
  UtilsMain in 'src\UtilsMain.pas' {Form2},
  MP4Types in '..\MP4Explorer\src\MP4Types.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
