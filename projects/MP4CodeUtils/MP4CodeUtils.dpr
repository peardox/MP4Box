program MP4CodeUtils;

uses
  System.StartUpCopy,
  FMX.Forms,
  UtilsMain in 'src\UtilsMain.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.