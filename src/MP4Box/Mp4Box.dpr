program Mp4Box;

uses
  System.StartUpCopy,
  FMX.Forms,
  MP4Main in 'MP4Main.pas' {Form1},
  MP4Boxes in 'MP4Boxes.pas',
  MP4KnownAtoms in 'MP4KnownAtoms.pas',
  MP4Types in 'MP4Types.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
