program MP4Explorer;

uses
  System.StartUpCopy,
  FMX.Forms,
  MP4Main in 'src\MP4Main.pas' {Form1},
  MP4Boxes in 'src\MP4Boxes.pas',
  MP4Types in 'src\MP4Types.pas',
  MP4Atoms in 'src\MP4Atoms.pas',
  MP4DerivedAtoms in 'src\MP4DerivedAtoms.pas',
  MP4AutoAtom in 'src\MP4AutoAtom.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
