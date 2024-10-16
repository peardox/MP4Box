unit MP4Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.ScrollBox, FMX.Memo, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts,
  Generics.Defaults, Generics.Collections, MP4Boxes
  ;

type
  TForm1 = class(TForm)
    Layout1: TLayout;
    Layout2: TLayout;
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    FAtomList: TAtomList;
    procedure ViewAtoms(const List: TAtomObjectList);
    { Private declarations }
  public
    { Public declarations }
    Property AtomList: TAtomList read FAtomList write FAtomList;
  end;

var
  Form1: TForm1;
  MoovPos: UInt64;
  MoovSize: UInt64;

implementation

uses IOUtils;

{$R *.fmx}

procedure TForm1.Button1Click(Sender: TObject);
var
  FName: String;
begin
  FName := 'C:\Video\Discworld\City Watch\Feet of Clay꞉ Discworld, Book 19.m4b';
//  FName := 'C:\Video\The.Adjustment.Bureau.mp4';
//  FName := 'C:\work\pratchett\Guards_Guards.m4b';

  AtomList := TAtomList.Create;
  AtomList.Filename := FName;
  if AtomList.Open then
    begin
      Memo1.Lines.Add('Decoding ' + FName);
      AtomList.DecodeStream;
    end;

    ViewAtoms(AtomList.Atoms);
end;

procedure TForm1.ViewAtoms(const List: TAtomObjectList);
var
  Atom: TAtom;
  I: Integer;
begin
    for I := 0 to List.Count - 1 do
      begin
        Atom := List[I];
        Memo1.Lines.Add(IntToHex(Atom.AbsPos) + ' : ' + IntToHex(Atom.Size) + ' = ' + StringOfChar(' ', Atom.Level * 4) +  Atom.FourCC);
        if Atom.Children <> Nil then
          ViewAtoms(Atom.Children);
      end;
end;

end.
