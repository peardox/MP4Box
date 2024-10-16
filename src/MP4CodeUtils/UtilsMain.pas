unit UtilsMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Memo.Types, FMX.ScrollBox,
  FMX.Memo, FMX.Layouts;

type
  TFourCC = LongWord;

  TForm2 = class(TForm)
    Layout1: TLayout;
    Layout2: TLayout;
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

  Atoms: Array[0..13] of String = (
    'udta', 'mdia', 'edts', 'tref', 'trak', 'moov', 'ilst',
    'ftyp', 'mdat', 'mvhd', 'tkhd', 'elts', 'meta', 'mvhd');

implementation

{$R *.fmx}

function MAKEFOURCC(ch0, ch1, ch2, ch3: AnsiChar): TFourCC;
begin
  Result := LongWord(Ord(ch0)) or (LongWord(Ord(ch1)) shl 8) or (LongWord(Ord(ch2)) shl 16) or (LongWord(Ord(ch3)) shl 24);
end;

function StringToFourCC(const AStr: String): TFourCC;
begin
  if Length(AStr) <> 4 then
    Raise Exception.Create('Fatal Error FourCC : ' + AStr + ' MUST be 4 chars');

  Result := MakeFourCC(AnsiChar(AStr[4]), AnsiChar(AStr[3]), AnsiChar(AStr[2]), AnsiChar(AStr[1]));
end;

procedure TForm2.Button1Click(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to Length(Atoms) - 1 do
    begin
      Memo1.Lines.Add(Atoms[I] + ' = $' + IntToHex(StringToFourCC(Atoms[I]), 4));
    end;

end;

end.
