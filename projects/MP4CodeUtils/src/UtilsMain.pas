unit UtilsMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Memo.Types, FMX.ScrollBox,
  FMX.Memo, FMX.Layouts, FMX.Media;

type
  TMP4FourCC = LongWord;

  TForm2 = class(TForm)
    Layout1: TLayout;
    Layout2: TLayout;
    Button1: TButton;
    Memo1: TMemo;
    MediaPlayer1: TMediaPlayer;
    Button2: TButton;
    OpenDialog1: TOpenDialog;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

  Atoms: Array[0..43] of String = (
    { Root Atoms}
    'ftyp', 'mdat', 'moov', 'pnot',
    { Floating Atoms}
    'free', 'skip', 'wide',
    { Container Atoms }
    'edts',
    'ilst',
    'mdia', 'meta', 'minf',
    'trak', 'tref', 'tkhd',
    'stbl',
    'udta',
    { Todo Atoms }
    'chap',
    'chpl',
    'elst',
    'hdlr',
    { Data Atoms }
    'mvhd',
    { Meta Atoms }
    '©ART',
    '©alb',
    '©cmt',
    '©day',
    '©des',
    '©gen',
    '©nam',
    '©nrt',
    '©pub',
    '@sti',
    '©too',
    '©wrt',
    'aART',
    'AACR',
    'CDEK',
    'CDET',
    'TCOM',
    'VERS',
    'covr',
    'cprt',
    'prID',
    'rldt'
    );

implementation

{$R *.fmx}

function MAKEFOURCC(ch0, ch1, ch2, ch3: AnsiChar): TMP4FourCC;
begin
  Result := LongWord(Ord(ch0)) or (LongWord(Ord(ch1)) shl 8) or (LongWord(Ord(ch2)) shl 16) or (LongWord(Ord(ch3)) shl 24);
end;

function StringToFourCC(const AStr: String): TMP4FourCC;
begin
  if Length(AStr) <> 4 then
    Raise Exception.Create('Fatal Error FourCC : ' + AStr + ' MUST be 4 chars');

  Result := MakeFourCC(AnsiChar(AStr[4]), AnsiChar(AStr[3]), AnsiChar(AStr[2]), AnsiChar(AStr[1]));
end;

function FourCCToString(const AFourCC: TMP4FourCC): String;
begin
  Result := Chr(Byte((AFourCC and $FF000000) shr 24)) +
            Chr(Byte((AFourCC and $FF0000) shr 16)) +
            Chr(Byte((AFourCC and $FF00) shr 8)) +
            Chr(Byte(AFourCC and $FF));
end;

procedure TForm2.Button1Click(Sender: TObject);
var
  I: Integer;
  cc4: TMP4FourCC;
begin
  for I := 0 to Length(Atoms) - 1 do
    begin
      cc4 := StringToFourCC(Atoms[I]);
      Memo1.Lines.Add(Atoms[I] + ' = $' + IntToHex(cc4, 4) + ' - ' + FourCCToString(cc4));
    end;

end;

procedure TForm2.Button2Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
    begin
      MediaPlayer1.FileName := OpenDialog1.FileName;
      MediaPlayer1.Play;
    end;
end;

end.
