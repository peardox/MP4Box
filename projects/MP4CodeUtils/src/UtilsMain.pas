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
    procedure MakeMP4AtomClass(var TypeDefs: String; var CodeDefs: String; const AFilename: String);
    function CreateDataCode(const AVarName: String; const AVarType: String): String;
    function GetSkipSize(const AVarType: String): String;
    procedure MakeMP4AtomWrapper(const AFilename: String);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}
uses MP4Types, IOUtils;

procedure TForm2.MakeMP4AtomWrapper(const AFilename: String);
var
  StartOfFile, TypeDefs, ImpDef, CodeDefs, EndOfFile: String;
begin
  StartOfFile :=  'unit MP4DerivedAtoms;' + SLineBreak +
                  '' + SLineBreak +
                  '{$M+}' + SLineBreak +
                  '' + SLineBreak +
                  'interface' + SLineBreak +
                  '' + SLineBreak +
                  'uses' + SLineBreak +
                  '  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,' + SLineBreak +
                  '  Generics.Defaults, Generics.Collections, MP4Atoms, MP4Types;' + SLineBreak +
                  '' + SLineBreak +
                  'type';

  TypeDefs := '';
  ImpDef := 'implementation' + SLineBreak;
  CodeDefs := '';
  EndOfFile := SLineBreak + 'end.' + SLineBreak;

  MakeMP4AtomClass(TypeDefs, CodeDefs, AFilename);

  Memo1.Lines.Add(StartOfFile);
  Memo1.Lines.Add(TypeDefs);
  Memo1.Lines.Add(ImpDef);
  Memo1.Lines.Add(CodeDefs);
  Memo1.Lines.Add(EndOfFile);
end;

procedure TForm2.MakeMP4AtomClass(var TypeDefs: String; var CodeDefs: String; const AFilename: String);
var
  cc4: TMP4FourCC;
  SAtom: String;
  SLines: TStringDynArray;
  I: Integer;
  Triplets: Integer;
begin
  SAtom := TPath.GetFileNameWithoutExtension(AFilename);
  SLines := TFile.ReadAllLines(AFilename);
  cc4 := StringToFourCC(SAtom);

  for I := Length(SLines) - 1 downto 0 do
    begin
      SLines[I] := SLines[I].Trim;
      if SLines[I] = String.Empty then
        Delete(Slines,I,1);
    end;

  Triplets := Length(SLines) div 3;
  if Length(SLines) <> (Triplets * 3) then
    Raise Exception.Create('Lines in ' + AFilename + ' not divisible by 3');

  TypeDefs := TypeDefs + '  ' + SLines[0] + ' = ' + SLines[1] + ' { $' + IntToHex(cc4, 4) + ': // ' + SAtom + ' }' + SLineBreak +
              '  ' + Slines[2] + SLineBreak +
              '  strict private' + SLineBreak;


  for I := 1 to Triplets - 1 do
    begin
      TypeDefs := TypeDefs + '    F' + SLines[(I * 3)] + ': ' + SLines[(I * 3) + 1] + ';' + SLineBreak;
      TypeDefs := TypeDefs + '    ' + SLines[(I * 3) + 2] + SLineBreak;
    end;

  TypeDefs := TypeDefs + '  public' + SLineBreak;
  TypeDefs := TypeDefs + '    procedure ReadFromStream(var BufPos: Int64; var AStream: TStream; const BufSize: Int64); override;' + SLineBreak;
  TypeDefs := TypeDefs + '  published' + SLineBreak;
  for I := 1 to Triplets - 1 do
    begin
      if not(SLines[(I * 3)].StartsWith('Reserved')) then
      TypeDefs := TypeDefs + '    property ' +
                  SLines[(I * 3)] + ': ' + SLines[(I * 3) + 1] +
                  ' read F' + SLines[(I * 3)] + ' write F' + SLines[(I * 3)] +
                  ';' + SLineBreak;
    end;
  TypeDefs := TypeDefs + '  end;' + SLineBreak;

  CodeDefs := CodeDefs + '{ ' + SLines[0] + ' }' + SLineBreak + SLineBreak;
  CodeDefs := CodeDefs + 'procedure ' + SLines[0] + '.ReadFromStream(var BufPos: Int64; var AStream: TStream; const BufSize: Int64);' + SLineBreak;
  CodeDefs := CodeDefs + 'begin' + SLineBreak;

  for I := 1 to Triplets - 1 do
    begin
      if SLines[(I * 3)].StartsWith('Reserved') then
        CodeDefs := CodeDefs + '  ' + CreateDataCode(SLines[(I * 3)], SLines[(I * 3) + 1]) + ';' + SLineBreak
      else
        CodeDefs := CodeDefs + '  F' + SLines[(I * 3)] + ' := ' + CreateDataCode(SLines[(I * 3)], SLines[(I * 3) + 1]) + ';' + SLineBreak;
    end;

  CodeDefs := CodeDefs + 'end;' + SLineBreak;
end;

procedure TForm2.Button1Click(Sender: TObject);
var
  FPath: String;
begin
  FPath := 'C:\git\MkMp4Obj\def';
  MakeMP4AtomWrapper(TPath.Combine(IncludeTrailingPathDelimiter(FPath), 'tkhd.def'));
{
  if OpenDialog1.Execute then
    begin
      // MediaPlayer1.FileName := OpenDialog1.FileName;
    end;
}
end;

procedure TForm2.Button2Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
    begin
      MediaPlayer1.FileName := OpenDialog1.FileName;
      MediaPlayer1.Play;
    end;
end;

function TForm2.CreateDataCode(const AVarName: String; const AVarType: String): String;
begin
  if AVarName.StartsWith('Reserved') then
    begin
      if AVarType.StartsWith('Array[0..') then
        begin
          Result := 'ReadSkip(BufPos, AStream, ' + GetSkipSize(AVarType) + ')';
        end
      else
        Raise Exception.Create('Readskip parse error');
    end
  else if AVarType = 'TDateTime' then
    Result := 'ReadMediaDateTime(BufPos, AStream)'
  else if AVarType = 'UInt64' then
    Result := 'ReadUInt64(BufPos, AStream)'
  else if AVarType = 'UInt32' then
    Result := 'ReadUInt32(BufPos, AStream)'
  else if AVarType = 'UInt16' then
    Result := 'ReadUInt16(BufPos, AStream)'
  else if AVarType = 'Byte' then
    Result := 'ReadByte(BufPos, AStream)'
  else if AVarType = 'TMP4Fixed32' then
    Result := 'ReadUInt32(BufPos, AStream)'
  else if AVarType = 'TMP4Fixed16' then
    Result := 'ReadUInt16(BufPos, AStream)'
  else if AVarType = 'Single' then
    Result := 'ReadSingle(BufPos, AStream)'
  else if AVarType = 'Double' then
    Result := 'ReadDouble(BufPos, AStream)'
  else if AVarType = 'TMP4Matrix3x3' then
    Result := 'ReadMatrix3x3(BufPos, AStream)'
  else if AVarType = 'TMP4FourCC' then
    Result := 'ReadFourCC(BufPos, AStream)'
  else if AVarType = 'TMP4ChapterData' then
    Result := 'ReadChapterDataList(BufPos, AStream, BufSize)'
  else if AVarType = 'TMP4MetaData' then
    Result := 'ReadMetaDataList(BufPos, AStream, BufSize)'

  else if AVarType = 'TCString' then
    Result := 'ReadCString(BufPos, AStream, BufSize)'
  else if AVarType = 'TMP4SampleDescriptionArray' then
    Result := 'ReadSampleDescriptionArray(BufPos, AStream, BufSize)'

  else
    Raise Exception.Create('Bad CreateDataCode');
end;

function TForm2.GetSkipSize(const AVarType: String): String;
var
  I: Integer;
  SkipNum: Integer;
begin
  SkipNum := 0;

  for I := 10 to Length(AVarType) do
  begin
    if (AVarType[I] >= '0') and (AVarType[I] <= '9') then
      SkipNum := (SkipNum * 10) + (Ord(AVarType[I]) - 48)
    else
      break;
  end;

  if SkipNum = 0 then
    Raise Exception.Create('Error extracting in GetSkipSize');

  Result := IntToStr(SkipNum + 1);
end;

end.
