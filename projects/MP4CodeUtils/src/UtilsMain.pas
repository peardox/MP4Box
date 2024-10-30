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
    procedure MakeMP4AtomClass(var IncDefs: String; var TypeDefs: String; var CodeDefs: String; const AFilename: String);
    function CreateDataCode(const AVarName: String; const AVarType: String): String;
    function GetSkipSize(const AVarType: String): String;
    procedure MakeMP4AtomWrapper(const AFilepath: String);
    procedure OutputData(const ACode, AInc: String);
    function MakeAutoAtomHead: String;
    function MakeAutoAtomTail: String;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}
uses MP4Types, IOUtils;

procedure TForm2.Button1Click(Sender: TObject);
var
  FPath: String;
begin
  FPath := '../../def';
  MakeMP4AtomWrapper(FPath);
{
  if OpenDialog1.Execute then
    begin
      // MediaPlayer1.FileName := OpenDialog1.FileName;
    end;
}
end;

procedure TForm2.OutputData(const ACode: String; const AInc: String);
var
  CheckPath: String;
begin
  CheckPath := '../../../MP4Explorer/src/';
  if FileExists(CheckPath + 'MP4Boxes.pas') then
    begin
      Memo1.Lines.Add(AInc);
      TFile.WriteAllText(CheckPath + 'MP4AutoAtom.pas', MakeAutoAtomHead + AInc + MakeAutoAtomTail);
      Memo1.Lines.Add(ACode);
      TFile.WriteAllText(CheckPath + 'MP4DerivedAtoms.pas', ACode);
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

procedure TForm2.MakeMP4AtomWrapper(const AFilepath: String);
var
  IncDefs, StartOfFile, TypeDefs, ImpDef, CodeDefs, EndOfFile: String;
  FinalCode: String;
  FinalInc: String;
begin
  StartOfFile :=  'unit MP4DerivedAtoms;' + SLineBreak +
                  '' + SLineBreak +
                  '{$M+}' + SLineBreak +
                  '' + SLineBreak +
                  'interface' + SLineBreak +
                  '' + SLineBreak +
                  'uses' + SLineBreak +
                  '  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,' + SLineBreak +
                  '  Generics.Defaults, Generics.Collections, MP4Atoms, MP4Types, MP4ExtendedTypes;' + SLineBreak +
                  '' + SLineBreak +
                  'type';

  IncDefs := '';
  TypeDefs := '';
  ImpDef := 'implementation' + SLineBreak;
  CodeDefs := '';
  EndOfFile := 'end.' + SLineBreak;
  try
    MakeMP4AtomClass(IncDefs, TypeDefs, CodeDefs, TPath.Combine(IncludeTrailingPathDelimiter(AFilepath), 'ftyp.def'));
    MakeMP4AtomClass(IncDefs, TypeDefs, CodeDefs, TPath.Combine(IncludeTrailingPathDelimiter(AFilepath), 'mvhd.def'));
    MakeMP4AtomClass(IncDefs, TypeDefs, CodeDefs, TPath.Combine(IncludeTrailingPathDelimiter(AFilepath), 'tkhd.def'));
    MakeMP4AtomClass(IncDefs, TypeDefs, CodeDefs, TPath.Combine(IncludeTrailingPathDelimiter(AFilepath), 'ilst.def'));
    MakeMP4AtomClass(IncDefs, TypeDefs, CodeDefs, TPath.Combine(IncludeTrailingPathDelimiter(AFilepath), 'chpl.def'));
    MakeMP4AtomClass(IncDefs, TypeDefs, CodeDefs, TPath.Combine(IncludeTrailingPathDelimiter(AFilepath), 'chap.def'));
    MakeMP4AtomClass(IncDefs, TypeDefs, CodeDefs, TPath.Combine(IncludeTrailingPathDelimiter(AFilepath), 'elst.def'));
    MakeMP4AtomClass(IncDefs, TypeDefs, CodeDefs, TPath.Combine(IncludeTrailingPathDelimiter(AFilepath), 'mdhd.def'));
    MakeMP4AtomClass(IncDefs, TypeDefs, CodeDefs, TPath.Combine(IncludeTrailingPathDelimiter(AFilepath), 'hdlr.def'));
    MakeMP4AtomClass(IncDefs, TypeDefs, CodeDefs, TPath.Combine(IncludeTrailingPathDelimiter(AFilepath), 'smhd.def'));
    MakeMP4AtomClass(IncDefs, TypeDefs, CodeDefs, TPath.Combine(IncludeTrailingPathDelimiter(AFilepath), 'stsd.def'));
    MakeMP4AtomClass(IncDefs, TypeDefs, CodeDefs, TPath.Combine(IncludeTrailingPathDelimiter(AFilepath), 'stco.def'));
    MakeMP4AtomClass(IncDefs, TypeDefs, CodeDefs, TPath.Combine(IncludeTrailingPathDelimiter(AFilepath), 'co64.def'));
    MakeMP4AtomClass(IncDefs, TypeDefs, CodeDefs, TPath.Combine(IncludeTrailingPathDelimiter(AFilepath), 'stts.def'));
    MakeMP4AtomClass(IncDefs, TypeDefs, CodeDefs, TPath.Combine(IncludeTrailingPathDelimiter(AFilepath), 'stsc.def'));
    MakeMP4AtomClass(IncDefs, TypeDefs, CodeDefs, TPath.Combine(IncludeTrailingPathDelimiter(AFilepath), 'stsz.def'));
  finally
    FinalCode := StartOfFile + TypeDefs + ImpDef + CodeDefs + EndOfFile;
    FinalInc := IncDefs;

    OutputData(FinalCode, FinalInc);
  end;

end;

procedure TForm2.MakeMP4AtomClass(var IncDefs: String; var TypeDefs: String; var CodeDefs: String; const AFilename: String);
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

  IncDefs := IncDefs + '    $' + IntToHex(cc4, 4) + ': // ' + SAtom + SLineBreak +
                       '      Result := ' + SLines[0] + '.Create(FStream, AtomRec);' + SLineBreak;

  TypeDefs := TypeDefs + '  ' + SLines[0] + ' = ' + SLines[1] + ' { $' + IntToHex(cc4, 4) + ': // ' + SAtom + ' }' + SLineBreak +
              '  ' + Slines[2] + SLineBreak +
              '  strict private' + SLineBreak;



  for I := 1 to Triplets - 1 do
    begin
      if not(SLines[(I * 3)].StartsWith('Reserved')) then
        TypeDefs := TypeDefs + '    F' + SLines[(I * 3)] + ': ' + SLines[(I * 3) + 1] + ';' + SLineBreak
      else
        TypeDefs := TypeDefs + '    F' + SLines[(I * 3)] + ': TBytes;' + SLineBreak;
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
  TypeDefs := TypeDefs + '  end;' + SLineBreak + SLineBreak;

  CodeDefs := CodeDefs + '{ ' + SLines[0] + ' }' + SLineBreak + SLineBreak;
  CodeDefs := CodeDefs + 'procedure ' + SLines[0] + '.ReadFromStream(var BufPos: Int64; var AStream: TStream; const BufSize: Int64);' + SLineBreak;
  CodeDefs := CodeDefs + 'begin' + SLineBreak;

  for I := 1 to Triplets - 1 do
    begin
    {
      if SLines[(I * 3)].StartsWith('Reserved') then
        CodeDefs := CodeDefs + '  ' + CreateDataCode(SLines[(I * 3)], SLines[(I * 3) + 1]) + ';' + SLineBreak
      else
    }
        CodeDefs := CodeDefs + '  F' + SLines[(I * 3)] + ' := ' + CreateDataCode(SLines[(I * 3)], SLines[(I * 3) + 1]) + ';' + SLineBreak;
    end;

  CodeDefs := CodeDefs + 'end;' + SLineBreak + SLineBreak;
end;

function TForm2.CreateDataCode(const AVarName: String; const AVarType: String): String;
begin
  if AVarName.StartsWith('Reserved') then
    begin
      if AVarType.StartsWith('Array[0..') then
        begin
          Result := 'ReadReserved(BufPos, AStream, ' + GetSkipSize(AVarType) + ')';
        end
      else
        Raise Exception.Create('ReadReserved parse error');
    end
  else if AVarType = 'TDateTime' then
    Result := 'ReadMediaDateTime(BufPos, AStream)'
  else if AVarType = 'UInt64' then
    Result := 'ReadUInt64(BufPos, AStream)'
  else if AVarType = 'UInt32' then
    Result := 'ReadUInt32(BufPos, AStream)'
  else if AVarType = 'UInt16' then
    Result := 'ReadUInt16(BufPos, AStream)'
  else if AVarType = 'Int64' then
    Result := 'ReadInt64(BufPos, AStream)'
  else if AVarType = 'Int32' then
    Result := 'ReadInt32(BufPos, AStream)'
  else if AVarType = 'Int16' then
    Result := 'ReadInt16(BufPos, AStream)'
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

  else if AVarType = 'TMP4FourCCArray' then
    Result := 'ReadFourCCArray(BufPos, AStream, (BufSize - BufPos) div SizeOf(TMP4FourCC))'

  else if AVarType = 'TMP4ChapterDataList' then
    Result := 'ReadChapterDataList(BufPos, AStream, BufSize)'
  else if AVarType = 'TMP4MetaDataList' then
    Result := 'ReadMetaDataList(BufPos, AStream, BufSize)'
  else if AVarType = 'TMP4EditDataList' then
    Result := 'ReadEditDataList(BufPos, AStream, BufSize)'
  else if AVarType = 'TMP4SampleDescDataList' then
    Result := 'ReadSampleDescDataList(BufPos, AStream, BufSize)'

  else if AVarType = 'TMP4HDLRString' then
    Result := 'ReadHDLR(BufPos, AStream, BufSize, FHandlerType)'
  else if AVarType = 'TUInt32Array' then
    Result := 'ReadUInt32Array(BufPos, AStream, FEntryCount)'
  else if AVarType = 'TUInt64Array' then
    Result := 'ReadUInt64Array(BufPos, AStream, FEntryCount)'
  else if AVarType = 'TSttsArray' then
    Result := 'ReadSttsArray(BufPos, AStream, FEntryCount)'
  else if AVarType = 'TStscArray' then
    Result := 'ReadStscArray(BufPos, AStream, FEntryCount)'

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

function TForm2.MakeAutoAtomHead: String;
begin
  Result := 'unit MP4AutoAtom;' + SLineBreak;
  Result := Result + '' + SLineBreak;
  Result := Result + 'interface' + SLineBreak;
  Result := Result + '' + SLineBreak;
  Result := Result + 'uses' + SLineBreak;
  Result := Result + '  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,' + SLineBreak;
  Result := Result + '  Generics.Defaults, Generics.Collections, MP4Atoms, MP4DerivedAtoms, MP4Types;' + SLineBreak;
  Result := Result + '' + SLineBreak;
  Result := Result + 'function CreateAutoAtom(FStream: TStream; const AtomRec: TAtomRec): TAtom;' + SLineBreak;
  Result := Result + '' + SLineBreak;
  Result := Result + 'implementation' + SLineBreak;
  Result := Result + '' + SLineBreak;
  Result := Result + 'function CreateAutoAtom(FStream: TStream; const AtomRec: TAtomRec): TAtom;' + SLineBreak;
  Result := Result + 'begin' + SLineBreak;
  Result := Result + '  case AtomRec.FourCC of' + SLineBreak;
  Result := Result + '    { Binary Blob Atom }' + SLineBreak;
  Result := Result + '    $6D646174: // mdat' + SLineBreak;
  Result := Result + '      Result := TAtomOpaqueData.Create(AtomRec);' + SLineBreak;
  Result := Result + '' + SLineBreak;
  Result := Result + '    { Empty Atoms }' + SLineBreak;
  Result := Result + '    $66726565: // free' + SLineBreak;
  Result := Result + '      Result := TAtomFree.Create(AtomRec);' + SLineBreak;
  Result := Result + '    $736B6970: // skip' + SLineBreak;
  Result := Result + '      Result := TAtomSkip.Create(AtomRec);' + SLineBreak;
  Result := Result + '    $77696465: // wide' + SLineBreak;
  Result := Result + '      Result := TAtomWide.Create(AtomRec);' + SLineBreak;
  Result := Result + '' + SLineBreak;
  Result := Result + '    { Container Atoms }' + SLineBreak;
  Result := Result + '    $64696E66: // dinf' + SLineBreak;
  Result := Result + '      Result := TAtomContainer.Create(AtomRec);' + SLineBreak;
  Result := Result + '    $65647473: // edts' + SLineBreak;
  Result := Result + '      Result := TAtomContainer.Create(AtomRec);' + SLineBreak;
  Result := Result + '    $6D646961: // mdia' + SLineBreak;
  Result := Result + '      Result := TAtomContainer.Create(AtomRec);' + SLineBreak;
  Result := Result + '    $6D696E66: // minf' + SLineBreak;
  Result := Result + '      Result := TAtomContainer.Create(AtomRec);' + SLineBreak;
  Result := Result + '    $6D6F6F76: // moov' + SLineBreak;
  Result := Result + '      Result := TAtomContainer.Create(AtomRec);' + SLineBreak;
  Result := Result + '    $7374626C: // stbl' + SLineBreak;
  Result := Result + '      Result := TAtomContainer.Create(AtomRec);' + SLineBreak;
  Result := Result + '    $7472616B: // trak' + SLineBreak;
  Result := Result + '      Result := TAtomContainer.Create(AtomRec);' + SLineBreak;
  Result := Result + '    $74726566: // tref' + SLineBreak;
  Result := Result + '      Result := TAtomContainer.Create(AtomRec);' + SLineBreak;
  Result := Result + '    $75647461: // udta' + SLineBreak;
  Result := Result + '      Result := TAtomContainer.Create(AtomRec);' + SLineBreak;
  Result := Result + '' + SLineBreak;
  Result := Result + '    { Full Container Atoms }' + SLineBreak;
  Result := Result + '    $6D657461: // meta' + SLineBreak;
  Result := Result + '      Result := TAtomMeta.Create(FStream, AtomRec);' + SLineBreak;
  Result := Result + '' + SLineBreak;
  Result := Result + '    { AutoGenerated Data Atoms }' + SLineBreak;
end;

function TForm2.MakeAutoAtomTail: String;
begin
  Result := '  else' + SLineBreak;
  Result := Result + '    Result := Nil;' + SLineBreak;
  Result := Result + '  end;' + SLineBreak;
  Result := Result + 'end;' + SLineBreak;
  Result := Result + '' + SLineBreak;
  Result := Result + 'end.' + SLineBreak;
  Result := Result + '';end;

end.
