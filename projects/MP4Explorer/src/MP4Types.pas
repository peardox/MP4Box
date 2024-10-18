unit MP4Types;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  Generics.Defaults, Generics.Collections;

type
  TFourCC = Array [0..3] of Byte;
  TFourCCString = String[4];
  TBuffer = TBytes;

  TMP4FourCC = Cardinal;
  { A standard 4CC }
  TMP4Flags = Array[0..2] of Byte;
  TMP4Fixed32 = Int32;
  { Fixed point number 16:16 }
  TMP4Fixed16 = Int16;
  { Fixed point number 8:8 }
  TMP4Matrix = Array[0..2, 0..2] of TMP4Fixed32;
  { 3x3 Matrix }
  TMP4FourCCArray = Array of Cardinal;
  { Array with redundant fields }

const
  MediaZeroDayTime: UInt32 = 1462;
  { Midnight 1st Jan 1904 for MP4 is 0 so adjust fot TDateTime }
  SecondsInDay: UInt32 = 86400;
  { Need to calc dates... }

function IsContainer(const AValue: TMP4FourCC): Boolean;
function IsFullAtom(const AValue: TMP4FourCC): Boolean;
function IsLeaf(const AValue: TMP4FourCC): Boolean;
function IsKnownAtom(const AValue: TMP4FourCC): Boolean;
function SwapBytes64(const aVal: Int64): Int64; inline;
function SwapBytes32(Value: UInt32): UInt32; inline;
function SwapBytes16(Value: UInt16): UInt16; inline;
function TFourCCToStr(a: TFourCC): String;
function StringToFourCC(const AStr: String): TMP4FourCC;
function FourCCToString(const AFourCC: TMP4FourCC): String;

implementation

{ Temp really hacky bodges }
const
  ContainerAtoms: Array[0..8] of String = ('udta', 'mdia', 'edts', 'tref', 'trak', 'moov', 'ilst', 'minf', 'stbl');
  LeafAtoms: Array[0..3] of String = ('mdat', 'mvhd', 'tkhd', 'elts');
  FullAtoms: Array[0..0] of String = ('meta');
  KnownAtoms: Array[0..1] of String = ('ftyp', 'mvhd');

function MAKEFOURCC(ch0, ch1, ch2, ch3: AnsiChar): TMP4FourCC;
begin
  Result := LongWord(Ord(ch0)) or (LongWord(Ord(ch1)) shl 8) or (LongWord(Ord(ch2)) shl 16) or (LongWord(Ord(ch3)) shl 24);
end;

function FourCCToString(const AFourCC: TMP4FourCC): String;
begin
  Result := Chr(Byte((AFourCC and $FF000000) shr 24)) +
            Chr(Byte((AFourCC and $FF0000) shr 16)) +
            Chr(Byte((AFourCC and $FF00) shr 8)) +
            Chr(Byte(AFourCC and $FF));
end;

function StringToFourCC(const AStr: String): TMP4FourCC;
begin
  if Length(AStr) <> 4 then
    Raise Exception.Create('Fatal Error FourCC : ' + AStr + ' MUST be 4 chars');

  Result := MakeFourCC(AnsiChar(AStr[4]), AnsiChar(AStr[3]), AnsiChar(AStr[2]), AnsiChar(AStr[1]));
end;

function IsContainer(const AValue: TMP4FourCC): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Length(ContainerAtoms) - 1 do
    begin
      if StringToFourCC(ContainerAtoms[I]) = AValue then
        begin
          Result := True;
          Break;
        end;
    end;
end;

function IsFullAtom(const AValue: TMP4FourCC): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Length(FullAtoms) - 1 do
    begin
      if StringToFourCC(FullAtoms[I]) = AValue then
        begin
          Result := True;
          Break;
        end;
    end;
end;

function IsLeaf(const AValue: TMP4FourCC): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Length(LeafAtoms) - 1 do
    begin
      if StringToFourCC(LeafAtoms[I]) = AValue then
        begin
          Result := True;
          Break;
        end;
    end;
end;

function IsKnownAtom(const AValue: TMP4FourCC): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Length(KnownAtoms) - 1 do
    begin
      if StringToFourCC(KnownAtoms[I]) = AValue then
        begin
          Result := True;
          Break;
        end;
    end;
end;

function SwapBytes64(const aVal: Int64): Int64; inline;
begin
    Int64Rec(Result).Bytes[0] :=  Int64Rec(aVal).Bytes[7];
    Int64Rec(Result).Bytes[1] :=  Int64Rec(aVal).Bytes[6];
    Int64Rec(Result).Bytes[2] :=  Int64Rec(aVal).Bytes[5];
    Int64Rec(Result).Bytes[3] :=  Int64Rec(aVal).Bytes[4];
    Int64Rec(Result).Bytes[4] :=  Int64Rec(aVal).Bytes[3];
    Int64Rec(Result).Bytes[5] :=  Int64Rec(aVal).Bytes[2];
    Int64Rec(Result).Bytes[6] :=  Int64Rec(aVal).Bytes[1];
    Int64Rec(Result).Bytes[7] :=  Int64Rec(aVal).Bytes[0];
end;

function SwapBytes32(Value: UInt32): UInt32; inline;
begin
  Result := (Value SHR 24) OR (Value SHL 24) OR ((Value AND $00FF0000) SHR 8) OR ((Value AND $0000FF00) SHL 8);
end;

function SwapBytes16(Value: UInt16): UInt16; inline;
begin
  Result := (Value SHR 8) OR (Value SHL 8);
end;

function TFourCCToStr(a: TFourCC): String;
begin
  Result := Chr(a[0])+Chr(a[1])+Chr(a[2])+Chr(a[3]);
end;

end.
