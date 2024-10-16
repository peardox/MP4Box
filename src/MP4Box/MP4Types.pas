unit MP4Types;

interface


uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  Generics.Defaults, Generics.Collections;

type
  TFourCC = Array [0..3] of Byte;
  TFourCCString = String[4];
  TBuffer = TBytes;

const
  MediaZeroDayTime: UInt32 = 1462;  // Midnight 1st Jan 1904 for MP4 is 0 so adjust fot TDateTime
  SecondsInDay: UInt32 = 86400;     // Need to calc dates...

function IsContainer(const AValue: String): Boolean;
function IsFullAtom(const AValue: String): Boolean;
function IsLeaf(const AValue: String): Boolean;
function IsKnownAtom(const AValue: String): Boolean;
function SwapBytes64(const aVal: Int64): Int64; inline;
function SwapBytes32(Value: UInt32): UInt32; inline;
function SwapBytes16(Value: UInt16): UInt16; inline;
function TFourCCToStr(a: TFourCC): String;

implementation

{ Temp really hacky bodges }
const
  ContainerAtoms: Array[0..6] of String = ('udta', 'mdia', 'edts', 'tref', 'trak', 'moov', 'ilst');
  LeafAtoms: Array[0..4] of String = ('ftyp', 'mdat', 'mvhd', 'tkhd', 'elts');
  FullAtoms: Array[0..0] of String = ('meta');
  KnownAtoms: Array[0..0] of String = ('mvhd');

function IsContainer(const AValue: String): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Length(ContainerAtoms) - 1 do
    begin
      if ContainerAtoms[I] = AValue then
        begin
          Result := True;
          Break;
        end;
    end;
end;

function IsFullAtom(const AValue: String): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Length(FullAtoms) - 1 do
    begin
      if FullAtoms[I] = AValue then
        begin
          Result := True;
          Break;
        end;
    end;
end;

function IsLeaf(const AValue: String): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Length(LeafAtoms) - 1 do
    begin
      if LeafAtoms[I] = AValue then
        begin
          Result := True;
          Break;
        end;
    end;
end;

function IsKnownAtom(const AValue: String): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Length(KnownAtoms) - 1 do
    begin
      if KnownAtoms[I] = AValue then
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
