unit MP4Types;

{$M+}

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  Generics.Defaults, Generics.Collections;

type
  TFourCC = Array [0..3] of Byte;
  TFourCCString = String[4];
  TBuffer = TBytes;

  TMP4FourCC = type Cardinal;
  { A standard 4CC }
  TMP4Flags = Array[0..2] of Byte;
  TMP4Fixed32 = Int32;
  { Fixed point number 16:16 }
  TMP4Fixed16 = Int16;
  { Fixed point number 8:8 }
  TMP4Matrix3x3 = Array of TMP4Fixed32;
  { 3x3 Matrix }
  TMP4FourCCArray = Array of TMP4FourCC;
  { Array with redundant fields }
  TMP4HDLRString = String;
  { String to hold messy HDLR atom }
  TUInt64Array = Array of UInt64;
  TUInt32Array = Array of UInt32;

  TSttsRec = record
    Count: Int32;
    Duration: Int32;
  end;
  TSttsArray = Array of TSttsRec;

  TStscRec = record
    FirstChunk: Int32;
    SamplesPerChunk: Int32;
    SampleDescID: Int32;
  end;
  TStscArray = Array of TStscRec;

const
  MediaZeroDayTime: UInt32 = 1462;
  { Midnight 1st Jan 1904 for MP4 is 0 so adjust fot TDateTime }
  SecondsInDay: UInt32 = 86400;
  { Need to calc dates... }

function SwapBytes64(const Value: UInt64): UInt64; inline; overload;
function SwapBytes64(const Value: Int64): Int64; inline; overload;
function SwapBytes32(Value: UInt32): UInt32; inline; overload;
function SwapBytes32(Value: Int32): Int32; inline; overload;
function SwapBytes16(Value: UInt16): UInt16; inline; overload;
function SwapBytes16(Value: Int16): Int16; inline; overload;
function FourCCToString(const AFourCC: TMP4FourCC): String;
function IsValidFourCC(const AFourCC: TMP4FourCC): Boolean;
function StringToFourCC(const AStr: String): TMP4FourCC;

implementation

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

function SwapBytes64(const Value: Int64): Int64;
begin
  Result := Int64(SwapBytes64(Uint64(Value)));
end;

function SwapBytes64(const Value: UInt64): UInt64;
begin
  Result := ((Value AND $FF00000000000000) SHR 56) OR
            ((Value AND $00FF000000000000) SHR 40) OR
            ((Value AND $0000FF0000000000) SHR 24) OR
            ((Value AND $000000FF00000000) SHR  8) OR
            ((Value AND $00000000FF000000) SHL  8) OR
            ((Value AND $0000000000FF0000) SHL 24) OR
            ((Value AND $000000000000FF00) SHL 40) OR
            ((Value AND $00000000000000FF) SHL 56);
end;

function SwapBytes32(Value: Int32): Int32;
begin
  Result := Int32(SwapBytes32(Uint32(Value)));
end;

function SwapBytes32(Value: UInt32): UInt32;
begin
  Result := ((Value AND $FF000000) SHR 24) OR
            ((Value AND $00FF0000) SHR 8) OR
            ((Value AND $0000FF00) SHL 8) OR
            ((Value AND $000000FF) SHL 24);
end;

function SwapBytes16(Value: Int16): Int16;
begin
  Result := Int16(SwapBytes16(Uint16(Value)));
end;

function SwapBytes16(Value: UInt16): UInt16;
begin
  Result := ((Value and $FF00) SHR 8) OR
            ((Value and $00FF) SHL 8);
end;

function IsValidFourCC(const AFourCC: TMP4FourCC): Boolean;
  function IsValid(const AByte: Byte): Boolean;
  begin
    Result := False;
    if (AByte >= $20) and (AByte <= $7E) then
      Result := True;
  end;
begin
  Result := IsValid(Byte((AFourCC and $FF000000) shr 24)) AND
            IsValid(Byte((AFourCC and $00FF0000) shr 16)) AND
            IsValid(Byte((AFourCC and $0000FF00) shr 8)) AND
            IsValid(Byte(AFourCC and $000000FF));
end;


end.
