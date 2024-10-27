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

  TMP4MetaData = class
  strict private
    FFourCC: TMP4FourCC;
    FDataType:  UInt32;
    FLocale: UInt32;
    FData: TBytes;
  public
  published
    property FourCC: TMP4FourCC read FFourCC write FFourCC;
    property DataType: UInt32 read FDataType write FDataType;
    property Locale: UInt32 read FLocale write FLocale;
    property Data: TBytes read FData write FData;
  end;
  TMP4MetaDataList = TObjectList<TMP4MetaData>;

  TMP4ChapterData = class
  strict private
    FTimestamp: UInt64;
    FChapNameLen: Byte;
    FChapName: TBytes;
  public
  published
    property Timestamp: UInt64 read FTimestamp write FTimestamp;
    property ChapNameLen: Byte read FChapNameLen write FChapNameLen;
    property ChapName: TBytes read FChapName write FChapName;
  end;
  TMP4ChapterDataList = TObjectList<TMP4ChapterData>;

const
  MediaZeroDayTime: UInt32 = 1462;
  { Midnight 1st Jan 1904 for MP4 is 0 so adjust fot TDateTime }
  SecondsInDay: UInt32 = 86400;
  { Need to calc dates... }

function SwapBytes64(const aVal: UInt64): UInt64; inline;
function SwapBytes32(Value: UInt32): UInt32; inline;
function SwapBytes16(Value: UInt16): UInt16; inline;
function TFourCCToStr(a: TFourCC): String;
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

function SwapBytes64(const aVal: UInt64): UInt64; inline;
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
  Result := ((Value and $FF00) SHR 8) OR ((Value and $00FF) SHL 8);
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

function TFourCCToStr(a: TFourCC): String;
begin
  Result := Chr(a[0])+Chr(a[1])+Chr(a[2])+Chr(a[3]);
end;

end.
