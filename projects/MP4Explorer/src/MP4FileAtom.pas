unit MP4FileAtom;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  Generics.Defaults, Generics.Collections, MP4Types;
type

  TMP4FileAtom = class
  strict private
  protected
    function UIntToMediaTime(const ATime: UInt32): TDateTime;
//    function ReadFourCCArray(var BufPos: Int64; var AStream: TStream; const ASize: Int32): TMP4FourCCArray;
    function ReadSingle(var BufPos: Int64; var AStream: TStream): Single;
    function ReadMediaDateTime(var BufPos: Int64; var AStream: TStream): TDateTime;
    function ReadFourCC(var BufPos: Int64; var AStream: TStream): TMP4FourCC;
    function ReadUInt64(var BufPos: Int64; var AStream: TStream): UInt64;
    function ReadUInt32(var BufPos: Int64; var AStream: TStream): UInt32;
    function ReadUInt16(var BufPos: Int64; var AStream: TStream): UInt16;
    function ReadByte(var BufPos: Int64; var AStream: TStream): Byte;
    function ReadTBytes(var BufPos: Int64; var AStream: TStream; const ASize: Int32): TBytes;
  end;

implementation

function TMP4FileAtom.UIntToMediaTime(const ATime: UInt32): TDateTime;
var
  Secs: UInt32;
  Days: UInt32;
  DateFloat: Double;
begin
  Days := ATime div SecondsInDay;
  Secs := ATime - (Days * SecondsInDay);
  DateFloat := MediaZeroDayTime + Days + (Secs / SecondsInDay);
  Result := TDateTime(DateFloat);
end;

function TMP4FileAtom.ReadFourCC(var BufPos: Int64;
  var AStream: TStream): TMP4FourCC;
begin
  if AStream.Position > (AStream.Size + 4) then
    Raise Exception.Create('Buffer too small');

  AStream.Read(Result, SizeOf(Result));
  Result := SwapBytes32(Result);
  BufPos := BufPos+4;
end;

function TMP4FileAtom.ReadUInt32(var BufPos: Int64;
  var AStream: TStream): UInt32;
begin
  if AStream.Position > (AStream.Size + 4) then
    Raise Exception.Create('Buffer too small');

  AStream.Read(Result, SizeOf(Result));
  Result := SwapBytes32(Result);
  BufPos := BufPos+4;
end;

function TMP4FileAtom.ReadUInt64(var BufPos: Int64;
  var AStream: TStream): UInt64;
begin
  if AStream.Position > (AStream.Size + SizeOf(UInt64)) then
    Raise Exception.Create('Buffer too small');

  AStream.Read(Result, SizeOf(Result));
  Result := SwapBytes64(Result);
  BufPos := BufPos+SizeOf(UInt64);
end;

function TMP4FileAtom.ReadSingle(var BufPos: Int64;
  var AStream: TStream): Single;
begin
  if AStream.Position > (AStream.Size + 4) then
    Raise Exception.Create('Buffer too small');

  AStream.Read(Result, SizeOf(Result));
  BufPos := BufPos+4;
end;

function TMP4FileAtom.ReadTBytes(var BufPos: Int64; var AStream: TStream;
  const ASize: Int32): TBytes;
begin
  if AStream.Position > (AStream.Size + ASize) then
    Raise Exception.Create('Buffer too small');

  SetLength(Result, ASize);
  AStream.Read(Result, ASize);
  BufPos := BufPos + ASize;
end;

function TMP4FileAtom.ReadByte(var BufPos: Int64; var AStream: TStream): Byte;
begin
  if AStream.Position > (AStream.Size + SizeOf(Byte)) then
    Raise Exception.Create('Buffer too small');

  AStream.Read(Result, SizeOf(Result));
  BufPos := BufPos+SizeOf(Byte);
end;

function TMP4FileAtom.ReadMediaDateTime(var BufPos: Int64;
  var AStream: TStream): TDateTime;
var
  T: UInt32;
begin
  if AStream.Position > (AStream.Size + 4) then
    Raise Exception.Create('Buffer too small');

  AStream.Read(T, SizeOf(T));
  Result := UIntToMediaTime(SwapBytes32(T));
  BufPos := BufPos+4;
end;

function TMP4FileAtom.ReadUInt16(var BufPos: Int64;
  var AStream: TStream): UInt16;
begin
  if AStream.Position > (AStream.Size + 2) then
    Raise Exception.Create('Buffer too small');

  AStream.Read(Result, SizeOf(Result));
  Result := SwapBytes16(Result);
  BufPos := BufPos+2;
end;


end.
