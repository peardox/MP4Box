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
    function ReadSingle32(var BufPos: Int64; var AStream: TStream): Single;
    function ReadSingle16(var BufPos: Int64; var AStream: TStream): Single;
    function ReadMediaDateTime(var BufPos: Int64; var AStream: TStream): TDateTime;
    function ReadFourCC(var BufPos: Int64; var AStream: TStream): TMP4FourCC;
    function ReadInt64(var BufPos: Int64; var AStream: TStream): Int64;
    function ReadInt32(var BufPos: Int64; var AStream: TStream): Int32;
    function ReadInt16(var BufPos: Int64; var AStream: TStream): Int16;
    function ReadUInt64(var BufPos: Int64; var AStream: TStream): UInt64;
    function ReadUInt32(var BufPos: Int64; var AStream: TStream): UInt32;
    function ReadUInt16(var BufPos: Int64; var AStream: TStream): UInt16;
    function ReadByte(var BufPos: Int64; var AStream: TStream): Byte;
    function ReadTBytes(var BufPos: Int64; var AStream: TStream; const ASize: Int32): TBytes;
    function ReadUInt64Array(var BufPos: Int64; var AStream: TStream; const AEntryCount: Int32): TUInt64Array;
    function ReadUInt32Array(var BufPos: Int64; var AStream: TStream; const AEntryCount: Int32): TUInt32Array;
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

function TMP4FileAtom.ReadInt16(var BufPos: Int64; var AStream: TStream): Int16;
begin
  Result := Int16(ReadUInt16(BufPos, AStream));
end;

function TMP4FileAtom.ReadInt32(var BufPos: Int64; var AStream: TStream): Int32;
begin
  Result := Int32(ReadUInt32(BufPos, AStream));
end;

function TMP4FileAtom.ReadInt64(var BufPos: Int64; var AStream: TStream): Int64;
begin
  Result := Int64(ReadUInt64(BufPos, AStream));
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

function TMP4FileAtom.ReadUInt64Array(var BufPos: Int64; var AStream: TStream;
  const AEntryCount: Int32): TUInt64Array;
var
  I: Int32;
begin
  if AStream.Position > (AStream.Size + (SizeOf(UInt64) * AEntryCount)) then
    Raise Exception.Create('Buffer too small');

  SetLength(Result, AEntryCount);

  for I := 0 to AEntryCount - 1 do
    begin
      AStream.Read(Result[I], SizeOf(UInt64));
      Result[I] := SwapBytes64(Result[I]);
    end;

  BufPos := BufPos+(SizeOf(UInt64) * AEntryCount);
end;

function TMP4FileAtom.ReadUInt32Array(var BufPos: Int64; var AStream: TStream;
  const AEntryCount: Int32): TUInt32Array;
var
  I: Int32;
begin
  if AStream.Position > (AStream.Size + (SizeOf(UInt32) * AEntryCount)) then
    Raise Exception.Create('Buffer too small');

  SetLength(Result, AEntryCount);

  for I := 0 to AEntryCount - 1 do
    begin
      AStream.Read(Result[I], SizeOf(UInt32));
      Result[I] := SwapBytes32(Result[I]);
    end;

  BufPos := BufPos+(SizeOf(UInt32) * AEntryCount);
end;

function TMP4FileAtom.ReadSingle16(var BufPos: Int64;
  var AStream: TStream): Single;
var
  AFixed16: UInt16;
begin
  if AStream.Position > (AStream.Size + 2) then
    Raise Exception.Create('Buffer too small');

  AFixed16 := ReadUInt16(BufPos, AStream);
  Result := AFixed16 / $100;
end;

function TMP4FileAtom.ReadSingle32(var BufPos: Int64;
  var AStream: TStream): Single;
var
  AFixed32: UInt32;
begin
  if AStream.Position > (AStream.Size + 4) then
    Raise Exception.Create('Buffer too small');

  AFixed32 := ReadUInt32(BufPos, AStream);
  Result := AFixed32 / $10000;
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
