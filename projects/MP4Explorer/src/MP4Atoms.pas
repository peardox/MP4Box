unit MP4Atoms;

{$M+}

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  Generics.Defaults, Generics.Collections, MP4Types, MP4ExtendedTypes, MP4SampleTypes, MP4FileAtom;

type
  TAtom = class;

  TAtomRec = record
    Parent: TAtom;
    AbsPos: Int64;
    FourCC: TMP4FourCC;
    Size: Int64;
    Is64Bit: Boolean;
    Level: Integer;
  end;


  TAtomObjectList = TObjectList<TAtom>;

  TAtom = class(TMP4FileAtom)
  strict private
    FParent: TAtom;
    FAbsPos: Int64;
    FFourCC: TMP4FourCC;
    FSize: Int64;
    FLevel: Integer;
    FIs64Bit: Boolean;
    FCanShowProps: Boolean;
    FIsHandled: Boolean;
    FChildren: TAtomObjectList;
  protected
  public
    constructor Create; overload;
    constructor Create(const ARec: TAtomRec); overload; virtual;
    constructor Create(const AAbsPos: Int64; const AFourCC: TMP4FourCC; const ASize: Int64; const AIs64Bit: Boolean; const ALevel: Integer; const AParent: TAtom); overload;
    property Parent: TAtom read FParent;
    property Is64Bit: Boolean read FIs64Bit;
    property CanShowProps: Boolean read FCanShowProps write FCanShowProps;
    property IsHandled: Boolean read FIsHandled write FIsHandled;
    property Level: Integer read FLevel;
    property Children: TAtomObjectList read FChildren write FChildren;
  published
    property FourCC: TMP4FourCC read FFourCC;
    property AbsPos: Int64 read FAbsPos;
    property Size: Int64 read FSize;
  end;

  TAtomAbstractData = class(TAtom)
  strict private
  protected
    function ReadMatrix3x3(var BufPos: Int64; var AStream: TStream): TMP4Matrix3x3;
    function ReadFourCCArray(var BufPos: Int64; var AStream: TStream; const ASize: Int32): TMP4FourCCArray;
    function ReadMetaDataList(var BufPos: Int64; var AStream: TStream; const ASize: Int32): TMP4MetaDataList;
    function ReadChapterDataList(var BufPos: Int64; var AStream: TStream; const ASize: Int32): TMP4ChapterDataList;
    function ReadEditDataList(var BufPos: Int64; var AStream: TStream; const ASize: Int32): TMP4EditDataList;
    function ReadReserved(var BufPos: Int64; var AStream: TStream; const ASize: Int32): TBytes;
    function ReadSampleDescDataList(var BufPos: Int64; var AStream: TStream; const ASize: Int32): TMP4SampleDescDataList;
    function ReadHDLR(var BufPos: Int64; var AStream: TStream; const ASize: Int32; const FCC: TMP4FourCC): String;
    function ReadSttsArray(var BufPos: Int64; var AStream: TStream; const AEntryCount: Int32): TSttsArray;
    procedure ReadSkip(var BufPos: Int64; var AStream: TStream; const ASize: Int32);

  public
    constructor Create(const ARec: TAtomRec); override;
  end;

  TAtomLite = class(TAtomAbstractData)
  public
    constructor Create(const ARec: TAtomRec); override;
  end;

  TAtomLiteData = class(TAtomLite)
  strict private
  public
    constructor Create(var AStream: TStream; const ARec: TAtomRec); overload; virtual;
    procedure PopulateFromStream(var AStream: TStream; const ASize: Int64);
    procedure ReadFromStream(var BufPos: Int64; var AStream: TStream; const BufSize: Int64); virtual; abstract;
  end;

  TAtomFull = class(TAtomLite)
  strict private
    FVersion: Byte;
    { 1 byte - Version - Always 0 }
    FFlags: TMP4Flags;
    { 3 bytes - Flags - Always 0 }
  public
    constructor Create(var AStream: TStream; const ARec: TAtomRec); overload; virtual;
  end;

  TAtomFullData = class(TAtomFull)
  strict private
  public
    constructor Create(var AStream: TStream; const ARec: TAtomRec); override;
    procedure PopulateFromStream(var AStream: TStream; const ASize: Int64);
    procedure ReadFromStream(var BufPos: Int64; var AStream: TStream; const BufSize: Int64); virtual; abstract;
  end;

  TAtomFree = class(TAtomLite);
  TAtomSkip = class(TAtomLite);
  TAtomWide = class(TAtomLite);
  TAtomOpaqueData = class(TAtomLite);
  TAtomContainer = class(TAtomLite);
  TAtomMeta = class(TAtomFull);

implementation

{ TAtom }

constructor TAtom.Create;
begin
  Inherited;
end;

constructor TAtom.Create(const AAbsPos: Int64; const AFourCC: TMP4FourCC; const ASize: Int64; const AIs64Bit: Boolean; const ALevel: Integer; const AParent: TAtom);
begin
  Create;
  FAbsPos := AAbsPos;
  FFourCC := AFourCC;
  FSize := ASize;
  FIs64Bit := AIs64Bit;
  FLevel := ALevel;
  FParent := AParent;
end;

constructor TAtom.Create(const ARec: TAtomRec);
begin
  Create;
  FAbsPos := ARec.AbsPos;
  FFourCC := ARec.FourCC;
  FSize := ARec.Size;
  FIs64Bit := ARec.Is64Bit;
  FLevel := ARec.Level;
  FParent := ARec.Parent;
end;

{ TAtomAbstractData }


function TAtomAbstractData.ReadFourCCArray(var BufPos: Int64;
  var AStream: TStream;  const ASize: Int32): TMP4FourCCArray;
var
  FCC: TMP4FourCC;
  I, C: Int32;
begin
  if AStream.Position > (AStream.Size + 4) then
    Raise Exception.Create('Buffer too small');

  SetLength(Result, ASize);

  C := 0;

  for I := 0 to ASize - 1 do
    begin
      AStream.Read(FCC, SizeOf(TMP4FourCC));
      if FCC <> 0 then
        begin
          Result[I] := FCC;
          Inc(C);
        end;
    end;

  SetLength(Result, C);

  BufPos := BufPos + (ASize * SizeOf(TMP4FourCC));
end;

function TAtomAbstractData.ReadHDLR(var BufPos: Int64; var AStream: TStream;
  const ASize: Int32; const FCC: TMP4FourCC): String;
var
  CheckFCC: TMP4FourCC;
  StrLen: Byte;
  PStr: TBytes;
begin
  { This one is very badly behaved }
  if (FCC = $6D646972 { mdir }) then
    begin
      { Seen in Meta }
      CheckFCC := ReadFourCC(BufPos, AStream);
      if (CheckFCC <> $6170706C { appl }) and (CheckFCC <> $00000000 { <NULL> }) then
        Raise Exception.Create('Invalid looking hdlr atom for mdir');
      ReadSkip(BufPos, AStream, 9);
      Result := '';
    end
  else
    begin
      ReadSkip(BufPos, AStream, 12);
      StrLen := ReadByte(BufPos, AStream);
      { String may be Pascal ShortString or C Zero Terminated so read first byte and test }
      { Regardless strings are always zero padded - even Pascal ShortStings }
      if StrLen = (ASize-BufPos-1) then
        begin
          { if (FCC = $736F756E) }
          SetLength(PStr, (ASize-BufPos));
          PStr := ReadTBytes(BufPos, AStream,(ASize-BufPos));
          SetString(Result, PAnsiChar(PStr), Length(PStr));
          { Assign Pascal ShortString }
          SetLength(PStr, 0);
          { Free the temp string }
        end
      else
        begin
          { if (FCC = $736F756E) }
          SetLength(PStr, ASize-BufPos);
          PStr := ReadTBytes(BufPos, AStream,ASize-BufPos);
          SetString(Result, PAnsiChar(PStr), Length(PStr));
          { Assign C-String remainder }
          Result := Chr(StrLen) + Result;
          { Tag on first char we tested for Pascal-string-ness }
          SetLength(PStr, 0);
          { Free the temp string }
        end;
    end;

end;

function TAtomAbstractData.ReadMetaDataList(var BufPos: Int64;
  var AStream: TStream; const ASize: Int32): TMP4MetaDataList;

  function ReadMetaData(var BufPos: Int64; var AStream: TStream): TMP4MetaData;
  var
    AItem: TMP4MetaData;
    DataFCC: TMP4FourCC;
    DataSize: UInt32;
  begin
    AItem := TMP4MetaData.Create;
    AItem.MetaSize := ReadUInt32(BufPos, AStream);
    // ReadSkip(BufPos, AStream, 4);
    { Just wanna skip this one }
    AItem.FourCC := ReadFourCC(BufPos, AStream);
    DataSize := ReadUInt32(BufPos, AStream);
    DataFCC := ReadFourCC(BufPos, AStream);
    if DataFCC = $64617461 { 'data' } then
      begin
        AItem.DataType := ReadUInt32(BufPos, AStream);
        AItem.Locale := ReadUInt32(BufPos, AStream);
        AItem.Data := ReadTBytes(BufPos, AStream, DataSize - 16);
      end
    else
      Raise Exception.Create('Metadata "data" atom not found');

    Result := AItem;
  end;

var
  AItem: TMP4MetaData;
  AList: TMP4MetaDataList;
begin
  AList := TMP4MetaDataList.Create(True);
  while BufPos < ASize do
    begin
      AItem := ReadMetaData(BufPos, AStream);
      if AItem <> Nil then
        AList.Add(AItem);
    end;
  if BufPos > ASize then
    Raise Exception.Create('MetaData OverRead for ' + ClassName + ' : Atom = ' + FourCCToString(FourCC));

  Result := AList;
end;

constructor TAtomAbstractData.Create(const ARec: TAtomRec);
begin
  inherited;
end;

function TAtomAbstractData.ReadChapterDataList(var BufPos: Int64;
  var AStream: TStream; const ASize: Int32): TMP4ChapterDataList;

  function ReadChapterData(var BufPos: Int64; var AStream: TStream): TMP4ChapterData;
  var
    AItem: TMP4ChapterData;
  begin
    AItem := TMP4ChapterData.Create;
    AItem.Timestamp := ReadUInt64(BufPos, AStream);
    AItem.ChapNameLen := ReadByte(BufPos, AStream);
    AItem.ChapName := ReadTBytes(BufPos, AStream, AItem.ChapNameLen);

    Result := AItem;
  end;

var
  AItem: TMP4ChapterData;
  AList: TMP4ChapterDataList;
begin
  AList := TMP4ChapterDataList.Create(True);
  while BufPos < ASize do
    begin
      AItem := ReadChapterData(BufPos, AStream);
      if AItem <> Nil then
        AList.Add(AItem);
    end;
  if BufPos > ASize then
    Raise Exception.Create('MetaData OverRead for ' + ClassName + ' : Atom = ' + FourCCToString(FourCC));

  Result := AList;
end;

function TAtomAbstractData.ReadEditDataList(var BufPos: Int64;
  var AStream: TStream; const ASize: Int32): TMP4EditDataList;

  function ReadEditData(var BufPos: Int64; var AStream: TStream): TMP4EditData;
  var
    AItem: TMP4EditData;
  begin
    AItem := TMP4EditData.Create;
    AItem.TrackDuration := ReadUInt32(BufPos, AStream);
    AItem.MediaTime := ReadUInt32(BufPos, AStream);
    AItem.MediaRate := ReadUInt32(BufPos, AStream);

    Result := AItem;
  end;

var
  AItem: TMP4EditData;
  AList: TMP4EditDataList;
begin
  AList := TMP4EditDataList.Create(True);
  while BufPos < ASize do
    begin
      AItem := ReadEditData(BufPos, AStream);
      if AItem <> Nil then
        AList.Add(AItem);
    end;
  if BufPos > ASize then
    Raise Exception.Create('MetaData OverRead for ' + ClassName + ' : Atom = ' + FourCCToString(FourCC));

  Result := AList;
end;

function TAtomAbstractData.ReadMatrix3x3(var BufPos: Int64;
  var AStream: TStream): TMP4Matrix3x3;
var
  Row, Col: Integer;
begin
  if AStream.Position > (AStream.Size + (4*9)) then
    Raise Exception.Create('Buffer too small');

  SetLength(Result, 9);
  for Row := 0 to 2 do
    for Col := 0 to 2 do
      Result[(Row*3) + Col] := ReadUInt32(BufPos, AStream);
end;

function TAtomAbstractData.ReadReserved(var BufPos: Int64; var AStream: TStream; const ASize: Int32): TBytes;
begin
  if AStream.Position > (AStream.Size + ASize) then
    Raise Exception.Create('Buffer too small');

  SetLength(Result, ASize);
  AStream.Read(Result, ASize);
  BufPos := BufPos + ASize;
end;

function TAtomAbstractData.ReadSampleDescDataList(var BufPos: Int64;
  var AStream: TStream; const ASize: Int32): TMP4SampleDescDataList;
var
  L: TMP4SampleDescDataList;
  D: TMP4SampleDescData;
begin
  L:= TMP4SampleDescDataList.Create(True);
  D := TMP4SampleDescData.Create;
  D.SampleDescSize := ReadUInt32(BufPos, AStream);
  D.FourCC := ReadFourCC(BufPos, AStream);
  D.Reserved := ReadReserved(BufPos, AStream, 6); // Six zero bytes
  D.DataRefIndex := ReadUInt16(BufPos, AStream);
  D.Data := TMP4SampleTypeAudio0.Create; // Variable Data
  D.Data.ReadFromStream(BufPos, AStream, ASize);
  L.Add(D);

  Result := L;
end;

procedure TAtomAbstractData.ReadSkip(var BufPos: Int64; var AStream: TStream;
  const ASize: Int32);
var
  T: TBytes;
begin
  if AStream.Position > (AStream.Size + ASize) then
    Raise Exception.Create('Buffer too small');

  SetLength(T, ASize);
  AStream.Read(T, ASize);
  SetLength(T, 0);
  BufPos := BufPos + ASize;
end;

function TAtomAbstractData.ReadSttsArray(var BufPos: Int64;
  var AStream: TStream; const AEntryCount: Int32): TSttsArray;
var
  I: Int32;
begin
  if AStream.Position > (AStream.Size + (SizeOf(TSttsRec) * AEntryCount)) then
    Raise Exception.Create('Buffer too small');

  SetLength(Result, AEntryCount);

  for I := 0 to AEntryCount - 1 do
    begin
      AStream.Read(Result[I].Count, SizeOf(Int32));
      Result[I].Count := SwapBytes32(Result[I].Count);
      AStream.Read(Result[I].Duration, SizeOf(Int32));
      Result[I].Duration := SwapBytes32(Result[I].Duration);
    end;

  BufPos := BufPos+(SizeOf(TSttsRec) * AEntryCount);
end;

{ TAtomLite }

constructor TAtomLite.Create(const ARec: TAtomRec);
begin
  inherited;
  IsHandled := True;
end;

{ TAtomLiteData }

constructor TAtomLiteData.Create(var AStream: TStream; const ARec: TAtomRec);
begin
  inherited Create(ARec);
  if ARec.Is64Bit then
    PopulateFromStream(AStream, ARec.Size - SizeOf(TMP4FourCC) - SizeOf(UInt32) - SizeOf(Int64))
    { PopulateStream needs size of atom - size of header }
  else
    PopulateFromStream(AStream, ARec.Size - SizeOf(TMP4FourCC) - SizeOf(UInt32));
    { PopulateStream needs size of atom - size of header }
end;

procedure TAtomLiteData.PopulateFromStream(var AStream: TStream; const ASize: Int64);
var
  BufPos: Int64;
begin
  BufPos := 0;

  ReadFromStream(BufPos, AStream, ASize);
  { Abstract Virtual method that fulls in descendant fields }

  if BufPos <  ASize then
    Raise Exception.Create('Data UnderRead for ' + ClassName);
  if BufPos >  ASize then
    Raise Exception.Create('Data OverRead for ' + ClassName);
end;

{ TAtomFull }

constructor TAtomFull.Create(var AStream: TStream; const ARec: TAtomRec);
begin
  inherited Create(ARec);

  if AStream.Position > (AStream.Size + 4) then
    Raise Exception.Create('Buffer too small');

  AStream.Read(FVersion, SizeOf(FVersion));
  AStream.Read(FFlags, SizeOf(FFlags));
end;

{ TAtomFullData }

constructor TAtomFullData.Create(var AStream: TStream; const ARec: TAtomRec);
begin
  inherited;
  if ARec.Is64Bit then
    PopulateFromStream(AStream, ARec.Size - SizeOf(TMP4FourCC) - SizeOf(UInt32) - SizeOf(Int64))
    { PopulateStream needs size of atom - size of header }
  else
    PopulateFromStream(AStream, ARec.Size - SizeOf(TMP4FourCC) - SizeOf(UInt32));
    { PopulateStream needs size of atom - size of header }
end;

procedure TAtomFullData.PopulateFromStream(var AStream: TStream; const ASize: Int64);
var
  BufPos: Int64;
begin
  BufPos := 4;

  ReadFromStream(BufPos, AStream, ASize);
  { Abstract Virtual method that fulls in descendant fields }

  if BufPos <  ASize then
    Raise Exception.Create('Data UnderRead for ' + ClassName);
  if BufPos >  ASize then
    Raise Exception.Create('Data OverRead for ' + ClassName);
end;

end.
