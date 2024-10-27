unit MP4Atoms;

{$M+}

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  Generics.Defaults, Generics.Collections, MP4Types;

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

  TAtom = class
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
    constructor Create(const ARec: TAtomRec); overload;
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
    function ReadSingle(var BufPos: Int64; var AStream: TStream): Single;
    function UIntToMediaTime(const ATime: UInt32): TDateTime;
    function ReadFourCCArray(var BufPos: Int64; var AStream: TStream; const ASize: Int32): TMP4FourCCArray;
    function ReadMediaDateTime(var BufPos: Int64; var AStream: TStream): TDateTime;
    function ReadFourCC(var BufPos: Int64; var AStream: TStream): TMP4FourCC;
    function ReadUInt64(var BufPos: Int64; var AStream: TStream): UInt64;
    function ReadUInt32(var BufPos: Int64; var AStream: TStream): UInt32;
    function ReadUInt16(var BufPos: Int64; var AStream: TStream): UInt16;
    function ReadByte(var BufPos: Int64; var AStream: TStream): Byte;
    function ReadTBytes(var BufPos: Int64; var AStream: TStream; const ASize: Int32): TBytes;
    function ReadMetaData(var BufPos: Int64; var AStream: TStream): TMP4MetaData;
    function ReadMetaDataList(var BufPos: Int64; var AStream: TStream; const ASize: Int32): TMP4MetaDataList;
    function ReadChapterData(var BufPos: Int64; var AStream: TStream): TMP4ChapterData;
    function ReadChapterDataList(var BufPos: Int64; var AStream: TStream; const ASize: Int32): TMP4ChapterDataList;
    procedure ReadSkip(var BufPos: Int64; var AStream: TStream; const ASize: Int32);
  public
    constructor Create(var AStream: TStream; const ARec: TAtomRec); overload;
    procedure PopulateFromStream(var AStream: TStream; const ASize: Int64); virtual; abstract;
    procedure ReadFromStream(var BufPos: Int64; var AStream: TStream; const BufSize: Int64); virtual; abstract;
  end;

  {
  TAtomTiny = class(TAtomAbstractData)
  public
    constructor Create(var AStream: TStream; const ARec: TAtomRec); overload;
  end;

  TAtomFull = class(TAtomAbstractData)
  public
    constructor Create(var AStream: TStream; const ARec: TAtomRec); overload;
  end;
  }

  TAtomTinyData = class(TAtomAbstractData)
  strict
  private
  public
    procedure PopulateFromStream(var AStream: TStream; const ASize: Int64); override;
    procedure ReadFromStream(var BufPos: Int64; var AStream: TStream; const BufSize: Int64); override;
  end;

  TAtomFullData = class(TAtomAbstractData)
  strict
  private
    FVersion: Byte;
    { 1 byte - Version - Always 0 }
    FFlags: TMP4Flags;
    { 3 bytes - Flags - Always 0 }
  public
    procedure PopulateFromStream(var AStream: TStream; const ASize: Int64); override;
  end;

  TAtomFree = class(TAtom);
  TAtomSkip = class(TAtom);
  TAtomWide = class(TAtom);

  TAtomOpaqueData = class(TAtom);
  TAtomContainer = class(TAtom);

  TAtomMeta = class(TAtomFullData)
  strict
  private
  public
    procedure ReadFromStream(var BufPos: Int64; var AStream: TStream; const BufSize: Int64); override;
  end;

  TAtomChpl = class(TAtomFullData)
  strict
  private
    FChapterCount: Byte;
    FList: TMP4ChapterDataList;
  public
    procedure ReadFromStream(var BufPos: Int64; var AStream: TStream; const BufSize: Int64); override;
  published
    property ChapterCount: Byte read FChapterCount write FChapterCount;
    property List: TMP4ChapterDataList read FList write FList;
  end;

  TAtomIlst = class(TAtomTinyData)
  strict private
    FList: TMP4MetaDataList;
  public
    procedure ReadFromStream(var BufPos: Int64; var AStream: TStream; const BufSize: Int64); override;
    property List: TMP4MetaDataList read FList write FList;
  end;

{ *************************************************** }

  TAtomFtyp = class(TAtomTinyData)
  strict
  private
    { ftyp object }
    FMajorBrand: TMP4FourCC;
    { 4 bytes - Major Brand }
    FMinorBrand: UInt32;
    { 4 bytes - Minor Brand }
    FCompatibleBrands: TMP4FourCCArray;
    { X bytes - Compatible Brands - Varable array of types }
  public
    procedure ReadFromStream(var BufPos: Int64; var AStream: TStream; const BufSize: Int64); override;
  published
    property MajorBrand: TMP4FourCC read FMajorBrand write FMajorBrand;
    { 4 bytes - Major Brand }
    property MinorBrand: UInt32 read FMinorBrand write FMinorBrand;
    { 4 bytes - Minor Brand }
    property CompatibleBrands: TMP4FourCCArray read FCompatibleBrands write FCompatibleBrands;
  end;

  TAtomMvhd = class(TAtomFullData)
  strict private
    FCreationTime: TDateTime;
    { 4 bytes - Creation Time }
    FModificationTime: TDateTime;
    { 4 bytes - Modification Time }
    FTimeScale: UInt32;
    { 4 bytes - TimeScale - units per second }
    FDuration: UInt32;
    { 4 bytes - Actual Duration - FDuration * (1/FTimeScale) Seconds }
    FPreferredRate: TMP4Fixed32;
    { 4 bytes - Preferred Rate - 1.0 = Normal }
    FPreferredVolume: TMP4Fixed16;
    { 2 bytes - Preferred Volume - 1.0 = Normal }
    {$HINTS OFF}
    FReserved: Array[0..9] of Byte;
    { 10 bytes - Reserved For Apple - Skipped completely }
    {$HINTS ON}
    FMatrixStructure: TMP4Matrix3x3;
    { 36 bytes - Matrix Structure - TMP4Fixed32 3x3 Matrix }
    FPreviewTime: UInt32;
    { 4 bytes - Preview time - in TimeScale units }
    FPreviewDurartion: UInt32;
    { 4 bytes - Preview duration - in TimeScale units }
    FPosterTime: UInt32;
    { 4 bytes - Poster Time - in TimeScale units }
    FSelectionTime: UInt32;
    { 4 bytes - Selection Time - in TimeScale units}
    FSelectionDuration: UInt32;
    { 4 bytes - Selection Duration - in TimeScale units}
    FCurrentTime: UInt32;
    { 4 bytes - Current Time - within movie }
    FNextTrackID: UInt32;
    { 4 bytes - Next Track ID }
  public
    procedure ReadFromStream(var BufPos: Int64; var AStream: TStream; const BufSize: Int64); override;
  published
    property CreationTime: TDateTime read FCreationTime write FCreationTime;
    property ModificationTime: TDateTime read FModificationTime write FModificationTime;
    property TimeScale: UInt32 read FTimeScale write FTimeScale;
    property Duration: UInt32 read FDuration write FDuration;
    property PreferredRate: TMP4Fixed32 read FPreferredRate write FPreferredRate;
    property PreferredVolume: TMP4Fixed16 read FPreferredVolume write FPreferredVolume;
    property MatrixStructure: TMP4Matrix3x3 read FMatrixStructure write FMatrixStructure;
    property PreviewTime: UInt32 read FPreviewTime write FPreviewTime;
    property PreviewDurartion: UInt32 read FPreviewDurartion write FPreviewDurartion;
    property PosterTime: UInt32 read FPosterTime write FPosterTime;
    property SelectionTime: UInt32 read FSelectionTime write FSelectionTime;
    property CurrentTime: UInt32 read FCurrentTime write FCurrentTime;
    property NextTrackID: UInt32 read FNextTrackID write FNextTrackID;
  end;

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

function TAtomAbstractData.UIntToMediaTime(const ATime: UInt32): TDateTime;
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

constructor TAtomAbstractData.Create(var AStream: TStream;
  const ARec: TAtomRec);
begin
  Create(ARec);
  IsHandled := True;
  if ARec.Is64Bit then
    PopulateFromStream(AStream, ARec.Size - SizeOf(TMP4FourCC) - SizeOf(UInt32) - SizeOf(Int64))
    { PopulateStream needs size of atom - size of header }
  else
    PopulateFromStream(AStream, ARec.Size - SizeOf(TMP4FourCC) - SizeOf(UInt32));
    { PopulateStream needs size of atom - size of header }
end;

function TAtomAbstractData.ReadFourCC(var BufPos: Int64;
  var AStream: TStream): TMP4FourCC;
begin
  if AStream.Position > (AStream.Size + 4) then
    Raise Exception.Create('Buffer too small');

  AStream.Read(Result, SizeOf(Result));
  Result := SwapBytes32(Result);
  BufPos := BufPos+4;
end;

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

function TAtomAbstractData.ReadMediaDateTime(var BufPos: Int64;
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

function TAtomAbstractData.ReadMetaData(var BufPos: Int64;
  var AStream: TStream): TMP4MetaData;
var
  AItem: TMP4MetaData;
//  MetaSize: UInt32;
  DataFCC: TMP4FourCC;
  DataSize: UInt32;
begin
  AItem := TMP4MetaData.Create;
//  DataSize := ReadUInt32(BufPos, AStream);
  ReadSkip(BufPos, AStream, 4);
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

function TAtomAbstractData.ReadMetaDataList(var BufPos: Int64;
  var AStream: TStream; const ASize: Int32): TMP4MetaDataList;
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

function TAtomAbstractData.ReadByte(var BufPos: Int64;
  var AStream: TStream): Byte;
begin
  if AStream.Position > (AStream.Size + SizeOf(Byte)) then
    Raise Exception.Create('Buffer too small');

  AStream.Read(Result, SizeOf(Result));
  BufPos := BufPos+SizeOf(Byte);
end;

function TAtomAbstractData.ReadChapterData(var BufPos: Int64;
  var AStream: TStream): TMP4ChapterData;
var
  AItem: TMP4ChapterData;
begin
  AItem := TMP4ChapterData.Create;
  AItem.Timestamp := ReadUInt64(BufPos, AStream);
  AItem.ChapNameLen := ReadByte(BufPos, AStream);
  AItem.ChapName := ReadTBytes(BufPos, AStream, AItem.ChapNameLen);

  Result := AItem;
end;

function TAtomAbstractData.ReadChapterDataList(var BufPos: Int64;
  var AStream: TStream; const ASize: Int32): TMP4ChapterDataList;
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

function TAtomAbstractData.ReadUInt16(var BufPos: Int64;
  var AStream: TStream): UInt16;
begin
  if AStream.Position > (AStream.Size + 2) then
    Raise Exception.Create('Buffer too small');

  AStream.Read(Result, SizeOf(Result));
  Result := SwapBytes16(Result);
  BufPos := BufPos+2;
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

function TAtomAbstractData.ReadUInt32(var BufPos: Int64;
  var AStream: TStream): UInt32;
begin
  if AStream.Position > (AStream.Size + 4) then
    Raise Exception.Create('Buffer too small');

  AStream.Read(Result, SizeOf(Result));
  Result := SwapBytes32(Result);
  BufPos := BufPos+4;
end;

function TAtomAbstractData.ReadUInt64(var BufPos: Int64;
  var AStream: TStream): UInt64;
begin
  if AStream.Position > (AStream.Size + SizeOf(UInt64)) then
    Raise Exception.Create('Buffer too small');

  AStream.Read(Result, SizeOf(Result));
  Result := SwapBytes64(Result);
  BufPos := BufPos+SizeOf(UInt64);
end;

function TAtomAbstractData.ReadSingle(var BufPos: Int64;
  var AStream: TStream): Single;
begin
  if AStream.Position > (AStream.Size + 4) then
    Raise Exception.Create('Buffer too small');

  AStream.Read(Result, SizeOf(Result));
  BufPos := BufPos+4;
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

function TAtomAbstractData.ReadTBytes(var BufPos: Int64; var AStream: TStream;
  const ASize: Int32): TBytes;
begin
  if AStream.Position > (AStream.Size + ASize) then
    Raise Exception.Create('Buffer too small');

  SetLength(Result, ASize);
  AStream.Read(Result, ASize);
  BufPos := BufPos + ASize;
end;

{ TAtomTinyData }

procedure TAtomTinyData.PopulateFromStream(var AStream: TStream;
  const ASize: Int64);
var
  BufPos: Int64;
begin
  BufPos := 0;

  ReadFromStream(BufPos, AStream, ASize);
  { Abstract Virtual method that fills in descendant fields }

  if BufPos <  ASize then
    Raise Exception.Create('Data UnderRead for ' + ClassName + ' : Atom = ' + FourCCToString(FourCC));
  if BufPos >  ASize then
    Raise Exception.Create('Data OverRead for ' + ClassName + ' : Atom = ' + FourCCToString(FourCC));
end;

procedure TAtomTinyData.ReadFromStream(var BufPos: Int64; var AStream: TStream;
  const BufSize: Int64);
begin
  { Do Nothing - A pure AtomTiny has no data - acts as a placeholder }
  { free, skip and wide use this }
end;

{ TAtomFullData }

procedure TAtomFullData.PopulateFromStream(var AStream: TStream; const ASize: Int64);
var
  BufPos: Int64;
begin
  if AStream.Position > (AStream.Size + 4) then
    Raise Exception.Create('Buffer too small');

  AStream.Read(FVersion, SizeOf(FVersion));
  AStream.Read(FFlags, SizeOf(FFlags));

  BufPos := 4;

  ReadFromStream(BufPos, AStream, ASize);
  { Abstract Virtual method that fulls in descendant fields }

  if BufPos <  ASize then
    Raise Exception.Create('Data UnderRead for ' + ClassName);
  if BufPos >  ASize then
    Raise Exception.Create('Data OverRead for ' + ClassName);
end;

{ TAtomFtyp }

procedure TAtomFtyp.ReadFromStream(var BufPos: Int64; var AStream: TStream; const BufSize: Int64);
begin
  FMajorBrand := ReadFourCC(BufPos, AStream);
  { 4 bytes - Major Brand }
  FMinorBrand := ReadUInt32(BufPos, AStream);
  { 4 bytes - Minor Brand }
  FCompatibleBrands := ReadFourCCArray(BufPos, AStream, (BufSize - BufPos) div SizeOf(TMP4FourCC));
end;

{ TAtomMvhd }

procedure TAtomMvhd.ReadFromStream(var BufPos: Int64;
  var AStream: TStream; const BufSize: Int64);
begin
  FCreationTime := ReadMediaDateTime(BufPos, AStream);
  FModificationTime := ReadMediaDateTime(BufPos, AStream);
  FTimeScale := ReadUInt32(BufPos, AStream);
  FDuration := ReadUInt32(BufPos, AStream);
  FPreferredRate := ReadUInt32(BufPos, AStream);
  FPreferredVolume := ReadUInt16(BufPos, AStream);
  ReadSkip(BufPos, AStream, 10);
  FMatrixStructure := ReadMatrix3x3(BufPos, AStream);
  FPreviewTime := ReadUInt32(BufPos, AStream);
  FPreviewDurartion := ReadUInt32(BufPos, AStream);
  FPosterTime := ReadUInt32(BufPos, AStream);
  FSelectionTime := ReadUInt32(BufPos, AStream);
  FSelectionDuration := ReadUInt32(BufPos, AStream);
  FCurrentTime := ReadUInt32(BufPos, AStream);
  FNextTrackID := ReadUInt32(BufPos, AStream);
end;

{ TAtomMeta }

procedure TAtomMeta.ReadFromStream(var BufPos: Int64; var AStream: TStream;
  const BufSize: Int64);
begin
  BufPos := BufSize;
end;

{ TAtomIlst }

procedure TAtomIlst.ReadFromStream(var BufPos: Int64; var AStream: TStream;
  const BufSize: Int64);
begin
  // inherited;
  FList := ReadMetaDataList(BufPos, AStream, BufSize);

end;

{ TAtomChpl }

procedure TAtomChpl.ReadFromStream(var BufPos: Int64; var AStream: TStream;
  const BufSize: Int64);
begin
  ReadSkip(BufPos, AStream, 4);
  FChapterCount := ReadByte(BufPos, AStream);
  FList := ReadChapterDataList(BufPos, AStream, BufSize);
end;

end.
