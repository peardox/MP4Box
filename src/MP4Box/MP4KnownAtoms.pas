unit MP4KnownAtoms;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  Generics.Defaults, Generics.Collections, MP4Types;

type
  TMp4Flags = Array[0..2] of Byte;
  TFixed32 = Int32;
  { Fixed point number 16:16 }
  TFixed16 = Int16;
  { Fixed point number 8:8 }
  TMatrix = Array[0..2, 0..2] of TFixed32;
  { 3x3 Matrix }

  TAtomBaseData = class(TObject)
  private
  protected
    function ReadSingle(var BufPos: Int64; var AStream: TStream): Single;
    function MediaTime(const ATime: UInt32): TDateTime;
    function ReadMediaDateTime(var BufPos: Int64; var AStream: TStream): TDateTime;
    function ReadUInt32(var BufPos: Int64; var AStream: TStream): UInt32;
    function ReadUInt16(var BufPos: Int64; var AStream: TStream): UInt16;
    procedure ReadSkip(var BufPos: Int64; var AStream: TStream; const ASize: Int32);
  public
    constructor Create; virtual;
  end;

  TAtomFullData = class(TAtomBaseData)
  private
    FVersion: Byte;
    { 1 byte - Version - Always 0 }
    FFlags: TMp4Flags;
    { 3 bytes - Flags - Always 0 }
  public
    constructor Create; override;
    constructor CreateFromStream(var AStream: TStream; const ASize: Int64);
    procedure PopulateFromStream(var AStream: TStream; const ASize: Int64);
    procedure ReadFromStream(var BufPos: Int64; var AStream: TStream); virtual; abstract;
  end;

  TAtomMvhd = class(TAtomFullData)
  private
    FCreationTime: TDateTime;
    { 4 bytes - Creation Time }
    FModificationTime: TDateTime;
    { 4 bytes - Modification Time }
    FTimeScale: UInt32;
    { 4 bytes - TimeScale - units per second }
    FDuration: UInt32;
    { 4 bytes - Actual Duration - FDuration * (1/FTimeScale) Seconds }
    FPreferredRate: TFixed32;
    { 4 bytes - Preferred Rate - 1.0 = Normal }
    FPreferredVolume: TFixed16;
    { 2 bytes - Preferred Volume - 1.0 = Normal }
    {$HINTS OFF}
    FReserved: Array[0..9] of Byte;
    { 10 bytes - Reserved For Apple - Skipped completely }
    {$HINTS ON}
    FMatrixStructure: TMatrix;
    { 36 bytes - Matrix Structure - TFixed16 3x3 Matrix }
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
    procedure ReadFromStream(var BufPos: Int64; var AStream: TStream); override;
    property CreationTime: TDateTime read FCreationTime write FCreationTime;
    property ModificationTime: TDateTime read FModificationTime write FModificationTime;
    property TimeScale: UInt32 read FTimeScale write FTimeScale;
    property Duration: UInt32 read FDuration write FDuration;
    property PreferredRate: TFixed32 read FPreferredRate write FPreferredRate;
    property PreferredVolume: TFixed16 read FPreferredVolume write FPreferredVolume;
    property MatrixStructure: TMatrix read FMatrixStructure write FMatrixStructure;
    property PreviewTime: UInt32 read FPreviewTime write FPreviewTime;
    property PreviewDurartion: UInt32 read FPreviewDurartion write FPreviewDurartion;
    property PosterTime: UInt32 read FPosterTime write FPosterTime;
    property SelectionTime: UInt32 read FSelectionTime write FSelectionTime;
    property CurrentTime: UInt32 read FCurrentTime write FCurrentTime;
    property NextTrackID: UInt32 read FNextTrackID write FNextTrackID;
  end;

implementation

{ TAtomData }

constructor TAtomBaseData.Create;
begin
end;

function TAtomBaseData.MediaTime(const ATime: UInt32): TDateTime;
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

function TAtomBaseData.ReadMediaDateTime(var BufPos: Int64;
  var AStream: TStream): TDateTime;
var
  T: UInt32;
begin
  if AStream.Position > (AStream.Size + 4) then
    Raise Exception.Create('Buffer too small');

  AStream.Read(T, SizeOf(T));
  Result := MediaTime(SwapBytes32(T));
  BufPos := BufPos+4;
end;

function TAtomBaseData.ReadUInt16(var BufPos: Int64;
  var AStream: TStream): UInt16;
begin
  if AStream.Position > (AStream.Size + 2) then
    Raise Exception.Create('Buffer too small');

  AStream.Read(Result, SizeOf(Result));
  Result := SwapBytes16(Result);
  BufPos := BufPos+2;
end;

function TAtomBaseData.ReadUInt32(var BufPos: Int64;
  var AStream: TStream): UInt32;
begin
  if AStream.Position > (AStream.Size + 4) then
    Raise Exception.Create('Buffer too small');

  AStream.Read(Result, SizeOf(Result));
  Result := SwapBytes32(Result);
  BufPos := BufPos+4;
end;

function TAtomBaseData.ReadSingle(var BufPos: Int64;
  var AStream: TStream): Single;
begin
  if AStream.Position > (AStream.Size + 4) then
    Raise Exception.Create('Buffer too small');

  AStream.Read(Result, SizeOf(Result));
  BufPos := BufPos+4;
end;

procedure TAtomBaseData.ReadSkip(var BufPos: Int64; var AStream: TStream;
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

{ TAtomFullData }

constructor TAtomFullData.Create;
begin
  inherited;
  FVersion := 0;
end;

constructor TAtomFullData.CreateFromStream(var AStream: TStream;
  const ASize: Int64);
begin
  Create;
  PopulateFromStream(AStream, ASize);
end;

procedure TAtomFullData.PopulateFromStream(var AStream: TStream; const ASize: Int64);
var
  BufPos: Int64;
begin
  if AStream.Position > (AStream.Size + 4) then
    Raise Exception.Create('Buffer too small');

  AStream.Read(FVersion, SizeOf(FVersion));
  AStream.Read(FFlags, SizeOf(FFlags));

  BufPos := 4;

  ReadFromStream(BufPos, AStream);
  { Abstract Virtual method that fulls in descendant fields }

  if BufPos <  ASize then
    Raise Exception.Create('Data UnderRead for ' + ClassName);
  if BufPos >  ASize then
    Raise Exception.Create('Data OverRead for ' + ClassName);
end;

{ TAtomMvhd }

procedure TAtomMvhd.ReadFromStream(var BufPos: Int64;
  var AStream: TStream);
begin
  FCreationTime := ReadMediaDateTime(BufPos, AStream);
  FModificationTime := ReadMediaDateTime(BufPos, AStream);
  FTimeScale := ReadUInt32(BufPos, AStream);
  FDuration := ReadUInt32(BufPos, AStream);
  FPreferredRate := ReadUInt32(BufPos, AStream);
  FPreferredVolume := ReadUInt16(BufPos, AStream);
  ReadSkip(BufPos, AStream, 10);
  FMatrixStructure[0][0] := ReadUInt32(BufPos, AStream);
  FMatrixStructure[0][1] := ReadUInt32(BufPos, AStream);
  FMatrixStructure[0][2] := ReadUInt32(BufPos, AStream);
  FMatrixStructure[1][0] := ReadUInt32(BufPos, AStream);
  FMatrixStructure[1][1] := ReadUInt32(BufPos, AStream);
  FMatrixStructure[1][2] := ReadUInt32(BufPos, AStream);
  FMatrixStructure[2][0] := ReadUInt32(BufPos, AStream);
  FMatrixStructure[2][1] := ReadUInt32(BufPos, AStream);
  FMatrixStructure[2][2] := ReadUInt32(BufPos, AStream);
  FPreviewTime := ReadUInt32(BufPos, AStream);
  FPreviewDurartion := ReadUInt32(BufPos, AStream);
  FPosterTime := ReadUInt32(BufPos, AStream);
  FSelectionTime := ReadUInt32(BufPos, AStream);
  FSelectionDuration := ReadUInt32(BufPos, AStream);
  FCurrentTime := ReadUInt32(BufPos, AStream);
  FNextTrackID := ReadUInt32(BufPos, AStream);
end;

end.
