unit MP4DerivedAtoms;

{$M+}

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  Generics.Defaults, Generics.Collections, MP4Atoms, MP4Types, MP4ExtendedTypes;

type  TAtomFtyp = class(TAtomLiteData) { $66747970: // ftyp }
  { File type compatibility atom - An atom that identifies the file type specifications with which the file is compatible. }
  strict private
    FMajorBrand: TMP4FourCC;
    { 4 bytes - Major Brand }
    FMinorBrand: TMP4FourCC;
    { 4 bytes - Minor Brand }
    FCompatibleBrands: TMP4FourCCArray;
    { X bytes - Compatible Brands - Varable array of types }
  public
    procedure ReadFromStream(var BufPos: Int64; var AStream: TStream; const BufSize: Int64); override;
  published
    property MajorBrand: TMP4FourCC read FMajorBrand write FMajorBrand;
    property MinorBrand: TMP4FourCC read FMinorBrand write FMinorBrand;
    property CompatibleBrands: TMP4FourCCArray read FCompatibleBrands write FCompatibleBrands;
  end;

  TAtomMvhd = class(TAtomFullData) { $6D766864: // mvhd }
  { Movie header atom - An atom that specifies the characteristics of an entire QuickTime movie. }
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
    FReserved: TBytes;
    { 10 bytes - Reserved For Apple - Skipped completely }
    FMatrixStructure: TMP4Matrix3x3;
    { 36 bytes - Matrix Structure - TMP4Fixed16 3x3 Matrix }
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
    property SelectionDuration: UInt32 read FSelectionDuration write FSelectionDuration;
    property CurrentTime: UInt32 read FCurrentTime write FCurrentTime;
    property NextTrackID: UInt32 read FNextTrackID write FNextTrackID;
  end;

  TAtomTkhd = class(TAtomFullData) { $746B6864: // tkhd }
  { Track header atom - An atom that specifies the characteristics of a single track within a movie. }
  strict private
    FCreationTime: TDateTime;
    { 4 bytes - Creation Time }
    FModificationTime: TDateTime;
    { 4 bytes - Modification Time }
    FTrackID: UInt32;
    { 4 bytes - Track ID }
    FReserved1: TBytes;
    { 4 bytes - Reserved For Apple - Skipped completely }
    FDuration: UInt32;
    { 4 bytes - Duration }
    FReserved2: TBytes;
    { 8 bytes - Reserved For Apple - Skipped completely }
    FLayer: UInt16;
    { 2 bytes - Layer }
    FAlternateGroup: UInt16;
    { 2 bytes - Alternate Group }
    FVolume: TMP4Fixed16;
    { 2 bytes - Volume }
    FReserved3: TBytes;
    { 2 bytes - Reserved For Apple - Skipped completely }
    FMatrixStructure: TMP4Matrix3x3;
    { 36 bytes - Matrix Structure  - TMP4Fixed32 3x3 Matrix }
    FTrackWidth: TMP4Fixed32;
    { 4 bytes - Track Width }
    FTrackHeight: TMP4Fixed32;
    { 4 bytes - Track Height }
  public
    procedure ReadFromStream(var BufPos: Int64; var AStream: TStream; const BufSize: Int64); override;
  published
    property CreationTime: TDateTime read FCreationTime write FCreationTime;
    property ModificationTime: TDateTime read FModificationTime write FModificationTime;
    property TrackID: UInt32 read FTrackID write FTrackID;
    property Duration: UInt32 read FDuration write FDuration;
    property Layer: UInt16 read FLayer write FLayer;
    property AlternateGroup: UInt16 read FAlternateGroup write FAlternateGroup;
    property Volume: TMP4Fixed16 read FVolume write FVolume;
    property MatrixStructure: TMP4Matrix3x3 read FMatrixStructure write FMatrixStructure;
    property TrackWidth: TMP4Fixed32 read FTrackWidth write FTrackWidth;
    property TrackHeight: TMP4Fixed32 read FTrackHeight write FTrackHeight;
  end;

  TAtomIlst = class(TAtomLiteData) { $696C7374: // ilst }
  { An atom that holds a list of actual metadata values that are present in the metadata atom }
  strict private
    FList: TMP4MetaDataList;
    { X bytes - Meta Data Atom List }
  public
    procedure ReadFromStream(var BufPos: Int64; var AStream: TStream; const BufSize: Int64); override;
  published
    property List: TMP4MetaDataList read FList write FList;
  end;

  TAtomChpl = class(TAtomFullData) { $6368706C: // chpl }
  { Chapter List Description Atom }
  strict private
    FReserved: TBytes;
    { 4 bytes - Unknown use }
    FChapterCount: Byte;
    { 1 byte - Number of chapters }
    FList: TMP4ChapterDataList;
    { X bytes - Chapter Data Object List}
  public
    procedure ReadFromStream(var BufPos: Int64; var AStream: TStream; const BufSize: Int64); override;
  published
    property ChapterCount: Byte read FChapterCount write FChapterCount;
    property List: TMP4ChapterDataList read FList write FList;
  end;

  TAtomChap = class(TAtomLiteData) { $63686170: // chap }
  { Chapter atom - Chapter or scene list. Usually references a text track. }
  strict private
    FTrackID: UInt32;
    { 4 bytes - Track ID }
  public
    procedure ReadFromStream(var BufPos: Int64; var AStream: TStream; const BufSize: Int64); override;
  published
    property TrackID: UInt32 read FTrackID write FTrackID;
  end;

  TAtomElst = class(TAtomFullData) { $656C7374: // elst }
  { Edit List Atom - An atom that maps from a time in a movie to a time in a media, and ultimately to media data. }
  strict private
    FEntryCount: UInt32;
    { 4 bytes - Number of entries }
    FList: TMP4EditDataList;
    { X bytes - Edit Data Object List}
  public
    procedure ReadFromStream(var BufPos: Int64; var AStream: TStream; const BufSize: Int64); override;
  published
    property EntryCount: UInt32 read FEntryCount write FEntryCount;
    property List: TMP4EditDataList read FList write FList;
  end;

  TAtomMdhd = class(TAtomFullData) { $6D646864: // mdhd }
  { Media header atom - An atom that specifies the characteristics of a media, including time scale and duration }
  strict private
    FCreationTime: TDateTime;
    { 4 bytes - Creation Time }
    FModificationTime: TDateTime;
    { 4 bytes - Modification Time }
    FTimeScale: UInt32;
    { 4 bytes - TimeScale - units per second }
    FDuration: UInt32;
    { 4 bytes - Actual Duration - FDuration * (1/FTimeScale) Seconds }
    FLanguage: UInt16;
    { 2 bytes - Language code, a wierd Mac one }
    FQuality: UInt16;
    { 2 bytes - A 16-bit integer that specifies the media’s playback quality. }
  public
    procedure ReadFromStream(var BufPos: Int64; var AStream: TStream; const BufSize: Int64); override;
  published
    property CreationTime: TDateTime read FCreationTime write FCreationTime;
    property ModificationTime: TDateTime read FModificationTime write FModificationTime;
    property TimeScale: UInt32 read FTimeScale write FTimeScale;
    property Duration: UInt32 read FDuration write FDuration;
    property Language: UInt16 read FLanguage write FLanguage;
    property Quality: UInt16 read FQuality write FQuality;
  end;

  TAtomHdlr = class(TAtomFullData) { $68646C72: // hdlr }
  { Metadata handler atom - An atom that defines the structure used for all types of metadata stored within the metadata atom. }
  strict private
    FPredefined: UInt32;
    { 4 bytes - Zero (0) }
    FHandlerType: TMP4FourCC;
    { 4 bytes - mdta - Handler type - A 32-bit integer that indicates the structure used in the metadata atom. }
    FName: TMP4HDLRString;
    { X bytes - Pascal / Null terminated UTF-8 string - Usually 0 }
  public
    procedure ReadFromStream(var BufPos: Int64; var AStream: TStream; const BufSize: Int64); override;
  published
    property Predefined: UInt32 read FPredefined write FPredefined;
    property HandlerType: TMP4FourCC read FHandlerType write FHandlerType;
    property Name: TMP4HDLRString read FName write FName;
  end;

  TAtomSmhd = class(TAtomFullData) { $736D6864: // smhd }
  { Sound media information header atom - An atom that stores the sound media’s control information, such as balance. }
  strict private
    FBalance: UInt16;
    { 2 bytes - Balance }
    FReserved1: TBytes;
    { 2 bytes - Reserved For Apple - Skipped completely }
  public
    procedure ReadFromStream(var BufPos: Int64; var AStream: TStream; const BufSize: Int64); override;
  published
    property Balance: UInt16 read FBalance write FBalance;
  end;

implementation
{ TAtomFtyp }

procedure TAtomFtyp.ReadFromStream(var BufPos: Int64; var AStream: TStream; const BufSize: Int64);
begin
  FMajorBrand := ReadFourCC(BufPos, AStream);
  FMinorBrand := ReadFourCC(BufPos, AStream);
  FCompatibleBrands := ReadFourCCArray(BufPos, AStream, (BufSize - BufPos) div SizeOf(TMP4FourCC));
end;

{ TAtomMvhd }

procedure TAtomMvhd.ReadFromStream(var BufPos: Int64; var AStream: TStream; const BufSize: Int64);
begin
  FCreationTime := ReadMediaDateTime(BufPos, AStream);
  FModificationTime := ReadMediaDateTime(BufPos, AStream);
  FTimeScale := ReadUInt32(BufPos, AStream);
  FDuration := ReadUInt32(BufPos, AStream);
  FPreferredRate := ReadUInt32(BufPos, AStream);
  FPreferredVolume := ReadUInt16(BufPos, AStream);
  FReserved := ReadReserved(BufPos, AStream, 10);
  FMatrixStructure := ReadMatrix3x3(BufPos, AStream);
  FPreviewTime := ReadUInt32(BufPos, AStream);
  FPreviewDurartion := ReadUInt32(BufPos, AStream);
  FPosterTime := ReadUInt32(BufPos, AStream);
  FSelectionTime := ReadUInt32(BufPos, AStream);
  FSelectionDuration := ReadUInt32(BufPos, AStream);
  FCurrentTime := ReadUInt32(BufPos, AStream);
  FNextTrackID := ReadUInt32(BufPos, AStream);
end;

{ TAtomTkhd }

procedure TAtomTkhd.ReadFromStream(var BufPos: Int64; var AStream: TStream; const BufSize: Int64);
begin
  FCreationTime := ReadMediaDateTime(BufPos, AStream);
  FModificationTime := ReadMediaDateTime(BufPos, AStream);
  FTrackID := ReadUInt32(BufPos, AStream);
  FReserved1 := ReadReserved(BufPos, AStream, 4);
  FDuration := ReadUInt32(BufPos, AStream);
  FReserved2 := ReadReserved(BufPos, AStream, 8);
  FLayer := ReadUInt16(BufPos, AStream);
  FAlternateGroup := ReadUInt16(BufPos, AStream);
  FVolume := ReadUInt16(BufPos, AStream);
  FReserved3 := ReadReserved(BufPos, AStream, 2);
  FMatrixStructure := ReadMatrix3x3(BufPos, AStream);
  FTrackWidth := ReadUInt32(BufPos, AStream);
  FTrackHeight := ReadUInt32(BufPos, AStream);
end;

{ TAtomIlst }

procedure TAtomIlst.ReadFromStream(var BufPos: Int64; var AStream: TStream; const BufSize: Int64);
begin
  FList := ReadMetaDataList(BufPos, AStream, BufSize);
end;

{ TAtomChpl }

procedure TAtomChpl.ReadFromStream(var BufPos: Int64; var AStream: TStream; const BufSize: Int64);
begin
  FReserved := ReadReserved(BufPos, AStream, 4);
  FChapterCount := ReadByte(BufPos, AStream);
  FList := ReadChapterDataList(BufPos, AStream, BufSize);
end;

{ TAtomChap }

procedure TAtomChap.ReadFromStream(var BufPos: Int64; var AStream: TStream; const BufSize: Int64);
begin
  FTrackID := ReadUInt32(BufPos, AStream);
end;

{ TAtomElst }

procedure TAtomElst.ReadFromStream(var BufPos: Int64; var AStream: TStream; const BufSize: Int64);
begin
  FEntryCount := ReadUInt32(BufPos, AStream);
  FList := ReadEditDataList(BufPos, AStream, BufSize);
end;

{ TAtomMdhd }

procedure TAtomMdhd.ReadFromStream(var BufPos: Int64; var AStream: TStream; const BufSize: Int64);
begin
  FCreationTime := ReadMediaDateTime(BufPos, AStream);
  FModificationTime := ReadMediaDateTime(BufPos, AStream);
  FTimeScale := ReadUInt32(BufPos, AStream);
  FDuration := ReadUInt32(BufPos, AStream);
  FLanguage := ReadUInt16(BufPos, AStream);
  FQuality := ReadUInt16(BufPos, AStream);
end;

{ TAtomHdlr }

procedure TAtomHdlr.ReadFromStream(var BufPos: Int64; var AStream: TStream; const BufSize: Int64);
begin
  FPredefined := ReadUInt32(BufPos, AStream);
  FHandlerType := ReadFourCC(BufPos, AStream);
  FName := ReadHDLR(BufPos, AStream, BufSize, FHandlerType);
end;

{ TAtomSmhd }

procedure TAtomSmhd.ReadFromStream(var BufPos: Int64; var AStream: TStream; const BufSize: Int64);
begin
  FBalance := ReadUInt16(BufPos, AStream);
  FReserved1 := ReadReserved(BufPos, AStream, 2);
end;

end.
