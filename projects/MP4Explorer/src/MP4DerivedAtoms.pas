unit MP4DerivedAtoms;

{$M+}

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  Generics.Defaults, Generics.Collections, MP4Atoms, MP4Types;

type
  TAtomTkhd = class(TAtomFullData) { $746B6864: // tkhd }
  { Track header atom - An atom that specifies the characteristics of a single track within a movie. }
  strict private
    FCreationTime: TDateTime;
    { 4 bytes - Creation Time }
    FModificationTime: TDateTime;
    { 4 bytes - Modification Time }
    FTrackID: UInt32;
    { 4 bytes - Track ID }
    FReserved1: Array[0..3] of Byte;
    { 4 bytes - Reserved For Apple - Skipped completely }
    FDuration: UInt32;
    { 4 bytes - Duration }
    FReserved2: Array[0..7] of Byte;
    { 8 bytes - Reserved For Apple - Skipped completely }
    FLayer: UInt16;
    { 2 bytes - Layer }
    FAlternateGroup: UInt16;
    { 2 bytes - Alternate Group }
    FVolume: TMP4Fixed16;
    { 2 bytes - Volume }
    FReserved3: Array[0..1] of Byte;
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

implementation

{ TAtomTkhd }

procedure TAtomTkhd.ReadFromStream(var BufPos: Int64; var AStream: TStream; const BufSize: Int64);
begin
  FCreationTime := ReadMediaDateTime(BufPos, AStream);
  FModificationTime := ReadMediaDateTime(BufPos, AStream);
  FTrackID := ReadUInt32(BufPos, AStream);
  ReadSkip(BufPos, AStream, 4);
  FDuration := ReadUInt32(BufPos, AStream);
  ReadSkip(BufPos, AStream, 8);
  FLayer := ReadUInt16(BufPos, AStream);
  FAlternateGroup := ReadUInt16(BufPos, AStream);
  FVolume := ReadUInt16(BufPos, AStream);
  ReadSkip(BufPos, AStream, 2);
  FMatrixStructure := ReadMatrix3x3(BufPos, AStream);
  FTrackWidth := ReadUInt32(BufPos, AStream);
  FTrackHeight := ReadUInt32(BufPos, AStream);
end;


end.

