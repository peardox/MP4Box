unit MP4SampleTypes;

{$M+}

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  Generics.Defaults, Generics.Collections, MP4Types, MP4FileAtom;

type
  TMP4SampleType = class(TMP4FileAtom)
  strict private
  protected
    FUnknownData: TBytes;
  public
    procedure ReadFromStream(var BufPos: Int64; var AStream: TStream; const BufSize: Int64); virtual; abstract;
  published
    property UnknownData: TBytes read FUnknownData write FUnknownData;
  end;
  TMP4SampleTypeList = TObjectList<TMP4SampleType>;

  TMP4SampleTypeUnknown = class(TMP4SampleType)
  strict private
  public
    procedure ReadFromStream(var BufPos: Int64; var AStream: TStream; const BufSize: Int64); override;
  published
  end;

  TMP4SampleTypeAudio0 = class(TMP4SampleType)
  strict private
    FVersion: UInt16;
    { 16-bit integer that holds the sample description version. }
    FRevisionLevel: UInt16;
    { A 16-bit integer. }
    FVendor: UInt32;
    { A 32-bit integer. }
    FChannels: UInt16;
    { A 16-bit integer that indicates the number of sound channels used by the sound sample. }
    FSampleSize: UInt16;
    { A 16-bit integer that specifies the number of bits in each uncompressed sound sample. }
    FCompressionID: UInt16;
    { A 16-bit integer. }
    FPacketSize: UInt16;
    { A 16-bit integer. }
    FSampleRate: Single;
    { A 32-bit unsigned fixed-point number (16.16) that indicates the rate at which the sound samples were obtained. }
  public
    procedure ReadFromStream(var BufPos: Int64; var AStream: TStream; const BufSize: Int64); override;
  published
    property Version: UInt16 read FVersion write FVersion;
    property RevisionLevel: UInt16 read FRevisionLevel write FRevisionLevel;
    property Vendor: UInt32 read FVendor write FVendor;
    property Channels: UInt16 read FChannels write FChannels;
    property SampleSize: UInt16 read FSampleSize write FSampleSize;
    property CompressionID: UInt16 read FCompressionID write FCompressionID;
    property PacketSize: UInt16 read FPacketSize write FPacketSize;
    property SampleRate: Single read FSampleRate write FSampleRate;
  end;


implementation

{ TMP4SampleTypeUnknown }

procedure TMP4SampleTypeUnknown.ReadFromStream(var BufPos: Int64;
  var AStream: TStream; const BufSize: Int64);
begin
  SetLength(FUnknownData, BufSize - BufPos);
  AStream.Read(FUnknownData, BufSize - BufPos);
end;

{ TMP4SampleTypeAudio0 }

procedure TMP4SampleTypeAudio0.ReadFromStream(var BufPos: Int64;
  var AStream: TStream; const BufSize: Int64);
begin
  FVersion := ReadUInt16(BufPos, AStream);
  FRevisionLevel := ReadUInt16(BufPos, AStream);
  FVendor := ReadUInt32(BufPos, AStream);
  FChannels := ReadUInt16(BufPos, AStream);
  FSampleSize := ReadUInt16(BufPos, AStream);
  FCompressionID := ReadUInt16(BufPos, AStream);
  FPacketSize := ReadUInt16(BufPos, AStream);
  FSampleRate := ReadSingle32(BufPos, AStream);;
  SetLength(FUnknownData, BufSize - BufPos);
  FUnknownData := ReadTBytes(BufPos, AStream, BufSize - BufPos);
end;

end.
