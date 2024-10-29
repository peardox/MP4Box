unit MP4ExtendedTypes;

{$M+}

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  Generics.Defaults, Generics.Collections, MP4Types, MP4SampleTypes;

type
  TMP4SampleDescData = class
  strict private
    FSampleDescSize: UInt32;
    FFourCC: TMP4FourCC;
    FReserved: TBytes; // Six zero bytes
    FDataRefIndex: UInt16;
    FData: TMP4SampleType; // Variable Data
  public
  published
    property SampleDescSize: UInt32 read FSampleDescSize write FSampleDescSize;
    property FourCC: TMP4FourCC read FFourCC write FFourCC;
    property Reserved: TBytes read FReserved write FReserved;
    property DataRefIndex: UInt16 read FDataRefIndex write FDataRefIndex;
    property Data: TMP4SampleType read FData write FData;
  end;

  TMP4SampleDescDataList = TObjectList<TMP4SampleDescData>;

  TMP4MetaData = class
  strict private
    FMetaSize: UInt32;
    FFourCC: TMP4FourCC;
    FDataType: UInt32;
    FLocale: UInt32;
    FData: TBytes;
  public
  published
    property MetaSize: UInt32 read FMetaSize write FMetaSize;
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

  TMP4EditData = class
  strict private
    FTrackDuration: UInt32;
    FMediaTime: UInt32;
    FMediaRate: TMP4Fixed32;
  public
  published
    property TrackDuration: UInt32 read FTrackDuration write FTrackDuration;
    property MediaTime: UInt32 read FMediaTime write FMediaTime;
    property MediaRate: TMP4Fixed32 read FMediaRate write FMediaRate;
  end;
  TMP4EditDataList = TObjectList<TMP4EditData>;

implementation

end.
