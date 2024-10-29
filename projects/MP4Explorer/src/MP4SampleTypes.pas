unit MP4SampleTypes;

{$M+}

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  Generics.Defaults, Generics.Collections, MP4Types;

type
  TMP4SampleType = class
  strict private
  protected
    FUnknownData: TBytes;
  public
    procedure ReadFromStream(var BufPos: Int64; var AStream: TStream; const BufSize: Int64); virtual; abstract;
  published
    property UnknownData: TBytes read FUnknownData write FUnknownData;
  end;

  TMP4SampleTypeUnknown = class(TMP4SampleType)
  strict private
  public
    procedure ReadFromStream(var BufPos: Int64; var AStream: TStream; const BufSize: Int64); override;
  published
  end;

  TMP4SampleTypeAudio0 = class(TMP4SampleType)
  strict private
  public
    procedure ReadFromStream(var BufPos: Int64; var AStream: TStream; const BufSize: Int64); override;
  published
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
  SetLength(FUnknownData, BufSize - BufPos);
  AStream.Read(FUnknownData, BufSize - BufPos);
end;

end.
