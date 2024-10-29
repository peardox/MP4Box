unit MP4SampleTypes;

{$M+}

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  Generics.Defaults, Generics.Collections, MP4Types;

type
  TMP4SampleType = class
  strict private
  end;

  TMP4SampleTypeUnknown = class(TMP4SampleType)
  strict private
    FData: TBytes;
  published
    property Data: TBytes read FData write FData;
  end;

  TMP4SampleTypeAudio0 = class(TMP4SampleType)
  strict private
    FData: TBytes;
  published
    property Data: TBytes read FData write FData;
  end;


implementation

end.
