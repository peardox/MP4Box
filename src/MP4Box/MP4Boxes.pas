unit MP4Boxes;

interface


uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  Generics.Defaults, Generics.Collections, MP4KnownAtoms, MP4Types;

type
  TMp4Box = packed record
    Size: UInt32;
    FourCC: TFourCC;
  end;

  TAtom = class;

  TAtomObjectList = TObjectList<TAtom>;

  TAtom = class(TObject)
  private
    FAbsPos: Int64;
    FFourCC: String;
    FSize: Int64;
    FVersionFlags: UInt32;
    FIs64Bit: Boolean;
    FLevel: Integer;
    FChildren: TAtomObjectList;
    FParent: TAtom;
    FData: TAtomBaseData;
  public
    constructor Create;
    constructor CreateWithData(const AAbspos: Int64; const ASize: Int64; const AIs64Bit: Boolean; const AFourCC: TFourCC; const ALevel: Integer; const AParent: TAtom = Nil);
    property AbsPos: Int64 read FAbsPos;
    property Size: Int64 read FSize;
    property Is64Bit: Boolean read FIs64Bit write FIs64Bit;
    property FourCC: String read FFourCC;
    property Parent: TAtom read FParent;
    property Level: Integer read FLevel;
    property Children: TAtomObjectList read FChildren write FChildren;
    property Data: TAtomBaseData read FData write FData;

  end;


  TAtomList = class(TObject)
  private
    FStream: TStream;
    FFilename: String;
    FAtomList: TAtomObjectList;
    procedure DecodeAtom(const AStart: Int64 = 0; const ASize: Int64 = 0; const ALevel: Integer = 0; const AParent: TAtom = Nil);
    procedure AddFullChildren(var Atom: TAtom);
  public
    constructor Create;
    function Add(const Value: TAtom): NativeInt;
    procedure AddChildren(var Atom: TAtom);
    procedure DecodeStream;
    procedure DecodeChild(var Atom: TAtom);
    function OpenFile(const AValue: String): Boolean;
    procedure GetBuffer(var ABuffer: TBuffer; const ASize: Integer);
    function Open: Boolean;
    function FindChild(const AValue: String): TAtom;
    property Atoms: TAtomObjectList read FAtomList write FAtomList;
    property Filename: String read FFilename write FFilename;
  end;

implementation

uses IOUtils;

{ TAtom }

constructor TAtom.Create;
begin
  FAbsPos := 0;
  FSize := 0;
  FVersionFlags := 0;
  FIs64Bit := False;
  FLevel := 0;
  FFourCC := '';
  FChildren := Nil;
  FData := Nil;
end;

constructor TAtom.CreateWithData(const AAbspos, ASize: Int64; const AIs64Bit: Boolean;
  const AFourCC: TFourCC; const ALevel: Integer; const AParent: TAtom);
begin
  Create;
  FAbsPos := AAbsPos;
  FSize := ASize;
  FIs64Bit := True;
  FLevel := ALevel;
  FFourCC := (Chr(AFourCC[0])+Chr(AFourCC[1])+Chr(AFourCC[2])+Chr(AFourCC[3]));
  FParent := AParent;
end;

{ TAtomList }

function TAtomList.Add(const Value: TAtom): NativeInt;
begin
  Result := FAtomList.Add(Value);
end;

procedure TAtomList.AddChildren(var Atom: TAtom);
begin
  Atom.Children := TAtomObjectList.Create(True);
  DecodeAtom(Atom.FAbsPos + 8, Atom.FSize - 8, Atom.Level + 1, Atom);
end;

procedure TAtomList.AddFullChildren(var Atom: TAtom);
begin
  Atom.Children := TAtomObjectList.Create(True);
  DecodeAtom(Atom.FAbsPos + 12, Atom.FSize - 12, Atom.Level + 1, Atom);
end;

constructor TAtomList.Create;
begin
  FAtomList := TAtomObjectList.Create(True);
end;

procedure TAtomList.DecodeAtom(const AStart: Int64 = 0; const ASize: Int64 = 0; const ALevel: Integer = 0; const AParent: TAtom = Nil);
var
  FPos,
  FEnd: Int64;
  B: TMp4Box;
  Size64: Int64;
  Atom: TAtom;
  Is64Bit: Boolean;
  HeaderSize: Integer;
begin
  FPos := AStart;
  FEnd := AStart + ASize;

  while FPos < FEnd do
    begin
      FStream.Position := FPos;
      FStream.Read(B, SizeOf(B));

      Size64 := SwapBytes32(B.Size);
      HeaderSize := 8;

      // Google 64bit mp4 atoms
      if Size64 = 1 then
        begin
          Is64Bit := True;
          HeaderSize := 16;
          if (FPos + SizeOf(B)) < FEnd then
            begin
              FStream.Position := FPos + SizeOf(B);
              FStream.Read(Size64, SizeOf(Int64));
              Size64 := SwapBytes64(Size64);
            end
          else
            Raise Exception.Create('Read overflow');
        end
      else
        Is64Bit := False;

      Atom := TAtom.CreateWithData(FPos, Size64, Is64Bit, B.FourCC, ALevel, AParent);

      if AParent = Nil then
        FAtomList.Add(Atom)
      else
        AParent.Children.Add(Atom);

      if IsContainer(Atom.FourCC) then
        begin
          AddChildren(Atom);
        end
      else if IsFullAtom(Atom.FourCC) then
        begin
          AddFullChildren(Atom);
        end
      else if IsKnownAtom(Atom.FourCC) then
        begin
          // AddFullChildren(Atom);
          Atom.FData := TAtomMvhd.CreateFromStream(FStream, Size64 - HeaderSize);

        end;

      FPos := FPos + Size64;
    end;
end;

procedure TAtomList.DecodeStream;
begin
  DecodeAtom(0, FStream.Size);
end;

procedure TAtomList.DecodeChild(var Atom: TAtom);
begin
  Atom.Children := TAtomObjectList.Create(True);
  DecodeAtom(Atom.FAbsPos + 8, Atom.FSize - 8, Atom.Level + 1, Atom);
end;

function TAtomList.FindChild(const AValue: String): TAtom;
var
  I: Integer;
begin
  Result := Nil;
  for I := 0 to FAtomList.Count do
    begin
      if FAtomList[I].FFourCC = AValue then
        begin
          Result := FAtomList[I];
          Break;
        end;
    end;
end;

procedure TAtomList.GetBuffer(var ABuffer: TBuffer; const ASize: Integer);
var
  ReadBytes: LongInt;
begin
  try
    SetLength(ABuffer, ASize);
    ReadBytes := FStream.Read(ABuffer, ASize);
    if ReadBytes <> ASize then
      Raise Exception.Create('Underread buffer');
  except
    on E : EOutOfMemory do
      Raise Exception.Create(E.ClassName + ' error raised, with message : ' + E.Message);
  end;
end;

function TAtomList.OpenFile(const AValue: String): Boolean;
begin
  FFilename := AValue;
  Result := Open;
end;

function TAtomList.Open: Boolean;
var
  FS: Int64;
begin
  Result := False;

  if FileExists(FFilename) then
    begin
      FS := TFile.GetSize(FFilename);
      if FS > 0 then
        begin
          if FS < 2147483647 then
            begin
              FStream := TMemoryStream.Create as TStream;
              TMemoryStream(FStream).LoadFromFile(FFilename);
            end
          else
            FStream := TFileStream.Create(FFilename, fmOpenRead) as TStream;
          Result := True;
        end;
    end;
end;

end.
