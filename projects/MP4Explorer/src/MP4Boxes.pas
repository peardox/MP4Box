unit MP4Boxes;

interface

{$DEFINE QUICKLOG}

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  Generics.Defaults, Generics.Collections,
  MP4AutoAtom,
  MP4Atoms, MP4Types;

type
  TMp4Box = packed record
    Size: UInt32;
    FourCC: TMP4FourCC;
  end;

  TAtomList = class
  strict
  private
    FStream: TStream;
    FFilename: String;
    FAtomList: TAtomObjectList;
    FUnhandledList: TList<TMP4FourCC>;
    procedure DecodeAtom(const AStart: Int64 = 0; const ASize: Int64 = 0; const ALevel: Integer = 0; const AParent: TAtom = Nil);
    procedure AddChild(var Atom: TAtom);
    procedure AddFullChild(var Atom: TAtom);
    procedure DecodeStream;
  public
    constructor Create;
    function LoadFromFile(const AFilename: String): Boolean; overload;
    function LoadFromFile(const AFilepath: String; const AFilename: String): Boolean; overload;
    function Open: Boolean;
    property Atoms: TAtomObjectList read FAtomList write FAtomList;
    property Filename: String read FFilename write FFilename;
    property UnhandledList: TList<TMP4FourCC> read FUnhandledList write FUnhandledList;
  end;

implementation

uses
{$IF defined(QUICKLOG) and defined(DEBUG)}
  Quick.Logger,
{$ENDIF}
  MP4DerivedAtoms,
  IOUtils;

{ TAtomList }

procedure TAtomList.AddChild(var Atom: TAtom);
begin
  Atom.Children := TAtomObjectList.Create(True);
  DecodeAtom(Atom.AbsPos + 8, Atom.Size - 8, Atom.Level + 1, Atom);
end;

procedure TAtomList.AddFullChild(var Atom: TAtom);
begin
  Atom.Children := TAtomObjectList.Create(True);
  DecodeAtom(Atom.AbsPos + 12, Atom.Size - 12, Atom.Level + 1, Atom);
end;

constructor TAtomList.Create;
begin
  FAtomList := TAtomObjectList.Create(True);
  FUnhandledList := TList<TMP4FourCC>.Create;
end;

procedure TAtomList.DecodeAtom(const AStart: Int64 = 0; const ASize: Int64 = 0; const ALevel: Integer = 0; const AParent: TAtom = Nil);
var
  FPos,
  FEnd: Int64;
  B: TMp4Box;
  Size64: Int64;
  Atom: TAtom;
  AtomRec: TAtomRec;
begin
  FPos := AStart;
  FEnd := AStart + ASize;

  while FPos < FEnd do
    begin
      FStream.Position := FPos;
      FStream.Read(B, SizeOf(B));

      AtomRec.Parent := AParent;
      AtomRec.AbsPos := FPos;
      AtomRec.Level := ALevel;
      AtomRec.FourCC := SwapBytes32(B.FourCC);
      AtomRec.Size := SwapBytes32(B.Size);

      if ((AtomRec.AbsPos = 0) and not(AtomRec.FourCC = $66747970)) then
        begin
        { If at start of file we MUST have a FourCC of 'ftyp' ($66747970) }
        { otherwise the file is not an MP4 formatted file }
          Raise Exception.Create('File is not MP4 formatted');
        end;

      // Google 64bit mp4 atoms
      if AtomRec.Size = 1 then
        begin
          AtomRec.Is64Bit := True;
          if (FPos + SizeOf(B)) < FEnd then
            begin
              FStream.Position := FPos + SizeOf(B);
              FStream.Read(Size64, SizeOf(Int64));
              AtomRec.Size := SwapBytes64(Size64);
            end
          else
            Raise Exception.Create('Read overflow');
        end
      else
        AtomRec.Is64Bit := False;


      Atom := CreateAutoAtom(FStream, AtomRec);
      if Atom = Nil then
        begin
          Atom := TAtom.Create(AtomRec);
          FUnhandledList.Add(Atom.FourCC);
      end;

{$IF defined(QUICKLOG) and defined(DEBUG)}
    Log(IntToStr(FPos) + ' : ' + FourCCToString(Atom.FourCC) + ' = ' + Atom.ClassName, etInfo);
{$ENDIF}

     if AParent = Nil then
        FAtomList.Add(Atom)
      else
        AParent.Children.Add(Atom);

      if Atom.ClassType = TAtomContainer then
        begin
          AddChild(Atom);
        end
      else if Atom.ClassType = TAtomMeta then
        begin
          AddFullChild(Atom);
        end;

       FPos := FPos + AtomRec.Size;
    end;
end;

procedure TAtomList.DecodeStream;
begin
  DecodeAtom(0, FStream.Size);
end;

function TAtomList.LoadFromFile(const AFilename: String): Boolean;
begin
  FFilename := AFilename;
  Result := Open;
  if Not Result then
    FFilename := String.Empty;
end;

function TAtomList.LoadFromFile(const AFilepath: String; const AFilename: String): Boolean;
begin
  FFilename := TPath.Combine(IncludeTrailingPathDelimiter(AFilepath), AFilename);
  Result := Open;
  if Not Result then
    FFilename := String.Empty;
end;

function TAtomList.Open: Boolean;
var
  FS: Int64;
begin
  Result := False;

  if not(FFilename.IsEmpty) and FileExists(FFilename) then
    begin
      FS := TFile.GetSize(FFilename);
      if FS > 0 then
        begin
          try
            if FS < (1024*1024*1024) then
              begin
                FStream := TMemoryStream.Create as TStream;
                TMemoryStream(FStream).LoadFromFile(FFilename);
              end
            else
              FStream := TFileStream.Create(FFilename, fmOpenRead) as TStream;
          finally
            DecodeStream;
            // FUnhandledList.Sort;
            Result := True;
          end;
        end;
    end;
end;

end.
