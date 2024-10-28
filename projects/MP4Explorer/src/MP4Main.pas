unit MP4Main;

{$DEFINE QUICKLOG}

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.ScrollBox, FMX.Memo, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts,
  Generics.Defaults, Generics.Collections, MP4Boxes, MP4Atoms, FMX.TreeView;

type
  TForm1 = class(TForm)
    Layout1: TLayout;
    Layout2: TLayout;
    Button1: TButton;
    BoxTree: TTreeView;
    Layout3: TLayout;
    Label1: TLabel;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
  private
    FAtomList: TAtomList;
    procedure FillTree(const List: TAtomList);
    function AddBoxNodes(const ParentItem: TTreeViewItem;
      const List: TAtomObjectList): TTreeViewItem;
    procedure BoxTreeItemClick(Sender: TObject);
    procedure AutoTest;
    { Private declarations }
  public
    { Public declarations }
    Property AtomList: TAtomList read FAtomList write FAtomList;
  end;

var
  Form1: TForm1;

implementation

uses
{$IF defined(QUICKLOG) and defined(DEBUG)}
    Quick.Logger,
    Quick.Logger.Provider.Files,
{$ENDIF}
  MP4DerivedAtoms,
  RTTI, TypInfo,
  IOUtils, MP4Types;

{$R *.fmx}

procedure TForm1.AutoTest;
var
  FPath: String;
  FName: String;
  I: Integer;
begin
  {$IF defined(MSWINDOWS)}
  FPath := 'C:\Video\Discworld\City Watch';
  {$ELSEIF defined(MACOS)}
  FPath := '/Users/simon/Documents/Pratchett';
  {$ELSEIF defined(LINUX64)}
  FPath := '/home/simon/Documents/Pratchett';
  {$ENDIF}

  FName := 'Feet of Clay꞉ Discworld, Book 19.m4b';
  FName := 'Guards_Guards.m4b';
//  FName := 'Night Watch.m4b';

  {$IF defined(mmMSWINDOWS)}

  FPath := 'C:\Users\simon\Music\Audible\Export';
  FName := 'Interesting Times_B09LZ2RNYV_LC_128_44100_Stereo.aax';
//  FName := 'Love Triangle_B0CKTM55D7_LC_128_44100_Stereo.aax';

//  FPath := 'C:\Users\simon\Desktop\iPlayer Recordings';
//  FName := 'The_Fall_of_the_Roman_Empire.mp4';
  {$ENDIF}

{$IF defined(QUICKLOG) and defined(DEBUG)}
  Log('Path = ' + FPath, etInfo);
  Log('File = ' + FName, etInfo);
{$ENDIF}

   AtomList := TAtomList.Create;
  if AtomList.LoadFromFile(FPath, FName) then
    begin
      FillTree(AtomList);
      if AtomList.UnhandledList.Count > 0 then
        begin
          for I := 0 to AtomList.UnhandledList.Count - 1 do
            begin
              Memo1.Lines.Add(FourCCToString(AtomList.UnhandledList[I]));
            end;
        end;
    end;

end;

procedure TForm1.BoxTreeItemClick(Sender: TObject);
var
  AItem: TTreeViewItem;
  Atom: TAtom;
  Ctx : TRttiContext;
  T : TRttiType;
  P : TRttiProperty;
  V: TValue;
  L: TMP4MetaDataList;
  I: TMP4MetaData;
begin
  L := Nil;
  if (Sender is TTreeViewItem) then
    begin
      AItem := Sender as TTreeViewItem;
      if AItem.TagObject is TAtom then
        begin
          Atom := AItem.TagObject as TAtom;
          if Assigned(Atom) then
            begin
              if Atom is TAtomIlst then
                L := TAtomIlst(Atom).List;

              Memo1.Lines.Clear;
              ctx := TRttiContext.Create();
              try
                T := ctx.GetType(Atom.ClassType);
                Memo1.Lines.Add('Type : ' + T.ToString);

                for P in T.GetProperties() do
                  begin
                  {
                  if P.PropertyType.ToString = 'TObjectList<MP4Atoms.TMP4MetaData>' then
                      L := TAtomIlst(Atom).List;
                      }
                    if not (P.Visibility in [mvPublished]) then
                      Continue;
//                    if not P.IsReadable then
//                      continue;

                    V := P.GetValue(Atom);
                    {
                    if P.Name = 'FourCC' then
                    begin
                      Memo1.Lines.Add(P.Name + ' : ' + FourCCToString(V.AsInteger));
                    end
                    else
                    }
                    Memo1.Lines.Add(P.Name + ' : ' +
                                    V.ToString + ' : ' +
                                    P.PropertyType.ToString);

                  end;
                if L <> Nil then
                  for I in L do
                    begin
                      case I.DataType of
                        1: Memo1.Lines.Add(FourCCToString(I.FourCC) + ' = ' + TEncoding.UTF8.GetString(I.Data));
                        2: Memo1.Lines.Add(FourCCToString(I.FourCC) + ' = ' + TEncoding.Unicode.GetString(I.Data));
                        4: Memo1.Lines.Add(FourCCToString(I.FourCC) + ' = ' + TEncoding.UTF8.GetString(I.Data));
                        5: Memo1.Lines.Add(FourCCToString(I.FourCC) + ' = ' + TEncoding.Unicode.GetString(I.Data));
                      else
                        Memo1.Lines.Add(FourCCToString(I.FourCC) + ' = ' + IntToStr(I.DataType));
                      end;
                    end;
              finally
                ctx.Free();
              end;
            end;
        end;
    end;

end;

function TForm1.AddBoxNodes(const ParentItem: TTreeViewItem; const List: TAtomObjectList): TTreeViewItem;
var
  Atom: TAtom;
  AItem: TTreeViewItem;
  I: Integer;
begin
  Result := Nil;
  for I := 0 to List.Count - 1 do
    begin
      Atom := List[I];
      AItem := TTreeViewItem.Create(Self);
      AItem.Text := FourCCToString(Atom.FourCC);
      AItem.TagObject := Atom;
      if Atom.ClassType = TAtom then
        AItem.ResultingTextSettings.FontColor:= TAlphaColorRec.Red
      else if Atom.IsHandled then
        AItem.ResultingTextSettings.FontColor:= TAlphaColorRec.Green;

      AItem.Parent := ParentItem;
//      AItem.ImageIndex := 2;
      AItem.OnClick := BoxTreeItemClick;
      if Atom.Children <> Nil then
        AddBoxNodes(AItem, Atom.Children);
    end;
end;

procedure TForm1.FillTree(const List: TAtomList);
var
  Node: TTreeViewItem;
begin
  BoxTree.BeginUpdate;
  Node := TTreeViewItem.Create(Self);
  Node.Text := List.Filename;
  Node.OnClick := BoxTreeItemClick;
  Node.Parent := BoxTree;
  Node.Expand;
  if Assigned(AtomList) then
     begin
      AddBoxNodes(Node, List.Atoms);
    end;
  BoxTree.ExpandAll;
  BoxTree.EndUpdate;
end;


procedure TForm1.FormCreate(Sender: TObject);
begin
{$IF defined(QUICKLOG) and defined(DEBUG)}
  Logger.Providers.Add(GlobalLogFileProvider);
  with GlobalLogFileProvider do
 	begin
    FileName := 'Logger.log';
    if FileExists(FileName) then
      DeleteFile(FileName);
    LogLevel := LOG_ALL;
    Enabled := True;
  end;
{$ENDIF}
  AutoTest;
end;

end.
