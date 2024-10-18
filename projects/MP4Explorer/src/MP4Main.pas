unit MP4Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.ScrollBox, FMX.Memo, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts,
  Generics.Defaults, Generics.Collections, MP4Boxes, FMX.TreeView;

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
    procedure AutoTest;
    { Private declarations }
  public
    { Public declarations }
    Property AtomList: TAtomList read FAtomList write FAtomList;
  end;

var
  Form1: TForm1;
  MoovPos: UInt64;
  MoovSize: UInt64;

implementation

uses
  IOUtils, MP4Types;

{$R *.fmx}

procedure TForm1.AutoTest;
var
  FPath: String;
  FName: String;
begin
  {$IF defined(MSWINDOWS)}
  FPath := 'C:\Video\Discworld\City Watch';
  {$ELSEIF defined(MACOS)}
  FPath := '/Users/simon/Documents/Pratchett';
  {$ELSEIF defined(LINUX64)}
  FPath := '/home/simon/Documents/Pratchett';
  {$ENDIF}

  FName := 'Feet of Clay꞉ Discworld, Book 19.m4b';
 // FName := 'Guards_Guards.m4b';

  AtomList := TAtomList.Create;
  if AtomList.LoadFromFile(FPath, FName) then
    begin
      FillTree(AtomList);
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
      AItem.Parent := ParentItem;
//      AItem.ImageIndex := 2;
//      AItem.OnClick := TreeViewItemClick;
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
  AutoTest;
end;

end.
