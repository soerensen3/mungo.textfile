unit mungo.textfile.sourceeditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics,
  ATFlatToolbar, ExtCtrls,
  Dialogs,

  mungo.intf.editor,
  mungo.intf.filepointer,
  mungo.intf.textfile.sourceeditor,
  mungo.components.colors,
  mungo.components.base,

  LazFileUtils,

  SynEdit,
  SynEditTypes;

type

  { TSourceEditor }

  TSourceEditor = class ( TSourceEditorIntf )
  private
    procedure OnEditorStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    protected
      FEditor: TSynEdit;
      //FToolBar: TToolbar;
      FUpdateTimer: TTimer;
      FActFileSave: TAction;
      FActFileSaveAs: TAction;
      FActEditCopy: TAction;
      FActEditCut: TAction;
      FActEditFind: TAction;
      FActEditPaste: TAction;
      FActEditUndo: TAction;
      FActEditRedo: TAction;
      FSaveAsDlg: TSaveDialog;
      procedure FileChange(Sender: TObject); override;
      function GetEditor: TSynEdit;
      procedure ActionFileSave(Sender: TObject); override;
      procedure ActionFileSaveAs(Sender: TObject);
      procedure ActionEditCopy(Sender: TObject);
      procedure ActionEditCut(Sender: TObject);
      procedure ActionEditFind(Sender: TObject);
      procedure ActionEditPaste(Sender: TObject);
      procedure ActionEditUndo(Sender: TObject);
      procedure ActionEditRedo(Sender: TObject);
      procedure OnUpdateTimer(Sender: TObject); override;

    public
      constructor Create( ARootControl: TObject; AFileInfo: TFilePointer); override; overload;
      constructor Create; override; overload;
      destructor Destroy; override;

      procedure UpdateSyntaxTree; override;
      procedure EditorFocus; override;
      procedure JumpTo( ACursor: TPoint ); override;

      class function GetSuggestedFileExtension: String;

      property Editor: TSynEdit read FEditor;
      //property ToolBar: TToolbar read FToolBar;
      property UpdateTimer: TTimer read FUpdateTimer;
  end;

procedure Register;

implementation

{ TSourceEditor }

procedure TSourceEditor.ActionFileSave(Sender: TObject);
begin
  FEditor.Lines.SaveToFile( FileInfo.FileName );
  FEditor.MarkTextAsSaved;
  Modified:= False;
end;

procedure TSourceEditor.ActionFileSaveAs(Sender: TObject);
var
  FN: String;
begin
  FSaveAsDlg.FileName:= FileInfo.FileName;
  if ( FSaveAsDlg.Execute ) then begin
    FN:= FileInfo.FileName;
    FFileInfo:= FilePointers.GetFilePointer( FSaveAsDlg.FileName );
    ActionFileSave( Sender );
  end;
end;

procedure TSourceEditor.ActionEditCopy(Sender: TObject);
begin
  Editor.CopyToClipboard;
end;

procedure TSourceEditor.ActionEditCut(Sender: TObject);
begin
  Editor.CutToClipboard;
end;

procedure TSourceEditor.ActionEditFind(Sender: TObject);
begin

end;

procedure TSourceEditor.ActionEditPaste(Sender: TObject);
begin
  Editor.PasteFromClipboard();
end;

procedure TSourceEditor.ActionEditUndo(Sender: TObject);
begin

end;

procedure TSourceEditor.ActionEditRedo(Sender: TObject);
begin

end;

procedure TSourceEditor.UpdateSyntaxTree;
begin
  if ( Assigned( SourceTreeIntf )) then
    SourceTreeIntf.ClearNodes;
end;

procedure TSourceEditor.EditorFocus;
begin
  UpdateSyntaxTree;
end;

procedure TSourceEditor.JumpTo(ACursor: TPoint);
begin
  Editor.CaretXY:= ACursor;
  Editor.SetFocus;
end;

class function TSourceEditor.GetSuggestedFileExtension: String;
begin
  Result:= '.txt';
end;

procedure TSourceEditor.OnUpdateTimer(Sender: TObject);
begin
  UpdateSyntaxTree;
  FUpdateTimer.Enabled:= False;
end;

procedure TSourceEditor.OnEditorStatusChange(Sender: TObject; Changes: TSynStatusChanges);
begin
  if ( scSelection in Changes ) then begin
    FActEditCopy.Enabled:= FEditor.SelAvail;
    FActEditCut.Enabled:= FEditor.SelAvail;
    FActEditPaste.Enabled:= FEditor.CanPaste;
  end else if (( scModified in Changes ) or ( scFocus in Changes )) then begin
    FActEditUndo.Enabled:= FEditor.CanUndo;
    FActEditRedo.Enabled:= FEditor.CanRedo;
  end;
end;

procedure TSourceEditor.FileChange(Sender: TObject);
begin
  Modified:= Editor.Modified;
  FUpdateTimer.Enabled:= False; // Reset timer
  FUpdateTimer.Enabled:= True;
end;

function TSourceEditor.GetEditor: TSynEdit;
begin
  Result:= TSynEdit( Control );
end;


constructor TSourceEditor.Create(ARootControl: TObject; AFileInfo: TFilePointer);
begin
  inherited Create( ARootControl, AFileInfo );
end;

constructor TSourceEditor.Create;
var
  Root: TWinControl;
  Images: TImageList;
begin
  Root:= Control as TWinControl;

  FEditor:= TSynEdit.Create( Root );
  FEditor.Parent:= Root;
  FEditor.Font.Name:= 'JetBrains Mono';
  FEditor.Font.Size:= 12;
  FEditor.Align:= alClient;
  FEditor.LineHighlightColor.Background:= Gray100;
  FEditor.OnStatusChange:=@OnEditorStatusChange;

  Images:= TImageList.Create( Root );
  Images.Width:= 16;
  Images.Height:= 16;
  Images.AddResourceName( HINSTANCE, 'DOCUMENT-SAVE' ); //0
  Images.AddResourceName( HINSTANCE, 'DOCUMENT-SAVE-AS' ); //1
  Images.AddResourceName( HINSTANCE, 'EDIT-COPY' ); //2
  Images.AddResourceName( HINSTANCE, 'EDIT-CUT' ); //3
  Images.AddResourceName( HINSTANCE, 'EDIT-FIND' ); //4
  Images.AddResourceName( HINSTANCE, 'EDIT-PASTE' ); //5
  Images.AddResourceName( HINSTANCE, 'EDIT-UNDO' ); //6
  Images.AddResourceName( HINSTANCE, 'EDIT-REDO' ); //7

  {with( ToolBar.AddButton('','Save', 0 )) do begin
    ShowCaption:= False;
    OnClick:=@ActionFileSave;
  end;}

  FActFileSave:= TAction.Create( 'Save', @ActionFileSave, 'Save', 0 );
  FActFileSave.Images:= Images;
  FActFileSaveAs:= TAction.Create( 'Save As', @ActionFileSaveAs, 'Save As', 1 );
  FActFileSaveAs.Images:= Images;


  FActEditUndo:= TAction.Create( 'Undo', @ActionEditCut, 'Undo', 6 );
  FActEditUndo.Images:= Images;
  FActEditRedo:= TAction.Create( 'Redo', @ActionEditRedo, 'Redo', 7 );
  FActEditRedo.Images:= Images;

  FActEditCopy:= TAction.Create( 'Copy', @ActionEditCopy, 'Copy', 2 );
  FActEditCopy.Images:= Images;
  FActEditCut:= TAction.Create( 'Cut', @ActionEditCut, 'Cut', 3 );
  FActEditCut.Images:= Images;
  FActEditPaste:= TAction.Create( 'Paste', @ActionEditPaste, 'Paste', 5 );
  FActEditPaste.Images:= Images;
  FActEditFind:= TAction.Create( 'Find', @ActionEditFind, 'Find', 4 );
  FActEditFind.Images:= Images;

  FSaveAsDlg:= TSaveDialog.Create( Root );

  {with( ToolBar.AddButton('')) do begin
    ShowCaption:= False;
    Action:= FActFileSave;
  end;

  ToolBar.AddButton('','Save As', 1 ).ShowCaption:= False;
  ToolBar.AddSpacer();
  ToolBar.AddButton('','Undo', 6 ).ShowCaption:= False;
  ToolBar.AddButton('','Redo', 7 ).ShowCaption:= False;
  ToolBar.AddButton('','Copy', 2 ).ShowCaption:= False;
  ToolBar.AddButton('','Paste', 5 ).ShowCaption:= False;
  ToolBar.AddButton('','Cut', 3 ).ShowCaption:= False;
  ToolBar.AddButton('','Find', 4 ).ShowCaption:= False;

  ToolBar.BringToFront;}
  ToolBar:= EditorIntf.CreateToolBar( Root );
  ToolBar.AddButton( FActFileSave );
  ToolBar.AddButton( FActFileSaveAs );
  ToolBar.AddSpacer();
  ToolBar.AddButton( FActEditUndo );
  ToolBar.AddButton( FActEditRedo );
  ToolBar.AddButton( FActEditCopy );
  ToolBar.AddButton( FActEditCut );
  ToolBar.AddButton( FActEditPaste );
  ToolBar.AddButton( FActEditFind );



  if ( FileExists( FileInfo.FileName )) then
    Editor.Lines.LoadFromFile( FileInfo.FileName );
  Editor.OnChange:=@FileChange;

  FUpdateTimer:= TTimer.Create( Root );
  FUpdateTimer.Enabled:= False;
  FUpdateTimer.OnTimer:=@OnUpdateTimer;

  FActEditCopy.Enabled:= FEditor.SelAvail;
  FActEditCut.Enabled:= FEditor.SelAvail;
  FActEditPaste.Enabled:= FEditor.CanPaste;
  FActEditUndo.Enabled:= FEditor.CanUndo;
  FActEditRedo.Enabled:= FEditor.CanRedo;
end;

destructor TSourceEditor.Destroy;
begin
  Editor.Free;
  UpdateTimer.Free;
  FreeAndNil( FActFileSave );
  inherited Destroy;
end;


procedure Register;
begin

end;

end.

