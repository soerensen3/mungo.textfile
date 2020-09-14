unit mungo.textfile.messages;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, ExtCtrls,

  laz.VirtualTrees,

  mungo.textfile.sourceeditor,
  mungo.components.base,
  mungo.components.colors,
  mungo.intf.editor;

type

  { TIDEMessages }

  TIDEMessages = class ( TMessageIntf )
    private
      procedure MessagesDblClick(Sender: TObject);

    protected
      procedure SetContext(AValue: TEditor); override;

    public
      Messages: TLazVirtualStringTree;
      Images: TImageList;
//      ToolBar: TToolbar;

      constructor Create; override;

      function AddMessage(AMsgType: Integer; AText: String; ATextPos: TPoint): Pointer; override;
      procedure ClearMessages; override;

      procedure MessagesGetImageIndex(Sender: TBaseVirtualTree;
        Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
        var Ghosted: Boolean; var ImageIndex: Integer);
      procedure MessagesGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
        Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);

      class function GetToolName: String; override;
  end;

const
  M_SYMBOLS_INFO = 0;
  M_SYMBOLS_ERROR = 1;
  M_SYMBOLS_WARNING = 2;

implementation

{ TIDEMessages }

procedure TIDEMessages.MessagesDblClick(Sender: TObject);
var
  Data: PMessageData;
begin
  if ( Assigned( Messages.FocusedNode )) then begin
    Data:= Messages.GetNodeData( Messages.FocusedNode );
    if (( not ( Data^.TextPos.IsZero )) and ( Context is TSourceEditor )) then begin
      TSourceEditor( Context ).JumpTo( Data^.TextPos );
    end;
  end;
end;

procedure TIDEMessages.SetContext(AValue: TEditor);
begin
  inherited SetContext(AValue);
end;

constructor TIDEMessages.Create;
var
  Btn: TButton;
  Root: TWinControl;
begin
  inherited;

  Root:= Control as TWinControl;

  //SearchPaths:= TFilePointerList.Create( False );
  Messages:= TLazVirtualStringTree.Create( Root );
//  FileTree.OnGetText:= @FileTreeGetText;
//  FileTree.OnGetImageIndex:= @FileTreeGetImageIndex;
//  SourceTree.OnClick:=@FileTreeClick;
//  SourceTree.OnDblClick:= @FileTreeDblClick;

  Messages.Parent:= Root;
  Messages.Align:= alClient;
  Messages.BorderStyle:= bsNone;
  Messages.OnGetImageIndex:= @MessagesGetImageIndex;
  Messages.OnGetText:= @MessagesGetText;
  Images:= TImageList.Create( Root );
  Messages.Images:= Images;
  Messages.NodeDataSize:= SizeOf( TMessagesData );

  Messages.OnDblClick:=@MessagesDblClick;

  Images.AddResourceName( HINSTANCE, 'SYMBOLS-INFO' );
  Images.AddResourceName( HINSTANCE, 'SYMBOLS-ERROR' );
  Images.AddResourceName( HINSTANCE, 'SYMBOLS-WARNING' );

{  ToolBar:= TToolbar.Create( Root );
  ToolBar.Parent:= TWinControl( Root );
  ToolBar.ButtonWidth:= 30;
  ToolBar.Align:= alTop;
  ToolBar.LeftAligned:= False;
  ToolBar.BringToFront;

  ToolBar.Images:= TImageList.Create( ToolBar );
  ToolBar.Images.Width:= 16;
  ToolBar.Images.Height:= 16;

  ToolBar.Images.AddResourceName( HINSTANCE, 'SYMBOLS-FILTER' );

  Btn:= ToolBar.AddButton('Filter','Filter', 0 );
  Btn.ShowCaption:= False;}

  AddMessage( M_SYMBOLS_INFO, 'Info', Point( 0, 0 ));
  AddMessage( M_SYMBOLS_ERROR, 'Error', Point( 0, 0 ));
  AddMessage( M_SYMBOLS_WARNING, 'Warning', Point( 0, 0 ));
end;

function TIDEMessages.AddMessage(AMsgType: Integer; AText: String; ATextPos: TPoint): Pointer;
var
  Data: PMessageData;
begin
  Result:= Messages.AddChild( nil );
  Data:= Messages.GetNodeData( Result );
  Data^.Message:= AText;
  Data^.MsgType:= AMsgType;
  Data^.TextPos:= ATextPos;
end;

procedure TIDEMessages.ClearMessages;
begin
  Messages.Clear;
end;

procedure TIDEMessages.MessagesGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  Data: PMessageData;
begin
  Data:= Messages.GetNodeData( Node );
  ImageIndex:= Data^.MsgType;
end;

procedure TIDEMessages.MessagesGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  Data: PMessageData;
begin
  Data:= Messages.GetNodeData( Node );
  CellText:= Data^.Message;
end;

class function TIDEMessages.GetToolName: String;
begin
  Result:= 'Messages';
end;

end.

