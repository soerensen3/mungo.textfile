unit mungo.textfile.sourcetree;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, ExtCtrls,

  laz.VirtualTrees,

  mungo.components.base,
  mungo.components.colors,
  mungo.intf.editor;

type

  { TIDESourceTree }

  TIDESourceTree = class ( TSourceTreeIntf )
  private
    procedure ActFilterExecute(Sender: TObject);
    protected
      procedure SetContext(AValue: TEditor); override;

    public
      SourceTree: TLazVirtualStringTree;
      Images: TImageList;
      FActFilter: TAction;
      //ToolBar: TToolbar;

      constructor Create; override;
      destructor Destroy; override;

      function AddNode( AParent: Pointer; ANodeType: Integer; ACaption: String; ATextPos: TPoint ): Pointer; override;
      procedure RemoveNode(ANode: Pointer); override;
      procedure ClearNodes; override;

      procedure SourceTreeGetImageIndex(Sender: TBaseVirtualTree;
        Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
        var Ghosted: Boolean; var ImageIndex: Integer);
      procedure SourceTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
        Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);

      class function GetToolName: String; override;
  end;

const
  ST_SYMBOLS_LIBRARY = 0;
  ST_SYMBOLS_CLASS = 1;
  ST_SYMBOLS_STRUCT = 2;
  ST_SYMBOLS_OBJECT = 3;
  ST_SYMBOLS_NODE = 4;
  ST_SYMBOLS_PROCEDURE = 5;
  ST_SYMBOLS_FUNCTION = 6;
  ST_SYMBOLS_METHOD = 7;
  ST_SYMBOLS_FIELD = 8;
  ST_SYMBOLS_PROPERTY = 9;

implementation

uses
  mungo.textfile.sourceeditor;

{ TIDESourceTree }

procedure TIDESourceTree.ActFilterExecute(Sender: TObject);
begin

end;

procedure TIDESourceTree.SetContext(AValue: TEditor);
begin
  inherited SetContext(AValue);
  if ( Context is TSourceEditor ) then
    TSourceEditor( Context ).UpdateSyntaxTree;
end;

constructor TIDESourceTree.Create;
var
  Btn: TButton;
  Root: TWinControl;
begin
  inherited;

  Root:= Control as TWinControl;

  //SearchPaths:= TFilePointerList.Create( False );
  SourceTree:= TLazVirtualStringTree.Create( Root );
//  FileTree.OnGetText:= @FileTreeGetText;
//  FileTree.OnGetImageIndex:= @FileTreeGetImageIndex;
//  SourceTree.OnClick:=@FileTreeClick;
//  SourceTree.OnDblClick:= @FileTreeDblClick;

  SourceTree.Parent:= Root;
  SourceTree.BorderStyle:= bsNone;
  SourceTree.Align:= alClient;
  SourceTree.OnGetImageIndex:= @SourceTreeGetImageIndex;
  SourceTree.OnGetText:= @SourceTreeGetText;
  Images:= TImageList.Create( Root );
  SourceTree.Images:= Images;
  SourceTree.NodeDataSize:= SizeOf( TSourceTreeNodeData );
  Images.AddResourceName( HINSTANCE, 'SYMBOLS-LIBRARY' );
  Images.AddResourceName( HINSTANCE, 'SYMBOLS-CLASS' );
  Images.AddResourceName( HINSTANCE, 'SYMBOLS-STRUCT' );
  Images.AddResourceName( HINSTANCE, 'SYMBOLS-OBJECT' );
  Images.AddResourceName( HINSTANCE, 'SYMBOLS-NODE' );
  Images.AddResourceName( HINSTANCE, 'SYMBOLS-PROCEDURE' );
  Images.AddResourceName( HINSTANCE, 'SYMBOLS-FUNCTION' );
  Images.AddResourceName( HINSTANCE, 'SYMBOLS-METHOD' );
  Images.AddResourceName( HINSTANCE, 'SYMBOLS-FIELD' );
  Images.AddResourceName( HINSTANCE, 'SYMBOLS-PROPERTY' );

  Images:= TImageList.Create( Root );
  Images.Width:= 16;
  Images.Height:= 16;

  Images.AddResourceName( HINSTANCE, 'SYMBOLS-FILTER' );

  FActFilter:= TAction.Create( 'Filter', @ActFilterExecute, 'Filter', 0 );
  FActFilter.Images:= Images;

  ToolBar:= EditorIntf.CreateToolBar( Root );
  ToolBar.AddButton( FActFilter );

//  Btn.OnClick:=@RefreshAction;

  AddNode( nil, ST_SYMBOLS_LIBRARY, 'Library', Point( 0, 0 ));
  AddNode( nil, ST_SYMBOLS_CLASS, 'Class', Point( 0, 0 ));
  AddNode( nil, ST_SYMBOLS_STRUCT, 'Struct', Point( 0, 0 ));
  AddNode( nil, ST_SYMBOLS_OBJECT, 'Object', Point( 0, 0 ));
  AddNode( nil, ST_SYMBOLS_NODE, 'Node', Point( 0, 0 ));
  AddNode( nil, ST_SYMBOLS_PROCEDURE, 'Procedure', Point( 0, 0 ));
  AddNode( nil, ST_SYMBOLS_FUNCTION, 'Function', Point( 0, 0 ));
  AddNode( nil, ST_SYMBOLS_METHOD, 'Method', Point( 0, 0 ));
  AddNode( nil, ST_SYMBOLS_FIELD, 'Field', Point( 0, 0 ));
  AddNode( nil, ST_SYMBOLS_PROPERTY, 'Property', Point( 0, 0 ));
end;

destructor TIDESourceTree.Destroy;
begin
  FreeAndNil( FActFilter );
  inherited Destroy;
end;

function TIDESourceTree.AddNode(AParent: Pointer; ANodeType: Integer; ACaption: String; ATextPos: TPoint): Pointer;
var
  Data: PSourceTreeNodeData;
begin
  Result:= SourceTree.AddChild( AParent );
  Data:= SourceTree.GetNodeData( Result );
  Data^.Caption:= ACaption;
  Data^.NodeType:= ANodeType;
  Data^.TextPos:= ATextPos;
end;

procedure TIDESourceTree.RemoveNode(ANode: Pointer);
begin

end;

procedure TIDESourceTree.ClearNodes;
begin
  SourceTree.Clear;
end;

procedure TIDESourceTree.SourceTreeGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  Data: PSourceTreeNodeData;
begin
  Data:= SourceTree.GetNodeData( Node );
  ImageIndex:= Data^.NodeType;
end;

procedure TIDESourceTree.SourceTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  Data: PSourceTreeNodeData;
begin
  Data:= SourceTree.GetNodeData( Node );
  CellText:= Data^.Caption;
end;

class function TIDESourceTree.GetToolName: String;
begin
  Result:= 'Source Tree';
end;

end.

