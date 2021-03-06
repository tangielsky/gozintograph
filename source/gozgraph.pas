unit gozgraph;

{$mode objfpc}{$H+}


{
Component to view a gozintograph structure


https://techpluscode.de/erzeugnisstruktur-visualisieren-als-gozintograph/
https://techpluscode.de/produktstruktur-analyse-im-gozintograph-viewer
https://github.com/tangielsky/tag/gozintograph

(C) 2021 Thomas Angielsky


}

interface

uses
  Classes, SysUtils, Graphics, Controls, LMessages, LCLType, LCLIntf, Math,
  Dialogs, ComCtrls, ComboEx;


type

  TGozItemType = (gitUndefined,gitItem,gitModule,gitProduct);
  TGozItemShape = (gisCircle,gisMixed);


  TGozLinkInformation = record
    Items : integer;
    Quantity : extended;
  end;

  TGozProperty = class
  private
    FId : integer;
  public
    constructor Create;
  published
    property Id : integer read FId write FId;
  end;


  { TGozFieldProperty }

  TGozFieldProperty = class(TGozProperty)
  private
    FName : string;
  public
    constructor Create;
  published
    property Name : string read FName write FName;
  end;

  { TGozValueProperty }

  TGozValueProperty = class(TGozProperty)
  private
    FField : TGozFieldProperty;
    FValue : string;
    procedure SetValue(AValue: string);
  public
    constructor Create;
    function GetFloatValue : float;
  published
    property Field : TGozFieldProperty read FField write FField;
    property Value : string read FValue write SetValue;
  end;


  { TGozPropertyList }

  TGozPropertyList = class
  private
    IdCount : integer;
    FList : TList;
    function ExistsFieldId(Id: integer): TGozFieldProperty;
    function GetId(Field: string): integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function AddField(Field: string): TGozFieldProperty;
    function AddValue(FieldProperty : TGozFieldProperty; Value : string) : TGozValueProperty;
    function ExistsValueId(Id: integer): TGozValueProperty;
    function GetValue(Id : integer) : string;
    function GetValueAsColor(Id : integer) : TColor;
    function GetValueAsFloat(Id: integer): float;
    procedure UpdateComboboxEx(ComboboxEx: TComboboxEx);
    procedure UpdateListview(Listview: TListview);
  published
    property List : TList read FList write FList;
  end;

  { TGozGraphConnection }

  TGozGraphConnection = class
  private
    FDestination : string;
    FHighlighted : boolean;
    FQuantity : extended;
    FSelected: boolean;
    FShowText : boolean;
    FSource : string;
    Fx1 : longint;
    Fx2 : longint;
    Fy1 : longint;
    Fy2 : longint;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property Destination : string read FDestination write FDestination;
    property Quantity : extended read FQuantity write FQuantity;
    property Selected : boolean read FSelected write FSelected;
    property ShowText : boolean read FShowText write FShowText;
    property Source : string read FSource write FSource;
    property x1 : longint read Fx1 write Fx1;
    property x2 : longint read Fx2 write Fx2;
    property y1 : longint read Fy1 write Fy1;
    property y2 : longint read Fy2 write Fy2;
  end;


  { TGozGraphLink }

  TGozGraphLink = class(TPersistent)
  private
    FDestination : string;
    FQuantity : extended;
    FSelected : boolean;
    FShowText : boolean;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property Destination: string read FDestination write FDestination;
    property Quantity: extended read FQuantity write FQuantity;
    property Selected: boolean read FSelected write FSelected;
    property ShowText: boolean read FShowText write FShowText;
  end;


  { TGozGraphItem }

  TGozGraphItem = class(TPersistent)
  private
    FCaption: string;
    FColor2 : TColor;
    FDescription : string;
    FHeight : integer;
    FItemType : TGozItemType;
    FLevel : integer;
    FLinks: TList;
    FPainted : boolean;
    FProperties : TGozPropertyList;
    FSelected: boolean;
    FWidth : integer;
    Fx : longint;
    Fy : longint;
  public
    IngoingInformation : TGozLinkInformation;
    OutgoingInformation : TGozLinkInformation;

    constructor Create;
    destructor Destroy; override;

    function ItemTypeColor : TColor;
    function ItemTypeCaption : string;
    procedure AddLink(Destination : string; Quantity : extended);
    procedure Clear;
    procedure SetSize(w,h : integer);
  published
    property Caption : string read FCaption write FCaption;
    property Color2: TColor read FColor2 write FColor2;
    property Description : string read FDescription write FDescription;
    property Height : integer read FHeight write FHeight;
    property ItemType : TGozItemType read FItemType write FItemType;
    property Level : integer read FLevel write FLevel;
    property Links : TList read FLinks write FLinks;
    property Painted : boolean read FPainted write FPainted;
    property Properties : TGozPropertyList read FProperties write FProperties;
    property Selected : boolean read FSelected write FSelected;
    property Width : integer read FWidth write FWidth;
    property x : longint read Fx write Fx;
    property y : longint read Fy write Fy;
  end;


  TOnGozGraphItemSelected = procedure(item : TGozGraphItem) of object;


  { TGozIntoGraph }
  TGozIntoGraph = class(TCustomControl)
    private
      FColor : TColor;
      FConnections : TList;
      FDefaultItemHeight : integer;
      FDefaultItemWidth : integer;
      FDistanceX : integer;
      FDistanceY : integer;
      FFilename : string;
      FFontSize : integer;
      FHighlightColor : TColor;
      FItems: TList;
      FItemShape : TGozItemShape;
      FLinkColor : TColor;
      FLinkHighlightColor : TColor;
      FLinkTextColor : TColor;
      FOnItemSelected: TOnGozGraphItemSelected;
      FProperties : TGozPropertyList;
      FScaleFactor : double;
      FSelectionAllDown : boolean;
      FSelectionAllUp : boolean;
      FShowCaptions : boolean;
      FShowLinks : boolean;
      FShowQuantities : boolean;
      FTextColor : TColor;

      xmax, ymax : longint;
      MovedItem : TGozGraphItem;
      MovedItemX, MovedItemY : longint;
      Moving : boolean;

      function ReScale(value: double): longint;
      function Scale(value: double): longint;
      function AddItem(ACaption: string): boolean;

      procedure AddConnection(Source, Destination: string; Quantity: extended; x1,y1,x2,y2 : longint; Selected : boolean);
      procedure ArrowTo(xa, ya, xe, ye, pb, pl: integer; Fill: boolean);
      procedure CalcLinks(item: TGozGraphItem; x, y: longint; level: integer);
      procedure ClearConnections;
      procedure DrawItems;
      procedure DrawLinks;
      procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
      procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
      procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
      procedure SelectLinksDown(searchitem: TGozGraphItem);
      procedure SelectLinksUp(searchitem: TGozGraphItem);
      procedure SetScaleFactor(AValue: double);
      procedure SetSelectionAllDown(AValue: boolean);
      procedure SetSelectionAllUp(AValue: boolean);
      procedure SetShowCaptions(AValue: boolean);
      procedure SetShowLinks(AValue: boolean);
      procedure SetShowQuantities(AValue: boolean);
      procedure UpdateItem(item: TGozGraphItem);
    protected
      procedure Paint; override;
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;

      function CountItemType(AItemType: TGozItemType; MultipleUse: boolean): integer;
      function ExistsItem(ACaption: string): boolean;
      function GetIngoingInformation(ACaption: string): TGozLinkInformation;
      function GetOutgoingInformation(ACaption: string): TGozLinkInformation;
      function GetItem(ACaption: string): TGozGraphItem;
      function GetItemAt(X, Y: integer): TGozGraphItem;
      function HasLinks(item: TGozGraphItem): boolean;
      function ImportFromCsvFile(AFilename: string; Delimiter: string;
        HasHeaders: boolean; PosSource, PosDestination, PosQuantity: integer
        ): boolean;
      function ImportPropsFromCsvFile(AFilename: string; Delimiter: string;
        HasHeaders: boolean; PosItem: integer; Listview : TListview ): boolean;
      function MaxLevel : integer;

      procedure AddLink(ParentItem, ChildItem : string; Quantity: extended);
      procedure Autolayout;
      procedure Clear;
      procedure ClearSelections;
      procedure Invalidate; override;
      procedure SelectItems(startitem: TGozGraphItem);
      procedure SetScaleFactorFromWidth(NewWidth: longint);
      procedure UpdateConnections;
      procedure UpdatePropertyListview(Listview: TListview; item: TGozGraphItem);

    published
      property Color : TColor read FColor write FColor;
      property Connections : TList read FConnections write FConnections;
      property DefaultItemHeight : integer read FDefaultItemHeight write FDefaultItemHeight;
      property DefaultItemWidth : integer read FDefaultItemWidth write FDefaultItemWidth;
      property DistanceX : integer read FDistanceX write FDistanceX;
      property DistanceY : integer read FDistanceY write FDistanceY;
      property Filename : string read FFilename write FFilename;
      property FontSize : integer read FFontSize write FFontSize;
      property HighlightColor : TColor read FHighlightColor write FHighlightColor;
      property Items : TList read FItems write FItems;
      property ItemShape : TGozItemShape read FItemShape write FItemShape;
      property LinkColor : TColor read FLinkColor write FLinkColor;
      property LinkHighlightColor : TColor read FLinkHighlightColor write FLinkHighlightColor;
      property LinkTextColor : TColor read FLinkTextColor write FLinkTextColor;
      property OnItemSelected: TOnGozGraphItemSelected read FOnItemSelected write FOnItemSelected;
      property Properties : TGozPropertyList read FProperties write FProperties;
      property ScaleFactor : double read FScaleFactor write SetScaleFactor;
      property SelectionAllDown : boolean read FSelectionAllDown write SetSelectionAllDown;
      property SelectionAllUp : boolean read FSelectionAllUp write SetSelectionAllUp;
      property ShowCaptions : boolean read FShowCaptions write SetShowCaptions;
      property ShowLinks : boolean read FShowLinks write SetShowLinks;
      property ShowQuantities : boolean read FShowQuantities write SetShowQuantities;
      property TextColor : TColor read FTextColor write FTextColor;
    end;

var
  gozProductColor : TColor;
  gozModuleColor : TColor;
  gozItemColor : TColor;



implementation

uses gozfunc;

{ TGozFieldProperty }

constructor TGozFieldProperty.Create;
begin
  inherited Create;
  FName:='';
end;

{ TGozValueProperty }

procedure TGozValueProperty.SetValue(AValue: string);
begin
  if AValue<>FValue then FValue:=AValue;
end;

constructor TGozValueProperty.Create;
begin
  inherited Create;
  FField:=nil;
  FValue:='';
end;

function TGozValueProperty.GetFloatValue: float;
begin
  result:=StrToFloatDef(FValue,0);
end;

{ TGozPropertyList }

constructor TGozPropertyList.Create;
begin
  inherited Create;

  IdCount:=0;
  FList:=TList.Create;
end;

destructor TGozPropertyList.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

procedure TGozPropertyList.Clear;
var
  i : integer;
  prop : TGozFieldProperty;
begin
  for i:=0 to FList.Count-1 do
    begin
      prop:=TGozFieldProperty(FList[i]);
      prop.Free;
    end;
  FList.Clear;
end;

function TGozPropertyList.GetId(Field : string) : integer;
var
  i : integer;
  prop : TGozFieldProperty;
begin
  result:=-1;
  for i:=0 to FList.Count-1 do
    begin
      prop:=TGozFieldProperty(FList[i]);
      if Uppercase(Field)=UpperCase(prop.Name) then
        begin
          result:=prop.Id;
          exit;
        end;
    end;
end;

function TGozPropertyList.ExistsFieldId(Id : integer) : TGozFieldProperty;
var
  i : integer;
  GozProperty : TGozFieldProperty;
begin
  result:=nil;
  for i:=0 to FList.Count-1 do
    begin
      GozProperty:=TGozFieldProperty(FList[i]);
      if Id=GozProperty.Id then
        begin
          result:=GozProperty;
          exit;
        end;
    end;
end;

function TGozPropertyList.ExistsValueId(Id : integer) : TGozValueProperty;
var
  i : integer;
  GozProperty : TGozValueProperty;
begin
  result:=nil;
  for i:=0 to FList.Count-1 do
    begin
      GozProperty:=TGozValueProperty(FList[i]);
      if Id=GozProperty.Id then
        begin
          result:=GozProperty;
          exit;
        end;
    end;
end;

function TGozPropertyList.AddField(Field: string): TGozFieldProperty;
var
  GozProperty : TGozFieldProperty;
begin
  IdCount:=IdCount+1;
  GozProperty:=TGozFieldProperty.Create;
  GozProperty.Id:=IdCount;
  GozProperty.Name:=Field;
  FList.Add(GozProperty);

  result:=GozProperty;
end;

function TGozPropertyList.AddValue(FieldProperty: TGozFieldProperty;
  Value: string): TGozValueProperty;
var
  prop : TGozValueProperty;
begin
  if FieldProperty<>nil then
    begin
      prop:=ExistsValueId(FieldProperty.Id);
      if prop=nil then
        begin
          prop:=TGozValueProperty.Create;
          prop.Id:=FieldProperty.Id;
          prop.Value:=Value;
          prop.Field:=FieldProperty;
          FList.Add(prop);
        end
      else prop.Value:=Value;
      result:=prop;
    end else result:=nil;
end;


function TGozPropertyList.GetValue(Id: integer): string;
var
  i : integer;
  GozProperty : TGozValueProperty;
begin
  result:='';
  for i:=0 to FList.Count-1 do
    begin
      GozProperty:=TGozValueProperty(FList[i]);
      if Id=GozProperty.Id then
        begin
          result:=GozProperty.Value;
          exit;
        end;
    end;
end;

function TGozPropertyList.GetValueAsColor(Id: integer): TColor;
begin
  result:=StringToColorDef(GetValue(Id),clNone);
end;

function TGozPropertyList.GetValueAsFloat(Id: integer): float;
begin
  result:=StrToFloatDef(GetValue(Id),0);
end;


procedure TGozPropertyList.UpdateListview(Listview: TListview);
var
  i : integer;
  GozProperty : TGozFieldProperty;
  item : TListitem;
begin
  for i:=0 to FList.Count-1 do
    begin
      GozProperty:=TGozFieldProperty(FList[i]);
      item:=Listview.Items.Add;
      item.ImageIndex:=0;
      item.Caption:=GozProperty.Name;
      item.Data:=GozProperty;
    end;
end;

procedure TGozPropertyList.UpdateComboboxEx(ComboboxEx: TComboboxEx);
var
  i : integer;
  GozProperty : TGozFieldProperty;
begin
  for i:=0 to FList.Count-1 do
    begin
      GozProperty:=TGozFieldProperty(FList[i]);
      ComboboxEx.ItemsEx.AddItem(GozProperty.Name,0,0,0,0,GozProperty);
    end;
end;

{ TGozProperty }

constructor TGozProperty.Create;
begin
  inherited Create;
  FId:=-1;
end;


{ TGozGraphConnection }

constructor TGozGraphConnection.Create;
begin
end;

destructor TGozGraphConnection.Destroy;
begin
  inherited Destroy;
end;

{ TGozIntoGraph }


constructor TGozIntoGraph.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FItems:=TList.Create;
  FConnections:=TList.Create;
  FProperties:=TGozPropertyList.Create;

  FProperties.AddField('Beschreibung');

  FColor:=clWhite;
  FTextColor:=clBlack;
  FHighlightColor:=clYellow;
  FLinkColor:=clSilver;
  FLinkTextColor:=clSilver;
  FLinkHighlightColor:=clBlack;

  FDefaultItemHeight:=50;
  FDefaultItemWidth:=50;
  FDistanceX:=75;
  FDistanceY:=150;

  FScaleFactor:=1.0;
  FSelectionAllUp:=false;
  FSelectionAllDown:=false;

  FShowCaptions:=true;
  FShowLinks:=true;
  FShowQuantities:=true;

  FFontsize:=9;
  Canvas.Font.Size:=Scale(FFontsize);
end;

destructor TGozIntoGraph.Destroy;
begin
  ClearConnections;
  FConnections.Free;

  Clear;
  FItems.Free;
  FProperties.Free;

  inherited Destroy;
end;

function TGozIntoGraph.HasLinks(item : TGozGraphItem) : boolean;
begin
  result:=false;
  if item<>nil then result:=item.Links.Count>0;
end;


procedure TGozIntoGraph.SelectLinksDown(searchitem : TGozGraphItem);
var
  j : integer;
  link : TGozGraphLink;
  linkitem : TGozGraphItem;
begin
  if searchitem=nil then exit;
  if searchitem.Selected then exit;

  searchitem.Selected:=true;
  for j:=0 to searchitem.Links.Count-1 do
    begin
      link:=TGozGraphLink(searchitem.Links[j]);
      linkitem:=GetItem(link.Destination);
      SelectLinksDown(linkitem);
    end;
end;


procedure TGozIntoGraph.SelectLinksUp(searchitem : TGozGraphItem);
var
  i,j : integer;
  link : TGozGraphLink;
  item : TGozGraphItem;
begin
  if searchitem=nil then exit;
  if searchitem.Selected then exit;

  searchitem.Selected:=true;

  for i:=0 to FItems.Count-1 do
    begin
      item:=TGozGraphItem(FItems[i]);
      if (item<>nil) and (item.Caption<>searchitem.Caption) then
        begin
          for j:=0 to item.Links.Count-1 do
            begin
              link:=TGozGraphLink(item.Links[j]);
              if link.Destination=searchitem.Caption then
                SelectLinksUp(item);
            end;
        end;
    end;

end;

procedure TGozIntoGraph.SelectItems(startitem : TGozGraphItem);
begin
  if startitem=nil then exit;

  if FSelectionAllDown then SelectLinksDown(startitem);
  if FSelectionAllUp then
    begin
       startitem.Selected:=false;
       SelectLinksUp(startitem);
    end;

  UpdateConnections;

  startitem.Selected:=true;
end;

procedure TGozIntoGraph.CalcLinks(item : TGozGraphItem; x,y : longint; level : integer);
var
  j,k : integer;
  x0,y0 : longint;
  link : TGozGraphLink;
  linkitem : TGozGraphItem;
begin
  item.Painted:=true;

  if item.Level=-1 then item.Level:=level
  else if item.Level<level then item.Level:=level;
  x0:=x;
  y0:=y+(item.Height+FDistanceY);
  for j:=0 to item.Links.Count-1 do
    begin
      link:=TGozGraphLink(item.Links[j]);
      linkitem:=GetItem(link.Destination);
      if (linkitem<>nil) then
        begin
          if (linkitem.Painted=false) then
            begin
              CalcLinks(linkitem,x0,y0,level+1);
              if linkitem.Height>item.Height then y0:=y+(linkitem.Height+FDistanceY);
              x0:=xmax;
              if j<item.Links.Count-1 then x0:=x0+(linkitem.Width+FDistanceX);
            end
          else
            begin
              //at multiuse always max of level
              if linkitem.y<y0 then
                begin
                  linkitem.y:=y0;
                  linkitem.Level:=level;
                end;
            end;
        end;
    end;

  if x0>xmax then xmax:=x0;
  if y0>ymax then ymax:=y0;

  item.x:=x;
  item.y:=y;

end;

procedure TGozIntoGraph.Autolayout;
var
  i : integer;
  x,y : longint;
  item : TGozGraphItem;
begin
  ClearConnections;
  xmax:=0;
  ymax:=0;
  x:=FDistanceX;
  y:=Canvas.TextHeight('Ag');

  for i:=0 to FItems.Count-1 do
    begin
      item:=TGozGraphItem(FItems[i]);
      item.Painted:=false;
      item.Level:=-1;
    end;

  for i:=0 to FItems.Count-1 do
    begin
      item:=TGozGraphItem(FItems[i]);
      if item.ItemType=gitProduct then
        begin
          CalcLinks(item,x,y,0);
          if xmax>x then x:=xmax;
          if i<FItems.Count-1 then x:=x+(item.Width+FDistanceX);
        end;
    end;

  Width:=Scale(xmax+(FDistanceX+FDefaultItemWidth));
  Height:=Scale(ymax+FDefaultItemHeight);

  UpdateConnections;
  Invalidate;
end;


procedure TGozIntoGraph.Paint;
var
  TxtH : integer;
begin
  inherited Paint;


  Canvas.AntialiasingMode:=amOff;

  Canvas.Brush.Style:=bsSolid;
  Canvas.Brush.Color:=FColor;
  Canvas.FillRect(ClientRect);

  if FShowLinks then DrawLinks;
  DrawItems;
end;

function TGozIntoGraph.Scale(value : double) : longint;
begin
  result:=Round(value*FScaleFactor);
end;

function TGozIntoGraph.ReScale(value : double) : longint;
begin
  result:=Round(value/FScaleFactor);
end;


procedure TGozIntoGraph.SetScaleFactor(AValue: double);
begin
  if FScaleFactor=AValue then Exit;
  FScaleFactor:=AValue;

  Width:=Scale(xmax+(FDistanceX+FDefaultItemWidth));
  Height:=Scale(ymax+FDefaultItemHeight);
  Canvas.Font.Size:=Scale(FFontsize);

  Invalidate;
end;


procedure TGozIntoGraph.SetScaleFactorFromWidth(NewWidth: longint);
var
  f : double;
begin
  f:=FScaleFactor/Width*NewWidth;
  SetScaleFactor(f);
end;

procedure TGozIntoGraph.SetSelectionAllDown(AValue: boolean);
begin
  if FSelectionAllDown=AValue then Exit;
  FSelectionAllDown:=AValue;

  Invalidate;
end;

procedure TGozIntoGraph.SetSelectionAllUp(AValue: boolean);
begin
  if FSelectionAllUp=AValue then Exit;
  FSelectionAllUp:=AValue;

  Invalidate;
end;

procedure TGozIntoGraph.SetShowCaptions(AValue: boolean);
begin
  if FShowCaptions=AValue then Exit;
  FShowCaptions:=AValue;

  Invalidate;
end;

procedure TGozIntoGraph.SetShowLinks(AValue: boolean);
begin
  if FShowLinks=AValue then Exit;
  FShowLinks:=AValue;

  Invalidate;
end;

procedure TGozIntoGraph.SetShowQuantities(AValue: boolean);
begin
  if FShowQuantities=AValue then Exit;
  FShowQuantities:=AValue;

  Invalidate;
end;


procedure TGozIntoGraph.ArrowTo(xa,ya,xe,ye,pb,pl:integer;Fill:boolean);
var
  m,t,sqm : real;
  x1,y1,x2,y2,xs,ys,la : real;
begin
  la:=sqrt(sqr(xe-xa)+sqr(ye-ya));
  if la<0.01 then exit;
  t:=(la-pl)/la;
  xs:=xa+t*(xe-xa);
  if xe<>xa then
    begin
      m:=(ye-ya)/(xe-xa);
      ys:=ya+t*m*(xe-xa);
      if m<>0 then
        begin
          sqm:=sqrt(1+1/sqr(m));
          x1:=xs+pb/sqm;
          y1:=ys-(x1-xs)/m;
          x2:=xs-pb/sqm;
          y2:=ys-(x2-xs)/m;
        end
      else
        begin
          x1:=xs; x2:=xs;
          y1:=ys+pb/1.0;
          y2:=ys-pb/1.0;
        end;
    end
  else
    begin
      xs:=xa;
      ys:=ya+t*(ye-ya);
      x1:=xs-pb/1.0;
      x2:=xs+pb/1.0;
      y1:=ys; y2:=ys;
    end;
  Canvas.MoveTo(xa,ya);
  Canvas.LineTo(round(xs),round(ys));
  if Fill then
    begin
      Canvas.Brush.Color:=Canvas.Pen.Color;
      Canvas.Brush.Style:=bsSolid;
      Canvas.Polygon([Point(xe,ye),Point(round(x1),round(y1)), Point(round(x2),round(y2)),Point(xe,ye)]);
    end
  else
    Canvas.Polyline([Point(xe,ye),Point(round(x1),round(y1)), Point(round(x2),round(y2)),Point(xe,ye)]);
end;

procedure TGozIntoGraph.DrawLinks;
var
  i,j : integer;
  connection : TGozGraphConnection;
  q : string;
begin
  Canvas.Pen.Style:=psSolid;

  for i:=0 to FConnections.Count-1 do
    begin
      connection:=TGozGraphConnection(FConnections[i]);

      if connection.Selected=true then Canvas.Pen.Color:=FLinkHighlightColor
      else Canvas.Pen.Color:=FLinkColor;

      ArrowTo(Scale(connection.x2),Scale(connection.y2),
        Scale(connection.x1),Scale(connection.y1),
        Scale(4),Scale(10),false);
    end;

  if FShowQuantities then
    begin
      Canvas.Brush.Style:=bsClear;
      Canvas.Brush.Color:=FColor;

      Canvas.Font.Color:=FLinkTextColor;
      for i:=0 to FConnections.Count-1 do
        begin
          connection:=TGozGraphConnection(FConnections[i]);
          q:=FloatToStr(connection.Quantity);
          Canvas.TextOut(Scale(((connection.x1)+((connection.x2)-(connection.x1)) div 2)-Canvas.TextWidth(q) div 2),
            Scale(((connection.y1)+((connection.y2)-(connection.y1)) div 2)-Canvas.TextHeight(q) div 2),q);
        end;
    end;

end;


procedure TGozIntoGraph.DrawItems;
var
  i,j : integer;
  th,oy,maxwidth : integer;
  s,s1,s2,itemcaption : string;
  item : TGozGraphItem;
  R : TRect;
begin
  Canvas.Brush.Style:=bsSolid;

  th:=Canvas.TextHeight('Ag');

  for i:=0 to FItems.Count-1 do
    begin
      item:=TGozGraphItem(FItems[i]);

      Canvas.Brush.Color:=item.ItemTypeColor;
      if item.Selected then Canvas.Brush.Color:=FHighlightColor;

      if FItemShape=gisMixed then
        begin
          if (item.Color2<>clNone) and (item.Selected<>true) then
            Canvas.Brush.Color:=item.Color2;

          if item.ItemType=gitProduct then
            Canvas.Rectangle(
              Scale(item.x), Scale(item.y),
              Scale(item.x+item.Width), Scale(item.y+item.Height))
          else if item.ItemType=gitModule then
            Canvas.Polygon([
              Point(Scale(item.x+(item.width) div 2), Scale(item.y)),
              Point(Scale(item.x+item.Width), Scale(item.y+item.Height)),
              Point(Scale(item.x), Scale(item.y+item.Height))])
          else
            Canvas.Ellipse(
              Scale(item.x), Scale(item.y),
              Scale(item.x+item.Width), Scale(item.y+item.Height))
        end
      else
        begin
          if (item.Color2=clNone) or (item.Selected) then
            Canvas.Ellipse(
              Scale(item.x), Scale(item.y),
              Scale(item.x+item.Width), Scale(item.y+item.Height))
          else
            begin
              Canvas.Pie(
                Scale(item.x), Scale(item.y),
                Scale(item.x+item.Width), Scale(item.y+item.Height),
                Scale(item.x+item.Width), Scale(item.y+(item.Height/2)),
                Scale(item.x), Scale(item.y+(item.Height/2))
                );

              Canvas.Brush.Color:=item.Color2;
              Canvas.Pie(
                Scale(item.x), Scale(item.y),
                Scale(item.x+item.Width), Scale(item.y+item.Height),
                Scale(item.x), Scale(item.y+(item.Height/2)),
                Scale(item.x+item.Width), Scale(item.y+(item.Height/2))
                );
            end;
        end;


      if item.Selected then Canvas.Brush.Color:=FHighlightColor
      else Canvas.Brush.Color:=FColor;

      //Text rechts
      //Canvas.TextOut(item.x+FDefaultItemWidth,item.y+(item.Height-th) div 2,IntToStr(item.Level)+':'+item.Caption);


      {
      s:={IntToStr(item.Level)+':'+}item.Caption;
      if Canvas.TextWidth(s)>Round(FDefaultItemWidth+FDistanceX/2) then
        begin
          j:=round(length(s)/2);
          s1:=copy(s,1,j);
          s2:=copy(s,j+1,length(s)-j);
        end
      else
        begin
          s1:='';
          s2:=s;
        end;

      Canvas.Font.Color:=FTextColor;
      Canvas.Brush.Style:=bsClear;
      Canvas.TextOut(Scale(item.x+FDefaultItemWidth div 2 - Canvas.TextWidth(s1) div 2),Scale(item.y-th*2),s1);
      Canvas.TextOut(Scale(item.x+FDefaultItemWidth div 2 - Canvas.TextWidth(s2) div 2),Scale(item.y-th),s2);
      }
      //R:=Rect(Scale(item.x),Scale(item.y),Scale(item.x+FDefaultItemWidth),Scale(item.y-2*th));
      //Canvas.TextRect(R, R.Left, R.Top, s, Canvas.TextStyle);
      //DrawText(Canvas.Handle, @sText[1], Length(sText), TextRect, DT_CALCRECT);
      //Canvas.Font.Color:=clBlack;
      //DrawText(Canvas.Handle, @s[1], Length(s), R, DT_WORDBREAK);


      itemcaption:=item.caption;
      if item.Description<>'' then itemcaption:=item.Description;

      if FShowCaptions then
        begin
          Canvas.Font.Color:=FTextColor;
          Canvas.Brush.Style:=bsClear;
          maxwidth:=Round((item.Width+FDistanceX-Canvas.TextWidth('...'))*0.9);
          s:='';

          for j:=1 to length(itemcaption) do
            if Canvas.TextWidth(s)<maxwidth then s:=s+copy(itemcaption,j,1)
            else
              begin
                s:=trim(s)+'...';
                break;
              end;
          //Canvas.TextOut(Scale(item.x+FDefaultItemWidth div 2 - Canvas.TextWidth(s) div 2),Scale(item.y-th),s);
          Canvas.TextOut(Scale(item.x+item.Width div 2 - Canvas.TextWidth(s) div 2),
            Scale(item.y-th),s);
        end;
    end;
end;


procedure TGozIntoGraph.Clear;
var
  i : integer;
  item : TGozGraphItem;
begin
  for i:=0 to FItems.Count-1 do
    begin
      item:=TGozGraphItem(FItems[i]);
      item.Free;
    end;
  FItems.Clear;
end;

procedure TGozIntoGraph.ClearConnections;
var
  i : integer;
  item : TGozGraphConnection;
begin
  for i:=0 to FConnections.Count-1 do
    begin
      item:=TGozGraphConnection(FConnections[i]);
      item.Free;
    end;
  FConnections.Clear;
end;

procedure TGozIntoGraph.ClearSelections;
var
  i : integer;
  item : TGozGraphItem;
begin
  for i:=0 to FItems.Count-1 do
    begin
      item:=TGozGraphItem(FItems[i]);
      item.Selected:=false;
    end;
end;


procedure TGozIntoGraph.AddConnection(Source, Destination : string; Quantity : extended;
  x1,y1,x2,y2 : longint; Selected : boolean);
var
  i : integer;
  connection : TGozGraphConnection;
begin
  for i:=0 to FConnections.Count-1 do
    begin
      connection:=TGozGraphConnection(FConnections[i]);
      if (connection.Source=Source) and (connection.Destination=Destination) then exit;
    end;

  connection:=TGozGraphConnection.Create;
  connection.Source:=Source;
  connection.Destination:=Destination;
  connection.Quantity:=Quantity;
  connection.Selected:=Selected;
  connection.x1:=x1;
  connection.y1:=y1;
  connection.x2:=x2;
  connection.y2:=y2;
  FConnections.Add(connection);
end;

procedure TGozIntoGraph.UpdateConnections;
var
  i,j : integer;
  item,item2 : TGozGraphItem;
  link : TGozGraphLink;
begin
  ClearConnections;
  for i:=0 to FItems.Count-1 do
    begin
      item:=TGozGraphItem(FItems[i]);
      for j:=0 to item.Links.Count-1 do
        begin
          link:=TGozGraphLink(item.Links[j]);
          item2:=GetItem(link.Destination);
          AddConnection(item.Caption,link.Destination,link.Quantity,
            item.x+item.Width div 2,item.y+item.Height,
            item2.x+item2.Width div 2,item2.y+item2.Height div 2,
            item.Selected and item2.Selected);
        end;
    end;
end;


procedure TGozIntoGraph.Invalidate;
begin
  inherited Invalidate;
end;


function TGozIntoGraph.ImportFromCsvFile(AFilename: string; Delimiter: string;
  HasHeaders: boolean; PosSource, PosDestination, PosQuantity: integer
  ): boolean;
var
  sl : TStringList;
  sr : TStringArray;
  i,j,k : integer;
  source,destination : string;
  quantity : extended;
begin
  result:=false;

  FFilename:=AFilename;

  Cursor:=crHourglass;

  sl:=TStringList.Create;
  sl.LoadFromFile(AFilename);

  Clear;

  try
    if HasHeaders then j:=1
    else j:=0;
    for i:=j to sl.Count-1 do
      begin
        sr:=sl[i].Split(Delimiter);

        source:=sr[PosSource-1];
        destination:=sr[PosDestination-1];

        if PosQuantity=-1 then quantity:=1
        else Val(sr[PosQuantity-1],quantity,j);

        if (source<>'') and (destination<>'') {and (quantity>0)} then
          AddLink(source,destination,quantity);
      end;

    Autolayout;
    result:=true;
  except
  end;

  Cursor:=crDefault;

  sl.Free;
end;


function TGozIntoGraph.ImportPropsFromCsvFile(
  AFilename: string; Delimiter: string; HasHeaders: boolean;
  PosItem: integer;
  Listview : TListview
  ): boolean;
var
  sl : TStringList;
  sr : TStringArray;
  i,j : integer;
  item : TGozGraphItem;
  Listitem : TListitem;
  prop : TGozFieldProperty;
begin
  result:=false;
  Cursor:=crHourglass;

  if FileExists(AFilename)=false then
    begin
      result:=false;
      exit;
    end;

  sl:=TStringList.Create;
  sl.LoadFromFile(AFilename);

  //add new property fields
  for i:=0 to Listview.Items.Count-2 do
    begin
      Listitem:=Listview.Items[i];
      if Listitem.ImageIndex=2 then
        begin
          prop:=FProperties.AddField(Listitem.Caption);
          listitem.Data:=prop;
        end;
    end;

  try
    if HasHeaders then j:=1
    else j:=0;

    for i:=j to sl.Count-1 do
      begin
        sr:=sl[i].Split(Delimiter);
        item:=GetItem(sr[PosItem-1]);
        if item<>nil then
          begin
            //add or update properties
            for j:=0 to Listview.Items.Count-2 do
              begin
                Listitem:=Listview.Items[j];
                if Listitem.ImageIndex>0 then item.Properties.AddValue(
                  TGozFieldProperty(Listitem.Data),
                  sr[StrInt(ListItem.SubItems[0])-1]);
              end;
          end;
      end;
    Autolayout;
    result:=true;
  except
  end;

  Cursor:=crDefault;

  sl.Free;
end;

function TGozIntoGraph.MaxLevel: integer;
var
  i : integer;
  item : TGozGraphItem;
begin
  result:=0;
  for i:=0 to FItems.Count-1 do
    begin
      item:=TGozGraphItem(FItems[i]);
      if item<>nil then
        if item.Level>result then result:=item.Level;
    end;
end;




function TGozIntoGraph.GetItem(ACaption : string) : TGozGraphItem;
var
  i : integer;
  item : TGozGraphItem;
begin
  result:=nil;
  for i:=0 to FItems.Count-1 do
    begin
      item:=TGozGraphItem(FItems[i]);
      if item<>nil then
        if ACaption=item.Caption then
          begin
            result:=item;
            exit;
          end;
    end;
end;



function TGozIntoGraph.ExistsItem(ACaption : string) : boolean;
begin
  result:=GetItem(ACaption)<>nil;
end;

function TGozIntoGraph.GetOutgoingInformation(ACaption : string) : TGozLinkInformation;
var
  i,j : integer;
  item : TGozGraphItem;
  link : TGozGraphLink;
begin
  result.Items:=0;
  result.Quantity:=0;
  for i:=0 to FItems.Count-1 do
    begin
      item:=TGozGraphItem(FItems[i]);
      if (item<>nil) and (item.Caption<>ACaption) then
        begin
          for j:=0 to item.Links.Count-1 do
            begin
              link:=TGozGraphLink(item.Links[j]);
              if link.Destination=ACaption then
                begin
                  result.Items:=result.Items+1;
                  result.Quantity:=result.Quantity+link.Quantity;
                end;
            end;
        end;
    end;
end;

function TGozIntoGraph.GetIngoingInformation(ACaption : string) : TGozLinkInformation;
var
  i : integer;
  item : TGozGraphItem;
  link : TGozGraphLink;
begin
  result.Items:=0;
  result.Quantity:=0;
  item:=GetItem(ACaption);
  if item<>nil then
    begin
      result.Items:=item.Links.Count;
      for i:=0 to item.Links.Count-1 do
        begin
          link:=TGozGraphLink(item.Links[i]);
          result.Quantity:=result.Quantity+link.Quantity;
        end;
    end;
end;

procedure TGozIntoGraph.UpdateItem(item : TGozGraphItem);
begin
  item.IngoingInformation:=GetIngoingInformation(item.Caption);
  item.OutgoingInformation:=GetOutgoingInformation(item.Caption);

  item.ItemType:=gitUndefined;

  if (item.IngoingInformation.Items=0) and (item.OutgoingInformation.Items>0) then item.ItemType:=gitItem;
  if (item.IngoingInformation.Items>0) and (item.OutgoingInformation.Items=0) then item.ItemType:=gitProduct;
  if (item.IngoingInformation.Items>0) and (item.OutgoingInformation.Items>0) then item.ItemType:=gitModule;
end;

procedure TGozIntoGraph.UpdatePropertyListview(Listview: TListview; item : TGozGraphItem);
var
  i : integer;
  prop : TGozFieldProperty;
  Listitem : TListitem;
begin
  if (Listview=nil) or (item=nil) then exit;

  for i:=0 to FProperties.List.Count-1 do
    begin
      prop:=TGozFieldProperty(FProperties.List[i]);
      Listitem:=Listview.Items.Add();
      Listitem.ImageIndex:=0;
      Listitem.Caption:=prop.Name;
      Listitem.SubItems.Add(item.Properties.GetValue(prop.Id));
      Listitem.Data:=prop;
    end;
end;

function TGozIntoGraph.AddItem(ACaption: string) : boolean;
var
  item : TGozGraphItem;
begin
  result:=false;
  if ExistsItem(ACaption)=false then
    begin
      item:=TGozGraphItem.Create;
      item.Caption:=ACaption;
      item.Width:=FDefaultItemWidth;
      item.Height:=FDefaultItemHeight;
      FItems.Add(item);
      result:=true;
    end;
end;

function TGozIntoGraph.CountItemType(AItemType: TGozItemType; MultipleUse: boolean): integer;
var
  item : TGozGraphItem;
  i,j : integer;
begin
  j:=0;
  for i:=0 to FItems.Count-1 do
    begin
      item:=TGozGraphItem(FItems[i]);
      if item.ItemType=AItemType then
        begin
          if MultipleUse=true then
            begin
              if item.OutgoingInformation.Items>1 then j:=j+1;
            end
          else j:=j+1;
        end;
    end;
  result:=j;
end;


procedure TGozIntoGraph.AddLink(ParentItem, ChildItem: string; Quantity: extended);
var
  item : TGozGraphItem;
begin
  AddItem(ParentItem);
  AddItem(ChildItem);

  //Add connection
  item:=GetItem(ParentItem);
  if item<>nil then
    begin
      item.AddLink(ChildItem,Quantity);
      UpdateItem(item);
      item:=GetItem(ChildItem);
      if item<>nil then UpdateItem(item);
    end;
end;



function TGozIntoGraph.GetItemAt(X, Y: integer): TGozGraphItem;
var
  i : Integer;
  item : TGozGraphItem;
begin
  result:=nil;
  for i:=0 to FItems.Count-1 do
    begin
      item:=TGozGraphItem(FItems[i]);
      if (x>=Scale(item.x)) and (x<=Scale(item.x+item.Width))
        and (y>=Scale(item.y)) and (y<=Scale(item.y+item.Height)) then exit(item);
    end;
end;

procedure TGozIntoGraph.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  if (ssLeft in Shift)  then
    begin
      if (MovedItem<>nil) and (Moving=true) then
        begin
          MovedItem.x:=ReScale(X)-MovedItemX;
          MovedItem.y:=ReScale(Y)-MovedItemY;
          Invalidate;
        end;
    end;
end;



procedure TGozIntoGraph.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  item : TGozGraphItem;
begin
  inherited MouseDown(Button, Shift, X, Y);

  MovedItem:=nil;
  item:=GetItemAt(X,Y);

  if Button=mbLeft then
    begin
      ClearSelections;

      if item<>nil then
        begin
          SelectItems(item);
          MovedItem:=item;
          MovedItemX:=ReScale(X)-item.x;
          MovedItemY:=ReScale(Y)-item.y;
          Moving:=true;
        end;

      Invalidate;
      if Assigned(FOnItemSelected) then FOnItemSelected(item);
    end;
end;

procedure TGozIntoGraph.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);

  if (MovedItem<>nil) and (Moving=true) then
    begin
      Moving:=false;
      MovedItem.x:=ReScale(X)-MovedItemX;
      MovedItem.y:=ReScale(Y)-MovedItemY;
      SelectItems(MovedItem);
      Invalidate;
    end;
end;



{ TGozGraphLink }

constructor TGozGraphLink.Create;
begin
  inherited Create;

  FDestination:='';
  FQuantity:=0;
  FSelected:=false;
  FShowText:=true;
end;

destructor TGozGraphLink.Destroy;
begin
  inherited Destroy;
end;



{ TGozGraphItem }

constructor TGozGraphItem.Create;
begin
  inherited Create;

  FLinks:=TList.Create;
  FProperties:=TGozPropertyList.Create;

  FCaption:='';
  FColor2:=clNone;
  FWidth:=50;
  FHeight:=50;
  Fx:=0;
  Fy:=0;
  FSelected:=false;
  FPainted:=false;
end;

destructor TGozGraphItem.Destroy;
begin
  Clear;
  FLinks.Free;
  FProperties.Free;

  inherited Destroy;
end;

procedure TGozGraphItem.Clear;
var
  i : integer;
  link : TGozGraphLink;
begin
  for i:=0 to FLinks.Count-1 do
    begin
      link:=TGozGraphLink(FLinks[i]);
      link.Free;
    end;
  FLinks.Clear;
  FProperties.Clear;
end;

procedure TGozGraphItem.SetSize(w, h: integer);
begin
  FWidth:=w;
  FHeight:=h;
end;


procedure TGozGraphItem.AddLink(Destination: string; Quantity: extended);
var
  connection : TGozGraphLink;
begin
  connection:=TGozGraphLink.Create;
  connection.Destination:=Destination;
  connection.Quantity:=Quantity;

  FLinks.Add(connection);
end;

function TGozGraphItem.ItemTypeColor: TColor;
begin
  if FItemType=gitItem then result:=gozItemColor
  else if FItemType=gitModule then result:=gozModuleColor
  else if FItemType=gitProduct then result:=gozProductColor
  else result:=clGray;
end;

function TGozGraphItem.ItemTypeCaption: string;
begin
  if FItemType=gitItem then result:='Einzelteil'
  else if FItemType=gitModule then result:='Baugruppe'
  else if FItemType=gitProduct then result:='Produkt'
  else result:='undefiniert';
end;


end.

