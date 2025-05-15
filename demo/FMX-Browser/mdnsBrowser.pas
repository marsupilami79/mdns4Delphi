unit mdnsBrowser;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.ListView.Types, FMX.ListView.Appearances, FMX.ListView.Adapters.Base,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.ListView,

  mdnsCore, mdnsResolver, FMX.Edit;

type
  TForm1 = class(TForm)
    ServicesLV: TListView;
    Button1: TButton;
    ServiceNameLbl: TLabel;
    ServiceTypeEdt: TEdit;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ServicesLVChangeRepainted(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private-Deklarationen }
    FResolver: TMdnsResolver;
    procedure OnResolved(Sender: TObject; const Result: TmdnsResult);
  public
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses System.Generics.Defaults;

type
  //hier gefunden: https://stackoverflow.com/questions/37471355/delphi-firemonkey-tlistviews-item-rearrange#58005587
  TMyListViewItemComparer_AscendingItemText = class( TComparer<TListViewItem> )
    function Compare(const Left, Right: TListViewItem): Integer; override;
  end;

  TMyListViewItemComparer_DescendingItemText = class( TComparer<TListViewItem> )
    function Compare(const Left, Right: TListViewItem): Integer; override;
  end;

{ TMyListViewItemComparer_Ascending }

function TMyListViewItemComparer_AscendingItemText.Compare(const Left,
  Right: TListViewItem): Integer;
begin
  result := CompareText(Left.Text,Right.Text);
end;

{ TMyListViewItemComparer_DescendingItemText }

function TMyListViewItemComparer_DescendingItemText.Compare(const Left,
  Right: TListViewItem): Integer;
begin
  result := CompareText(Right.Text,Left.Text);
end;

{------------------------------------------------------------------------------}

procedure TForm1.Button1Click(Sender: TObject);
begin
  FResolver.StopResolve;
  ServicesLV.Items.Clear;
  FResolver.ServiceType := '_services._dns-sd._udp.local';
  ServiceNameLbl.Text := FResolver.ServiceType;
  ServicesLV.ItemAppearance.ItemAppearance := 'ListItem';
  FResolver.StartResolve;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  FResolver.StopResolve;
  ServicesLV.Items.Clear;
  ServicesLV.ItemAppearance.ItemAppearance := 'ImageListItemBottomDetail';
  FResolver.ServiceType := ServiceTypeEdt.Text;
  ServiceNameLbl.Text := ServiceTypeEdt.Text;
  FResolver.StartResolve;
end;
procedure TForm1.FormCreate(Sender: TObject);
begin
  FResolver := TMdnsResolver.Create(self);
  FResolver.OnResolved := OnResolved;
end;

procedure TForm1.OnResolved(Sender: TObject; const Result: TmdnsResult);
var
  x: Integer;
  Index: Integer;
  Item: TListViewItem;
begin
  if Result.PTR.NameHost <> '' then begin
    index := -1;
    for x := 0 to ServicesLV.Items.Count - 1 do begin
      if ServicesLV.Items[x].Text = Result.PTR.NameHost then begin
        index := x;
        break;
      end
    end;

    if Index = -1 then begin
      Item := ServicesLV.Items.Add;
      Item.Text := Result.PTR.NameHost;
      Item.Detail := Result.Host;
      if Result.Port <> 0 then
        Item.Detail := Item.Detail + ':' + IntToStr(Result.Port);
      Item.TagString := Result.PTR.NameHost;
      ServicesLV.Items.Sort(TMyListViewItemComparer_AscendingItemText.Create as IComparer<TListViewItem>);
    end;
  end;
end;

procedure TForm1.ServicesLVChangeRepainted(Sender: TObject);
var
  ServiceName: String;
begin
  if Assigned(ServicesLV.Selected) then begin
    ServiceName := ServicesLV.Selected.TagString;
    if ServiceName.StartsWith('_') then begin
      FResolver.StopResolve;
      ServicesLV.Items.Clear;
      ServicesLV.ItemAppearance.ItemAppearance := 'ImageListItemBottomDetail';
      FResolver.ServiceType := ServiceName;
      ServiceNameLbl.Text := ServiceName;
      FResolver.StartResolve;
    end;
  end;
end;

end.
