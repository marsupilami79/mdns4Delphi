unit mdnsBrowser_Main;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  SysUtils, Types, Classes, Variants, Controls, Forms, Graphics, Dialogs, StdCtrls, ComCtrls,
  mdnsCore, mdnsResolver;

type

  { TFormBrowser }

  TFormBrowser = class(TForm)
    ServicesLV: TListView;
    Button1: TButton;
    ServiceNameLbl: TLabel;
    Button2: TButton;
    ServiceTypeEdt: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ServicesLVChangeRepainted(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private-Deklarationen }
    FResolver: TMdnsResolver;
    procedure OnResolvedItems(Sender: TObject; const Result: TmdnsResult);
    procedure OnResolvedCombo(Sender: TObject; const Result: TmdnsResult);
  public
    { Public-Deklarationen }
  end;

var
  FormBrowser: TFormBrowser;

implementation

{$IFDEF FPC}
  {$R *.lfm}
{$ELSE}
  {$R *.dfm}
{$ENDIF}


(*
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
*)
{------------------------------------------------------------------------------}

procedure TFormBrowser.Button1Click(Sender: TObject);
begin
  FResolver.StopResolve;
  ServiceTypeEdt.Items.Clear;
  FResolver.ServiceType := '_services._dns-sd._udp.local';
  ServiceNameLbl.Caption := FResolver.ServiceType;
  FResolver.OnResolved := OnResolvedCombo;
  FResolver.StartResolve;
end;

procedure TFormBrowser.Button2Click(Sender: TObject);
begin
  FResolver.StopResolve;
  ServicesLV.Items.Clear;
  FResolver.ServiceType := ServiceTypeEdt.Text;
  ServiceNameLbl.Caption := ServiceTypeEdt.Text;
  FResolver.OnResolved := OnResolvedItems;
  FResolver.StartResolve;
end;
procedure TFormBrowser.FormCreate(Sender: TObject);
begin
  FResolver := TMdnsResolver.Create(self);
  FResolver.OnResolved := OnResolvedItems;
end;

procedure TFormBrowser.OnResolvedItems(Sender: TObject; const Result: TmdnsResult);
var
  x: Integer;
  Index: Integer;
  Item: TListItem;
  detail: String;

begin
  if Result.PTR.NameHost <> '' then
  begin
    index := -1;
    for x := 0 to ServicesLV.Items.Count - 1 do begin
      if ServicesLV.Items[x].Caption = Result.PTR.NameHost then begin
        index := x;
        break;
      end
    end;

    if Index = -1 then
    begin
      Item:= ServicesLV.Items.Add;
      Item.Caption :=Result.PTR.NameHost;

      detail:= Result.Host;
      if Result.Port <> 0
      then detail := detail + ':' + IntToStr(Result.Port);

      Item.SubItems.Add(detail);
    end;
  end;
end;

procedure TFormBrowser.OnResolvedCombo(Sender: TObject; const Result: TmdnsResult);
var
  x: Integer;
  Index: Integer;
  Item: TListItem;
begin
  if Result.PTR.NameHost <> '' then
  begin
    index := -1;
    for x := 0 to ServiceTypeEdt.Items.Count - 1 do
      if ServiceTypeEdt.Items[x] = Result.PTR.NameHost then
      begin
        index := x;
        break;
      end;

    if Index = -1 then ServiceTypeEdt.Items.Add(Result.PTR.NameHost);
  end;
end;

procedure TFormBrowser.ServicesLVChangeRepainted(Sender: TObject);
var
  ServiceName: String;
begin
(*  if Assigned(ServicesLV.Selected) then begin
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
  *)
end;

end.
