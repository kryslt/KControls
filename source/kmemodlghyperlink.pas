{ @abstract(This unit contains a dialog for editing a hyperlink)
  @author(Tomas Krysl (tk@tkweb.eu))
  @created(28 Apr 2009)
  @lastmod(30 July 2015)

  Copyright (c) Tomas Krysl (tk@@tkweb.eu)<BR><BR>

  <B>License:</B><BR>
  This code is distributed as a freeware. You are free to use it as part
  of your application for any purpose including freeware, commercial and
  shareware applications. The origin of this source code must not be
  misrepresented; you must not claim your authorship. All redistributions
  of the original or modified source code must retain the original copyright
  notice. The Author accepts no liability for any damage that may result
  from using this code.
}
unit kmemodlghyperlink; // lowercase name because of Lazarus/Linux

interface

uses
{$IFDEF FPC}
  LCLType, LCLIntf, LMessages, LCLProc, LResources,
{$ELSE}
  Windows, Messages,
{$ENDIF}
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, KControls, KMemo;

type
  TKMemoHyperlinkForm = class(TForm)
    BUOk: TButton;
    BUCancel: TButton;
    LBText: TLabel;
    EDText: TEdit;
    LBHyperlink: TLabel;
    CoBURL: TComboBox;
  private
    { Private declarations }
  public
    { Public declarations }
    procedure Clear;
    procedure Load(AItem: TKMemoHyperlink);
    procedure Save(AItem: TKMemoHyperlink);
  end;

implementation

{$IFDEF FPC}
 {$R *.lfm}
{$ELSE}
 {$R *.dfm}
{$ENDIF}

uses
  KGraphics;

{ TKMemoHyperlinkForm }

procedure TKMemoHyperlinkForm.Clear;
begin
  EDText.Text := '';
  CoBURL.Text := '';
end;

procedure TKMemoHyperlinkForm.Load(AItem: TKMemoHyperlink);
begin
  if AItem <> nil then
  begin
    EDText.Text := AItem.Text;
    CoBURL.Text := AItem.URL;
  end;
end;

procedure TKMemoHyperlinkForm.Save(AItem: TKMemoHyperlink);
begin
  if AItem <> nil then
  begin
    if CoBURL.Items.IndexOf(CoBURL.Text) < 0 then
      CoBURL.Items.Add(CoBURL.Text);
    AItem.Text := EDText.Text;
    AItem.URL := CoBURL.Text;
  end;
end;

end.
