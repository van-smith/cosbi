unit uAbout;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, jpeg, ExtCtrls, Buttons, COSBI_Common;

type
  TfrmAbout = class(TForm)
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Memo1: TMemo;
    BitBtn1: TBitBtn;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TfrmAbout.FormActivate(Sender: TObject);
begin
  Unfade( self, 255, 4 );
end;

procedure TfrmAbout.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FadeFast100( self );
end;

end.
