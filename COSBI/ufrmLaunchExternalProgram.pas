unit ufrmLaunchExternalProgram;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, IniFiles;

type
  TfrmLaunchExternalProgram = class(TForm)
    btnBrowseForFileName: TButton;
    OpenDialog1: TOpenDialog;
    Label1: TLabel;
    BitBtnLaunch: TBitBtn;
    BitBtnCancel: TBitBtn;
    cboProgramPath: TComboBox;
    procedure BitBtnCancelClick(Sender: TObject);
    procedure btnBrowseForFileNameClick(Sender: TObject);
    procedure BitBtnLaunchClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    fProgramFilePath : string;
    procedure AddStringToList( asString : string; aStringList : TStrings );
    procedure ReadIniFile;
    procedure WriteIniFile;
  public
    { Public declarations }
    property ProgramFilePath : string read fProgramFilePath;
  end;

var
  frmLaunchExternalProgram: TfrmLaunchExternalProgram;

implementation

{$R *.dfm}

procedure TfrmLaunchExternalProgram.BitBtnCancelClick(Sender: TObject);
begin
  fProgramFilePath := '';
  Close;
end;

procedure TfrmLaunchExternalProgram.btnBrowseForFileNameClick(
  Sender: TObject);
begin
  if OpenDialog1.Execute then begin
    cboProgramPath.Text := OpenDialog1.FileName;
  end; // if
end; // procedure TfrmLaunchExternalProgram.btnBrowseForFileNameClick

procedure TfrmLaunchExternalProgram.BitBtnLaunchClick(Sender: TObject);
begin
  fProgramFilePath := cboProgramPath.Text;
  AddStringToList( fProgramFilePath, cboProgramPath.Items );
  WriteIniFile;
  Close;
end;

procedure TfrmLaunchExternalProgram.AddStringToList( asString : string;
                                                     aStringList : TStrings );
var
  i : integer;
begin
  if aStringList.IndexOf( asString ) = -1 then begin
    aStringList.Insert(0, asString );
    if aStringList.Count > 10 then begin
      aStringList.Delete(10);
    end; // if
  end; // if
end; // procedure TfrmLaunchExternalProgram.AddStringToList

procedure TfrmLaunchExternalProgram.FormCreate(Sender: TObject);
begin
  // load the list of the ten last programs
  ReadIniFile;
end;

procedure TfrmLaunchExternalProgram.ReadIniFile;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create( ChangeFileExt( Application.ExeName, '.INI' ) );
  try
    // read settings
    cboProgramPath.Items.Add( Ini.ReadString('LaunchExternalProgram', 'Item1', '' ) );
    cboProgramPath.Items.Add( Ini.ReadString('LaunchExternalProgram', 'Item2', '' ) );
    cboProgramPath.Items.Add( Ini.ReadString('LaunchExternalProgram', 'Item3', '' ) );
    cboProgramPath.Items.Add( Ini.ReadString('LaunchExternalProgram', 'Item4', '' ) );
    cboProgramPath.Items.Add( Ini.ReadString('LaunchExternalProgram', 'Item5', '' ) );
    cboProgramPath.Items.Add( Ini.ReadString('LaunchExternalProgram', 'Item6', '' ) );
    cboProgramPath.Items.Add( Ini.ReadString('LaunchExternalProgram', 'Item7', '' ) );
    cboProgramPath.Items.Add( Ini.ReadString('LaunchExternalProgram', 'Item8', '' ) );
    cboProgramPath.Items.Add( Ini.ReadString('LaunchExternalProgram', 'Item9', '' ) );
    cboProgramPath.Items.Add( Ini.ReadString('LaunchExternalProgram', 'Item10', '' ) );
  finally
    FreeAndNil( Ini );
  end; // try..finally
end; // procedure TfrmLaunchExternalProgram.ReadIniFile;

procedure TfrmLaunchExternalProgram.WriteIniFile;
var
  Ini: TIniFile;
  i : integer;
  liNumberOfItems : integer;
begin
  liNumberOfItems := cboProgramPath.Items.Count;
  if liNumberOfItems = 0 then exit;
  Ini := TIniFile.Create( ChangeFileExt( Application.ExeName, '.INI' ) );
  try
    try
      // write settings
      for i := 0 to liNumberOfItems do begin
        Ini.WriteString( 'LaunchExternalProgram', 'Item' + intToStr( i + 1 ), cboProgramPath.Items[ i ]);
      end; // for
    except
      ShowMessage( 'An error occurred while trying to write to the INI file.' );
    end; // try...except
  finally
    FreeAndNil( Ini );
  end; // try..finally
end;// procedure TfrmLaunchExternalProgram.WriteIniFile;

end.
