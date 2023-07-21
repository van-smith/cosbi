unit ufrmGraph;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TeEngine, Series, TeeProcs, Chart, ExtCtrls, StdCtrls, Gauges, cxCpu40,
  jpeg;

type
  TfrmGraph = class(TForm)
    panelButtons: TPanel;
    chartMain: TChart;
    panelProgress: TPanel;
    Series1: TLineSeries;
    lblCPUNumber: TLabel;
    GaugeCPU: TGauge;
    btnScrollLeft: TButton;
    btnScrollRight: TButton;
    btnScale: TButton;
    btnSaveGraph: TButton;
    btnSaveData: TButton;
    Series2: TLineSeries;
    TimerSample: TTimer;
    SaveDialog1: TSaveDialog;
    procedure FormCreate(Sender: TObject);
    procedure TimerSampleTimer(Sender: TObject);
    procedure btnScrollLeftClick(Sender: TObject);
    procedure btnScrollRightClick(Sender: TObject);
    procedure btnScaleClick(Sender: TObject);
    procedure btnSaveGraphClick(Sender: TObject);
    procedure btnSaveDataClick(Sender: TObject);
  private
    { Private declarations }
    fCPUSigma : double;
    fCPUNumber : integer;
    fTicks : integer;
    fSeriesMain : TChartSeries;
    fSeriesAverage : TChartSeries;
    fMainColor :  TColor;
    fAverageColor : TColor;
    function GetCPUNumber : integer;
    procedure SetCPUNumber( Value : integer);
    procedure EnableDisableButtons( abEnable : Boolean );
  public
    { Public declarations }
    procedure AddDataPoint( adDataPoint : double );
    procedure ChartClear;
    procedure ChartInitialize;
    procedure ChartStart;
    procedure ChartStop;
    procedure ChartUpdate;
    procedure ScrollLeft;
    procedure ScrollRight;
    procedure Scale;
    procedure SaveData;
    procedure SaveGraph;
    property CPUNumber : integer read GetCPUNumber write SetCPUNumber;
    procedure SaveGraphData(as_Title    : string;
                            as_Y_label  : string;
                            as_X_label  : string;
                            as_Filename : string );
  end;

implementation

{$R *.dfm}
const
  WINDOW_SIZE = 60;
  DEFAULT_FILENAME = 'CpuUtil.txt';

function TfrmGraph.GetCPUNumber : integer;
begin
  result := fCPUNumber;
end; // function TfrmGraph.GetCPUNumber : integer;

procedure TfrmGraph.AddDataPoint( adDataPoint : double );
begin

end; // procedure TfrmGraph.AddDataPoint( adDataPoint : double );

procedure TfrmGraph.EnableDisableButtons( abEnable : Boolean );
begin
  btnScrollLeft.Enabled := abEnable;
  btnScrollRight.Enabled := abEnable;
  btnScale.Enabled := abEnable;
  btnSaveGraph.Enabled := abEnable;
  btnSaveData.Enabled := abEnable;
end; // procedure TfrmGraph.EnableDisableButtons( abEnable : Boolean );

procedure TfrmGraph.ChartClear;
begin
  ChartStop;
  chartMain.BottomAxis.Scroll( 0, FALSE );
  chartMain.BottomAxis.SetMinMax(0, 60);
  ChartInitialize;
end; //procedure TfrmGraph.ChartClear;

procedure TfrmGraph.ChartStart;
begin
  TimerSample.Enabled := TRUE;
  EnableDisableButtons( FALSE );
end; // procedure TfrmGraph.ChartStart;

procedure TfrmGraph.ChartStop;
begin
  TimerSample.Enabled := FALSE;
  EnableDisableButtons( TRUE );
end; //procedure TfrmGraph.ChartStop;

procedure TfrmGraph.ChartInitialize;
var
  liCpuUtilize : integer;
begin
  chartMain.BottomAxis.Automatic := FALSE;
  fTicks := 0;
  fSeriesMain := chartMain.Series[ 0 ];
  fSeriesAverage := chartMain.Series[ 1 ];
  fSeriesMain.Clear;
  fSeriesAverage.Clear;
  fSeriesMain.Add( 0, '', fMainColor); // add a point @ 0 seconds
  fSeriesAverage.Add(0, '', fAverageColor);
  GaugeCPU.Progress := 0;
  fCPUSigma := 0;
  // first value is always 100, so discard:
  liCpuUtilize := cxCpu[ fCPUNumber ].Usage.Value.AsNumber;
end; // procedure TfrmGraph.ChartInitialize;

procedure TfrmGraph.SetCPUNumber(Value : integer);
begin
  fCPUNumber := Value;
  lblCPUNumber.Caption := 'CPU ' + IntToStr( Value ) + ' Utilization (%)';
end; // procedure TfrmGraph.SetCPUNumber(Value : integer);

procedure TfrmGraph.ScrollLeft;
begin
  if chartMain.BottomAxis.Automatic then begin
    chartMain.BottomAxis.Automatic := FALSE;
    chartMain.BottomAxis.SetMinMax(0, 60);
  end else begin
    chartMain.BottomAxis.Scroll( -60, TRUE );
  end; // if
end; // procedure TfrmGraph.ScrollLeft;

procedure TfrmGraph.ScrollRight;
begin
  if chartMain.BottomAxis.Automatic then begin
    chartMain.BottomAxis.Automatic := FALSE;
    chartMain.BottomAxis.SetMinMax(0, 60);
  end else begin
    chartMain.BottomAxis.Scroll( 60, TRUE );
  end; // if
end; // procedure TfrmGraph.ScrollRight;

procedure TfrmGraph.Scale;
begin
  chartMain.BottomAxis.Automatic := TRUE;
end; // procedure TfrmGraph.Scale;

procedure TfrmGraph.SaveData;
begin
  SaveDialog1.DefaultExt := 'txt';
  SaveDialog1.FileName := DEFAULT_FILENAME;
  SaveDialog1.Filter := 'Text files (*.txt)|*.TXT';
  if SaveDialog1.Execute then begin
    if FileExists( SaveDialog1.FileName ) then begin
      if MessageDlg('Hey, Bud, that file already exists!'+#13+#10+
                    'Is it okay for me to overwrite it?', mtWarning,
                    [mbYes, mbNo], 0) = mrNo then exit;
    end; // if
    SaveGraphData('CPU utilization history',
                  'seconds',
                  'cpu utilization (%)',
                  SaveDialog1.Filename);
  end; // if SaveDialog1
end; // procedure TfrmGraph.SaveData;

procedure TfrmGraph.SaveGraph;
begin
  SaveDialog1.DefaultExt := 'bmp';
  SaveDialog1.FileName := 'CpuUtil.bmp';
  SaveDialog1.Filter := 'Bitmap files (*.bmp)|*.BMP';
  if SaveDialog1.Execute then begin
    if FileExists( SaveDialog1.FileName ) then begin
      if MessageDlg('Hey, Bud, that file already exists!'+#13+#10+
                    'Is it okay for me to overwrite it?', mtWarning,
                    [mbYes, mbNo], 0) = mrNo then exit;
    end; // if
    chartMain.SaveToBitmapFile(SaveDialog1.FileName);
  end; // if
end; // procedure TfrmGraph.SaveGraph;

procedure TfrmGraph.FormCreate(Sender: TObject);
begin
  fMainColor := clRed;
  fAverageColor := clGray;
end;

procedure TfrmGraph.ChartUpdate;
var
  liCpuUtilize : integer;
  lsCpuUtilize : string;
begin
  inc( fTicks );
  liCpuUtilize := cxCpu[ fCPUNumber ].Usage.Value.AsNumber;
  lsCpuUtilize := cxCpu[ fCPUNumber ].Usage.Value.AsString;
  fSeriesMain.Add( liCpuUtilize, '', fMainColor);
  fCPUSigma := fCPUSigma + liCpuUtilize;
  fSeriesAverage.Add( fCPUSigma / fTicks, '', fAverageColor );
  GaugeCPU.Progress := liCpuUtilize;
  if fTicks mod WINDOW_SIZE = 0 then begin
    chartMain.BottomAxis.Scroll( WINDOW_SIZE, FALSE );
  end;
end; // procedure TfrmGraph.ChartUpdate;

procedure TfrmGraph.TimerSampleTimer(Sender: TObject);
begin
  ChartUpdate;
end;

procedure TfrmGraph.btnScrollLeftClick(Sender: TObject);
begin
  ScrollLeft;
end;

procedure TfrmGraph.btnScrollRightClick(Sender: TObject);
begin
  ScrollRight;
end;

procedure TfrmGraph.btnScaleClick(Sender: TObject);
begin
  Scale;
end;

procedure TfrmGraph.btnSaveGraphClick(Sender: TObject);
begin
  SaveGraph;
end;

procedure TfrmGraph.btnSaveDataClick(Sender: TObject);
begin
  SaveData;
end; // procedure TfrmGraph.btnSaveDataClick

procedure TfrmGraph.SaveGraphData(as_Title    : string;
                                    as_Y_label  : string;
                                    as_X_label  : string;
                                    as_Filename : string );
var
  ltxtfile_out  : TextFile;
  ls_FileName   : string;
  ls_OutLine    : string;
  ls_x          : string;
  ld_x          : double;
  ld_y          : double;
  n             : integer;
begin
//  if not assigned( fSystemInfo ) then begin
//    fSystemInfo := TSystemInfo.Create( self );
//    fSystemInfo.ShowStatus := TRUE;
//    fSystemInfo.StopWatch := StopWatch;
//    fSystemInfo.Initialize;
//  end; // if
  ls_FileName := Trim( as_Filename );
  if ls_FileName = '' then begin
    ls_FileName := DEFAULT_FILENAME;
    ls_FileName := ls_FileName + '.txt';
  end; // if
  AssignFile( ltxtfile_out, ls_FileName );
  Rewrite( ltxtfile_out );
  WriteLn( ltxtfile_out, as_Title);
  ls_OutLine := 'Date: ' + DateToStr( Date ) + ', Time: ' + TimeToStr( Time );
  WriteLn( ltxtfile_out, ls_OutLine );
//  WriteLn( ltxtfile_out, '' );
//  WriteLn( fSystemInfo.GetSystemSummary );

  WriteLn( ltxtfile_out, '' );
  WriteLn( ltxtfile_out, as_Y_label + '  ' + as_X_label);
  WriteLn( ltxtfile_out, '========  ===============');
  for n := 0 to fSeriesMain.Count - 1 do begin
    ls_x := fSeriesMain.XLabel[n];
    ld_x := fSeriesMain.Xvalue[n];
    ld_y := fSeriesMain.YValue[n];
    ls_Outline := Format('%6n' + Chr(9) + '   %11n',
      [ld_x, fSeriesMain.YValue[n]]);
    WriteLn( ltxtfile_out, ls_Outline);
  end;
  CloseFile( ltxtfile_out );
end; // procedure TfrmGraph.SaveGraphData

end.
