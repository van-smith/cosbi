�
 TFRMCOPYFILE 0�  TPF0TfrmCopyFilefrmCopyFileLeftTopy
AlphaBlend	AlphaBlendValueBorderStylebsDialogCaptionCOSBI File Copy BenchmarkClientHeight8ClientWidth5Color	clBtnFaceFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style OldCreateOrderPositionpoScreenCenter
OnActivateFormActivateOnClose	FormCloseOnCreate
FormCreateOnDeactivateFormDeactivatePixelsPerInch`
TextHeight TLabellblSourceDriveLeftTop-Width?HeightCaptionSource drive:  TLabelLabel3Left�Top-Width<HeightCaptionTarget drive:  TLabellblSizeLeftFTopWidth0HeightCaption
Size (MB):  TLabellblIterationsLeft� TopWidth+HeightCaption
Iterations  TButtonbtnRunLeftTopWidth9HeightHintRun the benchmarkCaption&Go!Default	ParentShowHintShowHint	TabOrder OnClickbtnRunClick  TMemomemoResultsLeft TopCWidth5Height�Hint2This memo control contains all of the test resultsAlignalBottomColorclBlackFont.CharsetANSI_CHARSET
Font.ColorclYellowFont.Height�	Font.NameArial
Font.Style 
ParentFontParentShowHintReadOnly	
ScrollBars
ssVerticalShowHint	TabOrder  TDriveComboBoxdriveComboSourceLeftHTop*WidthqHeightHintDrive used for file creationParentShowHintShowHint	TabOrderOnChangedriveComboSourceChange  TDriveComboBoxdriveComboTargetLeft�Top*WidthqHeightHint(The created file is copied to this driveParentShowHintShowHint	TabOrderOnChangedriveComboTargetChange  	TComboBoxcboSizeLeftxTopWidthAHeightHint1Size of each of the two output files in megabytesStylecsDropDownList
ItemHeightParentShowHintShowHint	TabOrderOnChangecboSizeChangeItems.Strings123451020304050100500100025005000100002000050000100000   
TStatusBar
StatusBar1Left Top'Width5HeightHintProgram statusPanelsWidth2  ParentShowHintShowHint	ExplicitTop�  	TCheckBoxcboxDetailsLeftHTopWidthQHeightHint*Show individual test results while testingCaptionShow &detailsParentShowHintShowHint	TabOrder  TButtonbtnSaveLeft�TopWidth9HeightHint,Save all test data in memo control to a fileCaption&SaveParentShowHintShowHint	TabOrderOnClickbtnSaveClick  TButton
btnScoringLeft�TopWidthAHeightHintExplain scoring schemeCaptionS&coringParentShowHintShowHint	TabOrderOnClickbtnScoringClick  TProgressBarpbar_overallLeft TopWidth5HeightHint!Progress of overall benchmark runAlignalBottomMaxParentShowHintSmooth	ShowHint	TabOrderExplicitTop�  TProgressBar	pbar_testLeft TopWidth5HeightHintProgress of current testAlignalBottomParentShowHintSmooth	ShowHint	TabOrder
ExplicitTop�  	TComboBoxcboIterationsLeft� TopWidth9HeightHintNumber of test repetitionsStylecsDropDownList
ItemHeightParentShowHintShowHint	TabOrderOnChangecboIterationsChangeItems.Strings123456789101520255010020025050010002000500010000   TSaveDialogSaveDialog1Left�TopG  TTimerTimer1EnabledIntervaldOnTimerTimer1TimerLeftHTop    