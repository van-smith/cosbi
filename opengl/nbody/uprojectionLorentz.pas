{: Parallel projection demo.<p>

   This simple demo shows how to do parallel projection and blend some custom
   OpenGL calls into the scene.<br>
   You can change the viewpoint with left clic drags, change the plane orientation
   with right clic drags, and move the plane up/down with the wheel.<p>

   The points and plane are rendered directly with regular scene objects,
   but the projection lines between the points and the plane are computed
   and rendered on the fly in a TGLDirectOpenGL. This is a typical case where
   a little bit of custom code helps a lot: we could have used many TGLLines
   object to draw the lines, but this would have resulted in a lot of object
   creation and update code, and ultimately in rather poor performance.<br>
   Note the position of the plane in the scene hierarchy: it is last as it is
   a blended object. Try making it the first object, it will appear opaque
   (though it is still transparent!).
}
unit uprojectionLorentz;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms, GLScene, GLObjects,
  GLWin32Viewer, GLMisc, OpenGL1x, GLTexture, VectorGeometry, GLGraph,
  GLGeomObjects, ExtCtrls, StdCtrls;

type
  TfrmLorenzAttractor = class(TForm)
    GLScene1: TGLScene;
    SceneViewer: TGLSceneViewer;
    GLCamera: TGLCamera;
    GLDummyCube: TGLDummyCube;
    GLPlane: TGLPlane;
    GLPoints: TGLPoints;
    DirectOpenGL: TGLDirectOpenGL;
    GLArrowLine1: TGLArrowLine;
    GLLightSource1: TGLLightSource;
    GLXYZGrid1: TGLXYZGrid;
    Timer1: TTimer;
    GLSphere1: TGLSphere;
    procedure FormCreate(Sender: TObject);
    procedure DirectOpenGLRender(Sender : TObject; var rci: TRenderContextInfo);
    procedure SceneViewerMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SceneViewerMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure Timer1Timer(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    fIndex : integer;
    procedure BallGo;
  public
    { Public declarations }
    mx, my : Integer;
  end;

var
  frmLorenzAttractor: TfrmLorenzAttractor;

implementation

{$R *.dfm}

const

  L_STEPS = 10000;

procedure TfrmLorenzAttractor.FormCreate(Sender: TObject);
var
   j : Integer;
   x0, y0, z0, x1, y1, z1 : double;
   h, a, b, c : double;

begin
  fIndex := 0;
   h := 0.01;
   a := 10;
   b := 28;
   c := 8 / 3;

   x0 := 0.1;
   y0 := 0;
   z0 := 0;

   for j := 0 to L_STEPS do begin
      x1 := x0 + h * a * (y0 - x0);
      y1 := y0 + h * (x0 * (b - z0) - y0);
      z1 := z0 + h * (x0 * y0 - c * z0);
      x0 := x1;
      y0 := y1;
      z0 := z1;
//      glColor3f( 1, 1, 0);
      GLPoints.Positions.Add(x0, y0, z0);
   end; // for
     GLPoints.Visible := FALSE;
//     BallGo;

   // generate a bunch of random points
  // for i:=1 to 100 do
//      GLPoints.Positions.Add((Random-0.5)*5, (Random-0.5)*5, (Random-0.5)*5);
end;

procedure TfrmLorenzAttractor.DirectOpenGLRender(Sender : TObject; var rci: TRenderContextInfo);
var
   i : Integer;
   mat : TMatrix;
   p, pProj : TVector;
   planePoint, planeNormal : TVector;
   plane : THmgPlane;
begin
   // Here we recover our plane point and normal...
   planePoint:=GLPlane.Position.AsVector;
   planeNormal:=GLPlane.Direction.AsVector;
   // ...which we use to create a plane (equation)
   plane:=PlaneMake(planePoint, planeNormal);
   // from that plane equation and our pojection direction
   // (which is here the plane normal)
   mat:=MakeParallelProjectionMatrix(plane, planeNormal);

   // save state, turn off lighting and specify the lines color
   glPushAttrib(GL_ENABLE_BIT);
   glDisable(GL_LIGHTING);

   // we'll be drawing a bunch of lines, to specify a line in OpenGL,
   // you only need to specify the line start and end vertices
   glBegin(GL_LINES);
      for i:=0 to GLPoints.Positions.Count-2 do begin
         glColor3f( i / L_STEPS , i /L_STEPS, 1);
         // read the point coordinates, directly from the TGLPoints list
         MakePoint(p, GLPoints.Positions.List[i]);
         // project this point on the plane with the matrix
         MakePoint(pProj, GLPoints.Positions.List[i + 1]);
//         pProj:=VectorTransform(p, mat);
         // specify the two vertices
         glVertex3fv(@p);
         glVertex3fv(@pProj);
      end;
   glEnd;

   // restore state
   glPopAttrib;
end;

procedure TfrmLorenzAttractor.SceneViewerMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   mx:=x; my:=y;
end;

procedure TfrmLorenzAttractor.SceneViewerMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
   if Shift=[ssLeft] then
      GLCamera.MoveAroundTarget(my-y, mx-x)
   else if Shift=[ssRight] then
      GLCamera.RotateObject(GLPlane, my-y, mx-x);
   mx:=x; my:=y;
end;

procedure TfrmLorenzAttractor.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
   GLPlane.Position.Y:=GLPlane.Position.Y+WheelDelta*0.001;
end;

procedure TfrmLorenzAttractor.Timer1Timer(Sender: TObject);
begin
    timer1.Enabled := FALSE;
    BallGo;

//  GLCamera.MoveAroundTarget(0.00, 0.04);
//  inc( fIndex );
//  if fIndex < L_STEPS then begin
//    GLSphere1.Position.AsAffineVector := GLPoints.Positions.List[ fIndex ];
//  end else begin
//    timer1.Enabled := FALSE;
//    fIndex := 0;
//  end;
end;

procedure TfrmLorenzAttractor.Button1Click(Sender: TObject);
begin
  BallGo;
//  for i := 0 to L_STEPS do begin;
//    GLSphere1.Position.AsAffineVector := GLPoints.Positions.List[ i ];
//    Application.ProcessMessages;
//  end;
end;

procedure TfrmLorenzAttractor.BallGo;
var
  i : integer;
begin
  for i := 0 to L_STEPS do begin;
    GLCamera.MoveAroundTarget(0.00, 0.04);
    inc( fIndex );
    if fIndex < L_STEPS then begin
      GLSphere1.Position.AsAffineVector := GLPoints.Positions.List[ i ];
    end else begin
//    timer1.Enabled := FALSE;
      fIndex := 0;
    end;
    Application.ProcessMessages;
  end; // for
  Application.Terminate;
end; // procedure TForm1.BallGo;
end.
