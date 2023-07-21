unit uNBody;
{
  COSBI: Comprehensive Open Source Benchmarking Initiative
  Copyright (c) 2004 Van Smith

  This program is free software; you can redistribute it and/or
  modify it under the terms of the GNU General Public License
  as published by the Free Software Foundation; either version 2
  of the License, or (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

  The current web address of the GNU General Public License is:
  http://www.gnu.org/licenses/gpl.html

  You can contact the authors of this software at:
  cosbi@vanshardware.com
  See www.vanshardware.com or www.cosbi.org for more contact details.
}
//==============================================================================
// Unit name: uNBody
// Unit description: OpenGL version of the nBody test.
// Author: Van Smith
// Date: March 16, 2005
// OS dependent: Yes: Windows
// Resolution dependent: No, but resolution and color depth will impact scores.
// External unit dependencies: GLScene, GLObjects,
//   GLWin32Viewer, GLMisc, OpenGL1x, GLTexture, VectorGeometry, GLGraph,
//   GLGeomObjects, ExtCtrls, StdCtrls, GLFireFX, GLCadencer;
//==============================================================================
// Modification History
// ------------ -------
// Ver  Date   Who    Description
// ==== ====== ====== ==========================================================
// 1.0 050316 Van     Created.
// 1.1 050908 Van     Added SSE2/3 support.
// 1.2 070214 Van     GLContext: hacked out failures on ATi GPUs when destroying
//                    form -- lines 909-911
//         if True then // ApplicationTerminated?                  // +070214van
//            vIgnoreContextActivationFailures:=True               // +070214van
//         else                                                    // +070214van
//==============================================================================

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms, GLScene, GLObjects,
  GLWin32Viewer, GLMisc, OpenGL1x, GLTexture, VectorGeometry, GLGraph,
  GLGeomObjects, ExtCtrls, StdCtrls, GLFireFX, GLCadencer, CosbiCpuid;

const
  NUM_BODIES = 3;
  END_TIME   = 30;

type
//   required for TnBodyTest below
//  TBody = record
//    m:   extended;
//    x:   extended;
//    y:   extended;
//    Vx:  extended;
//    Vy:  extended;
//    Fx:  extended;
//    Fy:  extended;
//  end; // TBody

  Txmm = packed record
    x: double;
    y: double;
  end; // Txmm

  TBody = packed record
  case byte of
  1: (
    x:   double;
    y:   double;
    Vx:  double;
    Vy:  double;
    Fx:  double;
    Fy:  double;
    m:   double;
    fill: double);    // fill is to ensure 16-byte alignment
  2: (
    xy:  Txmm;
    Vxy: Txmm;
    Fxy: Txmm;
    mm:  Txmm);
  end; // TnBodySSE2Test

  TfrmNBody = class(TForm)
    GLScene1: TGLScene;
    SceneViewer: TGLSceneViewer;
    GLCamera: TGLCamera;
    GLPlane: TGLPlane;
    GLPoints: TGLPoints;
    DirectOpenGL: TGLDirectOpenGL;
    GLXYZGrid1: TGLXYZGrid;
    Timer1: TTimer;
    GLSphere1: TGLSphere;
    GLSphere2: TGLSphere;
    GLSphere3: TGLSphere;
    GLSphere4: TGLSphere;
    GLLightSource2: TGLLightSource;
    GLLightSource1: TGLLightSource;
    GLFireFXManager1: TGLFireFXManager;
    GLCadencer1: TGLCadencer;
    procedure FormCreate(Sender: TObject);
//    procedure DirectOpenGLRender(Sender : TObject; var rci: TRenderContextInfo);
    procedure Timer1Timer(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
    fIndex : integer;
    fAbort : Boolean;
    p     : array[0..NUM_BODIES] of TBody;
    procedure BallGo;
    procedure RunTest;
    procedure RunTestSSE2;
    procedure RunTestSse2Scalar;
    procedure RunTestSSE3;
    procedure RunTestFPU;
    procedure IntializeFPU;
    procedure MoveBodies;
  public
    { Public declarations }
    mx, my : Integer;
  end;

var
  frmNBody: TfrmNBody;

implementation

{$R *.dfm}

const

  L_STEPS = 10000;

procedure TfrmNBody.IntializeFPU;
asm
  finit
  wait
end;

procedure TfrmNBody.FormCreate(Sender: TObject);
var
   j : Integer;
   x0, y0, z0, x1, y1, z1 : double;
   h, a, b, c : double;
begin
  fIndex := 0;
  fAbort := FALSE;
  h := 0.01;
  a := 10;
  b := 28;
  c := 8 / 3;
  x0 := 0.1;
  y0 := 0;
  z0 := 0;
  IntializeFPU;
  for j := 0 to L_STEPS do begin
    x1 := x0 + h * a * (y0 - x0);
    y1 := y0 + h * (x0 * (b - z0) - y0);
    z1 := z0 + h * (x0 * y0 - c * z0);
    x0 := x1;
    y0 := y1;
    z0 := z1;
//      glColor3f( 1, 1, 0);
    GLPoints.Positions.Add(x0, y0, z0);
  end; // for j
//    generate a bunch of random points
  for j := 1 to 400 do begin
    GLPoints.Positions.Add( (Random-0.5) * 12, (Random-0.5) * 10 - 2, 4 + (Random-0.5)* 20);
  end; // for j
end;

procedure TfrmNBody.Timer1Timer(Sender: TObject);
begin
  timer1.Enabled := FALSE;
  RunTest;
end;

procedure TfrmNBody.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #27 then begin
    fAbort := TRUE;
  end;
end;

procedure TfrmNBody.BallGo;
var
  i : integer;
begin
  for i := 0 to L_STEPS do begin;
    GLCamera.MoveAroundTarget(0.00, 0.04);
    inc( fIndex );
    if fIndex < L_STEPS then begin
      GLSphere1.Position.AsAffineVector := GLPoints.Positions.List[ i ];
    end else begin
      fIndex := 0;
    end;
    Application.ProcessMessages;
  end; // for
  Application.Terminate;
end; // procedure TForm1.BallGo;

procedure TfrmNBody.RunTest;
begin
  if (CpuidIsSse3Supported and (DetectCpuVendor <> eCpuVendorVIA)) then begin
    RunTestSSE3;
  end else if CpuidIsSse2Supported then begin
    if (DetectCpuVendor = eCpuVendorVIA) then begin
      RunTestSSE2Scalar;
    end else begin
      RunTestSSE2;
    end; // if
  end else begin
    RunTestFpu;
  end; // if
  Application.Terminate;
end;

procedure TfrmNBody.MoveBodies;
begin
  GLSphere1.Position.X := p[0].x;
  GLSphere1.Position.z := p[0].Y;
  GLCamera.Position.X := p[0].x - 2;
  GLCamera.Position.z := p[0].y - 2;
  GLCamera.Position.y := 2;
  GLCamera.Direction.X := p[0].x;
  GLCamera.Direction.z := p[0].y;
  GLCamera.Direction.y := 0;
  GLLightSource2.Position.X := p[0].x;
  GLLightSource2.Position.z := p[0].y;
  GLLightSource2.Position.y := 0;
  GLSphere2.Position.X := p[1].x;
  GLSphere2.Position.z := p[1].Y;
  GLSphere3.Position.x := p[2].x;
  GLSphere3.Position.z := p[2].Y;
  GLSphere4.Position.X := p[3].x;
  GLSphere4.Position.z := p[3].Y;
  GLCadencer1.Progress;
  if fAbort then begin
    ExitCode := 1;
    Application.Terminate;
    Abort;
  end;
  Application.ProcessMessages;
end; // procedure TfrmNBody.MoveBodies;

procedure TfrmNBody.RunTestFPU;
const
  G             = 0.01;
var
  i     : integer;
  j     : integer;
  timesteps : integer;
  n     : integer;
  dt    : extended;
  t     : extended;
  theta : extended;
  F     : extended;
  x,y,r : extended;
  dVx, dVy : extended;
begin
  // intitial conditions
  //mass 0:
  p[0].m  := 2000;
  p[0].x  := 0;
  p[0].y  := 0;
  p[0].Vx := 0;
  p[0].Vy := 0.15; //-0.084;
  //mass 1:
  p[1].m  := 0.02 * p[0].m;
  p[1].x  := 2;
  p[1].y  := 0;
  p[1].Vx := 0;
  p[1].Vy := 0.9 * sqrt( G * p[0].m / 2 );
  //mass 2:
  p[2].m  := 0.002 * p[0].m; //0.00001 * p[0].m;//0.002 * p[0].m; //
  p[2].x  := 2.1;
  p[2].y  := 0;
  p[2].Vx := 0;
  p[2].Vy := p[1].Vy + sqrt( G * p[1].m / 0.1);
  //mass 3:
  p[3].m  := 0.06 * p[0].m;
  p[3].x  := -2.5;
  p[3].y  := 0;
  p[3].Vx := 0;
  p[3].Vy := sqrt( G * p[0].m / 2.5 );
  MoveBodies;
  dt := 0.00001;
  t := 0;
  timesteps := round( END_TIME / dt );
  for j := 1 to timesteps do begin
    t := t + dt;
    // zero out forces for all bodies:
    for n := 0 TO NUM_BODIES do begin
      p[n].Fx := 0;
      p[n].Fy := 0;
    end;
    for n := 0 TO NUM_BODIES do begin
      for i := n to NUM_BODIES do begin
        if n <> i then begin
          r := sqrt( sqr( p[i].x - p[n].x ) + sqr( p[i].y - p[n].y ) );
          F := G * p[i].m * p[n].m / (r * r);
          y := p[i].y - p[n].y;
          x := p[i].x - p[n].x;
          theta := ArcTan(y / x);
          if x < 0 then theta := theta + PI;
          // the force on the body by another
          p[n].Fx := p[n].Fx + F * cos(theta);
          p[n].Fy := p[n].Fy + F * sin(theta);
          // the force that the body exerts on another
          p[i].Fx := p[i].Fx - F * cos(theta);
          p[i].Fy := p[i].Fy - F * sin(theta);
        end; // if n <> i
      end; // next i;
      dVx := p[n].Fx * dt / p[n].m;
      dVy := p[n].Fy * dt / p[n].m;
      p[n].Vx := p[n].Vx + dVx;
      p[n].Vy := p[n].Vy + dVy;
      p[n].x := p[n].x + p[n].Vx * dt;
      p[n].y := p[n].y + p[n].Vy * dt;
      if ( j mod 800 = 0 ) and ( n = NUM_BODIES ) then begin
        MoveBodies;
      end; // if j mod 100 = 0 then
    end; // NEXT n
  end; // NEXT j
end; // procedure TfrmNBody.RunTest;

procedure TfrmNBody.RunTestSSE2;
var
  i     : integer;
  n     : integer;
  fSinTheta, fCosTheta : double;
  t     : double;
  theta : double;
  dVx, dVy : double;
  G     : double;
  F     : double;
  x     : double;
  dtdt  : Txmm;
  dt    : double;
  // temp variables for SSE2:
  p_i_xy, p_i_VxVy, p_i_FxFy, p_i_mm, p_n_xy, p_n_Vxy, p_n_Fxy, p_n_mm : ^Txmm;
  timesteps : integer;
  j     : integer;
begin
  G := 0.01;
  // intitial conditions
  //mass 0:
  p[0].m  := 2000;
  p[0].fill := p[0].m;
  p[0].x  := 0;
  p[0].y  := 0;
  p[0].Vx := 0;
  p[0].Vy := 0.15; //-0.084;
  //mass 1:
  p[1].m  := 0.02 * p[0].m;
  p[1].fill := p[1].m;
  p[1].x  := 2;
  p[1].y  := 0;
  p[1].Vx := 0;
  p[1].Vy := 0.9 * sqrt( G * p[0].m / 2 );
  //mass 2:
  p[2].m  := 0.002 * p[0].m; //0.00001 * p[0].m;//0.002 * p[0].m; //
  p[2].fill := p[2].m;
  p[2].x  := 2.1;
  p[2].y  := 0;
  p[2].Vx := 0;
  p[2].Vy := p[1].Vy + sqrt( G * p[1].m / 0.1);
  //mass 3:
  p[3].m  := 0.06 * p[0].m;
  p[3].fill := p[3].m;
  p[3].x  := -2.5;
  p[3].y  := 0;
  p[3].Vx := 0;
  p[3].Vy := sqrt( G * p[0].m / 2.5 );
  MoveBodies;
  dt := 0.00001;
  t := 0;
  dtdt.x := dt; dtdt.y := dt;
  timesteps := round( END_TIME / dt );
  for j := 1 to timesteps do begin
    //t := t + dt;
    asm
      movsd xmm0, t
      addsd xmm0, dt
      movsd t, xmm0
    end;
    // zero out forces for all bodies:
    for n := 0 TO NUM_BODIES do begin
      p[n].Fx := 0;
      p[n].Fy := 0;
    end;
    for n := 0 TO NUM_BODIES do begin
      for i := n to NUM_BODIES do begin
        if n <> i then begin
          p_i_xy := @p[i].xy;
          p_n_xy := @p[n].xy;
          p_i_mm := @p[i].m;
          p_n_mm := @p[n].m;
          asm
            // calculate [x, y]:
            mov     eax,   p_i_xy
            movupd  xmm0,  Txmm [eax] // xmm0 := [p[i].x, p[i].y]
            mov     eax,   p_n_xy
            movupd  xmm1,  Txmm [eax] // xmm1 := [p[n].x, p[n].y]
            subpd   xmm0,  xmm1    // xmm0 := [x, y] := [p[i].x - p[n].x, p[i].y - p[n].y]
            // calculate y/x:
            movhlps xmm5,  xmm0    // xmm5 := [y, 0], xmm0 := [x, y]
            divsd   xmm5,  xmm0    // xmm5 := y/x
            movsd   theta, xmm5    // theta := y/x
            // store x for theta correction below:
            movsd   x,     xmm0
            // calculate r^ := x^2 + y^2:
            mulpd   xmm0,  xmm0    // xmm0 := [x^2, y^2]
            movhlps xmm1,  xmm0    // xmm1 := [y^2, 0], xmm0 := [x^2, y^2]
            addsd   xmm0,  xmm1    // xmm0 := x^2 + y^2 = r^2
            // calculate force component:
            mov     eax,   p_i_mm
            movsd   xmm1,  [eax]   // xmm1 := p[i].m
            mov     eax,   p_n_mm
            mulsd   xmm1,  [eax]   // xmm1 := p[i].m * p[n].m
            mulsd   xmm1,  G       // xmm1 := G * p[i].m * p[n].m
            divsd   xmm1,  xmm0    // xmm1 := G * p[i].m * p[n].m / r^2
            movsd   F,     xmm1    // F := G * p[i].m * p[n].m / r^2;
          end;
          theta := ArcTan( theta ); // theta := ArcTan( y/x )
          //y := p[i].y - p[n].y; see xmm5 above
          //x := p[i].x - p[n].x; see xmm4 above
          //theta := ArcTan(y / x); see above
          if x < 0 then theta := theta + PI;
          // the force on the body by another
          fCosTheta := F * cos(theta);
          fSinTheta := F * sin(theta);
          p[n].Fx := p[n].Fx + fCosTheta;
          p[n].Fy := p[n].Fy + fSinTheta;
          // the force that the body exerts on another
          p[i].Fx := p[i].Fx - fCosTheta;
          p[i].Fy := p[i].Fy - fSinTheta;
        end; // if n <> i
      end; // next i;
      //dVx := p[n].Fx * dt / p[n].m;
      //dVy := p[n].Fy * dt / p[n].m;
      p_n_Fxy  := @p[n].Fxy;
      p_n_Vxy  := @p[n].Vxy;
      p_n_xy   := @p[n].xy;
      p_n_mm   := @p[n].mm;
      asm
        movupd xmm7, dtdt      // xmm7 := dt, dt
        movapd xmm6, xmm7      // xmm6 := dt, dt
        mov    eax,  p_n_mm
        movupd xmm5, Txmm [eax]   // xmm5 := [p[n].m, p[n].m]
        divpd  xmm7, xmm5    // xmm7 := [ dt / p[n].m, dt / p[n].m]
        mov    eax,  p_n_Fxy
        movupd xmm0, Txmm [eax] // xmm0 := [p[n].Fx, p[n].Fy]
        mulpd  xmm0, xmm7    // xmm0 := [p[n].Fx * dt / p[n].m, p[n].Fy * dt / p[n].m] := [dVx, dVy]
        mov    eax,  p_n_Vxy
        movupd xmm1, Txmm [eax] // xmm1 := [p[n].Vx, p[n].Vy]
        addpd  xmm0, xmm1    // xmm0 := [p[n].Vx + dVx, p[n].Vy + dVy]
        movupd Txmm [eax], xmm0 // store p[n].Vx, p[n].Vy
        mulpd  xmm0, xmm6    // xmm0 := [p[n].Vx * dt, p[n].Vy * dt]
        mov    eax,  p_n_xy
        movupd xmm1, Txmm [eax] // xmm1 := [p[n].x, p[n].y]
        addpd  xmm0, xmm1    // xmm0 := [p[n].x + p[n].Vx * dt, p[n].y + p[n].Vy * dt]
        movupd Txmm [eax], xmm0 // store [p[n].x, store p[n].y
      end;
      if ( j mod 800 = 0 ) and ( n = NUM_BODIES ) then begin
        MoveBodies;
      end; // if j mod 100 = 0 then
    end; // NEXT n
  end; // NEXT j
end; // procedure TnBodyTest.RunTestSSE2;

procedure TfrmNBody.RunTestSSE3;
var
  dtdt  : Txmm;
  i     : integer;
  j     : integer;
  timesteps : integer;
  n     : integer;
  dt    : double;
  t     : double;
  theta : double;
  F     : double;
  x,y,r : double;
  dVx, dVy : double;
  // temp variables for SSE2:
  p_i_xy, p_i_VxVy, p_i_FxFy, p_i_mm : ^Txmm;
  p_n_xy, p_n_Vxy, p_n_Fxy, p_n_mm : ^Txmm;
  fCosTheta : double;
  fSinTheta : double;
  G : double;
begin
  G := 0.01;
  // intitial conditions
  //mass 0:
  p[0].m  := 2000;
  p[0].fill := p[0].m;
  p[0].x  := 0;
  p[0].y  := 0;
  p[0].Vx := 0;
  p[0].Vy := 0.15; //-0.084;
  //mass 1:
  p[1].m  := 0.02 * p[0].m;
  p[1].fill := p[1].m;
  p[1].x  := 2;
  p[1].y  := 0;
  p[1].Vx := 0;
  p[1].Vy := 0.9 * sqrt( G * p[0].m / 2 );
  //mass 2:
  p[2].m  := 0.002 * p[0].m; //0.00001 * p[0].m;//0.002 * p[0].m; //
  p[2].fill := p[2].m;
  p[2].x  := 2.1;
  p[2].y  := 0;
  p[2].Vx := 0;
  p[2].Vy := p[1].Vy + sqrt( G * p[1].m / 0.1);
  //mass 3:
  p[3].m  := 0.06 * p[0].m;
  p[3].fill := p[3].m;
  p[3].x  := -2.5;
  p[3].y  := 0;
  p[3].Vx := 0;
  p[3].Vy := sqrt( G * p[0].m / 2.5 );
  MoveBodies;
  dt := 0.00001;
  dtdt.x := dt;  dtdt.y := dt;
  t := 0;
  timesteps := round( END_TIME / dt );
  for j := 1 to timesteps do begin
    //t := t + dt;
    asm
      movsd xmm0, t
      addsd xmm0, dt
      movsd t, xmm0
    end;
    // zero out forces for all bodies:
    for n := 0 TO NUM_BODIES do begin
      p[n].Fx := 0;
      p[n].Fy := 0;
    end;
    for n := 0 TO NUM_BODIES do begin
      for i := n to NUM_BODIES do begin
        if n <> i then begin
          p_i_xy := @p[i].xy;
          p_n_xy := @p[n].xy;
          p_i_mm := @p[i].mm;
          p_n_mm := @p[n].mm;
          asm
            // calculate [x, y]:
            mov     eax,    p_i_xy
            movupd  xmm0,   Txmm [eax]  // xmm0 := [p[i].x, p[i].y]
            mov     eax,    p_n_xy
            movupd  xmm1,   Txmm [eax]  // xmm1 := p[n].x
            subpd   xmm0,   xmm1        // xmm0 := [x, y] := [p[i].x - p[n].x, p[i].y - p[n].y]
            // calculate y/x:
            movhlps xmm5,   xmm0        // xmm5 := [y, 0], xmm4 := [x, y]
            divsd   xmm5,   xmm0        // xmm5 := y/x
            movsd   theta,  xmm5        // theta := y/x
            // store x for theta correction below:
            movsd   x,      xmm0
            // calculate r^ := x^2 + y^2:
            mulpd   xmm0,   xmm0        // xmm0 := [x^2, y^2]
            //SSE3:
            haddpd  xmm0,   xmm0          // xmm0 := [x^2 + y^2, x^2 + y^2] := [r^2, r^2]
            // calculate force component:
            mov     eax,    p_i_mm
            movsd   xmm1,   [eax]       // xmm1 := p[i].m
            mov     eax,    p_n_mm
            mulsd   xmm1,   [eax]       // xmm1 := p[i].m * p[n].m
            mulsd   xmm1,   G           // xmm1 := G * p[i].m * p[n].m
            divsd   xmm1,   xmm0        // xmm1 := G * p[i].m * p[n].m / r^2
            movsd   F,      xmm1        // F := G * p[i].m * p[n].m / r^2;
          end;
          theta := ArcTan( theta ); // theta := ArcTan( y/x )
          //y := p[i].y - p[n].y; see xmm5 above
          //x := p[i].x - p[n].x; see xmm4 above
          //theta := ArcTan(y / x); see above
          if x < 0 then theta := theta + PI;
          // the force on the body by another
          fCosTheta := F * cos(theta);
          fSinTheta := F * sin(theta);
          p[n].Fx := p[n].Fx + fCosTheta;
          p[n].Fy := p[n].Fy + fSinTheta;
          // the force that the body exerts on another
          p[i].Fx := p[i].Fx - fCosTheta;
          p[i].Fy := p[i].Fy - fSinTheta;
        end; // if n <> i
      end; // next i;
      //dVx := p[n].Fx * dt / p[n].m;
      //dVy := p[n].Fy * dt / p[n].m;
      p_n_Fxy := @p[n].Fx;
      p_n_Vxy := @p[n].Vx;
      p_n_xy := @p[n].x;
      p_n_mm := @p[n].m;
      asm
        movupd xmm7, dtdt    // xmm7 := dt, dt
        movapd xmm6, xmm7    // xmm6 := dt, dt
        mov    eax,  p_n_mm
        movupd xmm5, [eax]   // xmm5 := [ p[n].m, p[n].m]
        divpd  xmm7, xmm5    // xmm7 := [ dt / p[n].m, dt / p[n].m ]
        mov    eax,  p_n_Fxy
        movupd xmm0, Txmm [eax] // xmm0 := [p[n].Fx, p[n].Fy]
        mulpd  xmm0, xmm7    // xmm0 := [p[n].Fx * dt / p[n].m, p[n].Fy * dt / p[n].m] = [dVx, dVy]
        mov    eax,  p_n_Vxy
        movupd xmm1, Txmm [eax] // xmm1 := [p[n].Vx, p[n].Vy]
        addpd  xmm0, xmm1    // xmm0 := [p[n].Vx + dVx, p[n].Vy + dVy]
        movupd Txmm [eax], xmm0 // store p[n].Vx, p[n].Vy
        mulpd  xmm0, xmm6    // xmm0 := [p[n].Vx * dt, p[n].Vy * dt]
        mov    eax,  p_n_xy
        movupd xmm1, Txmm [eax] // xmm1 := [p[n].x, p[n].y]
        addpd  xmm0, xmm1    // xmm0L := [p[n].x + p[n].Vx * dt, p[n].y + p[n].Vy * dt]
        movupd Txmm [eax], xmm0 // store p[n].x, p[n].y
      end;
      if ( j mod 800 = 0 ) and ( n = NUM_BODIES ) then begin
        MoveBodies;
      end; // if j mod 100 = 0 then
    end; // NEXT n
  end; // NEXT j
end; // procedure TfrmNBody.RunTestSSE3;

procedure TfrmNBody.RunTestSse2Scalar;
var
  i     : integer;
  j     : integer;
  timesteps : integer;
  n     : integer;
  dt    : double;
  t     : double;
  theta : double;
  F     : double;
  x,y,r : double;
  dVx, dVy : double;
  // temp variables for SSE2:
  p_i_m, p_i_x, p_i_y, p_i_Vx, p_i_Vy, p_i_Fx, p_i_Fy : ^double;
  p_n_m, p_n_x, p_n_y, p_n_Vx, p_n_Vy, p_n_Fx, p_n_Fy : ^double;
  fCosTheta : double;
  fSinTheta : double;
  G : double;
begin
  G := 0.01;
  // intitial conditions
  //mass 0:
  p[0].m  := 2000;
  p[0].x  := 0;
  p[0].y  := 0;
  p[0].Vx := 0;
  p[0].Vy := 0.15; //-0.084;
  //mass 1:
  p[1].m  := 0.02 * p[0].m;
  p[1].x  := 2;
  p[1].y  := 0;
  p[1].Vx := 0;
  p[1].Vy := 0.9 * sqrt( G * p[0].m / 2 );
  //mass 2:
  p[2].m  := 0.002 * p[0].m; //0.00001 * p[0].m;//0.002 * p[0].m; //
  p[2].x  := 2.1;
  p[2].y  := 0;
  p[2].Vx := 0;
  p[2].Vy := p[1].Vy + sqrt( G * p[1].m / 0.1);
  //mass 3:
  p[3].m  := 0.06 * p[0].m;
  p[3].x  := -2.5;
  p[3].y  := 0;
  p[3].Vx := 0;
  p[3].Vy := sqrt( G * p[0].m / 2.5 );
  MoveBodies;
  dt := 0.00001;
  t := 0;
  timesteps := round( END_TIME / dt );
  for j := 1 to timesteps do begin
    //t := t + dt;
    asm
      movsd xmm0, t
      addsd xmm0, dt
      movsd t, xmm0
    end;
    // zero out forces for all bodies:
    for n := 0 TO NUM_BODIES do begin
      p[n].Fx := 0;
      p[n].Fy := 0;
    end;
    for n := 0 TO NUM_BODIES do begin
      for i := n to NUM_BODIES do begin
        if n <> i then begin
          p_i_x := @p[i].x;
          p_n_x := @p[n].x;
          p_i_m := @p[i].m;
          p_n_m := @p[n].m;
          p_i_y := @p[i].y;
          p_n_y := @p[n].y;
          asm
            // calculate x:
            mov   eax,  p_i_x
            movsd xmm0, [eax]   // xmm0 := p[i].x
            mov   eax,  p_n_x
            subsd xmm0, [eax]   // xmm0 := x := p[i].x - p[n].x
            // store x in xmm4:
            movsd xmm4, xmm0    // xmm4 := x
            // store x for later use in arctan correction:
            movsd x, xmm0
            // square x:
            mulsd xmm0, xmm0    // xmm0: x^2
            // calculate y:
            mov   eax,  p_i_y
            movsd xmm2, [eax]   // xmm2 := p[i].y
            mov   eax,  p_n_y
            subsd xmm2, [eax]   // xmm2 := y := p[i].y - p[n].y
            // store y in xmm5:
            movsd xmm5, xmm2    // xmm5 := y
            // square y:
            mulsd xmm2, xmm2    // xmm2 := y^2
            // calculate r^2:
            addsd xmm0, xmm2    // xmm0 := x^2 + y^2
            // calculate y/x:
            divsd xmm5, xmm4    // xmm5 := y/x
            // store y/x in theta for arctan calculation:
            movsd theta, xmm5   // theta := y/x
            // calculate force component:
            mov   eax,  p_i_m
            movsd xmm1, [eax]   // xmm1 := p[i].m
            mov   eax,  p_n_m
            mulsd xmm1, [eax]   // xmm1 := p[i].m * p[n].m
            mulsd xmm1, G       // xmm1 := G * p[i].m * p[n].m
            divsd xmm1, xmm0    // xmm1 := G * p[i].m * p[n].m / r^2
            movsd F, xmm1       // F := G * p[i].m * p[n].m / r^2;
          end;
          theta := ArcTan( theta ); // theta := ArcTan( y/x )
          //y := p[i].y - p[n].y; see xmm5 above
          //x := p[i].x - p[n].x; see xmm4 above
          //theta := ArcTan(y / x); see above
          if x < 0 then theta := theta + PI;
          // the force on the body by another
          fCosTheta := F * cos(theta);
          fSinTheta := F * sin(theta);
          p[n].Fx := p[n].Fx + fCosTheta;
          p[n].Fy := p[n].Fy + fSinTheta;
          // the force that the body exerts on another
          p[i].Fx := p[i].Fx - fCosTheta;
          p[i].Fy := p[i].Fy - fSinTheta;
        end; // if n <> i
      end; // next i;
      //dVx := p[n].Fx * dt / p[n].m;
      //dVy := p[n].Fy * dt / p[n].m;
      p_n_Fx  := @p[n].Fx;
      p_n_m   := @p[n].m;
      p_n_Fy  := @p[n].Fy;
      p_n_Vx  := @p[n].Vx;
      p_n_Vy  := @p[n].Vy;
      p_n_x   := @p[n].x;
      p_n_y   := @p[n].y;
      asm
        movsd xmm7,   dt      // xmm7 := dt
        movsd xmm6,   xmm7    // xmm6 := dt
        mov   eax,    p_n_m
        divsd xmm6,   [eax]   // xmm6 := dt / p[n].m
        mov   eax,    p_n_Fx
        movsd xmm0,   [eax]   // xmm0 := p[n].Fx
        mulsd xmm0,   xmm6    // xmm0 := dVx := p[n].Fx * dt / p[n].m
        mov   eax,    p_n_Vx
        addsd xmm0,   [eax]   // xmm0 := p[n].Vx + dVx := new p[n].Vx
        movsd [eax],  xmm0    // store p[n].Vx
        // p[n].x := p[n].x + p[n].Vx * dt;
        mulsd xmm0,   xmm7    // xmm0 := p[n].Vx * dt
        mov   eax,    p_n_x
        addsd xmm0,   [eax]   // xmm0 := p[n].x + p[n].Vx * dt := new p[n].x
        movsd [eax],  xmm0    // store p[n].x
        // now calculate the y components:
        mov   eax,    p_n_Fy
        movsd xmm1,   [eax]   // xmm1 := p[n].Fy
        mulsd xmm1,   xmm6    // xmm1 := p[n].Fy * dt / p[n].m
        // p[n].Vy := p[n].Vy + dVy;
        mov   eax,    p_n_Vy
        addsd xmm1,   [eax]   // xmm1 := p[n].Vy := p[n].Vy + dVy
        movsd [eax],  xmm1    // store p[n].Vy
        // p[n].y := p[n].y + p[n].Vy * dt;
        mulsd xmm1,   xmm7    // xmm1 := p[n].Vy * dt
        mov   eax,    p_n_y
        addsd xmm1,   [eax]   // xmm1 := p[n].y + p[n].Vy * dt := new p[n].y
        movsd [eax],  xmm1    // store p[n].y
      end;
      if ( j mod 800 = 0 ) and ( n = NUM_BODIES ) then begin
        MoveBodies;
      end; // if j mod 100 = 0 then
    end; // NEXT n
  end; // NEXT j
end; // procedure TfrmNBody.RunTestSse2Scalar;

end.
