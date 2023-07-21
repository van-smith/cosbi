object frmNBody: TfrmNBody
  Left = 1391
  Top = 200
  BorderIcons = []
  BorderStyle = bsNone
  Caption = 'OpenGL: 4-Body Simulation'
  ClientHeight = 768
  ClientWidth = 1024
  Color = clBtnFace
  DefaultMonitor = dmPrimary
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poDefault
  WindowState = wsMaximized
  OnCreate = FormCreate
  OnKeyPress = FormKeyPress
  PixelsPerInch = 96
  TextHeight = 13
  object SceneViewer: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 1024
    Height = 768
    Camera = GLCamera
    Buffer.BackgroundColor = clBlack
    Buffer.FaceCulling = False
    Buffer.AntiAliasing = aa4xHQ
    FieldOfView = 162.238677978515600000
    Align = alClient
  end
  object GLScene1: TGLScene
    ObjectsSorting = osNone
    Left = 24
    Top = 24
    object DirectOpenGL: TGLDirectOpenGL
      UseBuildList = False
      Blend = False
    end
    object GLPoints: TGLPoints
      NoZWrite = False
      Static = False
      Style = psSmooth
      EffectsData = {
        0201060A54474C4246697265465802000610474C4669726546584D616E616765
        7231}
    end
    object GLPlane: TGLPlane
      Material.FrontProperties.Diffuse.Color = {0000003F0000003F0000803FCDCC4C3F}
      Material.BlendingMode = bmTransparency
      Material.MaterialOptions = [moNoLighting]
      Direction.Coordinates = {000000000000803F0000000000000000}
      ShowAxes = True
      Up.Coordinates = {00000000000000000000803F00000000}
      Height = 0.250000000000000000
      Width = 0.250000000000000000
      NoZWrite = False
      object GLXYZGrid1: TGLXYZGrid
        Up.Coordinates = {0000803F000000000000008000000000}
        AntiAliased = True
        LineColor.Color = {0000803F0000803F0000803F0000003F}
        XSamplingScale.Min = -50.000000000000000000
        XSamplingScale.Max = 50.000000000000000000
        XSamplingScale.Step = 10.000000000000000000
        YSamplingScale.Min = -50.000000000000000000
        YSamplingScale.Max = 50.000000000000000000
        YSamplingScale.Step = 10.000000000000000000
        ZSamplingScale.Step = 10.000000000000000000
        LinesStyle = glsLine
      end
    end
    object GLSphere1: TGLSphere
      Material.BackProperties.Ambient.Color = {0000803F0000803FCDCC4C3E0000803F}
      Material.BackProperties.Diffuse.Color = {0000803FEEED6D3FCDCC4C3F0000803F}
      Material.BackProperties.Emission.Color = {0000803F0000803F000000000000803F}
      Material.BackProperties.Shininess = 19
      Material.FrontProperties.Ambient.Color = {E8E7673FCDCC4C3FCDCC4C3E0000803F}
      Material.FrontProperties.Diffuse.Color = {F0EF6F3FDEDD5D3FCDCC4C3F0000803F}
      Material.FrontProperties.Emission.Color = {0000803F9F9E1E3F000000000000803F}
      Material.FrontProperties.Shininess = 47
      Material.FrontProperties.Specular.Color = {0000803FD3D2523F000000000000803F}
      Radius = 0.200000002980232200
      EffectsData = {
        0201060A54474C4246697265465802000610474C4669726546584D616E616765
        7231}
    end
    object GLSphere2: TGLSphere
      Material.BackProperties.Ambient.Color = {A3A2223FD3D2523FE6E5653F0000803F}
      Material.BackProperties.Shininess = 128
      Material.FrontProperties.Ambient.Color = {8B8A8A3EA3A2223FADAC2C3F0000803F}
      Material.FrontProperties.Diffuse.Color = {0000803F0000803F0000803F0000803F}
      Material.FrontProperties.Shininess = 128
      Position.Coordinates = {0000004000000000000000000000803F}
      Radius = 0.039999999105930330
    end
    object GLSphere3: TGLSphere
      Material.BackProperties.Ambient.Color = {0000803FF0EF6F3FF4F3733F0000803F}
      Material.BackProperties.Diffuse.Color = {0000803F0000803F0000803F0000803F}
      Material.BackProperties.Shininess = 128
      Material.FrontProperties.Ambient.Color = {0000803F0000803F0000803F0000803F}
      Material.FrontProperties.Diffuse.Color = {F6F5753FFAF9793F0000803F0000803F}
      Material.FrontProperties.Shininess = 128
      Position.Coordinates = {6666064000000000000000000000803F}
      Radius = 0.019999999552965160
    end
    object GLSphere4: TGLSphere
      Position.Coordinates = {000020C000000000000000000000803F}
      Radius = 0.100000001490116100
    end
    object GLLightSource2: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Diffuse.Color = {0000803F0000803F000000000000803F}
      LightStyle = lsOmni
      SpotCutOff = 180.000000000000000000
    end
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Diffuse.Color = {0000803F00000000000000000000803F}
      LightStyle = lsOmni
      SpotCutOff = 180.000000000000000000
    end
    object GLCamera: TGLCamera
      DepthOfView = 1000.000000000000000000
      FocalLength = 60.000000000000000000
      TargetObject = GLSphere1
      Position.Coordinates = {0000004000000040000000400000803F}
    end
  end
  object Timer1: TTimer
    Interval = 1
    OnTimer = Timer1Timer
    Left = 24
    Top = 56
  end
  object GLFireFXManager1: TGLFireFXManager
    FireDir.Coordinates = {00000000000000000000000000000000}
    Cadencer = GLCadencer1
    ParticleSize = 0.100000001490116100
    OuterColor.Color = {0000803F00000000000000000000803F}
    FireDensity = 1.000000000000000000
    FireEvaporation = 1.000000000000000000
    FireCrown = 0.050000000745058060
    ParticleLife = 2
    FireBurst = 20.000000000000000000
    FireRadius = 0.109999999403953600
    Disabled = False
    Paused = False
    ParticleInterval = 0.001000000047497451
    UseInterval = False
    Left = 56
    Top = 24
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    FixedDeltaTime = 0.100000000000000000
    Left = 56
    Top = 56
  end
end
