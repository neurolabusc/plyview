unit Unit1;
{$mode objfpc}{$H+}
//OpenGL 4.1 Core demo - displays PLY format images

//If both of the next two lines are commented, use the GLCOREARB library
// {$DEFINE GLEXT} //If this line is uncommented, use GL/GLEXT library - Fails with MacOS
// {$DEFINE DGL} //If this line is uncommented, use dglOpenGL library

//{$DEFINE RETINA} // <- requires patch to OpenGLContext.pas to support retina OpenGL

interface
uses
  {$IFDEF GLEXT}
  gl, glext,
  {$ELSE}
   {$IFDEF DGL}
    dglOpenGL,
   {$ELSE}
    glcorearb,
   {$ENDIF}
  {$ENDIF}
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Menus, OpenGLContext, glmath, lcltype, lclintf, Grids, mesh,
  meshify_simplify_quadric;
type
  TGLForm1 = class(TForm)
    ColorDialog1: TColorDialog;
    ErrorTimer: TTimer;
    MainMenu1: TMainMenu;
    FileMenu: TMenuItem;
    AppleMenu: TMenuItem;
    AboutMenu: TMenuItem;
    ColorMenu: TMenuItem;
    BackColorMenu: TMenuItem;
    GrayColorMenu: TMenuItem;
    DecimateMenu: TMenuItem;
    SaveDialog1: TSaveDialog;
    SaveMenu: TMenuItem;
    PerspectiveMenu: TMenuItem;
    RedColorMenu: TMenuItem;
    GreenColorMenu: TMenuItem;
    BlueColorMenu: TMenuItem;
    CustomColorMenu: TMenuItem;
    ObjectColorMenu: TMenuItem;
    RotateMenu: TMenuItem;
    OpenDialog1: TOpenDialog;
    OpenMenu: TMenuItem;
    procedure DecimateMenuClick(Sender: TObject);
    procedure OpenMesh(Filename: string);
    procedure AboutMenuClick(Sender: TObject);
    procedure BackColorMenuClick(Sender: TObject);
    procedure DropFiles(Sender: TObject; const FileNames: array of String);
    procedure ErrorTimerTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure GLboxPaint(Sender: TObject);
    procedure GLboxMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure GLboxMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLboxMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ObjectColorMenuClick(Sender: TObject);
    procedure OpenMenuClick(Sender: TObject);
    procedure PerspectiveMenuClick(Sender: TObject);
    procedure RotateMenuClick(Sender: TObject);
    procedure SaveMenuClick(Sender: TObject);
    procedure ShowmessageError(s: string);
    procedure GLBoxMouseWheel(Sender: TObject; Shift: TShiftState;   WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  private
    { private declarations }
  public
    { public declarations }
  end;
var
  GLForm1: TGLForm1;

implementation
{$R *.lfm}

type


TVtxNormClr = Packed Record
  vtx   : TPoint3f; //vertex coordinates
  norm : int32;
  clr : TRGBA;
end;

TShader = record
  vbo_face, vbo_point, vao_point, shaderProgram, vertexArrayObject: GLuint;
  nface, uniform_ModelViewProjectionMatrix, uniform_ModelViewMatrix, uniform_NormalMatrix: GLint;
  Distance: single;
  faces: TFaces;
  vertices: TVertices;
  vertexRGBA: TVertexRGBA;
  objColor, backColor: TColor;
  isPerspective : boolean;
end;

const
 kMaxDistance = 10;

 kVert = '#version 330'
+#10'layout(location = 0) in vec3 Vert;'
+#10'layout(location = 3) in vec3 Norm;'
+#10'layout(location = 6) in vec4 Clr;'
+#10'out vec3 vN, vL, vV;'
+#10'out vec4 vClr;'
+#10'uniform mat4 ModelViewProjectionMatrix;'
+#10'uniform mat4 ModelViewMatrix;'
+#10'uniform mat3 NormalMatrix;'
+#10'uniform vec3 light_pos = vec3(0.0, 20.0, 30.0); //LR, -DU+, -FN+'
+#10'void main() {'
+#10'    vN = normalize((NormalMatrix * Norm));'
+#10'    gl_Position = ModelViewProjectionMatrix * vec4(Vert, 1.0);'
+#10'    vL = normalize(light_pos);'
+#10'    vV = -vec3(ModelViewMatrix*vec4(Vert,1.0));'
+#10'    vClr = Clr;'
+#10'}';

//Blinn/Phong Shader GPLv2 (C) 2007 Dave Griffiths, FLUXUS GLSL library
kFrag = '#version 330'
+#10'in vec4 vClr;'
+#10'in vec3 vN, vL, vV;'
+#10'out vec4 color;'
+#10'uniform float Ambient = 0.4;'
+#10'uniform float Diffuse = 0.7;'
+#10'uniform float Specular = 0.6;'
+#10'uniform float Roughness = 0.1;'
+#10'void main() {'
+#10' vec3 n = normalize(vN);'
+#10' vec3 v = normalize(vV);'
+#10' vec3 h = normalize(vL+v);'
+#10' float diffuse = dot(vL,n);'
+#10' vec3 AmbientColour = vClr.rgb;'
+#10' vec3 DiffuseColour = vClr.rgb;'
+#10' vec3 SpecularColour = vec3(1.0, 1.0, 1.0);'
+#10' float specular =  pow(max(0.0,dot(n,h)),1.0/(Roughness * Roughness));'
+#10' color = vec4(AmbientColour*Ambient + DiffuseColour*diffuse*Diffuse +SpecularColour*specular* Specular, 1.0);'
+#10'}';

var
  gShader: TShader;
  GLBox:TOpenGLControl;
  gGLerror : string = '';
  gMouseX : integer = -1;
  gMouseY : integer = -1;
  gTranslateX : integer = 0;
  gTranslateY : integer = 0;
  gElevation : integer = 30;
  gAzimuth : integer = 30;

procedure TGLForm1.ShowmessageError(s: string);
begin
  if gGLerror <> '' then exit;
  gGLerror := s;
  ErrorTimer.Enabled := true;
end;

procedure ReportErrorsGL(glObjectID: GLuint);
var
  s : string;
  maxLength : GLint;
begin
  glGetShaderiv(glObjectID, GL_INFO_LOG_LENGTH, @maxLength);
  if (maxLength < 1) then exit;
  setlength(s, maxLength);
  //{$IFDEF DGL}
  //glGetShaderInfoLog(glObjectID, maxLength, maxLength, @s[1]);
  //{$ELSE}
  glGetShaderInfoLog(glObjectID, maxLength, @maxLength, @s[1]);
  //{$ENDIF}
  s:=trim(s);
  if (length(s) < 1) then exit;
  GLForm1.ShowmessageError('GLSL error '+s);
end;

procedure GetError(p: integer);  //report OpenGL Error
var
  Error: GLenum;
  s: string;
begin
 Error := glGetError();
 if Error = GL_NO_ERROR then exit;
 s := inttostr(p)+'->';
 if Error = GL_INVALID_ENUM then
    s := s+'GL_INVALID_ENUM'
 else if Error = GL_INVALID_VALUE then
    s := s+'GL_INVALID_VALUE'
 else
     s := s + inttostr(Error);
 GLForm1.ShowmessageError('GLSL error : '+s );
end;

function compileShaderOfType (shaderType: GLEnum;  shaderText: string): GLuint;
var
   status: GLint;
begin
   result := glCreateShader(shaderType);
   {$IFDEF DGL}
   glShaderSource(result, 1, PPGLChar(@shaderText), nil);
   {$ELSE}
   glShaderSource(result, 1, PChar(@shaderText), nil);
   {$ENDIF}
   glCompileShader(result);
   ReportErrorsGL(result);
   status := 0;
   glGetShaderiv(result, GL_COMPILE_STATUS, @status);
   if (status =  0) then  //report compiling errors.
      GLForm1.ShowmessageError('GLSL shader failure ' );
end;

function  initVertFrag(vert, frag: string): GLuint;
var
   fr, vt: GLuint;
begin
  result := 0;
  GetError(0);
  fr := compileShaderOfType(GL_FRAGMENT_SHADER, frag);
  if (fr = 0) then exit;
  result := glCreateProgram();
  glAttachShader(result, fr);
  if (length(vert) > 0) then begin
        vt := compileShaderOfType(GL_VERTEX_SHADER, vert);
        if (vt = 0) then exit;
        glAttachShader(result, vt);
  end;
  glLinkProgram(result);
  glDetachShader(result, fr);
  glDeleteShader(fr);
  if (length(vert) > 0) then begin
      glDetachShader(result, vt);
      glDeleteShader(vt);
  end;
  GetError(1);
end;

function crossProduct(v1, v2: TPoint3f): TPoint3f;
// http://openinggl.blogspot.com/2012/04/adding-lighting-normals.html
begin
     result := ptf(v1.y * v2.z - v1.z * v2.y, v1.z * v2.x - v1.x * v2.z,
     v1.x * v2.y - v1.y * v2.x);
end; // CrossProduct()

function getSurfaceNormal(v1, v2, v3: TPoint3f): TPoint3f;
var
   polyVector1, polyVector2: TPoint3f;
begin
 polyVector1 := ptf(v2.x - v1.x, v2.y - v1.y, v2.z - v1.z);
 polyVector2 := ptf(v3.x - v1.x, v3.y - v1.y, v3.z - v1.z);
 result := crossProduct(polyVector1, polyVector2);
 //make sure to eventually normalize the result!
end; // getSurfaceNormal()

procedure vectorAdd (var A: TPoint3f; B: TPoint3f);  inline;
//sum two vectors
begin
     A.X := A.X + B.X;
     A.Y := A.Y + B.Y;
     A.Z := A.Z + B.Z;
end; // vectorAdd()

procedure vectorNormalize(var v: TPoint3f);  inline;
var
   len: single;
begin
     len := sqrt( (v.X*v.X) + (v.Y*v.Y) + (v.Z*v.Z) );
     if len <= 0 then exit;
     v.X := v.X / len;
     v.Y := v.Y / len;
     v.Z := v.Z / len;
end; // normalize()

function Float2Int16(fv: single): int16;
var
   f: single;
begin
     f := fv;
     if f > 1 then
        f := 1;
     if f < -1 then
        f := -1;
     if f > 0 then
        result := round(f * 32767)
     else
         result := round(f * 32768);
end;

function AsGL_INT_2_10_10_10_REV(f: TPoint3f): int32;
//pack 3 32-bit floats as 10 bit signed integers, assumes floats normalized to -1..1
var
   x,y,z: uint16;
begin
     x := uint16(Float2Int16(f.X)) shr 6;
     y := uint16(Float2Int16(f.Y)) shr 6;
     z := uint16(Float2Int16(f.Z)) shr 6;
     result := (z shl 20)+ (y shl 10) + (x shl 0);
end;

procedure LoadBufferData (var faces: TFaces; var vertices: TVertices;  var vertexRGBA: TVertexRGBA) ;
const
    kATTRIB_VERT = 0;  //vertex XYZ are positions 0,1,2
    kATTRIB_NORM = 3;  //normal XYZ are positions 3,4,5
    kATTRIB_CLR = 6;   //color RGBA are positions 6,7,8,9
var
  vnc: array of TVtxNormClr;
  vNorm: array of TPoint3f;
  fNorm: TPoint3f;
  i: integer;
begin
  //compute surface normals...
  setlength(vNorm, length(vertices));
  fNorm := ptf(0,0,0);
  for i := 0 to (length(vertices)-1) do
      vNorm[i] := fNorm;
  for i := 0 to (length(faces)-1) do begin //compute the normal for each face
      fNorm := getSurfaceNormal(vertices[faces[i].X], vertices[faces[i].Y], vertices[faces[i].Z]);
      vectorAdd(vNorm[faces[i].X] , fNorm);
      vectorAdd(vNorm[faces[i].Y] , fNorm);
      vectorAdd(vNorm[faces[i].Z] , fNorm);
  end;
  for i := 0 to (length(vertices)-1) do
      vectorNormalize(vNorm[i]);
  //create VBO that combines vertex, normal and color information
  setlength(vnc, length(vertices));
  //set every vertex
  vnc[0].clr.r := red(gShader.objColor);
  vnc[0].clr.g := green(gShader.objColor);
  vnc[0].clr.b := blue(gShader.objColor);
  vnc[0].clr.a := 255;
  for i := 0 to (length(vertices) -1) do begin
      vnc[i].vtx := vertices[i];
      vnc[i].norm :=  AsGL_INT_2_10_10_10_REV(vNorm[i]);
      //vnc[i].norm := vNorm[i];
      vnc[i].clr := vnc[0].clr;
      //fNorm := getSurfaceNormal(vertices[faces[i].X], vertices[faces[i].Y], vertices[faces[i].Z]);
  end;
  if length(vertexRGBA) = length(vertices) then
     for i := 0 to (length(vertices) -1) do
         vnc[i].clr := vertexRGBA[i];
  if (gShader.vbo_point <> 0) then
     glDeleteBuffers(1, @gShader.vbo_point);
  glGenBuffers(1, @gShader.vbo_point);
  glBindBuffer(GL_ARRAY_BUFFER, gShader.vbo_point);
  glBufferData(GL_ARRAY_BUFFER, Length(vnc)*SizeOf(TVtxNormClr), @vnc[0], GL_STATIC_DRAW);
  glBindBuffer(GL_ARRAY_BUFFER, 0);
  // Prepare vertrex array object (VAO)
  if gShader.vao_point <> 0 then
     glDeleteVertexArrays(1,@gShader.vao_point);
  glGenVertexArrays(1, @gShader.vao_point);
  glBindVertexArray(gShader.vao_point);
  glBindBuffer(GL_ARRAY_BUFFER, gShader.vbo_point);
  //Vertices
  //{$IFDEF DGL}
  //glVertexAttribPointer(kATTRIB_VERT, 3, GL_FLOAT, FALSE, sizeof(TVtxNormClr), PChar(0));
  //{$ELSE}
  glVertexAttribPointer(kATTRIB_VERT, 3, GL_FLOAT, GL_FALSE, sizeof(TVtxNormClr), PChar(0));
  //{$ENDIF}
  glEnableVertexAttribArray(kATTRIB_VERT);
  //Normals typically stored as 3*32 bit floats (96 bytes), but we will pack them as 10-bit integers in a single 32-bit value with GL_INT_2_10_10_10_REV
  //  https://www.opengl.org/wiki/Vertex_Specification_Best_Practices
  //{$IFDEF DGL}
  //glVertexAttribPointer(kATTRIB_NORM, 3, GL_FLOAT, FALSE, sizeof(TVtxNormClr), PChar(sizeof(TPoint3f)));
  //glVertexAttribPointer(kATTRIB_NORM, 4, GL_INT_2_10_10_10_REV, FALSE, sizeof(TVtxNormClr), PChar(sizeof(TPoint3f)));
  //{$ELSE}
  //glVertexAttribPointer(kATTRIB_NORM, 3, GL_FLOAT, GL_FALSE, sizeof(TVtxNormClr), PChar(sizeof(TPoint3f)));
  glVertexAttribPointer(kATTRIB_NORM, 4, GL_INT_2_10_10_10_REV, GL_FALSE, sizeof(TVtxNormClr), PChar(sizeof(TPoint3f)));
  //{$ENDIF}
  glEnableVertexAttribArray(kATTRIB_NORM);
  //Color
  //{$IFDEF DGL}
  //glVertexAttribPointer(kATTRIB_CLR, 4, GL_UNSIGNED_BYTE, TRUE, sizeof(TVtxNormClr), PChar(2 * sizeof(TPoint3f)));
  //glVertexAttribPointer(kATTRIB_CLR, 4, GL_UNSIGNED_BYTE, TRUE, sizeof(TVtxNormClr), PChar(sizeof(int32)+ sizeof(TPoint3f)));
  //{$ELSE}
  //glVertexAttribPointer(kATTRIB_CLR, 4, GL_UNSIGNED_BYTE, GL_TRUE, sizeof(TVtxNormClr), PChar(2 * sizeof(TPoint3f)));
  glVertexAttribPointer(kATTRIB_CLR, 4, GL_UNSIGNED_BYTE, GL_TRUE, sizeof(TVtxNormClr), PChar(sizeof(int32)+ sizeof(TPoint3f)));
  //{$ENDIF}
  glEnableVertexAttribArray(kATTRIB_CLR);
  glBindBuffer(GL_ARRAY_BUFFER, 0);
  glBindVertexArray(0);
  gShader.uniform_ModelViewProjectionMatrix := glGetUniformLocation(gShader.shaderProgram, pAnsiChar('ModelViewProjectionMatrix'));
  gShader.uniform_ModelViewMatrix := glGetUniformLocation(gShader.shaderProgram, pAnsiChar('ModelViewMatrix'));
  gShader.uniform_NormalMatrix := glGetUniformLocation(gShader.shaderProgram, pAnsiChar('NormalMatrix'));
  if (gShader.vbo_face <> 0) then
     glDeleteBuffers(1, @gShader.vbo_face);
  glGenBuffers(1, @gShader.vbo_face);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, gShader.vbo_face);
  gShader.nface := Length(faces)* 3; //each face has 3 vertices
  glBufferData(GL_ELEMENT_ARRAY_BUFFER, Length(faces)*sizeof(TPoint3i), @faces[0], GL_STATIC_DRAW);
  GetError(2);
end;

procedure  InitGL;
begin
  {$IFDEF DGL} //use dglOpenGL
   InitOpenGL;
   ReadExtensions;
  {$ELSE}
    {$IFDEF GLEXT} //use gl/glext
     if not  Load_GL_version_3_2 then begin
       GLForm1.ShowmessageError('Error '+glGetString(GL_VENDOR)+'; OpenGL= '+glGetString(GL_VERSION)+'; Shader='+glGetString(GL_SHADING_LANGUAGE_VERSION));
       exit;
     end;
    {$ELSE} //use glcorearb
    //If your compiler does not find Load_GL_version_3_3_CORE you will need to update glext.pp
    if not  Load_GL_version_3_2_CORE then begin
       GLForm1.ShowmessageError('Error '+glGetString(GL_VENDOR)+'; OpenGL= '+glGetString(GL_VERSION)+'; Shader='+glGetString(GL_SHADING_LANGUAGE_VERSION));
       exit;
    end;
    {$ENDIF}
  {$ENDIF}
  GLForm1.caption := glGetString(GL_VENDOR)+'; OpenGL= '+glGetString(GL_VERSION)+'; Shader='+glGetString(GL_SHADING_LANGUAGE_VERSION);
  gShader.shaderProgram :=  initVertFrag(kVert,  kFrag);
  gShader.vao_point := 0;
  gShader.vbo_point := 0;
  gShader.vbo_face := 0;
  gShader.Distance:= 1.0;
  gShader.isPerspective := false;
  gShader.backColor := RGBToColor(255,255,255);
  gShader.objColor := RGBToColor(192,192,192);
end;

procedure SetOrtho (h,w: integer; Distance: single; isPerspective: boolean);
const
 kScaleX  = 0.6; //0.5 would fill whole screen,
var
   aspectRatio, scale: single;
begin
 if isPerspective then begin
    ngluPerspective(40.0, w/h, 0.01, kMaxDistance*2);
    exit;
 end;
 scale := kScaleX*Distance;
 AspectRatio := w / h;
 if (AspectRatio > 1) or (AspectRatio = 0) then //Wide window
     nglOrtho (-scale * AspectRatio, scale * AspectRatio, -scale, scale, 0.01, 5.0)
  else
    nglOrtho (-scale, scale, -scale/AspectRatio, scale/AspectRatio, 0.01, 5.0);
end;

procedure TGLForm1.GLboxPaint(Sender: TObject);
var
  m, mv : TnMat44;
  n: TnMat33;
  scale: single;
begin
  glUseProgram(gShader.shaderProgram);
  glClearColor( Red(gShader.backColor)/255, Green(gShader.backColor)/255, Blue(gShader.backColor)/255, 1.0); //Set blue background
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  glEnable(GL_DEPTH_TEST);
  nglMatrixMode(nGL_PROJECTION);
  nglLoadIdentity();
  {$IFDEF RETINA}
  SetOrtho(GLBox.BackingHeight, GLBox.BackingWidth, gShader.Distance, gShader.isPerspective);
  {$ELSE}
  SetOrtho(GLBox.Height, GLBox.Width, gShader.Distance, gShader.isPerspective);
  {$ENDIF}
  nglMatrixMode (nGL_MODELVIEW);
  //glDepthRange(0.001, 0.1);
  nglLoadIdentity ();
  {$IFDEF RETINA}
  nglTranslatef(gTranslateX/GLBox.BackingWidth , gTranslateY/GLBox.BackingHeight,0);
  {$ELSE}
  nglTranslatef(gTranslateX/GLBox.Width , gTranslateY/GLBox.Height,0);
  {$ENDIF}
  //object size normalized to be -1...+1 in largest dimension.
  //closest/furthest possible vertex is therefore -1.73..+1.73 (e.g. cube where corner is sqrt(1+1+1) from origin)
  scale := 1.0;
  nglScalef(0.5/Scale, 0.5/Scale, 0.5/Scale);
  if gShader.isPerspective then
      nglTranslatef(0,0, -Scale*4*gShader.Distance )
  else
     nglTranslatef(0,0,  -Scale*2 );
  nglRotatef(90-gElevation,-1,0,0);
  nglRotatef(gAzimuth,0,0,1);
  mv := ngl_ModelViewProjectionMatrix;
  m := ngl_ModelViewMatrix;
  n :=  ngl_NormalMatrix;
  {$IFDEF DGL}
  glUniformMatrix4fv(gShader.uniform_ModelViewProjectionMatrix, 1, FALSE, @mv[0,0]); // note model not MVP!
  glUniformMatrix4fv(gShader.uniform_ModelViewMatrix, 1, FALSE, @m[0,0]);
  glUniformMatrix3fv(gShader.uniform_NormalMatrix, 1, FALSE, @n[0,0]);
  {$ELSE}
  glUniformMatrix4fv(gShader.uniform_ModelViewProjectionMatrix, 1, GL_FALSE, @mv[0,0]); // note model not MVP!
  glUniformMatrix4fv(gShader.uniform_ModelViewMatrix, 1, GL_FALSE, @m[0,0]);
  glUniformMatrix3fv(gShader.uniform_NormalMatrix, 1, GL_FALSE, @n[0,0]);
  {$ENDIF}
  glBindVertexArray(gShader.vao_point);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER,gShader.vbo_face);
  glDrawElements(GL_TRIANGLES, gShader.nface, GL_UNSIGNED_INT, nil);
  glBindVertexArray(0);
  GLbox.SwapBuffers;
end;

procedure TGLForm1.GLboxMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
 if gMouseY < 0 then exit; //mouse is not down
 if not (ssShift in Shift) then begin
    gElevation := gElevation + (Y - gMouseY);
    gAzimuth := gAzimuth + (X - gMouseX);
 end else begin
    gTranslateX := gTranslateX + (X - gMouseX);
    gTranslateY := gTranslateY - (Y - gMouseY);
 end;
 gMouseX := X;
 gMouseY := Y;
 GLBox.Invalidate;
end;

procedure TGLForm1.GLboxMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
     gMouseY := -1; //release
end;

procedure TGLForm1.GLboxMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
     gMouseY := Y;
     gMouseX := X;
end;

procedure TGLForm1.GLboxMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
 if abs(WheelDelta) < 5 then exit;
 if abs(WheelDelta) < 5 then exit;
  if WheelDelta < 0 then
     gShader.Distance := gShader.Distance * 0.9
  else
    gShader.Distance := gShader.Distance * 1.1;
  if gShader.Distance > kMaxDistance then gShader.Distance := kMaxDistance;
  if gShader.Distance < 0.5 then gShader.Distance := 0.5;
   GLBox.Invalidate;
end;

procedure TGLForm1.OpenMesh(Filename: string);
begin
  LoadMesh(Filename, gShader.faces, gShader.vertices, gShader.vertexRGBA);
  GLBox.MakeCurrent(false);
  LoadBufferData(gShader.faces, gShader.vertices, gShader.vertexRGBA);
  GLBox.invalidate;
end; //OpenMesh

procedure TGLForm1.DecimateMenuClick(Sender: TObject);
var
  nTri, nTriDesired: integer;
  msStart: Dword;
  s: string;
  r: single;
begin
 msStart := gettickcount();
 nTri := length(gShader.Faces);
 s := '.3';
 if not inputquery('Track simplify', 'Enter reduction factor (e.g. 0.2 will decimate 80% of all triangles)', s) then exit;
 r := StrToFloatDef(s, 0.5);
 if (r <= 0.0) or (r > 1.0) then begin
    showmessage('Error: reduction factor should be BETWEEN 0 and 1');
    exit;
 end;
 nTriDesired := round(nTri * r);
 simplify_mesh(gShader.faces, gShader.vertices, nTriDesired, 2, true);
 caption := format('Faces %d -> %d (%.3f, %d ms)', [ nTri, length(gShader.Faces), length(gShader.Faces)/nTri , gettickcount() - msStart]) ;
 LoadBufferData(gShader.faces, gShader.vertices, gShader.vertexRGBA);
 GLBox.invalidate;
end;

procedure TGLForm1.OpenMenuClick(Sender: TObject);
begin
     if not OpenDialog1.execute then exit;
     OpenMesh(OpenDialog1.FileName);
end;

procedure TGLForm1.PerspectiveMenuClick(Sender: TObject);
begin
  gShader.isPerspective := PerspectiveMenu.Checked;
  GLBox.invalidate;
end;

procedure TGLForm1.RotateMenuClick(Sender: TObject);
var
   swap: TPoint3f;
   i : integer;
begin
  if (length(gShader.vertices) < 1) or  (length(gShader.faces) < 1) then exit;
  for i := 0 to (length(gShader.vertices) - 1) do begin
     swap.X := gShader.vertices[i].X;
     swap.Y := -gShader.vertices[i].Z;
     swap.Z := gShader.vertices[i].Y;
     gShader.vertices[i] := swap;
  end;
  GLBox.MakeCurrent(false);
  LoadBufferData(gShader.faces, gShader.vertices, gShader.vertexRGBA);
  GLBox.invalidate;
end;

procedure TGLForm1.SaveMenuClick(Sender: TObject);
begin
  if not SaveDialog1.Execute then exit;
  SaveObj(SaveDialog1.FileName, gShader.faces, gShader.vertices);
end;

procedure TGLForm1.DropFiles(Sender: TObject; const FileNames: array of String);
begin
  OpenMesh(Filenames[0]);
end;

procedure TGLForm1.AboutMenuClick(Sender: TObject);
const
  kSamp = 36;
var
  s: dword;
  i: integer;
begin
  s := gettickcount;
  for i := 1 to kSamp do begin
      gAzimuth := (gAzimuth + 10) mod 360;
      GLbox.Repaint;
   end;
  Showmessage('OpenGL 4.1 PLY viewer 4/2017 FPS: '+inttostr(round( (kSamp*1000)/(gettickcount-s))));
end;


procedure TGLForm1.ObjectColorMenuClick(Sender: TObject);
begin
  case (sender as TMenuItem).tag of
       1: gShader.ObjColor:= RGBToColor(210,148,148); //pink
       2: gShader.ObjColor:= RGBToColor(128,162,128); //green
       3: gShader.ObjColor:= RGBToColor(167,171,253); //blue
       4: begin //custom
          ColorDialog1.color := gShader.ObjColor;
          if not ColorDialog1.Execute then exit;
          gShader.ObjColor := ColorDialog1.Color;
       end
       else gShader.ObjColor:= RGBToColor(192,192,192); //gray
  end;
  GLBox.MakeCurrent(false);
  LoadBufferData(gShader.faces, gShader.vertices, gShader.vertexRGBA);
  GLBox.Invalidate;
end;

procedure TGLForm1.BackColorMenuClick(Sender: TObject);
begin
     If (ssShift in KeyDataToShiftState(vk_Shift)) then begin
        if green(gShader.backColor) > 128 then
           gShader.backColor := RGBToColor(0,0,0)
        else
            gShader.backColor := RGBToColor(255,255,255);
         GLBox.Invalidate;
         exit;
      end;
      ColorDialog1.color := gShader.BackColor;
      if not ColorDialog1.Execute then exit;
      gShader.BackColor := ColorDialog1.Color;
      GLBox.Invalidate;
end;

procedure TGLForm1.FormCreate(Sender: TObject);
begin
  GLbox:= TOpenGLControl.Create(GLForm1);
  Application.OnDropFiles:= @DropFiles;
  Application.ShowButtonGlyphs:= sbgNever;
  //OSX has two modes:
  //    NSOpenGLProfileLegacy provides support for OpenGL 2.1/GLSL1.2 and earlier
  //    NSOpenGLProfileVersion3_2Core provides support for AT LEAST OpenGL 3.2/GLSL3.2 CORE
  //    NSOpenGLProfileVersion4_1Core provides support for AT LEAST OpenGL 4.1/GLSL4.1 CORE
  //NOTE: CORE support removes deprecated LEGACY features
  //  In other words, Core OpenGL3.2 is NOT a superset of OpenGL2.1
  //  Functions like gl_FragColor, glTranslate etc. do not exist in CORE OpenGL
  //  Therefore, CORE is similar to embedded OpenGL, and old tutorials will not work
  {$IFDEF LCLCarbon}
   Error: Carbon only supports Legacy OpenGL. Solution: compile to the Cocoa widgetset (Project/ProjectOptions/Additions&Overrides)
  {$ENDIF}
  {$IFDEF Darwin}
  OpenMenu.ShortCut :=  ShortCut(Word('O'), [ssMeta]);
  SaveMenu.ShortCut :=  ShortCut(Word('S'), [ssMeta]);
  {$ELSE}
  AppleMenu.Caption := 'About';
  {$ENDIF}
  GLbox.OpenGLMajorVersion:= 3;
  GLbox.OpenGLMinorVersion:= 3;
  GLbox.AlphaBits:= 8;
  GLbox.AutoResizeViewport:= true;
  GLBox.Parent := self;
  GLBox.MultiSampling:= 4;
  GLBox.Align := alClient;
  GLBox.OnPaint := @GLboxPaint; //for "mode delphi" this would be "GLBox.OnPaint := GLboxPaint"
  GLBox.OnMouseDown := @GLboxMouseDown;
  GLBox.OnMouseMove := @GLboxMouseMove;
  GLBox.OnMouseUp := @GLboxMouseUp;
  GLBox.OnMouseWheel := @GLboxMouseWheel;
  GLBox.MakeCurrent(false);
  InitGL;
  {$IFDEF RETINA}
  GLBox.WantsBestResolutionOpenGLSurface:=true;
  {$ENDIF}
  OpenMesh('');
  PerspectiveMenu.Checked := gShader.isPerspective;
end;

procedure TGLForm1.ErrorTimerTimer(Sender: TObject);
begin
  ErrorTimer.Enabled := false;
  if gGLError = '' then exit;
  Showmessage(gGLError);
  gGLError := '';
end;

end.
