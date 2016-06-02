unit mesh;

{$mode objfpc}{$H+}
//read PLY format files
interface

uses
  Classes, SysUtils, dialogs;

type
  TRGBA = packed record
  r: Byte;
  g: Byte;
  b: Byte;
  a: Byte;
end;
TPoint3f = packed record
  X: single;
  Y: single;
  Z: single
end;
TPoint3i = packed record
   X: longint; //ensure 32-bit for simple GIfTI writing
   Y: longint;
   Z: longint;
 end;
TFaces = array of TPoint3i;
TVertices = array of TPoint3f;
TVertexRGBA = array of TRGBA;

function ptf(x,y,z: single): TPoint3f;
procedure LoadMesh(Filename: string; var faces: TFaces; var vertices: TVertices; var vertexRGBA: TVertexRGBA);
procedure SaveObj(const FileName: string; var faces: TFaces; var vertices: TVertices);

implementation

procedure SaveObj(const FileName: string; var faces: TFaces; var vertices: TVertices);
//create WaveFront object file
// https://en.wikipedia.org/wiki/Wavefront_.obj_file
var
   f : TextFile;
   FileNameObj: string;
   i : integer;
begin
  if (length(faces) < 1) or (length(vertices) < 3) then begin
     showmessage('You need to open a mesh before you can save it');
     exit;
  end;
  FileNameObj := changeFileExt(FileName, '.obj');
  AssignFile(f, FileNameObj);
  ReWrite(f);
  WriteLn(f, '# WaveFront Object format image created with Surf Ice');
  for i := 0 to (length(vertices)-1) do
      WriteLn(f, 'v ' + floattostr(vertices[i].X)+' '+floattostr(vertices[i].Y)+' '+ floattostr(vertices[i].Z));
  for i := 0 to (length(faces)-1) do
      WriteLn(f, 'f ' + inttostr(faces[i].X+1)+' '+inttostr(faces[i].Y+1)+' '+ inttostr(faces[i].Z+1)); //+1 since "A valid vertex index starts from 1 "
  //fprintf(fid, '# WaveFront Object format image created with MRIcroS\n');
  //fprintf(fid, 'v %.12g %.12g %.12g\n', vertex');
  //fprintf(fid, 'f %d %d %d\n', (face)');
  CloseFile(f);
end; // SaveObj()

function ptf(x,y,z: single): TPoint3f; //create float vector
begin
  result.X := x;
  result.Y := y;
  result.Z := z;
end; // ptf()

function pti(x,y,z: integer): TPoint3i; //create integer vector
begin
  result.X := x;
  result.Y := y;
  result.Z := z;
end; // pti()

procedure SwapLongWord(var s : LongWord);
type
  swaptype = packed record
    case byte of
      0:(Word1,Word2 : word); //word is 16 bit
      1:(Long:LongWord);
  end;
  swaptypep = ^swaptype;
var
  inguy:swaptypep;
  outguy:swaptype;
begin
  inguy := @s; //assign address of s to inguy
  outguy.Word1 := swap(inguy^.Word2);
  outguy.Word2 := swap(inguy^.Word1);
  s := outguy.Long;
end; // SwapLongWord()

function asSingle(i : longint): single;  overload;
type
  swaptype = packed record
    case byte of
      0:(Lng : longint);
      1:(Sngl : single);
  end;
  swaptypep = ^swaptype;
var
  inguy:swaptypep;
begin
  inguy := @i; //assign address of s to inguy
  result := inguy^.Sngl;
end; // asSingle()

function asSingle(b0,b1,b2,b3: byte): single; overload;
type
  swaptype = packed record
    case byte of
      0:(b0,b1,b2,b3 : byte);
      1:(Sngl : single);
  end;
var
  outguy:swaptype;
begin //should work with both little and big endian, as order is same
  outguy.b0 := b0;
  outguy.b1 := b1;
  outguy.b2 := b2;
  outguy.b3 := b3;
  result := outguy.Sngl;
end; // asSingle()

procedure SwapSingle(var s : single);
type
  swaptype = packed record
    case byte of
      0:(Word1,Word2 : word);
      1:(Sngl : single);
  end;
  swaptypep = ^swaptype;
var
  inguy:swaptypep;
  outguy:swaptype;
begin
  inguy := @s; //assign address of s to inguy
  outguy.Word1 := swap(inguy^.Word2);
  outguy.Word2 := swap(inguy^.Word1);
  s := outguy.Sngl;
end; // SwapSingle()

function FSize (lFName: String): longint;
var F : File Of byte;
begin
  result := 0;
  if not fileexists(lFName) then exit;
  Assign (F, lFName);
  Reset (F);
  result := FileSize(F);
  Close (F);
end;

procedure LoadObj(const FileName: string; var faces: TFaces; var vertices: TVertices);//; var vertexRGBA: TVertexRGBA);
//WaveFront Obj file used by Blender
// https://en.wikipedia.org/wiki/Wavefront_.obj_file
const
  kBlockSize = 8192;
var
   f: TextFile;
   fsz : int64;
   s : string;
   strlst : TStringList;
   i,j, num_v, num_f, new_f: integer;
begin
     fsz := FSize (FileName);
     if fsz < 32 then exit;
     //init values
     num_v := 0;
     num_f := 0;
     strlst:=TStringList.Create;
     setlength(vertices, (fsz div 70)+kBlockSize); //guess number of faces based on filesize to reduce reallocation frequencey
     setlength(faces, (fsz div 35)+kBlockSize); //guess number of vertices based on filesize to reduce reallocation frequencey
     //load faces and vertices
     AssignFile(f, FileName);
     Reset(f);
     DefaultFormatSettings.DecimalSeparator := '.';
     while not EOF(f) do begin
        readln(f,s);
        if length(s) < 7 then continue;
        if (s[1] <> 'v') and (s[1] <> 'f') then continue; //only read 'f'ace and 'v'ertex lines
        if (s[2] = 'p') or (s[2] = 'n') or (s[2] = 't') then continue; //ignore vp/vn/vt data: avoid delimiting text yields 20% faster loads
        strlst.DelimitedText := s;
        if (strlst.count > 3) and ( (strlst[0]) = 'f') then begin
           //warning: need to handle "f v1/vt1/vn1 v2/vt2/vn2 v3/vt3/vn3"
           //warning: face could be triangle, quad, or more vertices!
           new_f := strlst.count - 3;
           if ((num_f+new_f) >= length(faces)) then
              setlength(faces, length(faces)+new_f+kBlockSize);
           for i := 1 to (strlst.count-1) do
               if (pos('/', strlst[i]) > 1) then // "f v1/vt1/vn1 v2/vt2/vn2 v3/vt3/vn3" -> f v1 v2 v3
                  strlst[i] := Copy(strlst[i], 1, pos('/', strlst[i])-1);
           for j := 1 to (new_f) do begin
               faces[num_f].X := strtointDef(strlst[1], 0) - 1;
               faces[num_f].Y := strtointDef(strlst[j+1], 0) - 1;  //-1 since "A valid vertex index starts from 1"
               faces[num_f].Z := strtointDef(strlst[j+2], 0) - 1;  //-1 since "A valid vertex index starts from 1"
               inc(num_f);
           end;
        end;
        if (strlst.count > 3) and ( (strlst[0]) = 'v') then begin
           if ((num_v+1) >= length(vertices)) then
              setlength(vertices, length(vertices)+kBlockSize);
           vertices[num_v].X := strtofloatDef(strlst[1], 0);
           vertices[num_v].Y := strtofloatDef(strlst[2], 0);
           vertices[num_v].Z := strtofloatDef(strlst[3], 0);
           inc(num_v);
        end;
     end;
     CloseFile(f);
     strlst.free;
     setlength(faces, num_f);
     setlength(vertices, num_v);
end; // LoadObj()

procedure LoadPly(const FileName: string; var faces: TFaces; var vertices: TVertices; var vertexRGBA: TVertexRGBA);
// https://en.wikipedia.org/wiki/PLY_(file_format)
// http://paulbourke.net/dataformats/ply/
var
   fb: file;
   f: TextFile;
   isSwap, isVertexSection, isAscii, isLittleEndian, isUint32 : boolean;
   redOffset, greenOffset, blueOffset, AlphaOffset,
   hdrSz, sz, i, j,  num_v, num_f, num_vx, num_header_lines, vertexOffset, indexSectionExtraBytes: integer;
   str: string;
   byt: byte;
   flt: single;
   strlst : TStringList;
   i32: array [1..3] of longword;
   i16: array [1..3] of word;
   binByt: array of byte;
begin
  AssignFile(f, FileName);
  Reset(f);
  ReadLn(f, str);
  if pos('PLY', UpperCase(str)) <> 1 then begin
    showmessage('Not a PLY file');
    closefile(f);
    exit;
  end;
  strlst:=TStringList.Create;
  num_header_lines := 1;
  num_f := 0;
  num_v := 0;
  isAscii := false;
  isLittleEndian := false;
  isUint32 := true; //assume uint32 not short int16
  isVertexSection := false;
  indexSectionExtraBytes := 0;
  vertexOffset := 0;
  redOffset := 0; greenOffset := 0; blueOffset := 0; AlphaOffset := 0;
  while not EOF(f) do begin
     ReadLn(f, str);
     num_header_lines := num_header_lines + 1;
     if pos('END_HEADER', UpperCase(str)) = 1 then Break;
     if pos('FORMAT', UpperCase(str)) = 1 then begin
        strlst.DelimitedText := str;
        if pos('ASCII', UpperCase(strlst[1])) = 1 then
            isAscii := true;
        if pos('BINARY_LITTLE_ENDIAN', UpperCase(strlst[1])) = 1 then
            isLittleEndian := true;
     end;
     if pos('ELEMENT VERTEX', UpperCase(str)) = 1 then begin
        strlst.DelimitedText := str;
        num_v := StrToIntDef(strlst[2], 0); // "element vertex 62"
        isVertexSection := true;
     end;
     if pos('ELEMENT FACE', UpperCase(str)) = 1 then begin
        strlst.DelimitedText := str;
        num_f := StrToIntDef(strlst[2], 0); // "element face 120"
        isVertexSection := false;
     end;
     //detect "short" or "uint" from "property list uchar uint vertex_indices"

     if (isVertexSection) and (pos('PROPERTY', UpperCase(str)) = 1) then begin
        strlst.DelimitedText := str;
        if (strlst.count > 2) and (pos('RED', UpperCase(strlst[2])) = 1) then begin
           redOffset := vertexOffset;
           if (pos('UCHAR', UpperCase(strlst[1])) <> 1) then begin
             showmessage('Expected colors of data type "UCHAR", not "'+str+'"');
             closefile(f);
             exit;
           end;
        end;
        if (strlst.count > 2) and (pos('GREEN', UpperCase(strlst[2])) = 1) then
           greenOffset := vertexOffset;
        if (strlst.count > 2) and (pos('BLUE', UpperCase(strlst[2])) = 1) then
           blueOffset := vertexOffset;
        if (strlst.count > 2) and (pos('ALPHA', UpperCase(strlst[2])) = 1) then
           alphaOffset := vertexOffset;
        //showmessage(str+ inttostr(strlst.count));
        if isAscii then
          vertexOffset := vertexOffset + 1  //for ASCII we count items not bytes
        else if (pos('CHAR', UpperCase(strlst[1])) = 1) or (pos('UCHAR', UpperCase(strlst[1])) = 1) then
           vertexOffset := vertexOffset + 1
        else if (pos('SHORT', UpperCase(strlst[1])) = 1) or (pos('USHORT', UpperCase(strlst[1])) = 1) then
           vertexOffset := vertexOffset + 2
        else if (pos('INT', UpperCase(strlst[1])) = 1) or (pos('UINT', UpperCase(strlst[1])) = 1) or (pos('FLOAT', UpperCase(strlst[1])) = 1) then
           vertexOffset := vertexOffset + 4
        else if (pos('DOUBLE', UpperCase(strlst[1])) = 1) then
           vertexOffset := vertexOffset + 8
        else begin
            showmessage('Unexpected data type : "'+UpperCase(strlst[1])+'"');
            closefile(f);
            exit;
        end;
     end; //Vertex section properties
     if (not isVertexSection) and (pos('PROPERTY', UpperCase(str)) = 1) then begin
        //n.b. Wiki and MeshLab use  'VERTEX_INDICES' but Bourke uses "VERTEX_INDEX"
        strlst.DelimitedText := str;
        if (strlst.count > 4) and (pos('VERTEX_INDEX', UpperCase(strlst[4])) = 1) and (pos('SHORT', UpperCase(strlst[3])) = 1) then
           isUint32 := false
        else if (strlst.count > 4) and (pos('VERTEX_INDICES', UpperCase(strlst[4])) = 1) and (pos('SHORT', UpperCase(strlst[3])) = 1) then
           isUint32 := false
        else if (strlst.count > 2)  then begin
           if (pos('CHAR', UpperCase(strlst[1])) = 1) or  (pos('UCHAR', UpperCase(strlst[1])) = 1) then
              indexSectionExtraBytes := indexSectionExtraBytes + 1;
           if (pos('SHORT', UpperCase(strlst[1])) = 1) or  (pos('USHORT', UpperCase(strlst[1])) = 1) then
              indexSectionExtraBytes := indexSectionExtraBytes + 2;
           if (pos('INT', UpperCase(strlst[1])) = 1) or  (pos('UINT', UpperCase(strlst[1])) = 1)  or  (pos('FLOAT', UpperCase(strlst[1])) = 1) then
              indexSectionExtraBytes := indexSectionExtraBytes + 4;
           if (pos('DOUBLE', UpperCase(strlst[1])) = 1) then
              indexSectionExtraBytes := indexSectionExtraBytes + 8;
        end;
     end; //face section properties

  end;
  if EOF(f) or (num_v < 3) or (num_f < 1) then begin
    showmessage('Not a mesh-based PLY file (perhaps point based, try opening in MeshLab)');
    closefile(f);
    exit;
  end;

  setlength(vertices, num_v);
  setlength(faces, num_f);
  if redOffset > 2 then
     setlength(vertexRGBA,num_v);
  if isAscii then begin
    if redOffset > 2 then begin
       sz := redOffset;
       if (greenOffset > sz) then sz := greenOffset;
       if (blueOffset > sz) then sz := blueOffset;
       if (alphaOffset > sz) then sz := alphaOffset;
       for i := 0 to (num_v - 1) do begin
           read(f, vertices[i].X, vertices[i].Y, vertices[i].Z);  //XYZ are items 0,1,2
           for j := 3 to (sz) do begin
               read(f, flt);
               if j = redOffset then vertexRGBA[i].R := round(flt);
               if j = greenOffset then vertexRGBA[i].G := round(flt);
               if j = blueOffset then vertexRGBA[i].B := round(flt);
               if j = alphaOffset then vertexRGBA[i].A := round(flt);
           end;
           readln(f);
       end;
    end else
        for i := 0 to (num_v - 1) do
            readln(f, vertices[i].X, vertices[i].Y, vertices[i].Z);
    for i := 0 to (num_f - 1) do begin
      readln(f, num_vx, faces[i].X, faces[i].Y, faces[i].Z);
      if num_vx < 3 then begin
            showmessage('File does not have the expected number of triangle-based faces '+ FileName);
            closefile(f);
            exit;
      end;
      if num_vx > 3 then begin
          showmessage('Only able to read triangle-based PLY files. (Hint: open with MeshLab and export as CTM format) ');
          closefile(f);
          exit;
      end;
    end;
    closefile(f);
  end else begin //if ASCII else Binary
    closefile(f);
    isSwap := false;
    {$IFDEF ENDIAN_LITTLE}
    if not isLittleEndian then begin
    {$ELSE}
    if isLittleEndian then begin
    {$ENDIF}
      //showmessage('unsupported binary PLY feature: swapped bytes');
      //exit;
      isSwap := true;
    end;
    if vertexOffset < 12 then begin
       showmessage('Binary PLY files should have at least 12 bytes per vertex');
       exit;
    end;
    AssignFile(fb, FileName);
    FileMode := fmOpenRead;
    Reset(fb,1);
    num_vx := 0;
    sz := filesize(fb);
    i := 0;
    while (num_vx < num_header_lines) and (i < sz) do begin
          blockread(fb, byt, 1 );
          if byt = $0A then
             num_vx := num_vx + 1;
          i := i + 1;
    end;
    hdrSz := i;
    if (num_vx < num_header_lines) then begin
       closefile(fb);
       exit;
    end;
    if vertexOffset > 12 then begin
       setlength(binByt, vertexOffset);
       for i := 0 to (num_v -1) do begin
           blockread(fb, binByt[0], vertexOffset );//sizeof(clrV) );
           vertices[i].X := asSingle(binByt[0],binByt[1],binByt[2],binByt[3]);
           vertices[i].Y := asSingle(binByt[4],binByt[5],binByt[6],binByt[7]);
           vertices[i].Z := asSingle(binByt[8],binByt[9],binByt[10],binByt[11]);
           if redOffset > 0 then begin
              vertexRGBA[i].R := binByt[redOffset];
              vertexRGBA[i].G := binByt[greenOffset];
              vertexRGBA[i].B := binByt[blueOffset];
              if alphaOffset > 0 then
                 vertexRGBA[i].A := binByt[alphaOffset];
           end;
       end;
       i := 0;
    end else
        blockread(fb, vertices[0], 3 * 4 * num_v);
    if isSwap then begin
          for i := 0 to (num_v -1) do begin
              SwapSingle(vertices[i].X);
              SwapSingle(vertices[i].Y);
              SwapSingle(vertices[i].Z);
          end;
    end; //swapped
    for i := 0 to (num_f -1) do begin
        setlength(binByt, indexSectionExtraBytes);
        blockread(fb, byt, 1 );
        if byt <> 3 then begin
                showmessage('Only able to read triangle-based PLY files. Solution: open and export with MeshLab. Index: '+inttostr(i)+ ' Header Bytes '+inttostr(hdrSz)+' bytesPerVertex: '+inttostr(vertexOffset)+' faces: ' + inttostr(byt));
                closefile(fb);
                setlength(faces,0);
                setlength(vertices,0);
                exit;
        end;
        if isSwap then begin
           if isUint32 then begin
              blockread(fb, i32[1], 3 * 4 );
              SwapLongWord(i32[1]);
              SwapLongWord(i32[2]);
              SwapLongWord(i32[3]);
              faces[i] := pti(i32[1], i32[2], i32[3]);  //winding order matches MeshLab
           end else begin
               blockread(fb, i16[1], 3 * 2 );
               faces[i] := pti(swap(i16[1]), swap(i16[2]), swap(i16[3])); //winding order matches MeshLab
           end;
        end else begin
          if isUint32 then begin
             blockread(fb, i32[1], 3 * 4 );
             faces[i] := pti(i32[1], i32[2], i32[3]);  //winding order matches MeshLab
          end else begin
              blockread(fb, i16[1], 3 * 2 );
              faces[i] := pti(i16[1], i16[2], i16[3]); //winding order matches MeshLab
          end;

        end; //is Swapped else unSwapped
        if (indexSectionExtraBytes > 0) then
           blockread(fb, binByt[0], indexSectionExtraBytes);
    end;
    closefile(fb);
   end; //if ascii else binary
   strlst.Free;
end; // LoadPly()

procedure minMax(var v, mn, mx: TPoint3f);
begin
     if v.X > mx.X then
        mx.X := v.X
     else if v.X < mn.X then
        mn.X := v.X;
     if v.Y > mx.Y then
        mx.Y := v.Y
     else if v.Y < mn.Y then
        mn.Y := v.Y;
     if v.Z > mx.Z then
        mx.Z := v.Z
     else if v.Z < mn.Z then
        mn.Z := v.Z;
end; // minMax()

function SetDescriptives (var vertices: TVertices; out origin: TPoint3f): single;
//determine range of vertices in each dimension
var
   mn, mx: TPoint3f;
   Scale: single;
   i: integer;
begin
     if length(vertices) < 1 then exit;
     mx := vertices[0];
     mn := mx;
     for i := 0 to (length(vertices) - 1) do
         minMax(vertices[i], mn, mx);

     origin.X := (0.5 * (mx.X - mn.X)) + mn.X;
     origin.Y := (0.5 * (mx.Y - mn.Y)) + mn.Y;
     origin.Z := (0.5 * (mx.Z - mn.Z)) + mn.Z;
     Scale := abs(mx.X - origin.X);
     if abs(mx.Y - origin.Y) > Scale then
        Scale := abs(mx.Y - origin.Y);
     if abs(mx.Z - origin.Z) > Scale then
        Scale := abs(mx.Z - origin.Z);
     result := Scale;
end; // SetDescriptives()

procedure NormalizeMeshSize(var vertices: TVertices);
//make size mesh -1..1 in largest dimension
var
   i: integer;
   scale : single;
   origin: TPoint3f;
begin
  if (length(vertices) < 3) then exit;
  scale := SetDescriptives (vertices, origin);
  if scale = 0 then
     exit;
  for i := 0 to (length(vertices)-1) do begin
     vertices[i].X := (vertices[i].X - origin.X)/ scale;
     vertices[i].Y := (vertices[i].Y - origin.Y)/ scale;
     vertices[i].Z := (vertices[i].Z - origin.Z)/ scale;
  end;
end; //NormalizeMeshSize()

procedure MakePyramid(var faces: TFaces; var vertices: TVertices);
begin
     setlength(vertices, 5);
     vertices[0] := ptf(0,0,1);
     vertices[1] := ptf(1,1,-1);
     vertices[2] := ptf(1,-1,-1);
     vertices[3] := ptf(-1,-1,-1);
     vertices[4] := ptf(-1,1,-1);
     setlength(faces, 4);
     faces[0] := pti(2,1,0);
     faces[1] := pti(3,2,0);
     faces[2] := pti(4,3,0);
     faces[3] := pti(1,4,0);
end; // MakePyramid()

procedure LoadMesh(Filename: string; var faces: TFaces; var vertices: TVertices; var vertexRGBA: TVertexRGBA);
begin
  setlength(faces, 0);
  setlength(vertices,0);
  setlength(vertexRGBA, 0);
  if (length(Filename) > 0) and (FileExists(Filename)) and (UpperCase(ExtractFileExt(Filename)) = '.OBJ') then
         LoadObj(Filename, faces, vertices)
  else if (length(Filename) > 0) and (FileExists(Filename)) then
     LoadPly(Filename, faces, vertices, vertexRGBA);
  if (length(faces) < 1) or (length(vertices) < 3) then
      MakePyramid(faces, vertices);
  NormalizeMeshSize(vertices);
end; //LoadMesh()


end.

