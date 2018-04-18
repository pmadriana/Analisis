unit MMatriz;

{$mode objfpc}{$H+}
interface

uses
  Classes,Dialogs, SysUtils;
type
  matriz= array[0..100, 0..100] of Real;
  TMatriz=Class
  private
  public
    function Suma(A,B : matriz; n,m:integer):matriz;
    function Resta(A,B : matriz; n,m:integer):matriz;
    function MultEsc(A: matriz;esc:Real;n,m:integer):matriz;
    function Mult(A,B: matriz;n,m,c2:integer):matriz;
    function Transpuesta(A: matriz;n,m:integer):matriz;
    function Traza(A: matriz;n,m:integer):real;
    function Determinante(A: matriz;filas,col:integer):real;
    function cof(matrizz:matriz;r,c:integer):matriz;
    function Inversa(A:matriz; det: real; dim:integer): matriz;
    procedure Print(a: matriz; n,m:integer);
    //potencia, division(a/b = a*b(inve)), triangularizacion(gauss jordan)
    //INVERSA APARTE
    constructor create();
    destructor destroy();
  end;
var
  mat:matriz;
implementation
 constructor TMatriz.create();
begin
end;

destructor TMatriz.destroy;
begin
end;


 function TMatriz.Suma(A,B : matriz; n,m:integer):matriz;
  var
  i,j : integer;
  C:matriz;
  begin
  for i:=0 to m-1 do
     begin
      for j:=0 to n-1  do
       begin
           C[i,j]:=A[i,j]+B[i,j];//col
       end;
     end;

  Suma:=C;
  end;

  function TMatriz.Resta(A,B : matriz; n,m:integer):matriz;
  var
  i,j : integer;
  C:matriz;
  begin
  for i:=0 to m-1 do
     begin
      for j:=0 to n-1  do
       begin
           C[i,j]:=A[i,j]-B[i,j];//col
       end;
     end;
  Resta:=C;
  end;

  function TMatriz.MultEsc(A: matriz;esc:real;n,m:integer):matriz;
  var
  i,j : integer;
  C:matriz;
  begin
  for i:=0 to m-1 do
     begin
      for j:=0 to n-1  do
       begin
           C[i,j]:=A[i,j]*esc;//col
       end;
     end;
  MultEsc:=C;
  end;

  function TMatriz.Mult(A,B: matriz;n,m,c2:integer):matriz;
  var
  //Determinanate por triangullarizacion
  //Inversa Gauss
  //Simpsons
  //Lagrange simple
  i,j,k : integer;
  temp:Real;
  C:matriz;
  begin
  for i:=0 to n-1 do
     begin
      for j:=0 to c2-1 do
         begin
         C[j,i]:=0;
          for k:=0 to m-1  do
           begin
            //ShowMessage(FloatToStr(C[j,i])+' + '+FloatToStr(B[k,i])+' * '+FloatToStr(A[j,k]) );
            C[j,i]:=C[j,i]+(A[k,i]*B[j,k]);//col
            //ShowMessage(IntToStr(k)+' '+IntToStr(i)+' = '+ FloatToStr(temp));
           end;
          end;
      end;

  Mult:=C;
  end;
  function TMatriz.Transpuesta(A: matriz;n,m:integer):matriz;
  var
  i,j,i1,j1: integer;
  C:matriz;
  begin
  //ShowMessage(IntToStr(n));//2 i
  //ShowMessage(IntToStr(m));//3 j
  j1:=0;
  for i:=0 to n-1 do
      begin
      i1:=0;
      for j:=0 to m-1 do
         begin

          C[j1,i1]:=A[j,i];
         // ShowMessage('this is i1: '+IntToStr(i1)+' this j1 : '+ IntToStr(j1)+'  ' +FloatToStr(C[i1,j1]));
          i1:=i1+1;
          end;
          j1:=j1+1;
      end;
  Transpuesta:=C;
  end;

  function TMatriz.Traza(A: matriz;n,m:integer):Real;
  var
  i,j : integer;
  C:real;
  begin
  for i:=0 to m-1 do
     begin
       C:=A[i,i]+C;//col
     end;
  Traza:=C;
  end;
  function TMatriz.Determinante(A: matriz;filas,col:integer):Real;
  var
  i,j,n:Integer;
  factor,det:Real;
  B:matriz;

  begin

     if filas=2 then  { determinante de dos por dos, caso base }
        det:= A[0,0] * A[1,1] - A[0,1] * A[1,0]
     else
     begin
          det:= 0;
          for n:= 0 to filas-1 do
          begin
               for i:= 1 to filas-1 do
               begin
                    for j:= 0 to n-1 do
                        B[i-1,j]:= A[i,j];
                    for j:= n+1 to filas-1 do
                        B[i-1,j-1]:= A[i,j];
               end;
               if (n+2) mod 2=0 then i:=1 //Signo
                  else i:= -1;
               det:= det + i * A[0,n] * Determinante(B,filas-1,col);
          end;

     end;

     Determinante:= det;
  end;

 function Power( b: Real; n: Integer ): Real;
var i: Integer;
begin
   Result:= 1;
   for i:= 1 to n do
      Result:= Result * b;

end;

function TMatriz.cof(matrizz:matriz;r,c:integer): matriz ;
    var
    dim:integer;
  i,j,i1,j1,a,b, a_factor, b_factor: integer;
  mat_temp, ret, le_mat: matriz;
begin
   for i:=0 to c-1 do
   begin
        for j:=0 to r-1 do
             mat_temp[i,j]:= matrizz[i,j];
   end;
   for i:=0 to c-1 do
   begin
        for j:=0 to r-1 do
        begin
             a_factor:=0;
             for a:=0 to c-1 do
             begin
                  b_factor:=0;
                  if(a=i) then
                     begin
                          a_factor:=1;
                          continue;
                     end;
                  for b:=0 to r-1 do
                  begin
                       if(b=j) then
                          begin
                               b_factor :=1;
                               continue;
                          end;
                       le_mat[a-a_factor, b-b_factor] := mat_temp[a,b];
                  end;
             end;
             ret[i,j] := power(-1, i-j-2)* determinante(le_mat, r-1, c-1);
        end;
   end;
  Result:= ret;
end;

function TMatriz.Inversa(A:matriz; det: real; dim:integer): matriz;
 var
  i,j: integer;
  res, m_cof:matriz;
begin
  m_cof:=cof(A,dim,dim);
  for i:=0 to dim-1 do //fil
    for j:=0 to dim-1 do begin//col
      res[i,j]:= m_cof[i,j]/det;
     end;
  res:= Transpuesta(res,dim,dim);
  result:=res;
end;
procedure TMatriz.Print(a: matriz; n,m:integer);
var i,j: integer;
begin
     for i:=0 to m-1 do begin
         for j:=0 to n-1 do begin
             Write(FloatToStr(a[i,j])+' ');
         end;
         Write(LineEnding);
     end;
end;

 var mat1, mat2: TMatriz;
 det : Real;
 matriz2: array[0..100, 0..100] of Real;
 matriz3: array[0..100, 0..100] of Real;
begin
  matriz2[0,0]:=2;
  matriz2[0,1]:=1;
  matriz2[0,2]:=0;
  matriz2[1,0]:=1;
  matriz2[1,1]:=1;
  matriz2[1,2]:=2;
  matriz2[2,0]:=2;
  matriz2[2,1]:=3;
  matriz2[2,2]:=0;

  matriz3[0,0]:=1;
  matriz3[0,1]:=2;
  matriz3[0,2]:=3;
  matriz3[1,0]:=4;
  matriz3[1,1]:=5;
  matriz3[1,2]:=6;
  matriz3[2,0]:=7;
  matriz3[2,1]:=8;
  matriz3[2,2]:=9;

  mat1:= TMatriz.create();
  mat2:= TMatriz.create();
  mat1.Print(matriz2,3,3);
  writeln('determinante: ');
  det:=mat1.Determinante(matriz2,3,3);
  writeln(floattostr(det));
  {writeln('MATRIZ 2');
  mat2.print(matriz3,3,3);
  mat1.Mult(matriz2,matriz3,3,3,3);
  writeln('mult');
  mat1.print(mat1.Mult(matriz2,matriz3,3,3,3),3,3);  }
  writeln('inv');
  mat1.inversa(matriz2,det,3);
   mat1.print( mat1.inversa(matriz2, det,3),3,3);

  readln();
end.
