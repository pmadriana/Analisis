unit class_taylor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs;

type
    TTaylor = class
    ErrorAllowed: Real;
    Sequence,
    FunctionList: TstringList;  //lista de las func
    FunctionType: Integer;
    AngleType: Integer;
    x: Real;
    function Execute(): Real;
    private
      Error,
      Angle: Real;
      function sen(): Real;
      function cos(): Real;
      function exponencial():Real;
      function arcsen():Real;
      function tan():Real;
      function senh():Real;
      function cosh():Real;
      function csc():Real;
      function tanh():Real;
      function arctan():Real;
      function arctanh():Real;
    public

      constructor create;
      destructor Destroy; override;

  end;

const
  IsSin = 0;
  IsCos = 1;
  IsExp = 2;
  IsTan=3;
  IsCsc = 4;
  IsArcSen=5;
  IsArctan =6;
  IsSenh =7;
  IsCosh=8;
  Istanh=9;
  IsArctanh=10;

  AngleSexagedecimal = 0;
  AngleRadian = 1;


implementation

const
  Top = 100000;

constructor TTaylor.create;
begin
  Sequence:= TStringList.Create;
  FunctionList:= TStringList.Create;
  FunctionList.AddObject( 'sen', TObject( IsSin ) );
  FunctionList.AddObject( 'cos', TObject( IsCos ) );
  FunctionList.AddObject( 'Exp', TObject( IsExp) );
  FunctionList.AddObject('Tan', TObject(IsTan));
  FunctionList.AddObject('Csc', TObject(IsCsc) );
  FunctionList.AddObject( 'ArcSen', TObject( IsArcSen));
  FunctionList.AddObject('ArcTan',TObject(IsArctan));
  FunctionList.AddObject('SenH', TObject(IsSenh));
  FunctionList.AddObject('CosH', TObject(IsCosh));
  FunctionList.AddObject('TanH',TObject(IsTanh));
  FunctionList.AddObject('ArctanH',TObject(IsArctanh));
  Sequence.Add('');
  Error:= Top;
  x:= 0;

end;

destructor TTaylor.Destroy;
begin
  Sequence.Destroy;
  FunctionList.Destroy;
end;

function Power( b: Real; n: Integer ): Real;
var i: Integer;
begin
   Result:= 1;
   for i:= 1 to n do
      Result:= Result * b;

end;

function Factorial( n: Integer ): Double;
begin

     if n > 1 then
        Result:= n * Factorial( n -1 )

     else if n >= 0 then
        Result:= 1

     else
        Result:= 0;

end;

function Combinatoria(n: Integer; p:Integer):Double;
begin
   Result:= Factorial(n) /( Factorial(n-p)*Factorial(p));
end;

function Bernoulli(k: Integer): Extended;

var
    i: Integer;
begin
   Result := 0;
   if k = 0 then
        Result := 1
   else if k = 1 then
        Result := -1/2
   else if k mod 2 = 1 then
        Result := 0
   else
        begin
        for i:=0 to k-1 do
            Result := Result + Combinatoria(k,i) * (Bernoulli(i)/(k + 1 - i));
        Result := - Result;
        end;
end;

function TTaylor.Execute( ): Real;
begin

   case AngleType of
        AngleRadian: Angle:= x;    //si es radian angulo = x
        AngleSexagedecimal: Angle:=x * pi/180;     //si es sexagesimal conversion
   end;

   case FunctionType of
        IsSin: Result:= sen();   //function type depende si es seno o cos
        IsCos: Result:= cos();
        IsExp: Result:=exponencial();
        IsArcSen: Result:= arcsen();
        IsTan: Result:=tan();
        IsSenh: Result := senh();
        isCosh:Result:=cosh();
        IsCsc:Result:=csc();
        IsTanh:Result:=tanh();
        IsArctan:Result:=arctan();
        IsArctanh:Result:=arctanh();
   end;


end;

function TTaylor.sen(): Real;
var xn: Real;
     n: Integer;
begin
   Result:= 0;
   n:= 0;

   repeat
     xn:= Result;

     Result:= Result + Power(-1, n)/Factorial( 2*n + 1 ) * Power(Angle, 2*n + 1);
     if n > 0 then
        Error:= abs( Result - xn );

     Sequence.Add( FloatToStr( Result ) );
     n:= n + 1;

   until ( Error <= ErrorAllowed ) or ( n >= Top ) ;

end;


function TTaylor.cos(): Real;
var xn: real;
    n: Integer;

begin
  Result:= 0;
  n:= 0;

  repeat
    xn:= Result;
    Result:= Result + Power(-1, n)/Factorial(2*n) * Power( Angle, 2*n );
    Sequence.Add( FloatToStr( Result ) );
    if n > 0 then
       Error:= abs( Result - xn );

    n:= n + 1;
  until ( Error < ErrorAllowed ) or ( n >= Top );

end;

function TTaylor.exponencial(): Real;
var xn: Real;
    n: Integer;
    angle_of: Real;

    {case AngleType of
           AngleSexagedecimal: Angle:= x;    //si es radian angulo = x
           AngleRadian: Angle:= x * 180/pi;     //si es sexagesimal conversion   }
begin
  angle_of:= angle * 180 / pi;
  n:= 0;
  Result:= 0;

  repeat
    xn:= Result;
    Result:= Result + (Power(angle_of,n) / Factorial(n));
    Sequence.Add( FloatToStr( Result ) );
    if n > 0 then
       Error:= abs( Result - xn );

    n:= n + 1;
  until (Error < ErrorAllowed ) or (n >= Top );

end;

function TTaylor.senh(): Real;
var xn: Real;
     n: Integer;
     angle_of :Real;
begin
   Result:= 0;
   n:= 0;
    angle_of:= angle * 180 / pi;
   repeat
     xn:= Result;

     Result:= Result + 1/Factorial(2*n + 1) * Power(angle_of, 2*n + 1);
     if n > 0 then
        Error:= abs( Result - xn );

     Sequence.Add( FloatToStr( Result ) );
     n:= n + 1;

   until ( Error <= ErrorAllowed ) or ( n >= Top ) ;

end;

function TTaylor.cosh():Real;
var xn: real;
    n: Integer;
    angle_of : Real;
begin
  Result:= 0;
  n:= 0;
  angle_of:= angle * 180 / pi;
  repeat
    xn:= Result;
    Result:= Result + 1/Factorial(2*n) * Power( angle_of, 2*n );
    Sequence.Add( FloatToStr( Result ) );
    if n > 0 then
       Error:= abs( Result - xn );

    n:= n + 1;
  until ( Error < ErrorAllowed ) or ( n >= Top );

end;

function TTaylor.tan():Real;

var xn: Real;
     n: Integer;
begin
  if abs(angle) >= pi/2 then
  begin
       Result:= 0;
       exit();
  end;
   Result:= 0;
   n:= 1;

   repeat
     xn:= Result;

     Result:= Result + ((Bernoulli(2*n) * Power(-4,n) * (1-Power(4,n)) )/Factorial(2*n) )* Power(angle,2*n -1 );
     if n > 0 then
        Error:= abs( Result - xn );

     Sequence.Add( FloatToStr( Result ) );
     n:= n + 1;

   until ( Error <= ErrorAllowed ) or ( n >= Top ) ;

end;

function TTaylor.csc():Real;
var xn: real;
    n: Integer;
begin
  Result:= 0;
  n:= 1;
  repeat
    xn:= Result;
    Result:= Result + ((2*(Power(2,2*n-1) - 1))*Bernoulli(n)*Power(angle, 2*n-1)) / Factorial(2*n);
    Sequence.Add( FloatToStr( Result ) );
    if n > 0 then
       Error:= abs( Result - xn );

    n:= n + 1;
  until ( Error < ErrorAllowed ) or ( n >= Top );

end;

function TTaylor.tanh(): Real;
var xn:Real;
    n:Integer;
    angle_of:Real;
begin
   if abs(x * pi/180) > pi/2 then
    begin
         Result:= 0;
         exit();
    end;
    Result := 0;
    n := 0;
    angle_of:= angle * 180 / pi ;
    repeat
      xn:= Result;
      Result:= Result + ((Bernoulli(2*n)
      *power(4,n)*(power(4,n) - 1))/factorial(2*n)) * power(angle,2*n - 1);
      Sequence.Add( FloatToStr( Result ) );
      if n > 0 then
         Error:= abs( Result - xn );
      n:= n + 1;
    until ( Error < ErrorAllowed ) or ( n >= Top );
end;

function TTaylor.arctan(): Real;
var xn: Real;
     n: Integer;
     angle_of:Real;
begin
    if abs(angle) > 1 then
    begin
         Result:= 0;
         exit();
    end;
   Result:= 0;
   n:= 0;
   angle_of:= angle*180 / pi;
   repeat
     xn:= Result;

     Result:= Result + Power(-1, n)/( 2*n + 1 ) * Power(angle_of, 2*n + 1);
     if n > 0 then
        Error:= abs( Result - xn );

     Sequence.Add( FloatToStr( Result ) );
     n:= n + 1;

   until ( Error <= ErrorAllowed ) or ( n >= Top ) ;

end;

function TTaylor.arcsen():Real  ;
var xn: Real;
    n: Integer;
    angle_of : Real;
begin
   if abs(angle) > 1 then
    begin
         Result:= 0;
         exit();
    end;
  n := 0;
  Result := 0;
  angle_of:= angle*180 / pi;
  repeat
    xn := Result;
    Result := Result + Factorial( 2*n )/ ( Power( 4 , n)*Power( Factorial( n ) , 2)*(2*n + 1))*Power( angle_of, 2*n + 1);
    Sequence.add( FloatToStr ( Result ) );
    if n > 0 then
       Error := abs( Result - xn);

    n := n + 1;
  until (Error < ErrorAllowed) or (n >= Top);
end;
function TTaylor.arctanh(): Real;
var xn: Real;
     n: Integer;
     angle_of:Real;
begin
  if abs(angle) > 1 then
    begin
         Result:= 0;
         exit();
    end;
   Result:= 0;
   n:= 0;
   angle_of:= angle*180 / pi;
   repeat
     xn:= Result;

     Result:= Result + Power(1, n)/( 2*n + 1 ) * Power(Angle_of, 2*n + 1);
     if n > 0 then
        Error:= abs( Result - xn );

     Sequence.Add( FloatToStr( Result ) );
     n:= n + 1;

   until ( Error <= ErrorAllowed ) or ( n >= Top ) ;

end;
end.
                       
