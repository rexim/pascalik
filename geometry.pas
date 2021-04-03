unit geometry;

interface

uses Math;

type
    TPoint = record
        row: Integer;
        col: Integer;
    end;

    TRect = record
        Point1: TPoint;
        Point2: TPoint;
    end;

    function Modulo(A: Integer; B: Integer): Integer;

    function Make_Point(Row: Integer; Col: Integer): TPoint;
    function Make_Rect(Point1: TPoint; Point2: TPoint): TRect;

    function Point_Add(A, B: TPoint): TPoint;

implementation
    function Make_Point(Row: Integer; Col: Integer): TPoint;
    begin
        Make_Point.Row := Row;
        Make_Point.Col := Col;
    end;

    function Make_Rect(Point1: TPoint; Point2: TPoint): TRect;
    begin
        Make_Rect.Point1 := Point1;
        Make_Rect.Point2 := Point2;
    end;

    function Modulo(A: Integer; B: Integer): Integer;
    begin
        Modulo := ((A mod B) + B) mod B;
    end;

    function Point_Add(A, B: TPoint): TPoint;
    begin
        Point_Add.Row := A.Row + B.Row;
        Point_Add.Col := A.Col + B.Col;
    end;
end.
