unit Display;

interface

uses Geometry, Math;

type
    TDisplay = record
        rows : Integer;
        cols : Integer;
        pixels : Array of Char;
    end;

    function Create_Display(Rows: Integer; Cols: Integer; X: Char): TDisplay;
    procedure Display_Set(var Display: TDisplay; Point: TPoint; Pixel: Char);
    function Display_Get(Display: TDisplay; Point: TPoint): Char;
    procedure Print_Display(Display: TDisplay);

    procedure Display_Fill_Rect(var Display: TDisplay; Rect: TRect; Pixel: Char);

implementation
    function Create_Display(Rows: Integer; Cols: Integer; X: Char): TDisplay;
    var
        Index : Integer;
        Result : TDisplay;
    begin
        Result.Rows := Rows;
        Result.Cols := Cols;
        SetLength(Result.Pixels, Rows * Cols);
        for Index := 0 to Rows * Cols - 1 do
        begin
            Result.Pixels[Index] := X;
        end;
        Create_Display := Result;
    end;

    procedure Display_Set(var Display: TDisplay; Point: TPoint; Pixel: Char);
    begin
        Display.Pixels[Modulo(Point.Row, Display.Rows) * Display.Cols + Modulo(Point.Col, Display.Cols)] := Pixel;
    end;

    function Display_Get(Display: TDisplay; Point: TPoint): Char;
    begin
        Display_Get := Display.Pixels[Modulo(Point.Row, Display.Rows) * Display.Cols + Modulo(Point.Col, Display.Cols)];
    end;

    procedure Print_Display(Display: TDisplay);
    var
        Row, Col: Integer;
    begin
        for Row := 0 to Display.Rows - 1 do
        begin
            for Col := 0 to Display.Cols - 1 do
            begin
                Write(Display_Get(Display, Make_Point(Row, Col)));
            end;
            WriteLn();
        end;
    end;

    procedure Display_Fill_Rect(var Display: TDisplay; Rect: TRect; Pixel: Char);
    var
        Row, Col: Integer;
    begin
        for Row := Min(Rect.Point1.Row, Rect.Point2.Row) to Max(Rect.Point1.Row, Rect.Point2.Row) do
        begin
            for Col := Min(Rect.Point1.Col, Rect.Point2.Col) to Max(Rect.Point1.Col, Rect.Point2.Col)do
            begin
                Display_Set(Display, Make_Point(Row, Col), Pixel);
            end;
        end;
    end;
end.
