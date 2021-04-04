unit world;

interface

uses Geometry;

type
    TCell = (Empty, Floor, VertWall, HorzWall, Door, Passage);
    TDir = (Up, Down, Right, Left);

    TWorld = record
        Rows       : Integer;
        Cols       : Integer;
        Cells      : Array of TCell;
        Player_Pos : TPoint;
    end;

    function Create_World(Rows: Integer; Cols: Integer; Cell: TCell): TWorld;
    function World_Get(World: TWorld; Point: TPoint): TCell;
    procedure World_Set(var World: TWorld; Point: TPoint; Cell: TCell);
    procedure World_Fill_Rect(var World: TWorld; Rect: TRect; Cell: TCell);

    procedure World_Render(World: TWorld);
    procedure World_Move_Player(var World: TWorld; Dir: TDir);

const
    Cell_To_Char: array[TCell] of Char = (' ', '.', '|', '-', '+', '#');
    Dir_To_Offset: array[TDir] of TPoint = (
        (Row: -1; Col:  0),
        (Row:  1; Col:  0),
        (Row:  0; Col:  1),
        (Row:  0; Col: -1)
    );
    Walkable: array[TCell] of Boolean = (False, True, False, False, True, True);

implementation
    uses Math;

    function Create_World(Rows: Integer; Cols: Integer; Cell: TCell): TWorld;
    var
        Index : Integer;
        Result : TWorld;
    begin
        Result.Rows := Rows;
        Result.Cols := Cols;
        SetLength(Result.Cells, Rows * Cols);
        for Index := 0 to Rows * Cols - 1 do
        begin
            Result.Cells[Index] := Cell;
        end;
        Result.Player_Pos := Make_Point(0, 0);
        Create_World := Result
    end;

    function World_Get(World: TWorld; Point: TPoint): TCell;
    begin
        World_Get := World.Cells[Modulo(Point.Row, World.Rows) * World.Cols + Modulo(Point.Col, World.Cols)];
    end;

    procedure World_Set(var World: TWorld; Point: TPoint; Cell: TCell);
    begin
        World.Cells[Modulo(Point.Row, World.Rows) * World.Cols + Modulo(Point.Col, World.Cols)] := Cell;
    end;

    procedure World_Fill_Rect(var World: TWorld; Rect: TRect; Cell: TCell);
    var
        Row, Col : Integer;
    begin
        for Row := Min(Rect.Point1.Row, Rect.Point2.Row) to Max(Rect.Point1.Row, Rect.Point2.Row) do
        begin
            for Col := Min(Rect.Point1.Col, Rect.Point2.Col) to Max(Rect.Point1.Col, Rect.Point2.Col)do
            begin
                World_Set(World, Make_Point(Row, Col), Cell);
            end;
        end;
    end;

    procedure World_Render(World: TWorld);
    var
        Row, Col: Integer;
    begin
        for Row := 0 to World.Rows - 1 do
        begin
            for Col := 0 to World.Cols - 1 do
            begin
                if (Row = World.Player_Pos.Row) and (Col = World.Player_Pos.Col) then
                    Write('@')
                else
                    Write(Cell_To_Char[World_Get(World, Make_Point(Row, Col))]);
            end;
            WriteLn();
        end;
    end;

    procedure World_Move_Player(var World: TWorld; Dir: TDir);
    var
        New_Pos : TPoint;
    begin
        New_Pos := Point_Add(World.Player_Pos, Dir_To_Offset[Dir]);
        if Walkable[World_Get(World, New_Pos)] then
            World.Player_Pos := New_Pos;
    end;
end.
