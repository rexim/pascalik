program Pascalik;

uses Math;

type
    TCell = (Empty, Floor, VertWall, HorzWall, Door, Passage);
    TDir = (Up, Down, Right, Left);

    TWorld = record
        Rows       : Integer;
        Cols       : Integer;
        Cells      : Array of TCell;
        Player_Row : Integer;
        Player_Col : Integer;
    end;

const
    Cell_To_Char: array[TCell] of Char = (' ', '.', '|', '-', '+', '#');
    Row_Offset: array[TDir] of Integer = (-1, 1, 0, 0);
    Col_Offset: array[TDir] of Integer = (0, 0, 1, -1);
    Walkable: array[TCell] of Boolean = (False, True, False, False, True, True);

function Modulo(A: Integer; B: Integer): Integer;
begin
    Modulo := ((A mod B) + B) mod B;
end;

function Create_World(Rows, Cols: Integer; Cell: TCell): TWorld;
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
    Result.Player_Row := 0;
    Result.Player_Col := 0;
    Create_World := Result
end;

function World_Get(World: TWorld; Row, Col: Integer): TCell;
begin
    World_Get := World.Cells[Modulo(Row, World.Rows) * World.Cols + Modulo(Col, World.Cols)];
end;

procedure World_Set(var World: TWorld; Row, Col: Integer; Cell: TCell);
begin
    World.Cells[Modulo(Row, World.Rows) * World.Cols + Modulo(Col, World.Cols)] := Cell;
end;

procedure World_Fill_Rect(var World: TWorld; Row1, Col1, Row2, Col2: Integer; Cell: TCell);
var
    Row, Col : Integer;
begin
    for Row := Min(Row1, Row2) to Max(Row1, Row2) do
    begin
        for Col := Min(Col1, Col2) to Max(Col1, Col2)do
        begin
            World_Set(World, Row, Col, Cell);
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
            if (Row = World.Player_Row) and (Col = World.Player_Col) then
                Write('@')
            else
                Write(Cell_To_Char[World_Get(World, Row, Col)]);
        end;
        WriteLn();
    end;
end;

procedure World_Move_Player(var World: TWorld; Dir: TDir);
var
    New_Row, New_Col: Integer;
begin
    New_Row := World.Player_Row + Row_Offset[Dir];
    New_Col := World.Player_Col + Col_Offset[Dir];
    if Walkable[World_Get(World, New_Row, New_Col)] then
    begin
        World.Player_Row := New_Row;
        World.Player_Col := New_Col;
    end;
end;


const
    ROWS: Integer = 10;
    COLS: Integer = 10;

var
    World: TWorld;
    Quit: Boolean = False;
    Commands : String;
    Command : Char;
begin
    World := Create_World(ROWS, COLS, Empty);
    World_Fill_Rect(World, 0, 0, 5, 5, Floor);

    World_Render(World);
    while not Quit do
    begin
        Write('> ');
        ReadLn(Commands);
        for Command in Commands do
        begin
            case Command of
            'w': World_Move_Player(World, Up);
            's': World_Move_Player(World, Down);
            'a': World_Move_Player(World, Left);
            'd': World_Move_Player(World, Right);
            'q': Quit := True;
            end;
        end;
        World_Render(World);
    end;
end.
