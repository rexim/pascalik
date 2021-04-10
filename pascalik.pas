{-*- mode: opascal; opascal-indent-level: 4 -*-}
program Pascalik;
uses Math;

{$R+}

const
    Camera_Rows = 10;
    Camera_Cols = 20;

    World_Rows = 100;
    World_Cols = 200;

    Room_Rows = 5;
    Room_Cols = 8;

type
    TCell = (Empty, Floor, VertWall, HorzWall, Door);
    TDir = (Up, Down, Right, Left);

    TItemKind = (Nothing, Gold, Health);
    TItem = record
        case Kind : TItemKind of
           Gold: (Amount: Integer)
    end;

    TWorld = record
        Cells         : array [0..World_Rows-1, 0..World_Cols-1] of TCell;
        Items         : array [0..World_Rows-1, 0..World_Cols-1] of TItem;
        Player_Row    : Integer;
        Player_Col    : Integer;
        Player_Gold   : Integer;
        {$ifdef DEVBUILD}
        Player_Noclip : Boolean;
        {$endif}
    end;
const
    Item_To_Char: array[TItemKind] of Char = ('?', '*', '&');
    Cell_To_Char: array[TCell] of Char = (' ', '.', '|', '-', '+');
    Row_Offset: array[TDir] of Integer = (-1, 1, 0, 0);
    Col_Offset: array[TDir] of Integer = (0, 0, 1, -1);
    Walkable: array[TCell] of Boolean = (False, True, False, False, True);

    function Modulo(A: Integer; B: Integer): Integer;
    begin
        Modulo := A;
    end;

    function Make_Gold(Amount: Integer): TItem;
    begin
        Make_Gold.Kind := Gold;
        Make_Gold.Amount := Amount;
    end;

    procedure World_Fill_Rect(var World: TWorld; Row1, Col1, Row2, Col2: Integer; Cell: TCell);
    var
        Row, Col : Integer;
    begin
        for Row := Min(Row1, Row2) to Max(Row1, Row2) do
            for Col := Min(Col1, Col2) to Max(Col1, Col2) do
                World.Cells[Row, Col] := Cell;
    end;

    procedure World_Flood_Rect(var World: TWorld; Row, Col, Height, Width: Integer; Cell: TCell);
    begin
        World_Fill_Rect(World, Row, Col, Row + Height - 1, Col + Width - 1, Cell);
    end;

    procedure World_Spawn_Player(var World: TWorld);
    var
        Row, Col: Integer;
    begin
        for Row := 0 to World_Rows - 1 do
            for Col := 0 to World_Cols - 1 do
                if Walkable[World.Cells[Row, Col]] then
                begin
                    World.Player_Row := Row;
                    World.Player_Col := Col;
                    Exit;
                end;
    end;

    procedure World_Generate(var World: TWorld);
    var
        Row, Col: Integer;
    begin
        {Clean Cells}
        for Row := 0 to World_Rows-1 do
            for Col := 0 to World_Cols-1 do
                World.Cells[Row, Col] := Empty;

        {Generate Rooms}
        // TODO: generate walls and doors between the rooms
        for Row := 0 to 1 do
            for Col := 0 to 5 do
            begin
                World_Flood_Rect(
                    World,
                    1 + (Room_Rows + 1) * Row,
                    1 + (Room_Cols + 1) * Col,
                    Room_Rows,
                    Room_Cols,
                    Floor);
            end;


        {Spawn Player}
        World_Spawn_Player(World);

        {Generating Items}
        World.Items[3][3] := Make_Gold(69);
    end;

    procedure World_Render(World: TWorld; Camera_Rows, Camera_Cols: Integer);
    var
        Row, Col: Integer;
        Cam_Row1, Cam_Col1, Cam_Row2, Cam_Col2: Integer;
    begin
        Cam_Row1 := Max(0, World.Player_Row - Camera_Rows div 2);
        Cam_Col1 := Max(0, World.Player_Col - Camera_Cols div 2);
        Cam_Row2 := Min(World_Rows - 1, Cam_Row1 + Camera_Rows - 1);
        Cam_Col2 := Min(World_Cols - 1, Cam_Col1 + Camera_Cols - 1);

        for Row := Cam_Row1 to Cam_Row2 do
        begin
            for Col := Cam_Col1 to Cam_Col2 do
                if (Row = World.Player_Row) and (Col = World.Player_Col) then
                    Write('@')
                else if World.Items[Row][Col].Kind <> Nothing then
                    Write(Item_To_Char[World.Items[Row][Col].Kind])
                else
                    Write(Cell_To_Char[World.Cells[Row, Col]]);
            WriteLn();
        end;
        WriteLn('Gold: ', World.Player_Gold);
        {$ifdef DEVBUILD}
        WriteLn('Noclip: ', World.Player_Noclip);
        {$endif}
    end;

    procedure World_Move_Player(var World: TWorld; Dir: TDir);
    var
        New_Row, New_Col: Integer;
    begin
        New_Row := World.Player_Row + Row_Offset[Dir];
        New_Col := World.Player_Col + Col_Offset[Dir];

        if (0 <= New_Row) and (New_Row < World_Rows) then
            if (0 <= New_Col) and (New_Col < World_Cols) then
                if Walkable[World.Cells[New_Row, New_Col]] {$ifdef DEVBUILD}or World.Player_Noclip{$endif} then
                begin
                    World.Player_Row := New_Row;
                    World.Player_Col := New_Col;

                    with World.Items[New_Row][New_Col] do
                        case Kind of
                           Gold:
                               begin
                                   Inc(World.Player_Gold, Amount);
                                   Kind := Nothing;
                               end
                        end
                end
    end;

var
    World: TWorld;
    Quit: Boolean = False;
    Commands : String;
    Command : Char;
begin
    Randomize;
    World_Generate(World);

    World_Render(World, CAMERA_ROWS, CAMERA_COLS);
    while not Quit do
    begin
        Write('> ');
        ReadLn(Commands);
        for Command in Commands do
            case Command of
               'w': World_Move_Player(World, Up);
               's': World_Move_Player(World, Down);
               'a': World_Move_Player(World, Left);
               'd': World_Move_Player(World, Right);
               {$ifdef DEVBUILD}
               '{': World.Player_Noclip := not World.Player_Noclip;
               {$endif}
               'q': Quit := True;
            end;
        World_Render(World, CAMERA_ROWS, CAMERA_COLS);
    end;
end.
