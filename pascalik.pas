{-*- mode: opascal; opascal-indent-level: 4 -*-}
program Pascalik;
uses Math;

{$R+}

const
    Camera_Rows = 10;
    Camera_Cols = 20;

    World_Rows = 100;
    World_Cols = 200;

type
    TCell = (Empty, Floor, VertWall, HorzWall, Door);
    TDir = (Up, Down, Right, Left);

    TItemKind = (Nothing, Gold, Health);
    TItem = record
        Row, Col : Integer;
        case Kind : TItemKind of
           Gold: (Amount: Integer)
    end;

    TWorld = record
        Cells       : array [0..World_Rows-1, 0..World_Cols-1] of TCell;
        // TODO: Make TWorld.Items a square array of the size of the world
        Items       : array of TItem;
        Player_Row  : Integer;
        Player_Col  : Integer;
        Player_Gold : Integer;
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

    function Make_Gold(Row, Col, Amount: Integer): TItem;
    var
        Result: TItem;
    begin
        Result.Kind := Gold;
        Result.Row := Row;
        Result.Col := Col;
        Result.Amount := Amount;
        Make_Gold := Result;
    end;

    procedure World_Fill_Rect(var World: TWorld; Row1, Col1, Row2, Col2: Integer; Cell: TCell);
    var
        Row, Col : Integer;
    begin
        for Row := Min(Row1, Row2) to Max(Row1, Row2) do
            for Col := Min(Col1, Col2) to Max(Col1, Col2) do
                World.Cells[Row, Col] := Cell;
    end;

    procedure World_Place_Room(var World: TWorld; Row, Col, Height, Width: Integer);
    begin
        {Floor}
        World_Fill_Rect(World, Row + 1, Col + 1, Row + Height - 2, Col + Width - 2, Floor);

        {Wall Frame}
        World_Fill_Rect(World, Row, Col, Row + Height - 1, Col, VertWall);
        World_Fill_Rect(World, Row, Col + Width - 1, Row + Height - 1, Col + Width - 1, VertWall);
        World_Fill_Rect(World, Row, Col, Row, Col + Width - 1, HorzWall);
        World_Fill_Rect(World, Row + Height - 1, Col, Row + Height - 1, Col + Width - 1, HorzWall);
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
        Index, Row, Col: Integer;
    begin
        {Clean Cells}
        for Row := 0 to World_Rows-1 do
            for Col := 0 to World_Cols-1 do
                World.Cells[Row, Col] := Empty;

        {Spawn Player}
        World_Spawn_Player(World);

        {Generating Items}
        SetLength(World.Items, 1);
        World.Items[0] := Make_Gold(3, 3, 69);
    end;

    function World_Item_At(World: TWorld; Row, Col: Integer; var Item_Index: Integer): Boolean;
    var
        Index : Integer;
    begin
        for Index := Low(World.Items) to High(World.Items) do
            if (World.Items[Index].Row = Row) and (World.Items[Index].Col = Col) and (World.Items[Index].Kind <> Nothing) then
            begin
                Item_Index := Index;
                Exit(True);
            end;
        Exit(False);
    end;

    procedure World_Render(World: TWorld; Camera_Rows, Camera_Cols: Integer);
    var
        Row, Col: Integer;
        Item_Index: Integer;
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
                else if World_Item_At(World, Row, Col, Item_Index) then
                    Write(Item_To_Char[World.Items[Item_Index].Kind])
                else
                    Write(Cell_To_Char[World.Cells[Row, Col]]);
            WriteLn();
        end;
        WriteLn('Gold: ', World.Player_Gold);
    end;

    procedure World_Move_Player(var World: TWorld; Dir: TDir);
    var
        New_Row, New_Col: Integer;
        Item_Index: Integer;
    begin
        New_Row := World.Player_Row + Row_Offset[Dir];
        New_Col := World.Player_Col + Col_Offset[Dir];

        if (0 <= New_Row) and (New_Row < World_Rows) then
            if (0 <= New_Col) and (New_Col < World_Cols) then
                if Walkable[World.Cells[New_Row, New_Col]] then
                begin
                    World.Player_Row := New_Row;
                    World.Player_Col := New_Col;

                    if World_Item_At(World, New_Row, New_Col, Item_Index) then
                        with World.Items[Item_Index] do
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
               'q': Quit := True;
            end;
        World_Render(World, CAMERA_ROWS, CAMERA_COLS);
    end;
end.
