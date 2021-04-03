program Pascalik;

uses
    Geometry, Display, World;

const
    ROWS: Integer = 10;
    COLS: Integer = 10;

var
    D: TDisplay;
    W: TWorld;
    Quit: Boolean = False;
    Commands : String;
    Command : Char;
begin
    W := Create_World(ROWS, COLS, Empty);
    World_Fill_Rect(W, Make_Rect(Make_Point(0, 0), Make_Point(5, 5)), Floor);

    D := Create_Display(ROWS, COLS, ' ');

    World_Render(W, D);
    Print_Display(D);
    while not Quit do
    begin
        Write('> ');
        ReadLn(Commands);
        for Command in Commands do
        begin
            case Command of
            'w': World_Move_Player(W, Up);
            's': World_Move_Player(W, Down);
            'a': World_Move_Player(W, Left);
            'd': World_Move_Player(W, Right);
            'q': Quit := True;
            end;
        end;
        World_Render(W, D);
        Print_Display(D);
    end;
end.
