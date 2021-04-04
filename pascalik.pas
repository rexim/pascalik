program Pascalik;

uses
    World;

const
    ROWS: Integer = 10;
    COLS: Integer = 10;

var
    W: TWorld;
    Quit: Boolean = False;
    Commands : String;
    Command : Char;
begin
    W := Create_World(ROWS, COLS, Empty);
    World_Fill_Rect(W, 0, 0, 5, 5, Floor);

    World_Render(W);
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
        World_Render(W);
    end;
end.
