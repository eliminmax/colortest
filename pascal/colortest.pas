{
SPDX-FileCopyrightText: 2024 Eli Array Minkoff

SPDX-License-Identifier: GPL-3.0-only
}

program colortest;
uses sysutils;
var esc: char;
var i: integer;
var ii: integer;
begin
    esc := #27;
    { Print the first 16 colors - these vary by terminal configuration }
    writeLn();
    for i := 0 to 15 do
    begin
        write(esc,'[48;5;',IntToStr(i),'m  ');
    end;
    writeLn(esc,'[0m');
    writeLn();

    { Print the 6 sides of the color cube - these are more standardized,
    but the order is a bit odd, thus the need for this trickery }
    for i := 0 to 5 do
    begin
        for ii := 0 to 5 do
        begin
            write(esc,'[48;5;',IntToStr((i * 6) + 16 + ii),'m  ');
        end;
        write(esc,'[0m  ');
        for ii := 36 to 41 do
        begin
            write(esc,'[48;5;',IntToStr((i * 6) + 16 + ii),'m  ');
        end;
        write(esc,'[0m  ');
        for ii := 72 to 77 do
        begin
            write(esc,'[48;5;',IntToStr((i * 6) + 16 + ii),'m  ');
        end;
        writeLn(esc,'[0m');
    end;
    writeLn();
    for i := 18 to 23 do
    begin
        for ii := 0 to 5 do
        begin
            write(esc,'[48;5;',IntToStr((i * 6) + 16 + ii),'m  ');
        end;
        write(esc,'[0m  ');
        for ii := 36 to 41 do
        begin
            write(esc,'[48;5;',IntToStr((i * 6) + 16 + ii),'m  ');
        end;
        write(esc,'[0m  ');
        for ii := 72 to 77 do
        begin
            write(esc,'[48;5;',IntToStr((i * 6) + 16 + ii),'m  ');
        end;
        writeLn(esc,'[0m');
    end;
    writeLn();

    { Finally, the 24 grays }
    for i := 232 to 255 do
    begin
        write(esc,'[48;5;',IntToStr(i),'m  ');
    end;
    writeLn(esc,'[0m');
    writeLn();
end.
