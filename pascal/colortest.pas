{
SPDX-FileCopyrightText: 2024 Eli Array Minkoff

SPDX-License-Identifier: GPL-3.0-only
}

program colortest;
uses sysutils;
var esc: char;
var i: integer; { general iterator }
var ii: integer; { inner iterator }

procedure colorCell(n: integer);
{ prints the color cell }
begin
    write(esc, '[48;5;',IntToStr(n),'m  ');
end; { end of colCell }

procedure cubeRowParts(n: integer);
{ prints a sequence of 6 color cells starting with color n }
begin
    for ii := n to (n+5) do colorCell(ii);
end;

procedure cubeRow(n: integer);
{ prints a row of the color cube }
begin
    cubeRowParts(n);
    write(esc, '[0m  ');
    cubeRowParts(n + 36);
    write(esc, '[0m  ');
    cubeRowParts(n + 72);
    writeLn(esc, '[0m');
    
end;

begin
    esc := #27;
    { Print the first 16 colors - these vary by terminal configuration }
    writeLn();
    for i := 0 to 15 do colorCell(i);
    writeLn(esc,'[0m');
    writeLn();

    { Print the 6 sides of the color cube - these are more standardized,
    but the order is a bit odd, thus the need for the above trickery }
    for i := 0 to 5 do cubeRow((i * 6) + 16);
    writeLn();
    for i := 18 to 23 do cubeRow((i * 6) + 16);
    writeLn();

    { Finally, the 24 grays }
    for i := 232 to 255 do colorCell(i);
    writeLn(esc,'[0m');
    writeLn();
end.
