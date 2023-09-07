#!/usr/bin/env escript
%%! -sname colortest -mnesia debug verbose

colorCell(N) -> io_lib:format("\x1b[48;5;~Bm  ", [N]).
rowA(I) -> lists:concat([colorCell(I+II) || II <- lists:seq(0, 5)] ++ ["\x1b[0m  "]).
rowB(I) -> lists:concat([colorCell(I+II) || II <- lists:seq(36, 41)] ++ ["\x1b[0m  "]).
rowC(I) -> lists:concat([colorCell(I+II) || II <- lists:seq(72, 77)] ++ ["\x1b[0m\n"]).
cubeRow(I) -> lists:concat([rowA(I), rowB(I), rowC(I)]).
main(_) ->
    % Print the first 16 colors - these vary by terminal configuration
    io:format("\n"),
    io:format(lists:concat([colorCell(I) || I <- lists:seq(0, 15)])),
    io:format("\x1b[0m\n\n"),
    % Print the 6 sides of the color cube - these are more standardized
    % but the order is a bit odd, thus the need for the above trickery
    io:format(lists:concat([cubeRow(I) || I <- lists:seq(16, 46, 6)])),
    io:format("\n"),
    io:format(lists:concat([cubeRow(I) || I <- lists:seq(124, 154, 6)])),
    io:format("\n"),
    % finally, the 24 grays
    io:format(lists:concat([colorCell(I) || I <- lists:seq(232, 255)])),
    io:format("\x1b[0m\n\n").
