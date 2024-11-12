#!/usr/bin/env escript
%%! -sname colortest -mnesia debug verbose
% SPDX-FileCopyrightText: 2023 Eli Array Minkoff
% SPDX-License-Identifier: GPL-3.0-only

colorCell(N) -> io_lib:format("\x1b[48;5;~Bm  ", [N]).
cubeRowPart(N) -> lists:concat([colorCell(I) || I <- lists:seq(N, N+5)] ++ ["\x1b[0m"]).
cubeRow(N) -> lists:concat(
                [cubeRowPart(N), ["  "],
                 cubeRowPart(N+36), ["  "],
                 cubeRowPart(N+72), ["\n"]
                ]).

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
