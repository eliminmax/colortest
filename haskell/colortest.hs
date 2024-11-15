-- SPDX-FileCopyrightText: 2023-2024 Eli Array Minkoff
--
-- SPDX-License-Identifier: GPL-3.0-only

colorCell n = concat ["\ESC[48;5;", (show n), "m  "]
cubeRowPart n = concat ([colorCell(i) | i <- [n..n+5]] ++ ["\ESC[0m"])
cubeRow n = concat ([cubeRowPart n, "  ", cubeRowPart (n + 36), "  ", cubeRowPart (n + 72), "\n"])

main :: IO ()
main = do
    -- Print the first 16 colors - these vary by terminal configuration
    putStrLn ""
    putStrLn (concat [colorCell i | i <- [0..15]] ++ "\ESC[0m")
    putStrLn ""
    -- Print the 6 sides of the color cube - these are more standardized
    -- but the order is a bit odd, thus the need for the above trickery
    putStrLn (concat [cubeRow i | i <- [16,22..46]])
    putStrLn (concat [cubeRow i | i <- [124,130..154]])
    -- finally, the 24 grays
    putStrLn (concat [colorCell i | i <- [232..255]] ++ "\ESC[0m\n")
    return ()
