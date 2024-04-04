-- SPDX-FileCopyrightText: 2023 Eli Array Minkoff
--
-- SPDX-License-Identifier: GPL-3.0-only

colorCell i = concat ["\ESC[48;5;", (show i), "m  "]
rowA i = concat ([colorCell (i + ii) | ii <- [0..5]] ++ ["\ESC[0m  "])
rowB i = concat ([colorCell (i + ii) | ii <- [36..41]] ++ ["\ESC[0m  "])
rowC i = concat ([colorCell (i + ii) | ii <- [72..77]] ++ ["\ESC[0m\n"])
cubeRow i = concat [rowA i, rowB i, rowC i]

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
