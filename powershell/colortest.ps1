#!/usr/bin/env pwsh

# SPDX-FileCopyrightText: 2023 Eli Array Minkoff
#
# SPDX-License-Identifier: GPL-3.0-only

# Powershell is not my language of choice, and I'm essentially converting colortest.py, so it's a bit ugly

$esc = [char]27
# Print the first 16 colors - these vary by terminal configuration
''
(0..15) | Join-String -OutputPrefix "$esc[48;5;" -Separator "m  $esc[48;5;" -OutputSuffix "m  $esc[0m"
''

# Print the 6 sides of the color cube - these are more standardized
# but the order is a bit odd, thus the need for this trickery
For ($i = 16; $i -lt 52; $i += 6) {
    $row_a = foreach($ii in (0..5)){$i+$ii}
    $row_b = foreach($ii in (36..41)){$i+$ii}
    $row_c = foreach($ii in (72..77)){$i+$ii}
    (
        ($row_a | Join-String -OutputPrefix "$esc[48;5;" -Separator "m  $esc[48;5;" -OutputSuffix "m  $esc[0m  "),
        ($row_b | Join-String -OutputPrefix "$esc[48;5;" -Separator "m  $esc[48;5;" -OutputSuffix "m  $esc[0m  "),
        ($row_c | Join-String -OutputPrefix "$esc[48;5;" -Separator "m  $esc[48;5;" -OutputSuffix "m  $esc[0m")
    ) | Join-String
}
''
For ($i = 124; $i -lt 160; $i += 6) {
    $row_a = foreach($ii in (0..5)){$i+$ii}
    $row_b = foreach($ii in (36..41)){$i+$ii}
    $row_c = foreach($ii in (72..77)){$i+$ii}

    (
        ($row_a | Join-String -OutputPrefix "$esc[48;5;" -Separator "m  $esc[48;5;" -OutputSuffix "m  $esc[0m  "),
        ($row_b | Join-String -OutputPrefix "$esc[48;5;" -Separator "m  $esc[48;5;" -OutputSuffix "m  $esc[0m  "),
        ($row_c | Join-String -OutputPrefix "$esc[48;5;" -Separator "m  $esc[48;5;" -OutputSuffix "m  $esc[0m")
    ) | Join-String
}
''

# Finally, the 24 grays
(232..255) | Join-String -OutputPrefix "$esc[48;5;" -Separator "m  $esc[48;5;" -OutputSuffix "m  $esc[0m"
''
