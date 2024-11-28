#!/usr/bin/env pwsh

# SPDX-FileCopyrightText: 2023 - 2024 Eli Array Minkoff
#
# SPDX-License-Identifier: GPL-3.0-only

$esc = [char]27
<#
    Return a string of whitespace and ANSI color codes to display a sequence of
    colored squares for each 8-bit color code in $ColorNumbers, ending with a
    sequence to clear color formatting
#>
function Join-ColorCells {
    [CmdletBinding()]
    param (
        [Parameter(Mandatory)]
        [int[]]$ColorNumbers
    )
    $ColorNumbers | Join-String -OutputPrefix "$esc[48;5;" `
                                -Separator "m  $esc[48;5;" `
                                -OutputSuffix "m  $esc[0m"
}

<#
    Return a string of whitespace and ANSI color codes to display a sequence of
    6 colored squares with adjacent 8-bit color codes, starting with the value
    of $StartingNumber
#>
function Join-CubeRowPart {
    [CmdletBinding()]
    param (
        [Parameter(Mandatory)]
        [int]$StartingNumber
    )
    Join-ColorCells -ColorNumbers (($StartingNumber)..($StartingNumber + 5))
}

<#
    Return a string of whitespace and ANSI color codes to display 3 sequences of
    6 colored squares with adjacent 8-bit color codes, starting with the value
    of $RowStartingNumber, $RowStartingNumber + 36, and $RowStartingNumber + 72
#>
function Join-CubeRow {
    [CmdletBinding()]
    param (
        [Parameter(Mandatory)]
        [int]$RowStartingNumber
    )
    (
        $(Join-CubeRowPart -StartingNumber $RowStartingNumber) + "  " `
        + $(Join-CubeRowPart -StartingNumber ($RowStartingNumber + 36)) + "  " `
        + $(Join-CubeRowPart -StartingNumber ($RowStartingNumber + 72))
    )
}

<# Print the first 16 colors - these vary by terminal configuration #>
''
Join-ColorCells -ColorNumbers (0..15)
''

<# Print the 6 sides of the color cube - these are more standardized
but the order is a bit odd, thus the need for the above trickery #>
For ($i = 16; $i -lt 52; $i += 6) {
    Join-CubeRow -RowStartingNumber $i
}
''
For ($i = 124; $i -lt 160; $i += 6) {
    Join-CubeRow -RowStartingNumber $i
}
''

<# Finally, the 24 grays #>
Join-ColorCells -ColorNumbers (232..255)
''
