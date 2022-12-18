#!/usr/bin/env python3

# Print the first 16 colors - these vary by terminal configuration
print()
print(*[f'\x1b[48;5;{i}m  ' for i in range(16)], sep='', end='\x1b[0m\n')
print()

# Print the 6 sides of the color cube - these are more standardized
# but the order is a bit odd, thus the need for this trickery
for i in range(16, 52, 6):
    row_a = [i+ii for ii in range(6)]
    row_b = [i+ii for ii in range(36, 42)]
    row_c = [i+ii for ii in range(72, 78)]

    print(*[f'\x1b[48;5;{ii}m  ' for ii in row_a], sep='', end='\x1b[0m  ')
    print(*[f'\x1b[48;5;{ii}m  ' for ii in row_b], sep='', end='\x1b[0m  ')
    print(*[f'\x1b[48;5;{ii}m  ' for ii in row_c], sep='', end='\x1b[0m\n')
print()
for i in range(124, 160, 6):
    row_a = [i+ii for ii in range(6)]
    row_b = [i+ii for ii in range(36, 42)]
    row_c = [i+ii for ii in range(72, 78)]

    print(*[f'\x1b[48;5;{ii}m  ' for ii in row_a], sep='', end='\x1b[0m  ')
    print(*[f'\x1b[48;5;{ii}m  ' for ii in row_b], sep='', end='\x1b[0m  ')
    print(*[f'\x1b[48;5;{ii}m  ' for ii in row_c], sep='', end='\x1b[0m\n')
print()
# Finally, the 24 grays
print(*[f'\x1b[48;5;{i}m  ' for i in range(232, 256)], sep='', end='\x1b[0m\n')
print()
