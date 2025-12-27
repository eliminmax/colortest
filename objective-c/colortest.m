/* SPDX-FileCopyrightText: 2022 - 2024 Eli Array Minkoff
 *
 * SPDX-License-Identifier: GPL-3.0-only */

/* The Objective-C Foundation library is needed for anything useful that is
 * specific to Objective C, and it does not provide anywhere string type, which
 * is called NSString, has no simple way to write to stdout without just using
 * stdio.h anyway. Either I just copy the C implementation, which feels lazy, or
 * I acutally use the NSString datatype and create a wrapper class to write to
 * stdout, which is both uglier and less efficient.
 *
 * Anyway, the following has been tested for GNUstep only. */
#import <Foundation/Foundation.h>

@interface stdoutWriter : NSObject {
    NSFileHandle *stdoutHandle;
}
- (id)init;
- (void)writeToStdout:(NSString *)str;
@end

@implementation stdoutWriter

- (id)init {
    stdoutHandle = [NSFileHandle fileHandleWithStandardOutput];
    return self;
}

- (void)writeToStdout:(NSString *)str {
    [stdoutHandle writeData:[str dataUsingEncoding:NSUTF8StringEncoding]];
}

@end

static stdoutWriter *writer;

static void colorCell(unsigned short n) {
    [writer writeToStdout:[NSString stringWithFormat:@"\x1b[48;5;%hum  ", n]];
}

static void cubeRowPart(unsigned short n) {
    unsigned short i;
    for (i = n; i < n + 6; i++) colorCell(i);
}

static void cubeRow(unsigned short n) {
    cubeRowPart(n);
    [writer writeToStdout:@"\x1b[0m  "];
    cubeRowPart(n + 36);
    [writer writeToStdout:@"\x1b[0m  "];
    cubeRowPart(n + 72);
    [writer writeToStdout:@"\x1b[0m\n"];
}

int main(void) {
    /* Set up allocation pool for garbage collection */
    unsigned short i;
    NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
    writer = [[stdoutWriter alloc] init];
    /* Print the first 16 colors - these vary by terminal configuration */
    [writer writeToStdout:@"\n"];
    for (i = 0; i < 16; i++) colorCell(i);
    [writer writeToStdout:@"\x1b[0m\n\n"];

    /* Print the 6 sides of the color cube - these are more standardized,
     * but the order is a bit odd, thus the need for the above trickery */
    for (i = 16; i < 52; i += 6) cubeRow(i);
    [writer writeToStdout:@"\n"];
    for (i = 124; i < 160; i += 6) cubeRow(i);
    [writer writeToStdout:@"\n"];

    /* Finally, the 24 grays */
    for (i = 232; i < 256; i++) colorCell(i);
    [writer writeToStdout:@"\x1b[0m\n\n"];

    /* drain allocation pool */
    [pool drain];
    return 0;
}
