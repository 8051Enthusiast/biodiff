Hexagex
-------

Hexagexes are regexes using hexadecimals and bits for matching binary data.

The syntax is mostly the same as in the regex crate, but the literals have been replaced:
 * `0`-`9`, `a`-`f`: 4-bits sequences interpreted as base 16 numbers
 * `.`: 4-bit wildcard
 * `O`, `I`: 1-bit value corresponding to 0 and 1
 * `_`: 1-bit wildcard
 * `^`, `$`: these match the start and end of the text, not of the line
 * `\t`: this is an escape to interprete the content immediately after it into a non-hexagex regular regex

A hexagex binary can be compiled with `cargo build` that allows one to find matches with `hexagex [exp] [file]`.

Examples
========

 * `01020304`: match the bytes [1, 2, 3, 4]
 * `([2-7].){5,}00`: match bytes from `0x20` to `0x7f` of length at least 5 with a null terminator
 * `I_I_I_I_|_O_O_O_O`: match bytes that have all even bits set or odd bits unset
 * `[0-69CEF][6-F]|[0EF]0|[23EF]2|[0-37-F]3|[018ACDEF]4|D[67]|(.1|[78AD]I___|[0-69CEF]5|[2-79]4|[4-8A-D][02]|[78A][67]|92)..|(B[4-F]|[01]2|[1239]0|[456]3|[78D]5)....`: match 8051 instructions
