biodiff
=======

[![Crates.io](https://img.shields.io/crates/v/biodiff)](https://crates.io/crates/biodiff)
[![Packaging status](https://repology.org/badge/tiny-repos/biodiff.svg)](https://repology.org/project/biodiff/versions)

Compare binary files using alignment algorithms.

![Terminal screenshot of biodiff. One can see two files displayed in hex above each other with an ascii column. There are areas that are skipped in one file and displayed in green in the other one. Common bytes are displayed as white and differing ones (aside from missing ones) as red.](https://user-images.githubusercontent.com/54916925/123559715-fda1aa80-d79d-11eb-8bcd-90316e388e48.png)

What is this
------------
This is a tool for binary diffing.

The tool is able to show two binary files side by side so that similar places will be at the same position on both sides
and bytes missing from one side are padded.
It uses bio-informatics algorithms from the [`rust-bio`](https://rust-bio.github.io/) library (typically used for DNA sequence alignment) for that.
The dialog boxes for configuration are done using [`cursive`](https://github.com/gyscos/cursive).

Features
--------
* Unaligned view for moving both sides independently as contiguous byte segments
* Aligned view for comparing corresponding bytes of both files
* Many configurable byte representations (bases 2, 8, 10, 16; mixed ascii/hex, braille, roman numerals)
* Right-to-left mode, horizontal and vertical split, ascii and bar column
* Configurable bytes per row, adjustable by pressing `[`, `]`, `0`
* Automatic determination of width by finding repetitions in visible/selected bytes by pressing '='
* Search using text, regex and hexagex

Usage
-----
Execute `biodiff file_a file_b` in a terminal and you should be dropped into a hex view showing two files side by side.
Initially, the files will not be aligned and displayed without gaps on each side.
By moving the cursor and views to a place where the left side and right side are similar and pressing `F3` (or `3`), they can be aligned.
This is done block by block in standard configuration, which means that bytes near the cursor are aligned first and further aligned blocks are displayed later on both sides.

It is also possible to do global and local alignment (of the whole files at once) by changing the settings using `F4` (be sure to consult the help on the parameters).
Generally, since it takes quadratic time and space, the global/local alignment will not work well for files bigger than 64kB.
There is also a "banded" algorithm which is faster, but slightly less accurate.

You can also select a region on one file and by pressing F3 the aligning algorithm will do a glocal alignment using the selected bytes as a pattern to find the corresponding bytes on the other file.

Installation
------------
If you're lucky, there will be a package available in your primary package manager, see the [repology page](https://repology.org/project/biodiff/versions).
There should be downloadable binary files for some environments under the [releases page](https://github.com/8051Enthusiast/biodiff/releases).
Alternatively, you can also install this using `cargo` by doing `cargo install biodiff`.

You can also execute directly using code from this repository by executing `cargo run --release -- file_a file_b`.
Note that configuration files are only guaranteed to stay compatible between tagged releases.

By default, settings are stored in a [platform-specific user directory](https://github.com/dirs-dev/dirs-rs#Features).
To use a custom settings directory, set the `BIODIFF_CONFIG_DIR` environment variable to the desired directory path before running `biodiff`.
If the directory doesn't exist, it will be automatically created.

License
-------
This project is licensed under the MIT license.
