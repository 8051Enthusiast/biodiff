biodiff
=======

Compare binary files using alignment algorithms.

What is this
------------
This is a tool for binary diffing.

The tool is able to show two binary files side by side so that similar places will be at the same position on both sides
and bytes missing from one side are padded.
It uses bio-informatics algorithms from the [`rust-bio`](https://rust-bio.github.io/) library (typically used for DNA sequence alignment) for that.

Usage
-----
Execute `biodiff file_a file_b` in a terminal and you should be dropped into a hex view showing two files side by side.
Initially, the files will not be aligned and displayed without gaps on each side.
By moving the cursor and views to a place where the left side and right side are similar and pressing `F3`, they can be aligned.
This is done block by block in standard configuration, which means that bytes near the cursor are aligned first and the other displayed later.

It is also possible to do global and local alignment (of the whole files at once) by changing the settings using `F4` (be sure to consult the help on the parameters).
Generally, since it takes quadratic time and space, the global/local alignment will not work well for files bigger than 64kB.
There is also a "banded" algorithm which is faster, but slightly less accurate.

It woul

Installation
------------
There should be downloadable binary files for some environments under the releases page.
Alternatively, you can also install this using `cargo` by doing `cargo install biodiff`.

You can also execute directly using code from this repository by executing `cargo run --release -- file_a file_b`.

License
-------
This project is licensed under MIT.