#!/bin/sh
bindgen --allowlist-function 'wavefront_.*' \
        --allowlist-var 'wavefront_.*' \
        --blocklist-type 'timespec' \
        --blocklist-type 'FILE' \
        --blocklist-type '_IO_.+' \
        --raw-line '#![allow(non_upper_case_globals,non_snake_case,non_camel_case_types,unused)]' \
        --raw-line 'pub type FILE = libc::FILE;' \
        --raw-line 'pub type timespec = libc::timespec;' \
        WFA2-lib/wavefront/wavefront_align.h -o src/lib.rs \
        -- -IWFA2-lib
