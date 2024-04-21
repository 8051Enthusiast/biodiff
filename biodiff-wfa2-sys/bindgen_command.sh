#!/bin/sh
bindgen --allowlist-function 'wavefront_.*' \
        --allowlist-var 'wavefront_.*' \
        --blocklist-type 'timespec' \
        --blocklist-type 'FILE' \
        --blocklist-type '_IO_.+' \
        --raw-line '#![allow(non_upper_case_globals,non_snake_case,non_camel_case_types,unused)]' \
        --raw-line 'pub type FILE = libc::FILE;' \
        --raw-line 'pub type timespec = libc::timespec;' \
        --with-derive-custom 'profiler_timer_t=Copy,Clone' \
        --with-derive-custom 'alignment_system_t=Copy,Clone' \
        --with-derive-custom 'wavefront_aligner_attr_t=Copy,Clone' \
        WFA2-lib/wavefront/wavefront_align.h -o src/lib.rs \
        -- -IWFA2-lib
