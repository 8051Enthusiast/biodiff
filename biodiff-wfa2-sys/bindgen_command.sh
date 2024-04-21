#!/bin/sh
bindgen --allowlist-function 'wavefront_.*' --allowlist-var 'wavefront_.*' WFA2-lib/wavefront/wavefront_align.h -o src/bindings.rs -- -IWFA2-lib
