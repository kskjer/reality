#!/bin/bash
# note: can pass --debug
DFLT=s:/Legend\ of\ Zelda\,\ The\ -\ Ocarina\ of\ Time\ \(U\)\ \(V1.0\)\ \[\!\].z64
R="${ROM:-${DFLT:-default}}"
./mupen-build.sh "$1" && (
	cd ext/mupen64plus-ui-console/projects/VisualStudio2013/Release && RUST_BACKTRACE=1 ./mupen64plus-ui-console.exe --emumode 0 --gfx mupen64plus-video-glide64mk2 "$R" 2>&1
) | less


