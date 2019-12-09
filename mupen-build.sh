#!/bin/bash
export RUSTFLAGS="-C target-cpu=native"
FLAG=`if [[ "$1" == "--debug" ]]; then echo ''; else echo '--release'; fi`;
MODE=`if [[ "$1" == "--debug" ]]; then echo 'debug'; else echo 'release'; fi`;
cargo build $FLAG -p reality-mupen --target i686-pc-windows-msvc && cp -v ./target/i686-pc-windows-msvc/$MODE/reality_mupen.dll ext/mupen64plus-ui-console/projects/VisualStudio2013/Release/reality-mupen.dll
