#!/bin/sh
set -eu

if [ "$#" -ne 1 ]; then
    echo "USAGE: $(basename "$0") FILE"
    echo "assembles FILE and runs the resulting binary in an x86 environment"
    exit 1
fi

CONTAINER_RUNTIME="${CONTAINER_RUNTIME:-docker}"
IMAGE_MINI_SHELL="${IMAGE_MINI_SHELL:-mini-c-compiler-x86:latest}"

file="$1"

if [ ! -f "$file" ]; then
    echo "Error: file not found: $file" >&2
    exit 1
fi

if [ "${ID2202_INSIDE_SHELL:-}" ]; then
    if [ "$(uname -m)" != "x86_64" ]; then
        echo "Error: This script must be run outside the dev-shell on non-x86 hardware."
        exit 1
    fi
    tmpdir="$(mktemp -d)"
    nasm -felf64 -o "$tmpdir/obj.o" "$file"
    gcc -z noexecstack -no-pie -o "$tmpdir/a.out" "$tmpdir/obj.o"
    "$tmpdir/a.out"
    rm -rf "$tmpdir"
else
    srcdir="$(cd "$(dirname "$file")" && pwd -P)"
    srcfile="$(basename "$file")"
    "$CONTAINER_RUNTIME" run --rm -it \
        --platform linux/amd64 \
        -v "$srcdir:/id2202:ro" \
        "$IMAGE_MINI_SHELL" \
        bash -c "nasm -felf64 -o /root/obj.o /id2202/'$srcfile' \
              && gcc -z noexecstack -no-pie -o /root/a.out /root/obj.o \
              && /root/a.out"
fi
