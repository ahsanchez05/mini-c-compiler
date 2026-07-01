#!/bin/sh
set -eu

if [ "$#" -ne 1 ]; then
    echo "USAGE: $(basename "$0") FILE"
    echo "assembles FILE and runs the resulting binary in an x86 environment"
    exit 1
fi

CONTAINER_RUNTIME="${CONTAINER_RUNTIME:-docker}"
IMAGE_X86_SHELL="${IMAGE_X86_SHELL:-mini-c-compiler-x86:latest}"

file="$1"

if [ ! -f "$file" ]; then
    echo "Error: file not found: $file" >&2
    exit 1
fi

srcdir="$(cd "$(dirname "$file")" && pwd -P)"
srcfile="$(basename "$file")"
"$CONTAINER_RUNTIME" run --rm -it \
    --platform linux/amd64 \
    -v "$srcdir:/workspace:ro" \
    "$IMAGE_X86_SHELL" \
    bash -c "nasm -felf64 -o /tmp/obj.o /workspace/'$srcfile' \
          && gcc -z noexecstack -no-pie -o /tmp/a.out /tmp/obj.o \
          && /tmp/a.out"
