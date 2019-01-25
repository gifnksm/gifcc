#!/bin/bash

index=0

compiletest() {
    filename="${1}"
    name=$(basename "${filename%%.c}")

    for kind in token ast; do
        target/gifcc --output "${kind}" "${filename}" > target/test/${name}."${kind}"
        if [ "$?" -ne 0 ]; then
            echo "test [${name}]: gifcc(${kind}) failed"
            exit 1
        fi
    done

    target/gifcc "${filename}" > target/test/${name}.s
    if [ "$?" -ne 0 ]; then
        echo "test [${name}]: gifcc(asm) failed"
        exit 1
    fi

    gcc -o target/test/${name} target/test/${name}.s
    if [ "$?" -ne 0 ]; then
        echo "test [${name}]: gcc failed"
        exit 1
    fi
    target/test/${name}
    if [ "$?" -ne 0 ]; then
        echo "test [${name}]: exec failed"
        exit 1
    fi

    echo "test [${name}]: OK"
}

mkdir -p target/test
for src in test/*.c; do
    compiletest "${src}"
done

