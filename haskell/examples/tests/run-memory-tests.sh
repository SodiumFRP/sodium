#!/bin/bash -e
FILES=memory-test-*.hs
for ff in $FILES ; do
    f=${ff%.hs}
    ghc $ff -prof -auto-all
    rm -f $f.done
done
echo running...
for ff in $FILES ; do
    f=${ff%.hs}
    (./$f +RTS -hy ; hp2ps -c $f.hp ; convert -rotate -90 -flatten $f.ps $f.png ; touch $f.done) &
done
for ff in $FILES ; do
    f=${ff%.hs}
    while [[ ! -f "$f.done" ]] ; do
        sleep 0.1
    done
done
echo done
eog *.png
