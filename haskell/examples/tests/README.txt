Run each memory test like this:

    ghc memory-test-1.hs -prof -auto-all
    ./memory-test-1 +RTS -hc
    (interrupt it after a few seconds)
    hp2ps memory-test-1.hp
    evince memory-test-1.ps

Make sure it isn't leaking memory.

----

Another important test is to uncomment 'forever' in unit-tests.hs and run it that
way, making sure that you don't get occasional failures. This makes sure we don't
drop the ball in the memory management, which is a little tricky.

    main = forever $ runTestTT tests

