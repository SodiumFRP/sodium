module SemanticTests where

import Semantic


operational_split :: SemanticTest
operational_split = SemanticTest "Operational" "split" $
    LetStream (MkStream [([0],["a", "b"])]) $
    LetStream (Split (StreamRef prev1)) $
    AssertEquals prev1 [([0,0], "a"), ([0,0], "b")] $
    End

operational_defer1 :: SemanticTest
operational_defer1 = SemanticTest "Operational" "defer1" $
    LetStream (MkStream [([0],"a"),([1],"b")]) $
    LetStream (Defer (StreamRef prev1)) $
    AssertEquals prev1 [([0,0], "a"), ([1,0], "b")] $
    End

operational_defer2 :: SemanticTest
operational_defer2 = SemanticTest "Operational" "defer2" $
    LetStream (MkStream [([0],"a"),([1],"b")]) $  -- Defer this one
    LetStream (MkStream [([1],"B")]) $            -- Don't defer this one
    LetStream (OrElse (Defer (StreamRef prev2)) (StreamRef prev1)) $
    AssertEquals prev1 [([0,0], "a"), ([1], "B"), ([1,0], "b")] $
    End

stream_orelse :: SemanticTest
stream_orelse = SemanticTest "Stream" "orElse1" $
    LetStream (MkStream [([0], 0 :: Int), ([2], 2)]) $
    LetStream (MkStream [([1], 10 :: Int), ([2], 20), ([3], 30)]) $
    LetStream (OrElse (StreamRef prev2) (StreamRef prev1)) $
    AssertEquals prev1 [([0],0),([1],10),([2],2),([3],30)] $
    End

operational_deferSimultaneous :: SemanticTest
operational_deferSimultaneous = SemanticTest "Operational" "deferSimultaneous" $
    LetStream (MkStream [([1],"b")]) $
    LetStream (MkStream [([0],"A"),([1],"B")]) $
    LetStream (OrElse (Defer (StreamRef prev2)) (Defer (StreamRef prev1))) $
    AssertEquals prev1 [([0,0], "A"), ([1,0], "b")] $
    End

semanticTests :: [SemanticTest]
semanticTests =
    [operational_split, operational_defer1, operational_defer2,
     stream_orelse, operational_deferSimultaneous]

