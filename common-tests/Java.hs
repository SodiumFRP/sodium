module Java (format) where

import Operational
import Type
import Warning
import Control.Monad
import Control.Monad.Trans.Writer

format :: [Test] -> String
format tests = execWriter $ do
    forM_ warning $ \w -> do
        tell "// "
        tell w
        tell "\n"
    tell "\n"
    tell "package nz.sodium;\n\
         \\n\
         \import java.util.ArrayList;\n\
         \import java.util.Arrays;\n\
         \import java.util.List;\n\
         \import java.util.Optional;\n\
         \\n\
         \import junit.framework.TestCase;\n\
         \\n\
         \public class TestCommon extends TestCase {\n"
    forM_ tests $ \(Test group name sts) -> do
        tell $ "\n  public void test_"++group++"_"++name++"() {\n"
        forM_ sts $ \st -> case st of
            Transactional rs [] -> do
                tell $ "    Transaction.runVoid(() -> {\n"
                mapM_ (reactive "      ") rs
                tell $ "    });\n"
            Transactional rs [(t, Var vi, Var vo)] -> do
                tell "    "
                ptype t
                tell " "
                tell vo
                tell " = "
                tell "Transaction.run(() -> {\n"
                mapM_ (reactive "      ") rs
                tell "      return "
                tell vi
                tell ";\n"
                tell "    });\n"
            NewList ty (Var v) -> do
                tell $ "    List<"
                ptype ty
                tell "> "
                tell v
                tell " = new ArrayList<>();\n"
            AssertEqual vl (Var vr) -> do
                tell "    assertEquals("
                literal vl
                tell ","
                tell vr
                tell ");\n"
            Unlisten (Var vr) -> do
                tell "    "
                tell vr
                tell ".unlisten();\n"
        tell "  }\n"
    tell "}\n"

ptype (StreamT t) = tell "Stream<" >> ptype t >> tell ">"
ptype (StreamSinkT t) = tell "StreamSink<" >> ptype t >> tell ">"
ptype StringT = tell "String"
ptype IntT = tell "Integer"
ptype (ListT ty) = tell "List<" >> ptype ty >> tell ">"
ptype ListenerT = tell "Listener"

reactive indent (NewStream ty (Var vr)) = do
    tell indent
    tell "StreamSink<"
    ptype ty
    tell "> "
    tell vr
    tell " = new StreamSink();\n"
reactive indent (Assign ty (Var vr) expr) = do
    tell indent
    ptype ty
    tell " "
    tell vr
    tell " = "
    expression expr
    tell ";\n"
reactive indent (Send (Var vr) l) = tell (indent++vr++".send(") >> literal l >> tell ");\n"
reactive indent (Listen (Var v1) (Var v2) (ty, Var v3) hs) = do
    tell indent
    tell "Listener "
    tell v1
    tell " = "
    tell v2
    tell ".listen(("
    ptype ty
    tell " "
    tell v3
    tell ") -> {\n"
    mapM_ handle hs
    tell indent
    tell "});\n"

expression (ExprVar (Var vr)) = tell vr
expression (Split expr) = do
    tell "Operational.split("
    expression expr
    tell ")"
expression (Defer expr) = do
    tell "Operational.defer("
    expression expr
    tell ")"
expression (OrElse e1 e2) = do
    expression e1
    tell ".orElse("
    expression e2
    tell ")"

handle (AppendList (Var vr) vl) = do
    tell "        "
    tell vr
    tell ".add("
    value vl
    tell ");\n"

value :: Val -> Writer String ()
value (V (Var vr)) = tell vr
value (Lit l) = literal l

literal (StringL s) = tell $ "\"" ++ s ++ "\""
literal (IntL i) = tell $ show i
literal (ListL ty vs) = do
    tell "Arrays.<"
    ptype ty
    tell ">asList("
    case vs of
        [] -> return ()
        (a:as) -> do
            literal a
            forM_ as $ \a_ -> do
                tell ","
                literal a_
    tell ")"

