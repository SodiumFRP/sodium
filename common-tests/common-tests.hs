import OperationalTests
import Operational (fromSemantic)
import SemanticTests
import qualified Java
import System.IO

main = do
    let tests = operationalTests ++ map fromSemantic semanticTests
    generate "../java/tests/java8/sodium/CommonTester.java" (Java.format tests)

generate :: FilePath -> String -> IO ()
generate fn text = do
    writeFile fn text
    putStrLn $ "generated "++fn 

