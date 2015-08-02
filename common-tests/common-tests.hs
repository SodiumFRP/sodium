import Test
import Suite
import qualified Java
import System.IO

main = do
    writeFile "../java/tests/java8/sodium/CommonTester.java" (Java.format suite)

