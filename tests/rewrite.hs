import Prelude
import Control.Exception.Assert
import Control.Monad
import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import System.Directory
import System.Exit
import System.FilePath
import System.Posix.Redirect

main :: IO ()
main = byPred assert "false" id False $ do
    defaultMainWithHooksArgs simpleUserHooks
        { buildHook = hook }
        [ "build", "--ghc-option=-ddump-rule-firings" ]
    putStrLn "http://youtu.be/HOLYYYsFqcI"

hook :: PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO ()
hook pd lbi uh bf = do
    forM_ ["hi", "o"] $ \ suf -> removeFile $
        buildDir lbi </> "rewrite" </> "rewrite-tmp" </> "Main" <.> suf
    (err, (out, _)) <- redirectStderr . redirectStdout $
        buildHook simpleUserHooks pd lbi uh bf
    let combined = err ++ out
    unless ("Rule fired: assertMessage" `elem` lines combined) $ do
        putStr combined
        putStrLn "Rule NOT fired: assertMessage"
        exitWith (ExitFailure 1)

