{-# LANGUAGE CPP #-}

import Prelude
#if !MIN_VERSION_base(4,6,0)
    hiding (catch)
#endif
import Control.Exception (IOException, catch)
import Control.Exception.Assert
import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.Monoid
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
    -- more reliable way to force a rebuild?
    removeDirectoryRecursive (buildDir lbi </> "rewrite" </> "rewrite-tmp")
        `catch` \ e -> return () `const` (e :: IOException)

    -- some versions of GHC prints to stderr
    (err, (out, _)) <- redirectStderr . redirectStdout $
        buildHook simpleUserHooks pd lbi uh bf
    let combined = BS.lines err <> BS.lines out
    unless (BS.pack "Rule fired: assertMessage" `elem` combined) $ do
        mapM_ BS.putStrLn combined
        putStrLn "Rule NOT fired: assertMessage"
        exitWith (ExitFailure 1)

