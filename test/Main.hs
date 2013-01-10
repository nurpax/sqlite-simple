
import Common
import Control.Exception (bracket)
import Control.Monad     (when)
import System.Exit       (exitFailure)
import System.IO

import Simple
import ParamConv
import Errors
import Utf8Strings
import UserInstances
import TestImports()
import Fold
import Statement

tests :: [TestEnv -> Test]
tests =
    [ TestLabel "Simple"    . testSimpleSelect
    , TestLabel "Simple"    . testSimpleOnePlusOne
    , TestLabel "Simple"    . testSimpleParams
    , TestLabel "Simple"    . testSimpleTime
    , TestLabel "Simple"    . testSimpleTimeFract
    , TestLabel "ParamConv" . testParamConvInt
    , TestLabel "ParamConv" . testParamConvFloat
    , TestLabel "ParamConv" . testParamConvDateTime
    , TestLabel "ParamConv" . testParamConvBools
    , TestLabel "Errors"    . testErrorsColumns
    , TestLabel "Errors"    . testErrorsInvalidParams
    , TestLabel "Errors"    . testErrorsWithStatement
    , TestLabel "Utf8"      . testUtf8Simplest
    , TestLabel "Utf8"      . testBlobs
    , TestLabel "Instances" . testUserFromField
    , TestLabel "Fold"      . testFolds
    , TestLabel "Statement" . testBind
    , TestLabel "Statement" . testDoubleBind
    ]

-- | Action for connecting to the database that will be used for testing.
--
-- Note that some tests, such as Notify, use multiple connections, and assume
-- that 'testConnect' connects to the same database every time it is called.
testConnect :: IO Connection
testConnect = open ":memory:"

withTestEnv :: (TestEnv -> IO a) -> IO a
withTestEnv cb =
    withConn $ \conn ->
        cb TestEnv
            { conn     = conn
            , withConn = withConn
            }
  where
    withConn = bracket testConnect close

main :: IO ()
main = do
  mapM_ (`hSetBuffering` LineBuffering) [stdout, stderr]
  Counts{cases, tried, errors, failures} <-
    withTestEnv $ \env -> runTestTT $ TestList $ map ($ env) tests
  when (cases /= tried || errors /= 0 || failures /= 0) $ exitFailure
