
import Common
import Control.Exception (bracket)
import Control.Monad     (when)
import System.Exit       (exitFailure)
import System.IO

import Simple
import ParamConv
import Errors

tests :: [TestEnv -> Test]
tests =
    [ TestLabel "Simple"    . testSimpleSelect
    , TestLabel "Simple"    . testSimpleOnePlusOne
    , TestLabel "Simple"    . testSimpleParams
    , TestLabel "ParamConv" . testParamConvInt
    , TestLabel "ParamConv" . testParamConvFloat
    , TestLabel "Errors"    . testErrorsColumns
    , TestLabel "Errors"    . testErrorsInvalidParams
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
