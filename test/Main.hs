import Common
import Control.Exception (bracket)
import Control.Monad     (when)
import System.Exit       (exitFailure)
import System.IO

import Debug
import DirectSqlite
import Errors
import Fold
import ParamConv
import QQ
import Simple
import Function
import Statement
import TestImports()
import TestImports
import UserInstances
import Utf8Strings

tests :: [TestEnv -> Test]
tests =
    [ TestLabel "Simple"    . testSimpleSelect
    , TestLabel "Simple"    . testSimpleOnePlusOne
    , TestLabel "Simple"    . testSimpleParams
    , TestLabel "Simple"    . testSimpleTime
    , TestLabel "Simple"    . testSimpleTimeFract
    , TestLabel "Simple"    . testSimpleInsertId
    , TestLabel "Simple"    . testSimpleMultiInsert
    , TestLabel "Simple"    . testSimpleUTCTime
    , TestLabel "Simple"    . testSimpleUTCTimeTZ
    , TestLabel "Simple"    . testSimpleUTCTimeParams
    , TestLabel "Simple"    . testSimpleQueryCov
    , TestLabel "Simple"    . testSimpleStrings
    , TestLabel "Simple"    . testSimpleChanges
    , TestLabel "ParamConv" . testParamConvNull
    , TestLabel "ParamConv" . testParamConvInt
    , TestLabel "ParamConv" . testParamConvIntWidths
    , TestLabel "ParamConv" . testParamConvIntWidthsFromField
    , TestLabel "ParamConv" . testParamConvFloat
    , TestLabel "ParamConv" . testParamConvDateTime
    , TestLabel "ParamConv" . testParamConvBools
    , TestLabel "ParamConv" . testParamConvToRow
    , TestLabel "ParamConv" . testParamConvFromRow
    , TestLabel "ParamConv" . testParamConvComposite
    , TestLabel "ParamConv" . testParamNamed
    , TestLabel "Errors"    . testErrorsColumns
    , TestLabel "Errors"    . testErrorsInvalidParams
    , TestLabel "Errors"    . testErrorsInvalidNamedParams
    , TestLabel "Errors"    . testErrorsWithStatement
    , TestLabel "Errors"    . testErrorsColumnName
    , TestLabel "Errors"    . testErrorsTransaction
    , TestLabel "Errors"    . testErrorsImmediateTransaction
    , TestLabel "Errors"    . testErrorsExclusiveTransaction
    , TestLabel "Errors"    . testErrorsSavepoint
    , TestLabel "Utf8"      . testUtf8Simplest
    , TestLabel "Utf8"      . testBlobs
    , TestLabel "Instances" . testUserFromField
    , TestLabel "Instances" . testSQLDataFromField
    , TestLabel "Fold"      . testFolds
    , TestLabel "Statement" . testBind
    , TestLabel "Statement" . testDoubleBind
    , TestLabel "Statement" . testPreparedStatements
    , TestLabel "Statement" . testPreparedStatementsColumnCount
    , TestLabel "Debug"     . testDebugTracing
    , TestLabel "Functions" . testExternalAddition
    , TestLabel "Direct"    . testDirectSqlite
    , TestLabel "Imports"   . testImports
    , TestLabel "QQ"        . testSimpleQQ
    , TestLabel "QQ"        . testMultiLinedQQ
    ]

-- | Action for connecting to the database that will be used for testing.
--
-- Note that some tests, such as Notify, use multiple connections, and assume
-- that 'testConnect' connects to the same database every time it is called.
testConnect :: IO Connection
testConnect = open ":memory:"

withTestEnv :: (TestEnv -> IO a) -> IO a
withTestEnv cb =
  withConn $ \conn -> cb TestEnv { conn = conn }
  where
    withConn = bracket testConnect close

main :: IO ()
main = do
  mapM_ (`hSetBuffering` LineBuffering) [stdout, stderr]
  Counts{cases, tried, errors, failures} <-
    withTestEnv $ \env -> runTestTT $ TestList $ map ($ env) tests
  when (cases /= tried || errors /= 0 || failures /= 0) $ exitFailure
