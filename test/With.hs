{-# LANGUAGE DerivingVia, RankNTypes, ScopedTypeVariables, TypeApplications #-}

module With (
    testsWith
  ) where

import           Control.Exception (Exception, bracket_, catch)
import           Control.Monad (replicateM, when)
import           Data.List (intercalate)
import           Data.Semigroup (Sum(..))
import           Data.String (fromString)
import qualified Data.Text as T
import           Data.Typeable (Typeable)

import           Common
import           Database.SQLite.Simple.FromRow (RowParser, numFieldsRemaining)

newtype IntWrapper = IntWrapper Int
  deriving (Eq, Show)
  deriving (Semigroup, Monoid) via Sum Int

data TestRowError
  = WrongArity
  | TooManyNullColumns
  deriving (Eq, Show, Typeable)

instance Exception TestRowError

data AccessMode = QueryMode | FoldMode
  deriving Show

data BindingMode = Underscore | Positional | Named
  deriving Show

data Table
  = ReversePairTable
  | ConcatText1Table
  | ConcatText2Table
  | ConcatText3Table
  | ConcatText4Table
  deriving (Eq, Show)

data PathKind = Happy | Unhappy
  deriving (Eq, Show)

-- TableDef is the declarative per-table metadata. It keeps the fixture rows,
-- parser, and binding-mode-specific expected happy results in one place.
data TableDef good bad r e = TableDef
  { tableDefName      :: String
  , tableDefColumns   :: [ColumnDef]
  , tableDefParser    :: RowParser r
  , tableDefGoodRows  :: [good]
  , tableDefBadRows   :: [bad]
  , tableDefHappyRowsUnsuffixed :: [r]
  , tableDefHappyRowsSuffixed   :: [r]
  , tableDefError               :: e
  }

data ColumnDef = ColumnDef
  { columnDefName :: String
  , columnDefType :: String
  }

-- TableSpec is the fully compiled form used by the test matrix:
-- setup/teardown, bad-row injection, and the final assertion.
data TableSpec = TableSpec
  { withTable       :: forall a. Connection -> IO a -> IO a
  , injectBadRow    :: Connection -> IO ()
  , assertFor       :: AccessMode -> BindingMode -> PathKind -> Connection -> Assertion
  }

-- Cover the product of access mode, binding mode, table shape, and path kind.
-- ConcatText4Table is only meaningful on the unhappy path.
testsWith :: [TestEnv -> Test]
testsWith =
  [ mkTest accessMode bindingMode table pathKind
  | accessMode <- [QueryMode, FoldMode]
  , bindingMode <- [Underscore, Positional, Named]
  , table <- [ReversePairTable, ConcatText1Table, ConcatText2Table, ConcatText3Table, ConcatText4Table]
  , pathKind <- [Happy, Unhappy]
  , table /= ConcatText4Table || pathKind == Unhappy
  ]

assertExceptionThrown :: forall e a. (Eq e, Show e, Exception e) => IO a -> e -> Assertion
assertExceptionThrown action expectedError =
  catch
    (action >> assertFailure ("Expected error: " ++ show expectedError ++ ", but nothing was thrown"))
    (\(e :: e) -> assertEqual "assertExceptionThrown" expectedError e)

-- A 2-column parser that deliberately swaps the SQL column order.
reversePairRowParser :: RowParser (IntWrapper, T.Text)
reversePairRowParser = (\txt i -> (IntWrapper i, txt)) <$> field <*> field

-- A row-level parser with cross-column validation. It accepts only 2-3
-- columns, rejects rows with too many NULLs, then reverses and concatenates the
-- rendered column payloads.
concatTextRowParser :: RowParser T.Text
concatTextRowParser = do
  n <- numFieldsRemaining
  cols <- replicateM n (field :: RowParser SQLData)
  if n /= 2 && n /= 3 then
    returnRowError WrongArity
  else if length (filter isNull cols) > 1 then
    returnRowError TooManyNullColumns
  else
    return . T.concat . reverse $ map payload cols
  where
    isNull SQLNull = True
    isNull _       = False

    payload (SQLText t)    = t
    payload SQLNull        = T.empty
    payload (SQLInteger i) = T.pack (show i)
    payload (SQLFloat d)   = T.pack (show d)
    payload (SQLBlob b)    = T.pack (show b)

expectedTypeError :: ResultError
expectedTypeError =
  ConversionFailed
    { errSQLType = "NULL"
    , errHaskellType = "Text"
    , errMessage = "need a text"
    }

mkTest :: AccessMode -> BindingMode -> Table -> PathKind -> TestEnv -> Test
mkTest accessMode bindingMode table pathKind TestEnv{conn} =
  TestLabel label . TestCase $
    withTable tableSpec conn $ do
      when (pathKind == Unhappy) $
        injectBadRow tableSpec conn
      assertFor tableSpec accessMode bindingMode pathKind conn
  where
    tableSpec = compileTable table
    label =
      "With/"
      ++ show accessMode
      ++ "/"
      ++ functionLabel accessMode bindingMode
      ++ "/"
      ++ show table
      ++ "/"
      ++ show pathKind

compileTable :: Table -> TableSpec
compileTable ReversePairTable =
  compileTableDef TableDef
    { tableDefName = "with_reverse_pair"
    , tableDefColumns = [ColumnDef "t1" "TEXT", ColumnDef "i" "INTEGER"]
    , tableDefParser = reversePairRowParser
    , tableDefGoodRows =
        ([ ("a", 1)
         , ("b", 2)
         ] :: [(T.Text, Int)])
    , tableDefBadRows =
        ([(Nothing @T.Text, Just @Int 8)] :: [(Maybe T.Text, Maybe Int)])
    , tableDefHappyRowsUnsuffixed =
        ([ (IntWrapper 1, "a")
         , (IntWrapper 2, "b")
         ] :: [(IntWrapper, T.Text)])
    , tableDefHappyRowsSuffixed =
        ([ (IntWrapper 1, "az")
         , (IntWrapper 2, "bz")
         ] :: [(IntWrapper, T.Text)])
    , tableDefError = expectedTypeError
    }
compileTable ConcatText1Table =
  compileTableDef TableDef
    { tableDefName = "with_concat_text1"
    , tableDefColumns = [ColumnDef "t1" "TEXT", ColumnDef "i" "INTEGER"]
    , tableDefParser = concatTextRowParser
    , tableDefGoodRows =
        ([ (Just "c", Just 3)
         , (Nothing, Just 4)
         , (Just "d", Nothing)
         ] :: [(Maybe T.Text, Maybe Int)])
    , tableDefBadRows =
        ([(Nothing, Nothing)] :: [(Maybe T.Text, Maybe Int)])
    , tableDefHappyRowsUnsuffixed = (["4", "3c", "d"] :: [T.Text])
    , tableDefHappyRowsSuffixed = (["4", "3cz", "dz"] :: [T.Text])
    , tableDefError = TooManyNullColumns
    }
compileTable ConcatText2Table =
  compileTableDef TableDef
    { tableDefName = "with_concat_text2"
    , tableDefColumns = [ColumnDef "t1" "TEXT", ColumnDef "t2" "TEXT"]
    , tableDefParser = concatTextRowParser
    , tableDefGoodRows =
        ([ (Just "e", Just "f")
         , (Nothing, Just "g")
         , (Just "h", Nothing)
         ] :: [(Maybe T.Text, Maybe T.Text)])
    , tableDefBadRows =
        ([(Nothing, Nothing)] :: [(Maybe T.Text, Maybe T.Text)])
    , tableDefHappyRowsUnsuffixed = (["g", "fe", "h"] :: [T.Text])
    , tableDefHappyRowsSuffixed = (["g", "fez", "hz"] :: [T.Text])
    , tableDefError = TooManyNullColumns
    }
compileTable ConcatText3Table =
  compileTableDef TableDef
    { tableDefName = "with_concat_text3"
    , tableDefColumns = [ColumnDef "t1" "TEXT", ColumnDef "i" "INTEGER", ColumnDef "t2" "TEXT"]
    , tableDefParser = concatTextRowParser
    , tableDefGoodRows =
        ([ (Just "i", Just 5, Just "j")
         , (Nothing, Just 6, Just "k")
         , (Just "l", Nothing, Just "m")
         , (Just "n", Just 7, Nothing)
         ] :: [(Maybe T.Text, Maybe Int, Maybe T.Text)])
    , tableDefBadRows =
        ([(Nothing, Nothing, Just "o")] :: [(Maybe T.Text, Maybe Int, Maybe T.Text)])
    , tableDefHappyRowsUnsuffixed = (["k6", "j5i", "ml", "7n"] :: [T.Text])
    , tableDefHappyRowsSuffixed = (["k6", "j5iz", "mlz", "7nz"] :: [T.Text])
    , tableDefError = TooManyNullColumns
    }
compileTable ConcatText4Table =
  compileTableDef TableDef
    { tableDefName = "with_concat_text4"
    , tableDefColumns =
        [ ColumnDef "t1" "TEXT"
        , ColumnDef "t2" "TEXT"
        , ColumnDef "t3" "TEXT"
        , ColumnDef "t4" "TEXT"
        ]
    , tableDefParser = concatTextRowParser
    , tableDefGoodRows =
        ([("p", "q", "r", "s")] :: [(T.Text, T.Text, T.Text, T.Text)])
    , tableDefBadRows = ([] :: [(T.Text, T.Text, T.Text, T.Text)])
    , tableDefHappyRowsUnsuffixed = ([] :: [T.Text])
    , tableDefHappyRowsSuffixed = ([] :: [T.Text])
    , tableDefError = WrongArity
    }

-- Turn declarative table metadata into executable test behavior.
compileTableDef
  :: (ToRow good, ToRow bad, Eq r, Show r, Monoid r, Eq e, Show e, Exception e)
  => TableDef good bad r e
  -> TableSpec
compileTableDef TableDef
  { tableDefName
  , tableDefColumns
  , tableDefParser
  , tableDefGoodRows
  , tableDefBadRows
  , tableDefHappyRowsUnsuffixed
  , tableDefHappyRowsSuffixed
  , tableDefError
  } =
  TableSpec
    { withTable = \conn ->
        bracket_
          (do
            execute_ conn (createTableSql tableDefName tableDefColumns)
            executeMany conn (insertSql tableDefName tableDefColumns) tableDefGoodRows)
          (execute_ conn ("DROP TABLE " <> fromString tableDefName))
    , injectBadRow = \conn ->
        executeMany conn (insertSql tableDefName tableDefColumns) tableDefBadRows
    , assertFor =
        mkAssertFor
          tableDefParser
          tableDefName
          tableDefColumns
          tableDefHappyRowsUnsuffixed
          tableDefHappyRowsSuffixed
          tableDefError
    }

-- The same table data is checked through both query-like and fold-like APIs.
-- Underscore uses the unsuffixed expectations; positional and named use the
-- suffixed expectations because they bind the same "z" suffix parameter.
mkAssertFor
  :: (Eq r, Show r, Monoid r, Eq e, Show e, Exception e)
  => RowParser r
  -> String
  -> [ColumnDef]
  -> [r]
  -> [r]
  -> e
  -> AccessMode -> BindingMode -> PathKind -> Connection -> Assertion
mkAssertFor parser tableName columns happyRowsUnsuffixed happyRowsSuffixed unhappyError accessMode bindingMode pathKind conn =
  case (accessMode, pathKind) of
    (QueryMode, Happy) -> do
      actual <- runQuerySpec parser tableName columns bindingMode conn
      expectedHappyRows @=? actual
    (QueryMode, Unhappy) ->
      assertExceptionThrown
        (runQuerySpec parser tableName columns bindingMode conn)
        unhappyError
    (FoldMode, Happy) -> do
      actual <- runFoldSpec parser tableName columns bindingMode conn
      mconcat expectedHappyRows @=? actual
    (FoldMode, Unhappy) ->
      assertExceptionThrown
        (runFoldSpec parser tableName columns bindingMode conn)
        unhappyError
  where
    expectedHappyRows =
      case bindingMode of
        Underscore -> happyRowsUnsuffixed
        Positional -> happyRowsSuffixed
        Named      -> happyRowsSuffixed

-- BindingMode changes how the first selected column is formed and how the
-- suffix parameter is passed to the library call.
runQuerySpec :: RowParser r -> String -> [ColumnDef] -> BindingMode -> Connection -> IO [r]
runQuerySpec parser tableName columns bindingMode conn =
  case bindingMode of
    Underscore ->
      queryWith_ parser conn (selectSql Underscore tableName columns)
    Positional ->
      queryWith parser conn (selectSql Positional tableName columns) (Only @T.Text "z")
    Named ->
      queryNamedWith parser conn (selectSql Named tableName columns) [":suffix" := ("z" :: T.Text)]

runFoldSpec :: Monoid r => RowParser r -> String -> [ColumnDef] -> BindingMode -> Connection -> IO r
runFoldSpec parser tableName columns bindingMode conn =
  case bindingMode of
    Underscore ->
      foldWith_ parser conn (selectSql Underscore tableName columns) mempty appendMonoid
    Positional ->
      foldWith parser conn (selectSql Positional tableName columns) (Only @T.Text "z") mempty appendMonoid
    Named ->
      foldNamedWith parser conn (selectSql Named tableName columns) [":suffix" := ("z" :: T.Text)] mempty appendMonoid
  where
    appendMonoid acc row = return (acc <> row)

createTableSql :: String -> [ColumnDef] -> Query
createTableSql name columns =
  fromString
    ( "CREATE TABLE "
        ++ name
        ++ " ("
        ++ intercalate ", " [columnDefName ++ " " ++ columnDefType | ColumnDef{columnDefName, columnDefType} <- columns]
        ++ ")"
    )

insertSql :: String -> [ColumnDef] -> Query
insertSql name columns =
  fromString
    ( "INSERT INTO "
        ++ name
        ++ " VALUES ("
        ++ intercalate ", " (replicate (length columns) "?")
        ++ ")"
    )

selectSql :: BindingMode -> String -> [ColumnDef] -> Query
selectSql bindingMode tableName (firstCol:restCols) =
  fromString
    ( "SELECT "
        ++ intercalate ", " (renderFirst bindingMode firstCol : map columnDefName restCols)
        ++ " FROM "
        ++ tableName
        ++ " ORDER BY t1"
    )
  where
    renderFirst Underscore = columnDefName
    renderFirst Positional = \col -> columnDefName col ++ " || ?"
    renderFirst Named = \col -> columnDefName col ++ " || :suffix"
selectSql _ _ [] = error "selectSql requires at least one column"

functionLabel :: AccessMode -> BindingMode -> String
functionLabel QueryMode Underscore = "queryWith_"
functionLabel QueryMode Positional = "queryWith"
functionLabel QueryMode Named      = "queryNamedWith"
functionLabel FoldMode Underscore  = "foldWith_"
functionLabel FoldMode Positional  = "foldWith"
functionLabel FoldMode Named       = "foldNamedWith"
