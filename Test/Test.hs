{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import           Opaleye (Column, Nullable, Query, QueryArr, (.==), (.>))
import qualified Opaleye as O

import qualified Database.PostgreSQL.Simple as PGS
import qualified Data.Profunctor.Product.Default as D
import qualified Data.Profunctor.Product as PP
import qualified Data.Profunctor as P
import qualified Data.Ord as Ord
import qualified Data.List as L
import           Data.Monoid ((<>))
import qualified Data.String as String

import qualified System.Exit as Exit

import qualified Control.Applicative as A
import qualified Control.Arrow as Arr
import           Control.Arrow ((&&&), (***), (<<<), (>>>))

import           GHC.Int (Int64)

-- { Set your test database info here.  Then invoke the 'main'
--   function to run the tests, or just use 'cabal test'.  The test
--   database must already exist and the test user must have
--   permissions to modify it.

connectInfo :: PGS.ConnectInfo
connectInfo =  PGS.ConnectInfo { PGS.connectHost = "localhost"
                               , PGS.connectPort = 25433
                               , PGS.connectUser = "tom"
                               , PGS.connectPassword = "tom"
                               , PGS.connectDatabase = "opaleye_test" }

-- }

{-

Status
======

The tests here are very superficial and pretty much the bare mininmum
that needs to be tested.


Future
======

The overall approach to testing should probably go as follows.

1. Test all individual units of functionality by running them on a
   table and checking that they produce the expected result.  This type
   of testing is amenable to the QuickCheck approach if we reimplement
   the individual units of functionality in Haskell.

2. Test that "the denotation is an arrow morphism" is correct.  I
   think in combination with 1. this is all that will be required to
   demonstrate that the library is correct.

   "The denotation is an arrow morphism" means that for each arrow
   operation, the denotation preserves the operation.  If we have

       f :: QueryArr wiresa wiresb

   then [f] should be something like

       [f] :: a -> IO [b]
       f as = runQuery (toValues as >>> f)

   For example, take the operation >>>.  We need to check that

       [f >>> g] = [f] >>> [g]

   for all f and g, where [] means the denotation.  We would also want
   to check that

       [id] = id

   and

       [first f] = first [f]

   I think checking these operations is sufficient because all the
   other QueryArr operations are implemented in terms of them.

   (Here I'm taking a slight liberty as `a -> IO [b]` is not directly
   an arrow, but it could be made one straightforwardly.  (For the laws
   to be satisfied, perhaps we have to assume that the IO actions
   commute.))

   I don't think this type of testing is amenable to QuickCheck.  It
   seems we have to check the properties for arbitrary arrows indexed by
   arbitrary types.  I don't think QuickCheck supports this sort of
   randomised testing.

Note
----

This seems to be equivalent to just reimplementing Opaleye in
Haskell-side terms and comparing the results of queries run in both
ways.

-}

twoIntTable :: String
            -> O.Table (Column Int, Column Int) (Column Int, Column Int)
twoIntTable n = O.Table n (PP.p2 (O.required "column1", O.required "column2"))

table1 :: O.Table (Column Int, Column Int) (Column Int, Column Int)
table1 = twoIntTable "table1"

table1F :: O.Table (Column Int, Column Int) (Column Int, Column Int)
table1F = fmap (\(col1, col2) -> (col1 + col2, col1 - col2)) table1

table2 :: O.Table (Column Int, Column Int) (Column Int, Column Int)
table2 = twoIntTable "table2"

table3 :: O.Table (Column Int, Column Int) (Column Int, Column Int)
table3 = twoIntTable "table3"

table4 :: O.Table (Column Int, Column Int) (Column Int, Column Int)
table4 = twoIntTable "table4"

table1Q :: Query (Column Int, Column Int)
table1Q = O.queryTable table1

table2Q :: Query (Column Int, Column Int)
table2Q = O.queryTable table2

table3Q :: Query (Column Int, Column Int)
table3Q = O.queryTable table3

table1dataG :: Num a => [(a, a)]
table1dataG = [ (1, 100)
              , (1, 100)
              , (1, 200)
              , (2, 300) ]

table1data :: [(Int, Int)]
table1data = table1dataG

table1columndata :: [(Column Int, Column Int)]
table1columndata = table1dataG

table2dataG :: Num a => [(a, a)]
table2dataG = [ (1, 100)
              , (3, 400) ]

table2data :: [(Int, Int)]
table2data = table2dataG

table2columndata :: [(Column Int, Column Int)]
table2columndata = table2dataG

table3dataG :: Num a => [(a, a)]
table3dataG = [ (1, 50) ]

table3data :: [(Int, Int)]
table3data = table3dataG

table3columndata :: [(Column Int, Column Int)]
table3columndata = table3dataG

table4dataG :: Num a => [(a, a)]
table4dataG = [ (1, 10)
              , (2, 20) ]

table4data :: [(Int, Int)]
table4data = table4dataG

table4columndata :: [(Column Int, Column Int)]
table4columndata = table4dataG

dropAndCreateTable :: String -> PGS.Query
dropAndCreateTable t = String.fromString ("DROP TABLE IF EXISTS " ++ t ++ ";"
                                          ++ "CREATE TABLE " ++ t
                                          ++ " (column1 integer, column2 integer);")

dropAndCreateDB :: PGS.Connection -> IO ()
dropAndCreateDB conn = mapM_ execute ["table1", "table2", "table3", "table4"]
  where execute = PGS.execute_ conn . dropAndCreateTable

type Test = PGS.Connection -> IO Bool

testG :: D.Default O.QueryRunner wires haskells =>
         Query wires
         -> ([haskells] -> b)
         -> PGS.Connection
         -> IO b
testG q p conn = do
  result <- O.runQuery conn q
  return (p result)

testSelect :: Test
testSelect = testG table1Q
             (\r -> L.sort table1data == L.sort r)

testProduct :: Test
testProduct = testG query
                 (\r -> L.sort (A.liftA2 (,) table1data table2data) == L.sort r)
  where query = table1Q &&& table2Q

testRestrict :: Test
testRestrict = testG query
               (\r -> filter ((== 1) . fst) (L.sort table1data) == L.sort r)
  where query = proc () -> do
          t <- table1Q -< ()
          O.restrict -< fst t .== 1
          Arr.returnA -< t

testNum :: Test
testNum = testG query expected
  where query :: Query (Column Int)
        query = proc () -> do
          t <- table1Q -< ()
          Arr.returnA -< op t
        expected = \r -> L.sort (map op table1data) == L.sort r
        op :: Num a => (a, a) -> a
        op (x, y) = abs (x - 5) * signum (x - 4) * (y * y + 1)

testDiv :: Test
testDiv = testG query expected
  where query :: Query (Column Double)
        query = proc () -> do
          t <- Arr.arr (O.doubleOfInt *** O.doubleOfInt) <<< table1Q -< ()
          Arr.returnA -< op t
        expected r = L.sort (map (op . toDoubles) table1data) == L.sort r
        op :: Fractional a => (a, a) -> a
        -- Choosing 0.5 here as it should be exactly representable in
        -- floating point
        op (x, y) = y / x * 0.5
        toDoubles :: (Int, Int) -> (Double, Double)
        toDoubles = fromIntegral *** fromIntegral

-- TODO: need to implement and test case_ returning tuples
testCase :: Test
testCase = testG q (== expected)
  where q :: Query (Column Int)
        q = table1Q >>> proc (i, j) -> do
          Arr.returnA -< O.case_ [(j .== 100, 12), (i .== 1, 21)] 33
        expected :: [Int]
        expected = [12, 12, 21, 33]

testDistinct :: Test
testDistinct = testG (O.distinct table1Q)
               (\r -> L.sort (L.nub table1data) == L.sort r)

-- FIXME: the unsafeCoerce is currently needed because the type
-- changes required for aggregation are not currently dealt with by
-- Opaleye.
aggregateCoerceFIXME :: QueryArr (Column Int) (Column Int64)
aggregateCoerceFIXME = Arr.arr aggregateCoerceFIXME'

aggregateCoerceFIXME' :: Column a -> Column Int64
aggregateCoerceFIXME' = O.unsafeCoerce

testAggregate :: Test
testAggregate = testG (Arr.second aggregateCoerceFIXME
                        <<< O.aggregate (PP.p2 (O.groupBy, O.sum))
                                           table1Q)
                      (\r -> [(1, 400) :: (Int, Int64), (2, 300)] == L.sort r)

testAggregateProfunctor :: Test
testAggregateProfunctor = testG q expected
  where q = O.aggregate (PP.p2 (O.groupBy, countsum)) table1Q
        expected r = [(1, 1200) :: (Int, Int64), (2, 300)] == L.sort r
        countsum = P.dimap (\x -> (x,x))
                           (\(x, y) -> aggregateCoerceFIXME' x * y)
                           (PP.p2 (O.sum, O.count))

testOrderByG :: O.Order (Column Int, Column Int)
                -> ((Int, Int) -> (Int, Int) -> Ordering)
                -> Test
testOrderByG orderQ order = testG (O.orderBy orderQ table1Q)
                                  (L.sortBy order table1data ==)

testOrderBy :: Test
testOrderBy = testOrderByG (O.desc snd)
                           (flip (Ord.comparing snd))

testOrderBy2 :: Test
testOrderBy2 = testOrderByG (O.desc fst <> O.asc snd)
                            (flip (Ord.comparing fst) <> Ord.comparing snd)

testOrderBySame :: Test
testOrderBySame = testOrderByG (O.desc fst <> O.asc fst)
                               (flip (Ord.comparing fst) <> Ord.comparing fst)

testLOG :: (Query (Column Int, Column Int) -> Query (Column Int, Column Int))
           -> ([(Int, Int)] -> [(Int, Int)]) -> Test
testLOG olQ ol = testG (olQ (orderQ table1Q))
                       (ol (order table1data) ==)
  where orderQ = O.orderBy (O.desc snd)
        order = L.sortBy (flip (Ord.comparing snd))

testLimit :: Test
testLimit = testLOG (O.limit 2) (take 2)

testOffset :: Test
testOffset = testLOG (O.offset 2) (drop 2)

testLimitOffset :: Test
testLimitOffset = testLOG (O.limit 2 . O.offset 2) (take 2 . drop 2)

testOffsetLimit :: Test
testOffsetLimit = testLOG (O.offset 2 . O.limit 2) (drop 2 . take 2)

testDistinctAndAggregate :: Test
testDistinctAndAggregate = testG q expected
  where q = O.distinct table1Q
            &&& (Arr.second aggregateCoerceFIXME
                 <<< O.aggregate (PP.p2 (O.groupBy, O.sum)) table1Q)
        expected r = L.sort r == L.sort expectedResult
        expectedResult = A.liftA2 (,) (L.nub table1data)
                                      [(1 :: Int, 400 :: Int64), (2, 300)]

one :: Query (Column Int)
one = Arr.arr (const (1 :: Column Int))

-- The point of the "double" tests is to ensure that we do not
-- introduce name clashes in the operations which create new column names
testDoubleG :: (Eq haskells, D.Default O.QueryRunner columns haskells) =>
               (QueryArr () (Column Int) -> QueryArr () columns) -> [haskells]
               -> Test
testDoubleG q expected1 = testG (q one &&& q one) (== expected2)
  where expected2 = A.liftA2 (,) expected1 expected1

testDoubleDistinct :: Test
testDoubleDistinct = testDoubleG O.distinct [1 :: Int]

testDoubleAggregate :: Test
testDoubleAggregate = testDoubleG (O.aggregate O.count) [1 :: Int64]

testDoubleLeftJoin :: Test
testDoubleLeftJoin = testDoubleG lj [(1 :: Int, Just (1 :: Int))]
  where lj :: Query (Column Int)
          -> Query (Column Int, Column (Nullable Int))
        lj q = O.leftJoin q q (uncurry (.==))

testDoubleValues :: Test
testDoubleValues = testDoubleG v [1 :: Int]
  where v :: Query (Column Int) -> Query (Column Int)
        v _ = O.values [1]

testDoubleUnionAll :: Test
testDoubleUnionAll = testDoubleG u [1 :: Int, 1]
  where u q = q `O.unionAll` q

aLeftJoin :: Query ((Column Int, Column Int),
                    (Column (Nullable Int), Column (Nullable Int)))
aLeftJoin = O.leftJoin table1Q table3Q (\(l, r) -> fst l .== fst r)

testLeftJoin :: Test
testLeftJoin = testG aLeftJoin (== expected)
  where expected :: [((Int, Int), (Maybe Int, Maybe Int))]
        expected = [ ((1, 100), (Just 1, Just 50))
                   , ((1, 100), (Just 1, Just 50))
                   , ((1, 200), (Just 1, Just 50))
                   , ((2, 300), (Nothing, Nothing)) ]

testLeftJoinNullable :: Test
testLeftJoinNullable = testG q (== expected)
  where q :: Query ((Column Int, Column Int),
                    ((Column (Nullable Int), Column (Nullable Int)),
                     (Column (Nullable Int),
                      Column (Nullable Int))))
        q = O.leftJoin table3Q aLeftJoin cond

        cond (x, y) = fst x .== fst (fst y)

        expected :: [((Int, Int), ((Maybe Int, Maybe Int), (Maybe Int, Maybe Int)))]
        expected = [ ((1, 50), ((Just 1, Just 100), (Just 1, Just 50)))
                   , ((1, 50), ((Just 1, Just 100), (Just 1, Just 50)))
                   , ((1, 50), ((Just 1, Just 200), (Just 1, Just 50))) ]

testThreeWayProduct :: Test
testThreeWayProduct = testG q (== expected)
  where q = A.liftA3 (,,) table1Q table2Q table3Q
        expected = A.liftA3 (,,) table1data table2data table3data

testValues :: Test
testValues = testG (O.values values) (values' ==)
  where values :: [(Column Int, Column Int)]
        values = [ (1, 10)
                 , (2, 100) ]
        values' :: [(Int, Int)]
        values' = [ (1, 10)
                  , (2, 100) ]

{- FIXME: does not yet work
testValuesDouble :: Test
testValuesDouble = testG (O.values values) (values' ==)
  where values :: [(Column Int, Column Double)]
        values = [ (1, 10.0)
                 , (2, 100.0) ]
        values' :: [(Int, Double)]
        values' = [ (1, 10.0)
                  , (2, 100.0) ]
-}

testValuesEmpty :: Test
testValuesEmpty = testG (O.values values) (values' ==)
  where values :: [Column Int]
        values = []
        values' :: [Int]
        values' = []

testUnionAll :: Test
testUnionAll = testG (table1Q `O.unionAll` table2Q)
                     (\r -> L.sort (table1data ++ table2data) == L.sort r)

testTableFunctor :: Test
testTableFunctor = testG (O.queryTable table1F) (result ==)
  where result = fmap (\(col1, col2) -> (col1 + col2, col1 - col2)) table1data

-- TODO: This is getting too complicated
testUpdate :: Test
testUpdate conn = do
  _ <- O.runUpdate conn table4 update cond
  result <- runQueryTable4

  if result /= expected
    then return False
    else do
    _ <- O.runDelete conn table4 condD
    resultD <- runQueryTable4

    if resultD /= expectedD
      then return False
      else do
      returned <- O.runInsertReturning conn table4 insertT returning
      resultI <- runQueryTable4

      return ((resultI == expectedI) && (returned == expectedR))

  where update (x, y) = (x + y, x - y)
        cond (_, y) = y .> 15
        condD (x, _) = x .> 20
        expected :: [(Int, Int)]
        expected = [ (1, 10)
                   , (22, -18)]
        expectedD :: [(Int, Int)]
        expectedD = [(1, 10)]
        runQueryTable4 = O.runQuery conn (O.queryTable table4)

        insertT :: (Column Int, Column Int)
        insertT = (1, 2)

        expectedI :: [(Int, Int)]
        expectedI = [(1, 10), (1, 2)]
        returning (x, y) = x - y
        expectedR :: [Int]
        expectedR = [-1]


allTests :: [Test]
allTests = [testSelect, testProduct, testRestrict, testNum, testDiv, testCase,
            testDistinct, testAggregate, testAggregateProfunctor,
            testOrderBy, testOrderBy2, testOrderBySame, testLimit, testOffset,
            testLimitOffset, testOffsetLimit, testDistinctAndAggregate,
            testDoubleDistinct, testDoubleAggregate, testDoubleLeftJoin,
            testDoubleValues, testDoubleUnionAll,
            testLeftJoin, testLeftJoinNullable, testThreeWayProduct, testValues,
            testValuesEmpty, testUnionAll, testTableFunctor, testUpdate
           ]

main :: IO ()
main = do
  conn <- PGS.connect connectInfo

  dropAndCreateDB conn

  let insert (writeable, columndata) =
        mapM_ (O.runInsert conn writeable) columndata

  mapM_ insert [ (table1, table1columndata)
               , (table2, table2columndata)
               , (table3, table3columndata)
               , (table4, table4columndata) ]

  results <- mapM ($ conn) allTests

  print results

  let passed = and results

  putStrLn (if passed then "All passed" else "Failure")
  Exit.exitWith (if passed then Exit.ExitSuccess
                           else Exit.ExitFailure 1)
