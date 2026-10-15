module Main where

import Prelude hiding (Left, Right)
import Brick.BChan (newBChan, readBChan)
import Control.Concurrent (forkIO, killThread)
import Control.Exception (bracket)
import Control.Monad (foldM, forM_, replicateM_, unless)
import Control.Monad.State.Strict (execStateT)
import qualified Data.Map as M
import qualified Data.Sequence as Seq
import qualified Data.Text.Lazy as TL
import qualified Data.Vector as Vector
import qualified Graphics.Vty as V
import Graphics.Vty.PictureToSpans (displayOpsForPic)
import Graphics.Vty.Span (SpanOp(..))
import System.Timeout (timeout)

import Tetris
import Tetris.Coord (addCoord, subtractCoord, clockwise, counterclockwise)
import UI.Grid
import UI.FrameClock
import UI.GravityClock
import UI.Image (compactImage, replaceRegion)
import UI.Ticker

assert :: String -> Bool -> IO ()
assert label ok = unless ok $ fail label

fixture :: Game
fixture = Game 0 (Block I (Coord 6 22) (map (addCoord (Coord 6 22)) (relCells I))) O
  (Seq.fromList $ take 28 $ cycle [I .. L]) 0 0 M.empty False

testCoordinates :: IO ()
testCoordinates = do
  -- Scalar and tuple oracles do not use the coordinate operations that also
  -- construct fixtures for the hard-drop and renderer comparisons below.
  let points = [(x, y) | x <- [-3 .. 3], y <- [-3 .. 3]]
  forM_ points $ \(x, y) -> do
    assert "clockwise quarter turn" (clockwise (Coord x y) == Coord y (-x))
    assert "counterclockwise quarter turn" (counterclockwise (Coord x y) == Coord (-y) x)
    forM_ points $ \(dx, dy) -> do
      assert "coordinate addition is component-wise" $
        addCoord (Coord x y) (Coord dx dy) == Coord (x + dx) (y + dy)
      assert "coordinate subtraction preserves operand order" $
        subtractCoord (Coord x y) (Coord dx dy) == Coord (x - dx) (y - dy)
      assert "coordinate ordering is lexicographic x then y" $
        compare (Coord x y) (Coord dx dy) == compare (x, y) (dx, dy)
  let cells = M.fromList [(Coord 2 3, I), (Coord 1 22, O), (Coord 2 8, T), (Coord 3 1, S)]
  assert "map predecessor finds nearest lower cell in the same column" $
    M.lookupLT (Coord 2 7) cells == Just (Coord 2 3, I)
  assert "map predecessor can belong to the preceding column" $
    M.lookupLT (Coord 2 1) cells == Just (Coord 1 22, O)
  putStrLn "PASS: strict coordinates, column-first ordering, arithmetic, and quarter turns"

testRotations :: IO ()
testRotations = do
  -- Absolute expected cells independently pin shape geometry, rotation
  -- direction, and the nonzero pivot; round trips alone miss reversed turns.
  let expectedTurns =
        [ (I, [Coord 6 8, Coord 6 9, Coord 6 11])
        , (O, [Coord 5 10, Coord 5 9, Coord 6 9])
        , (T, [Coord 6 9, Coord 7 10, Coord 6 11])
        , (S, [Coord 7 9, Coord 7 10, Coord 6 11])
        , (Z, [Coord 6 9, Coord 7 10, Coord 7 11])
        , (J, [Coord 6 9, Coord 6 11, Coord 7 11])
        , (L, [Coord 7 9, Coord 6 9, Coord 6 11])
        ]
  forM_ expectedTurns $ \(t, expected) -> do
    let pivot = Coord 6 10
        original = Block t pivot (map (addCoord pivot) $ relCells t)
        g = fixture { _block = original }
    turned <- execStateT rotate g
    assert ("exact first rotation: " ++ show t) (_block turned == Block t pivot expected)
    cycled <- execStateT (replicateM_ (if t == I then 2 else 4) rotate) g
    assert ("rotation cycle, including clockwise I return: " ++ show t) (_block cycled == original)
  let original = Block T (Coord 6 10) [Coord 5 10, Coord 6 9, Coord 7 10]
      g = fixture { _block = original }
  left <- execStateT (shift Left) g
  right <- execStateT (shift Right) g
  down <- execStateT (shift Down) g
  assert "left translation moves origin and cells" $
    _block left == Block T (Coord 5 10) [Coord 4 10, Coord 5 9, Coord 6 10]
  assert "right translation moves origin and cells" $
    _block right == Block T (Coord 7 10) [Coord 6 10, Coord 7 9, Coord 8 10]
  assert "down translation moves origin and cells" $
    _block down == Block T (Coord 6 9) [Coord 5 9, Coord 6 8, Coord 7 9]
  putStrLn "PASS: exact shape rotations, I alternation, stationary O, and translations"

-- The old full-board scan is deliberately retained as an independent oracle.
referenceDrop :: Game -> Block
referenceDrop g = Block (_shape b) (down $ _origin b) (map down $ _extra b)
  where
    b = _block g
    distances = [y - obstacleY - 1 | Coord x y <- coords b,
                  Coord obstacleX obstacleY <- M.keys (_board g),
                  x == obstacleX, obstacleY < y]
    distance = minimum $ map (\(Coord _ y) -> y - 1) (coords b) ++ distances
    down (Coord x y) = Coord x (y - distance)

testDrop :: IO ()
testDrop = do
  let turn (Coord x y) = Coord (-y) x
      blocks = [Block t origin (map (addCoord origin) cells)
               | t <- [I .. L], rotation <- [0 .. 3], x <- [1 .. boardWidth],
                 let origin = Coord x 23,
                 let cells = iterate (map turn) (relCells t) !! rotation,
                 all (\(Coord cx _) -> cx >= 1 && cx <= boardWidth) (origin : map (addCoord origin) cells)]
  forM_ (zip [(0 :: Int) ..] blocks) $ \(i, b) ->
    forM_ [0 .. 9] $ \seed -> do
      let occupied = M.fromList [(Coord x y, J) | x <- [1 .. boardWidth], y <- [1 .. 24],
                        (x * 31 + y * 17 + seed * 13) `mod` 19 < 5,
                        Coord x y `notElem` coords b]
          g = fixture { _block = b, _board = occupied }
          actual = evalTetris hardDroppedBlock g
      assert ("drop agrees with reference: " ++ show (i, seed)) (actual == referenceDrop g)
  assert "empty-board drop lands on floor" $
    minimum [y | Coord _ y <- coords $ evalTetris hardDroppedBlock fixture] == 1
  putStrLn $ "PASS: hard-drop reference comparison (" ++ show (length blocks * 10) ++ " cases)"

testClears :: IO ()
testClears = do
  forM_ [1 .. 4] $ \count -> do
    let b = case count of
          1 -> Block I (Coord 6 1) (map (addCoord (Coord 6 1)) $ relCells I)
          2 -> Block O (Coord 6 2) (map (addCoord (Coord 6 2)) $ relCells O)
          3 -> Block L (Coord 5 3) [Coord 5 2, Coord 5 1, Coord 6 1]
          _ -> Block I (Coord 5 4) [Coord 5 y | y <- [1 .. 3]]
        -- Extra cells away from cleared rows make row shifting observable.
        brd = M.fromList $ [(Coord x y, O) | x <- [1 .. boardWidth], y <- [1 .. count], Coord x y `notElem` coords b]
                       ++ [(Coord 1 (count + 1), T), (Coord 10 22, L)]
        g = fixture { _block = b, _board = brd, _level = 2 }
    result <- execStateT timeStep g
    assert ("line clear: " ++ show count) (_linesCleared result == count)
    assert "shifted rows preserve column ordering and offscreen cells" $
      _board result == M.fromList [(Coord 1 1, T), (Coord 10 (22 - count), L)]
    assert "line score preserved" (_score result == 3 * ([0, 40, 100, 300, 1200] !! count))
    assert "fixed level preserved" (_level result == 2)
  let row = M.fromList [(Coord x 1, O) | x <- [1 .. boardWidth], x `notElem` [4 .. 7]]
      b = Block I (Coord 6 1) (map (addCoord (Coord 6 1)) $ relCells I)
  progressed <- execStateT timeStep $ fixture
    { _block = b, _board = row, _linesCleared = 9, _progression = True }
  assert "progression on tenth cleared line" (_level progressed == 1 && _linesCleared progressed == 10)
  let vertical = Block I (Coord 5 4) [Coord 5 y | y <- [1 .. 3]]
      separatedRows = M.fromList $ [(Coord x y, O) | x <- [1 .. boardWidth], x /= 5, y <- [1, 3]]
                                ++ [(Coord 1 2, T), (Coord 2 4, L), (Coord 10 22, S)]
  separated <- execStateT timeStep $ fixture { _block = vertical, _board = separatedRows }
  assert "non-adjacent full rows shift each surviving row by its own distance" $
    _board separated == M.fromList [(Coord 5 1, I), (Coord 5 2, I), (Coord 1 1, T), (Coord 2 2, L), (Coord 10 20, S)]
      && _linesCleared separated == 2 && _score separated == 100
  locked <- execStateT (hardDrop >> timeStep >> shift Left) fixture
  assert "drop locks four cells immediately" (M.size (_board locked) == 4)
  assert "new piece is immediately visible and movable" (_origin (_block locked) == Coord 5 20)
  unchanged <- execStateT timeStep fixture
  assert "gravity moves one row without freezing" (_origin (_block unchanged) == Coord 6 21 && M.null (_board unchanged))
  putStrLn "PASS: line clearing, scoring, progression, gravity, and immediate drop"

testSpawn :: IO ()
testSpawn = do
  let floorPiece = Block I (Coord 6 1) (map (addCoord (Coord 6 1)) $ relCells I)
  forM_ [I .. L] $ \t -> do
    initial <- initGame 0 False
    assert "initial piece is visible without gravity" $
      all (\(Coord _ y) -> y <= boardHeight) (coords $ _block initial)
    entered <- execStateT (hardDrop >> timeStep) $ fixture { _nextShape = t }
    assert "all replacement shapes enter the visible board immediately" $
      _shape (_block entered) == t && _origin (_block entered) == Coord 6 boardHeight
    obstructed <- execStateT timeStep $ fixture
      { _block = floorPiece, _nextShape = t, _board = M.singleton (Coord 6 20) J }
    assert "entry never jumps through an occupied cell" $
      all (`M.notMember` _board obstructed) (coords $ _block obstructed)
        && (case _origin (_block obstructed) of Coord _ y -> y > boardHeight)
    toppedOut <- execStateT timeStep $ fixture
      { _block = floorPiece, _nextShape = t, _board = M.singleton (Coord 6 21) J }
    assert "blocked hidden spawn retains top-out detection" (isGameOver toppedOut)
  putStrLn "PASS: visible spawn, blocked entry, and hidden-row top-out for all shapes"

testRepeatedClears :: IO ()
testRepeatedClears = do
  let square = Block O (Coord 6 20) [Coord 5 20, Coord 5 19, Coord 6 19]
      initial = fixture { _block = square, _nextShape = O, _nextShapeBag = Seq.replicate 2000 O }
      place g offset = execStateT (do
        sequence_ $ replicate (abs offset) $ shift $ if offset < 0 then Left else Right
        hardDrop
        timeStep) g
      clearPair g n = do
        result <- foldM place g [-4, -2, 0, 2, 4]
        assert "repeated double clears leave no settled cells" (M.null $ _board result)
        assert "line counts and score remain correct across long sessions" $
          _linesCleared result == n * 2 && _score result == n * 100
        assert "fixed-level double clears preserve speed and spawn" $
          _level result == 0 && _origin (_block result) == Coord 6 20 && not (isGameOver result)
        pure result
  _ <- foldM clearPair initial [1 .. 250]
  putStrLn "PASS: 1,250 consecutive piece locks, 500 cleared lines, scoring, and fixed level"

testTicker :: IO ()
testTicker = do
  forM_ [0 .. 15] $ \n ->
    assert "normal levels preserve gravity speed" $
      levelToDelay n == floor (400000 * (0.85 :: Double) ^ (2 * n))
  assert "negative levels are safe" (levelToDelay minBound == 400000)
  assert "large levels never spin" (levelToDelay maxBound == 1000)
  ticker <- newTicker
  chan <- newBChan 1
  bracket (forkIO $ runTicker ticker chan) killThread $ \_ -> do
    none <- timeout 20000 $ readBChan chan
    assert "ticker sleeps before scheduling" (none == Nothing)
    generation <- scheduleTick ticker (Just 1000)
    tick <- timeout 1000000 $ readBChan chan
    assert "scheduled tick delivered" (tick == Just (Tick generation))
    backlog <- timeout 30000 $ readBChan chan
    assert "unacknowledged tick does not repeat" (backlog == Nothing)
    _ <- scheduleTick ticker (Just 100000)
    _ <- scheduleTick ticker Nothing
    paused <- timeout 150000 $ readBChan chan
    assert "pause cancels pending timer" (paused == Nothing)
    _ <- scheduleTick ticker (Just 100000)
    latest <- scheduleTick ticker (Just 1000)
    resumed <- timeout 1000000 $ readBChan chan
    assert "rescheduling invalidates old timer" (resumed == Just (Tick latest))
    stale <- timeout 150000 $ readBChan chan
    assert "cancelled timer never produces catch-up ticks" (stale == Nothing)
  putStrLn "PASS: ticker suspension, cancellation, generation, and backpressure"
  frameTimer <- newTicker
  frameChan <- newBChan 1
  bracket (forkIO $ runFrameTicker frameTimer frameChan) killThread $ \_ -> do
    generation <- scheduleTick frameTimer (Just 1000)
    event <- timeout 1000000 $ readBChan frameChan
    assert "frame timer events are distinct from gravity" (event == Just (FrameTick generation))
    extra <- timeout 20000 $ readBChan frameChan
    assert "frame timer does not redraw an idle screen" (extra == Nothing)

testFrameClock :: IO ()
testFrameClock = do
  let clock = newFrameClock 120 0
  assert "120Hz budget rounds nanoseconds upward" (frameDelayMicros 0 clock == 8334)
  assert "early input waits for the existing budget" (frameDelayMicros 8000000 clock == 334)
  assert "advancing early does not move the deadline" (advanceFrameClock 1000000 clock == clock)
  assert "frame is due at its deadline" (frameDelayMicros 8333334 clock == 0)
  let second = advanceFrameClock 8400000 clock
  assert "late frames retain clock phase" (frameDelayMicros 8400000 second == 8267)
  let afterStall = advanceFrameClock 500000000 clock
  assert "stalls skip expired frames without immediate catch-up" $
    frameDelayMicros 500000000 afterStall >= 4167
      && frameDelayMicros 500000000 afterStall <= 12500
  let step (count, c) now = if frameDelayMicros now c == 0
        then (count + 1, advanceFrameClock now c) else (count, c)
      (renders, _) = foldl step ((0 :: Int), clock) [1000000, 2000000 .. 1000000000]
  assert "1000 input events fit about 120 frames in one second" (renders >= 119 && renders <= 120)
  forM_ [0, 1, 8000000, 1000000000] $ \now -> do
    let unlimited = newFrameClock 0 0
    assert "uncapped mode never defers rendering" (frameDelayMicros now unlimited == 0)
  assert "other frame rates have independent budgets" (frameDelayMicros 0 (newFrameClock 60 0) == 16667)
  putStrLn "PASS: 120Hz frame budgets, coalescing, clock phase, stall recovery, and uncapped mode"

testGravityClock :: IO ()
testGravityClock = do
  let initial = newGravityClock 16000 0
  assert "gravity starts after one interval" (gravityDeadline initial == 16000000)
  assert "early updates retain the deadline" (advanceGravityClock 16000 1000000 initial == initial)
  forM_ [0, 4000000, 8000000] $ \work -> do
    let clocks = take 200 $ iterate (\clock -> advanceGravityClock 16000 (gravityDeadline clock + work) clock) initial
    assert "processing work does not accumulate into gravity intervals" $
      map gravityDeadline clocks == [16000000, 32000000 .. 3200000000]
  assert "stall skips missed slots without queuing catch-up ticks" $
    gravityDeadline (advanceGravityClock 16000 101000000 initial) == 112000000
  assert "level changes reset the interval at the new speed" $
    gravityDeadline (advanceGravityClock 8000 20000000 initial) == 28000000
  assert "resume starts a fresh interval" (gravityDeadline (newGravityClock 16000 1000000000) == 1016000000)
  putStrLn "PASS: elapsed-time gravity, sustained handler work, stalls, level changes, and reset"

testGrid :: IO ()
testGrid = do
  let shapeAttr t = V.withBackColor V.defAttr (V.Color240 $ fromIntegral $ fromEnum t + 1)
      solid t = V.string (shapeAttr t) " ."
      empty = V.string V.defAttr " ."
      pixels image bounds = map (concatMap expand . Vector.toList) $ Vector.toList $
        displayOpsForPic ((V.picForImage image) { V.picBackground = V.Background ' ' V.defAttr }) bounds
      expand (TextSpan attr _ _ txt) = [(attr, c) | c <- TL.unpack txt]
      expand (Skip n) = replicate n (V.defAttr, ' ')
      expand (RowEnd n) = replicate n (V.defAttr, ' ')
  forM_ [0 .. 19] $ \seed -> do
    let brd = M.fromList [(Coord x y, toEnum $ (x + y) `mod` 7)
                        | x <- [1 .. boardWidth], y <- [1 .. 22],
                          (x * 31 + y * 17 + seed * 13) `mod` 19 < seed `mod` 10]
        grid = makeGrid shapeAttr V.defAttr brd
    forM_ [Nothing, Just "[]", Just "◤◢", Just "XX", Just "界界"] $ \preview -> do
      let overlays = [(Coord x y, solid T) | x <- [4 .. 6], y <- [seed, seed + 1]]
                  ++ [(Coord 4 seed, solid L), (Coord 0 1, solid I), (Coord 11 20, solid I)]
                  ++ case preview of
                       Nothing -> []
                       Just chars -> [(Coord x y, V.string (V.withForeColor V.defAttr V.red) chars)
                                     | x <- [4 .. 7], y <- [1, seed]]
          cells = M.union (solid <$> brd) $ foldr (uncurry M.insert) M.empty overlays
          reference = V.vertCat
            [V.horizCat [M.findWithDefault empty (Coord x y) cells | x <- [1 .. boardWidth]]
            | y <- [boardHeight, boardHeight - 1 .. 1]]
          actual = gridImage grid overlays
      forM_ [(20, 20), (12, 8), (5, 1), (40, 25), (0, 0)] $ \bounds ->
        assert ("cached grid matches cell renderer: " ++ show (seed, preview, bounds)) $
          pixels actual bounds == pixels reference bounds
  let panel = V.horizCat
        [ V.vertCat [V.string V.defAttr "border", V.charFill (shapeAttr I) 'x' (8 :: Int) 4]
        , V.vertCat [V.string (shapeAttr O) "λ━→", V.string V.defAttr "  padded text"]
        , V.charFill V.defAttr ' ' (2 :: Int) 3
        ]
  forM_ [(30, 10), (10, 3), (1, 1), (0, 0)] $ \bounds ->
    assert "compacting a padded colored panel preserves visible pixels" $
      pixels panel bounds == pixels (compactImage V.defAttr panel) bounds
  let background = V.charFill V.defAttr '.' (24 :: Int) 12
      patch = V.charFill (shapeAttr T) 'X' (6 :: Int) 4
  forM_ [(0, 0), (5, 3), (-2, -1), (22, 10), (30, 20), (-10, -10)] $ \position@(x, y) -> do
    let layers = (V.picForLayers [V.translate x y patch, background])
          { V.picBackground = V.Background ' ' V.defAttr }
        expected = map (concatMap expand . Vector.toList) $ Vector.toList $
          displayOpsForPic layers (24, 12)
    assert "region replacement matches layered rendering, including clipped patches" $
      pixels (replaceRegion position patch background) (24, 12) == expected
  putStrLn "PASS: cached grid pixel/color equivalence (500 board/preview/viewport cases)"

main :: IO ()
main = testCoordinates >> testRotations >> testDrop >> testClears >> testSpawn >> testRepeatedClears >> testTicker >> testFrameClock >> testGravityClock >> testGrid
