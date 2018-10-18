module Draw where
import           Data.Maybe
import           DrawableMachine
import           Graphics.Gloss.Data.Picture hiding (Blank)
import           Prelude                     hiding (Word)


blockSize = 64 :: Float

-- Tape leftMost, rightMost, visible tape
data Tape = Tape Word Word [Word] deriving(Eq, Ord, Show)

data Direction = Left | Right deriving(Eq, Ord, Show)

data RenderAction = HeadMove Direction
                   | HeadWriteAndMove Word Direction
                   | HeadWrite Word
                   | NoMove
                   deriving(Eq, Ord, Show)

block :: Word -> Picture
block Blank = let rect = rectangleWire blockSize blockSize
                  circ = circle ((blockSize - 5) / 2)
                  value = text "B"
                  in pictures [rect, circ, value]

block (W c) = let rect = rectangleWire blockSize blockSize
                  value = text [c]
                  in pictures [rect, value]

arrowHead = let recS = (blockSize - 10)
                rect = rectangleSolid recS recS
                triangle = polygon [(0,recS), (0.5 * recS, 1.5 * recS ), (recS, recS)]
                in pictures [rect, triangle]

-- writeAction :: writeActionTime
--             -> Time between 0 and writeActionTime
--             -> Picture
headWriteAction :: Float -> Float -> Picture
headWriteAction writeActionTime time
  | time <= writeActionTime = let t = abs ((2 * time / writeActionTime) - 1)
                                  yoffset = blockSize - t * blockSize
                                  in translate 0 yoffset arrowHead

-- moveHeadLeft :: moveTime
--             -> Time between 0 and moveTime
--             -> Picture
moveHeadLeft :: Float -> Float -> Picture
moveHeadLeft moveTime time
  | time <= moveTime = let t = abs ((2 * time / writeActionTime) - 1)
                           xoffset = blockSize - t * blockSize
                           in translate (-xoffset) 0 arrowHead

moveHeadRight :: Float -> Float -> Picture
moveHeadRight moveTime time
  | time <= moveTime = let t = abs ((2 * time / writeActionTime) - 1)
                           xoffset = blockSize - t * blockSize
                           in translate xoffset 0 arrowHead

tape :: Tape -> Picture
tape (Tape _ _ tape) = let ls = [0..length tape - 1]
                           offsets = map (*blockSize) ls
                           pics = map (\(w, o) -> translate o (block w)) (zip ls offsets)
                           in pictures pics

moveTapeLeft :: Float -> Tape -> Float -> Picture
moveTapeLeft moveTime (Tape _ rightMost tape) time
  | time <= moveTime = let blankL = translate (-blockSize) blank
                           blankR = translate (blockSize * length tape) blank
                           entireTape = tape (Tape _ _ (tape ++ rightMost))
                           t = time / moveTime
                           offsetTape = translate (- t * blockSize) 0 entireTape
                           in pictures (blankL:blankR:[offsetTape])

moveTapeRight :: Float -> Tape -> Float -> Picture
moveTapeRight moveTime (Tape _ rightMost tape) time
 | time <= moveTime = let blankL = translate (-blockSize) blank
                          blankR = translate (blockSize * length tape) blank
                          entireTape = tape (Tape _ _ (tape ++ rightMost))
                          t = time / moveTime
                          offsetTape = translate (t * blockSize) 0 entireTape
                          in pictures (blankL:blankR:[offsetTape])

data MachineWithTime m = MWT Float m deriving(Eq, Show, Ord)
(+++) (MWT t m) t' = MWT (t+t') m

tapesFromMachine :: DrawableMachine m -> (Tape, Tape, Tape)
tapesFromMachine m = let (leftI, rightI) = inputTape10LeftRight m
                         hI = inputHead m
                         (lMI, rMI) = input11thWords m
                         (leftM, rightM) = memoryTape10LeftRight m
                         hM = memoryHead m
                         (lMM, rMM) = memory11thWords m
                         (leftO, rightO) = outputTape10LeftRight m
                         hO = outputHead m
                         (lMM, rMM) = output11thWords m
                         in (Tape lMI rMI (leftI ++ [hI] ++ rightI), Tape lMM rMM (leftM ++ [hM] ++ rightM), Tape lMO rMO (leftO ++ [hO] ++ rightO))


-- sim :: DrawableMachine m => Float -> Float -> MachineWithTime m -> MachineWithTime
-- sim simulationInterval t m
--   | t > simulationInterval = let (m', (iAction, mAction, oAction)) = delta m
--                                  i = case iAction of Stay ->
