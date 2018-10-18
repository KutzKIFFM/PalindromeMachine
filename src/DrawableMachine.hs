module DrawableMachine where
import           Prelude hiding (Word)

data Word = W Char | Blank
            deriving(Eq, Ord, Show)

data HeadMovement = Stay | MoveLeft | MoveRight
                    deriving(Eq, Ord, Show)

data WriteAction = DontWrite | Write Word
                   deriving(Eq, Ord, Show)

type InputTapeAction = HeadMovement
type OutputTapeAction = HeadMovement
type MemoryTapeAction = (HeadMovement, WriteAction)

type MachineAction = (InputTapeAction, MemoryTapeAction, OutputTapeAction)

class DrawableMachine c where
  -- The first Element of each list is the left-most element
  inputTape10LeftRight :: c -> ([Word], [Word])
  memoryTape10LeftRight :: c -> ([Word], [Word])
  outputTape10LeftRight :: c -> ([Word], [Word])

  -- The word on the respective heads
  inputHead :: c -> Word
  memoryHead :: c -> Word
  writeHead :: c -> Word

  -- The 11-th elemnt left/right in case of head movement
  input11thWords :: c -> (Word, Word)
  memory11thWords :: c -> (Word, Word)
  output11thWords :: c -> (Word, Word)

  delta :: c -> (c, MachineAction)

  --initialState
  fromInput :: [Word] -> c


data DummyState =  DummyState0 |  DummyState1 deriving(Eq, Ord, Show)
data DummyMachine = Dummy [Word] DummyState deriving(Eq, Ord, Show)

instance DrawableMachine DummyMachine where
  fromInput ls = Dummy (ls ++ repeat Blank) DummyState0
  inputTape10LeftRight (Dummy tape _) = (replicate 10 Blank, take 10 . tail $ tape)
  memoryTape10LeftRight = const (replicate 10 Blank, replicate 10 Blank)
  outputTape10LeftRight = const (replicate 10 Blank, replicate 10 Blank)
  input11thWords (Dummy tape _) = (Blank, tail tape !! 10)
  memory11thWords = const (Blank, Blank)
  output11thWords = const (Blank, Blank)
  delta (Dummy tape DummyState0) = let ac = (Stay, (MoveRight, DontWrite), MoveLeft)
                                       in (Dummy tape DummyState1, ac)
  delta (Dummy tape DummyState1) = let ac = (Stay, (MoveLeft, DontWrite), MoveRight)
                                       in (Dummy tape DummyState0, ac)
