
module Day6.Day6 where
import           Control.Monad               (forM, forM_)
import           Control.Monad.Primitive     (PrimMonad (..), PrimState (..))
import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Unboxed.Mutable as VM

data LightAction = TurnOn | TurnOff | Toggle deriving Show

type Point = (Int,Int)

data Instruction = Instruction { action :: LightAction,
                                 from   :: Point,
                                 to     :: Point
                               } deriving Show

type AllLights a = V.Vector a

yardWidth = 1000

updateWithInstructions :: (PrimMonad m, V.Unbox a, HandleLightAction a) => V.Vector a -> [Instruction] -> m (V.Vector a)
updateWithInstructions allLights ins = do
  mAllLights <- V.thaw allLights
  forM_ ins $ updateWithInstruction mAllLights
  V.freeze mAllLights

updateWithInstruction :: (PrimMonad m, V.Unbox a, HandleLightAction a) => VM.MVector (PrimState m) a -> Instruction -> m (VM.MVector (PrimState m) a)
updateWithInstruction allLights ins = do
   let (x1,y1) = from ins
   let (x2,y2) = to ins
   let a = action ins
   forM_ [x1 .. x2 ] $ \x ->
      forM_ [ y1 .. y2 ] $ \y ->
        handleAction allLights (x * yardWidth + y) a
   return allLights

class HandleLightAction a where
    handleAction :: (PrimMonad m ) => VM.MVector (PrimState m) a -> Int -> LightAction -> m (VM.MVector (PrimState m) a)

instance HandleLightAction Bool where
  handleAction lights idx ins = do
    case ins of
      TurnOn  -> VM.write lights idx True
      TurnOff -> VM.write lights idx False
      Toggle  -> VM.modify lights not idx
    return lights

instance HandleLightAction Int where
  handleAction lights idx ins = do
    case ins of
      TurnOn  -> VM.modify lights (+1) idx
      TurnOff -> VM.modify lights dim idx
      Toggle  -> VM.modify lights (+ 2) idx
    return lights

dim :: Int -> Int
dim i = if i - 1 < 0 then 0 else i - 1
