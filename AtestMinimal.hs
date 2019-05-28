import Data.Array.Accelerate.LLVM.PTX
import Data.Array.Accelerate

l = run $ unit 0 :: Scalar Float
p = fill (constant $ Z:.10000:.10000) 1 :: Acc ( Matrix Float)  
q = run $ (T2 (unit (0 :: Exp Float)) p)
t = run1 $ \(T2 a b) -> (T2 a b)
f = (\(a,_) -> print a)

main :: IO ()
main = f $ t q
          
