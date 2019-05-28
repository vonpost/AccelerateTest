import Data.Array.Accelerate.LLVM.PTX
import Data.Array.Accelerate
import qualified Prelude as P

type Parameter = Matrix Float
type Input = Vector Float
type Output = Vector Float
type Loss = Scalar Float

someModel :: Acc (Parameter) -> Acc (Input,Output) -> Acc (Parameter)
someModel p (T2 a b) = map (* 0.1) p 
lossFunction :: Acc (Parameter) -> Acc (Loss) 
lossFunction = foldAll (+) 0   
train :: Acc ((Loss, Parameter), (Input,Output)) -> Acc (Loss, Parameter)
train (T2 (T2 loss p) (T2 a b)) =  let newParameter = someModel p (T2 a b)
                                   in (T2 (unit $ (the loss) + (the $ lossFunction newParameter))
                                        newParameter)

run1Model :: (Loss, Parameter) -> (Input,Output) -> (Loss, Parameter)
run1Model = (\(l,p) (a,b) -> run1Model' ((l,p),(a,b)) ) 
  where run1Model' = run1 train

trainingExamples :: [(Input, Output)]
trainingExamples = P.take 1000 $ P.cycle $ P.map (\[a,b,c] -> (fromList (Z:.2) [a,b] :: Input, fromList (Z:.1) [c] :: Output)) [[0,0,0],[0,1,0],[1,1,1],[1,0,1]]

trainingLog :: Loss -> Parameter -> [(Loss,Parameter)]
trainingLog l p = P.scanl run1Model (l,p) trainingExamples

main :: P.IO ()
main = do
  let l = run $ unit 0
  let p = run $ fill (constant $ Z:.5000:.5000) 1
  P.mapM_ (\(loss,_) -> P.print loss) (trainingLog l p)
          
