import Control.Monad.Writer
import Control.Monad.State

data Pole = Pole { bLhs :: Int, bRhs :: Int} deriving (Show)

sitLeft :: Int -> Pole -> Maybe Pole
sitLeft n pole = makeNextPole ((bLhs pole) + n) (bRhs pole)

sitRight :: Int -> Pole -> Maybe Pole
sitRight n pole = makeNextPole (bLhs pole) ((bRhs pole) + n)

makeNextPole :: Int -> Int -> Maybe Pole
makeNextPole l r 
	| abs (l - r) > 2 = Nothing
	| otherwise = Just (Pole l r)

startingPole = Pole 0 0
failureTolerantComputations = do { p1 <- sitLeft 1 startingPole; 
                                   p2 <- sitLeft 1 p1; 
								   p3 <- sitRight 5 p2;  -- Nothing after here... 
								   p4 <- sitLeft 1 p3;
								   p5 <- sitLeft 1 p4;
								   return p5;
                                 }
								 
-- Adding logging to sitLeft and sitRight
-- When the pole becomes unbalanced and transforms to 'Nothing' future computations should still be logged
sitLeftUsingWriter :: Int -> Maybe Pole -> Writer String (Maybe Pole)
sitLeftUsingWriter n Nothing = writer (Nothing, " [sitLeft " ++ (show n) ++ "] on Nothing")
sitLeftUsingWriter n (Just pole) = let newPole = (sitLeft n pole) in writer (newPole, " [sitLeft " ++ (show n) ++ "] yeilding " ++ show newPole)

sitRightUsingWriter :: Int -> Maybe Pole -> Writer String (Maybe Pole)
sitRightUsingWriter n Nothing = writer (Nothing, " [sitRight " ++ (show n) ++ "] on Nothing")
sitRightUsingWriter n (Just pole) = let newPole = (sitRight n pole) in writer (newPole, " [sitRight " ++ (show n) ++ "] yeilding " ++ show newPole)

failureTolerantComputationsWithLogging = do { 
								   p1 <- sitLeftUsingWriter 1 (Just startingPole); 
                                   p2 <- sitLeftUsingWriter 1 p1; 
								   p3 <- sitRightUsingWriter 5 p2;  -- Nothing after here... 
								   p4 <- sitLeftUsingWriter 1 p3;
								   p5 <- sitLeftUsingWriter 1 p4;
								   return p5;
                                 }
-- Using WriterT
-- The inner monad is transformed into a 'm (a, w)' where m is the inner monad
-- In our case, it is a Maybe (Pole, String). 
-- As computations are sequenced one after the other, the Maybe inner monad can become a Nothing. In which case, subsequent computations yield Nothing and we would lose the logging part.

describe :: Int -> String -> Maybe Pole -> Maybe (Pole, String)
describe numBirds poleSide poleResult = do { p <- poleResult;
                  return (p, show numBirds ++ " birds " ++ poleSide ++ " yielding " ++ (show p))
				}

sitLeftWithLogging :: Int -> Pole -> WriterT String Maybe Pole
sitLeftWithLogging n pole = let m = sitLeft n pole in WriterT $ describe n "sitLeft" m

sitRightWithLogging :: Int -> Pole -> WriterT String Maybe Pole
sitRightWithLogging n pole = let m = sitRight n pole in WriterT $ describe n "sitRight" m

sitSequence = do { p1 <- sitLeftWithLogging 1 startingPole; 
                   p2 <- sitLeftWithLogging 10 p1;
                   return p2
				 }

-- Model the Pole oprations (sitLeft and sitRight) as a State computation that operate on a state (Pole)
-- Advantages: we can have a sequnce of state computations as one operation that take an initial state as a parameter. The state is then changed by every computation and passed on to the next computation.
-- For now model a sitLeft and sitRight such that the state is a (Maybe Pole)
sitLeftModifyState :: Int -> State (Maybe Pole) ()
sitLeftModifyState n = state (\s -> ((), do {pole <- s; sitLeft n pole}))

sitRightModifyState :: Int -> State (Maybe Pole) ()
sitRightModifyState n = state (\s -> ((), do {pole <- s; sitRight n pole}))

sitStateSteps = do { sitLeftModifyState 1; sitRightModifyState 2; sitRightModifyState 2; sitLeftModifyState 2;}