module Example.ArrowTutorial where

import Control.Arrow

import Control.Category
import Prelude hiding ((.), id)

import Control.Applicative

newtype State' s a =
    State' {
        runState' :: s -> (a, s) 
    }
    
data State s a =
    State {
        usesPut :: Bool,
        runState :: s -> (a, s)
    }

get :: State s s
get = State False (\s -> (s, s))

put :: s -> State s ()
put s = State True (\_ -> ((), s))

instance Functor (State s) where
    fmap f (State m c) =
        State m $ \s' ->
            let (x, s) = c s'
            in (f x, s)
            
instance Applicative (State s) where
    pure x = State False (\s -> (x, s))
    
    State mf cf <*> State mx cx =
        State (mf || mx) $ \s'' ->
            let (f, s') = cf s''
                (x, s) = cx s'
            in (f x , s)
            
instance Monad (State s) where
    return = pure
    
    State mf cf >>= f =
        State (mf || undefined) $ \s'' ->
            let (x', s') = cf s''
            in runState (f x') s'
            

getMult :: Num s => s -> State s s
getMult f = State False $ \s -> (f*s, s)

data StateArrow s a b =
    StateA {
        usesPutA    :: Bool,
        runStateArrow :: (a, s) -> (b, s)
    }
    
getMultA :: Num s => StateArrow s s s
getMultA = StateA False $ \(f, s) -> (f*s, s)

getA :: StateArrow s a s
getA = StateA False $ \(_, s) -> (s, s)

putA :: StateArrow s s ()
putA = StateA True $ \(s, _) -> ((), s)

class Category cat where
    id :: cat a a
    (.) :: cat b c -> cat a b -> cat a c
    
            