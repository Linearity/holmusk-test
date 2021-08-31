{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Sim where

import Control.Monad.State
import Data.Foldable
import Data.Maybe
import qualified Data.Sequence as S
import Optics
import System.Random
import Debug.Trace
import Data.Traversable

data SimConfig = SimConfig
    { arrival :: ArrivalParameters
    , service :: ServiceParameters
    }

data ArrivalParameters = ArrivalParameters
    { arrivalAlpha :: Double
    }

data ServiceParameters = ServiceParameters
    { serviceAlpha :: Double
    , serviceBeta :: Double
    , serviceRho :: Double
    }

serviceYellow :: ServiceParameters
serviceYellow = ServiceParameters 2 5 200

serviceRed :: ServiceParameters
serviceRed = ServiceParameters 2 2 200

serviceBlue :: ServiceParameters
serviceBlue = ServiceParameters 5 1 200

type Duration = Double

data Customer = Customer
    { timeServing :: Duration
    , timeWaiting :: Duration
    }
    deriving Show

_timeServing :: Lens Customer Customer Duration Duration
_timeServing = lens timeServing (\c x -> c { timeServing = x })

_timeWaiting :: Lens Customer Customer Duration Duration
_timeWaiting = lens timeWaiting (\c x -> c { timeWaiting = x })

data SimState = SimState
    { rng :: StdGen
    , queue :: S.Seq Customer
    , sinceLastArrival :: Duration
    , waitTimes :: [Duration]
    }

_rng :: Lens SimState SimState StdGen StdGen
_rng = lens rng (\s x -> s { rng = x })

_queue :: Lens SimState SimState (S.Seq Customer) (S.Seq Customer)
_queue = lens queue (\s x -> s { queue = x })

_sinceLastArrival :: Lens SimState SimState Duration Duration
_sinceLastArrival = lens sinceLastArrival (\s x -> s { sinceLastArrival = x })

_waitTimes :: Lens SimState SimState [Duration] [Duration]
_waitTimes = lens waitTimes (\s x -> s { waitTimes = x })

rollRng :: MonadState SimState m => (StdGen -> (b, StdGen)) -> m b
rollRng f = do
    g <- use _rng
    let (x, g') = f g
    assign _rng g'
    return x

simulation :: SimConfig -> SimState -> [Duration] -> ([S.Seq Customer], SimState)
simulation config state0 dts = flip runState state0 $ do
    for dts $ \dt -> do
        x1 <- rollRng (randomR (0, 1))
        probA <- gets (arrivalProb alpha . view _sinceLastArrival)
        if x1 < probA
            then customerArrives config             -- arrival during last time step
            else modifying _sinceLastArrival (+ dt) -- one further time step since last arrival
        serve dt                                    -- one time step of service
        modifying _queue $ \case
            S.Empty -> S.Empty
            c S.:<| rest -> c <| ((_timeWaiting %~ (+ dt)) <$> rest)
        use _queue
  where
    alpha = arrivalAlpha (arrival config)

customerArrives :: MonadState SimState m => SimConfig -> m ()
customerArrives (SimConfig _ (ServiceParameters alpha beta rho)) = do
    x2 <- rollRng (randomR (0, 1))
    let c = Customer
            { timeServing = serviceTime rho alpha beta x2
            , timeWaiting = 0
            }
    modifying _queue (|> c)
    assign _sinceLastArrival 0

serve :: MonadState SimState m => Duration -> m ()
serve dt
    | dt <= 0 = return ()                   -- no time to serve anybody
    | otherwise = do
        q <- use _queue
        case q of
            S.Empty -> return ()
            c S.:<| rest -> do              -- a customer to be served
                let d = c ^. _timeServing - dt
                if d <= 0                   -- customer done in this time step
                    then do
                        modifying _waitTimes ((c ^. _timeWaiting) :)
                        assign _queue rest
                        serve (-d)          -- use remaining time on next customers
                    else assign _queue ((c & _timeServing .~ d) <| rest)

arrivalProb :: Floating a => a -> a -> a
arrivalProb alpha t = 1 - exp (- t / alpha)

serviceTime :: Floating a => a -> a -> a -> a -> a
serviceTime rho alpha beta x = rho * (x ** (alpha - 1)) * ((1 - x) ** (beta - 1))