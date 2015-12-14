module Main where

data Reindeer = Reindeer
  { speed     :: Int   -- ^ units of distance flown per second
  , stamina   :: Int   -- ^ number of seconds flown before rest
  , breaktime :: Int   -- ^ number of seconds rested before flying
  , points    :: Int   -- ^ number of points accumulated
  , position  :: Int   -- ^ units of distance flown
  , statetime :: Int   -- ^ time remaining until state change
  , state     :: State -- ^ current activity
  }

data State = Flying | Resting

main :: IO ()
main =
  do start <- loadInput
     let end = iterate raceStep start !! 2503
     print (maximum (map position end))
     print (maximum (map points end))

loadInput :: IO [Reindeer]
loadInput = map parseLine . lines <$> readFile "input14.txt"

parseLine :: String -> Reindeer
parseLine str =
  case words str of
    [_, _, _, n, _, _, m, _, _, _, _, _, _, o, _] ->
       Reindeer { speed     = read n
                , stamina   = read m
                , breaktime = read o
                , points    = 0
                , position  = 0
                , state     = Flying
                , statetime = read m
                }
    _ -> error str

-- | Advance all 'Reindeer' in the race one second and award points.
raceStep :: [Reindeer] -> [Reindeer]
raceStep rs = rs2
  where
  rs1  = map reindeerStep rs
  best = maximum (map position rs1)
  rs2  = map (awardPoint best) rs1

-- | Award a point if this 'Reindeer' is at the current best position.
awardPoint :: Int -> Reindeer -> Reindeer
awardPoint best r
  | position r == best = r { points = points r + 1 }
  | otherwise          = r

-- | Update a reindeer for one second of the race. Move him if he's flying.
reindeerStep :: Reindeer -> Reindeer
reindeerStep r =
  case state r of
    Flying
      | statetime r > 0 ->
         r { position = position r + speed r, statetime = statetime r - 1 }
      | otherwise ->
         reindeerStep r { state = Resting, statetime = breaktime r }
    Resting
      | statetime r > 0 ->
         r { statetime = statetime r - 1 }
      | otherwise ->
         reindeerStep r { state = Flying, statetime = stamina r }
