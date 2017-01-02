import Control.Monad
import Control.Monad.State


data Direction = N | S | E | W deriving (Show, Eq)

type Position = (Int, Int)
type Health = Int

-- Items have a flavortext and a function for their action
data Effect = Effect { flavor :: String
                     , function :: (IO ()) -- function type will probably change
                     }
instance Show Effect where
    show = flavor



class Positional a where
    getPos :: a -> Position



data Actor = Actor { friendly :: Bool
                   , posActor :: Position
                   } deriving (Show)

instance Positional Actor where
    getPos = posActor

data Item = Item { nameItem :: String
                 , posItem :: Position
                 , effectItem :: Effect
                 } deriving (Show)
instance Positional Item where
    getPos = posItem


data Room = Room { description :: String
                   , actors :: [Actor]
                   , itemsRoom :: [Item]
                   , doors :: [(Direction, Room)]
                   } deriving (Show)

-- going to be part of the State monad
data Player = Player { healthPlayer :: Health
                     , posPlayer :: Position 
                     , itemsPlayer :: [Item]
                     }
instance Positional Player where
    getPos = posPlayer

data GameState = Game { player :: Player
                      , room :: Room
                      }



overlap :: Positional a => a -> a -> Bool
overlap ob1 ob2 = (getPos ob1) == (getPos ob2)


initialGameState = Game { player = Player {healthPlayer = 100, posPlayer = (0, 0), itemsPlayer = []},
                          room = Room {description = "Big room",
                                        actors = [],
                                        itemsRoom = [],
                                        doors = []
                                        }
                        }
transitionRoom :: Room -> IO ()
transitionRoom x = do
    print (description x)

loop :: StateT GameState IO ()
loop = do
    st <- get
    let r = room st
    return ()

main = do
    transitionRoom (room initialGameState)
    f <- runStateT loop initialGameState
    return ()


a = Actor {friendly = False, posActor = (0, 0)}
b = Actor {friendly = False, posActor = (0, 1)}