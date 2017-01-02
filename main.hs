data Direction = N | S | E | W deriving (Show, Eq)

type Position = (Int, Int)

data Effect = Effect { flavor :: String
			         , function :: (IO ()) 
			     	 }
instance Show Effect where
	show = flavor



class Positional a where
	getPos :: a -> Position

--data Positional a = Positional { getPos :: a -> Position }


data Actor = Actor { friendly :: Bool
				   , posActor :: Position
				   } deriving (Show)
instance Positional Item where
	getPos = posActor

data Item = Item { name :: String
				 , posItem :: Position
				 , effect :: Effect
				 } deriving (Show)
instance Positional Item where
	getPos = posItem





data Room = Room { description :: String
				   , actors :: [Actor]
				   , items :: [Item]
				   , doors :: [Direction]
				   } deriving (Show)
