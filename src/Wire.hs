module Wire where

-- | Gives the direction a wire runs, which determines what wires it connects to and propagates values to
data WireType = Vertical | Horizontal | NE | SE | SW | NW | Overlap
  deriving (Eq, Show, Enum, Bounded)

-- | Returns True iff the given wire can propagate values to the north
connectsToNorth :: WireType -> Bool
connectsToNorth Vertical = True
connectsToNorth NE = True
connectsToNorth NW = True
connectsToNorth Overlap = True
connectsToNorth _ = False

connectsToSouth :: WireType -> Bool
connectsToSouth Vertical = True
connectsToSouth SE = True
connectsToSouth SW = True
connectsToSouth Overlap = True
connectsToSouth _ = False

connectsToEast :: WireType -> Bool
connectsToEast Horizontal = True
connectsToEast NE = True
connectsToEast SE = True
connectsToEast Overlap = True
connectsToEast _ = False

connectsToWest :: WireType -> Bool
connectsToWest Horizontal = True
connectsToWest NW = True
connectsToWest SW = True
connectsToWest Overlap = True
connectsToWest _ = False

wireToChar :: WireType -> Char
wireToChar Vertical = '|'
wireToChar Horizontal = '-'
wireToChar NE = '⌞'
wireToChar SE = '⌜'
wireToChar SW = '⌟'
wireToChar NW = '⌞'
wireToChar Overlap = '+'