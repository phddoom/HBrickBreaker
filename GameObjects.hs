-----------------------------------------------------------------------------
--
-- Module      :  GameObjects
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module GameObjects (Drawable(..), Moveable(..))where
import MAGColor

class Drawable a where
    draw   :: a -> IO ()
    color  :: a -> MAGColor
    render :: a -> Bool

class Moveable a where
    move :: a -> a
