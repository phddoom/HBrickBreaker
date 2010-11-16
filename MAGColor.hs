module MAGColor where
import Graphics.Rendering.OpenGL

type MAGColors = [MAGColor]

data MAGColor = MAGColor { red   :: GLfloat,
                           green :: GLfloat,
                           blue  :: GLfloat,
                           alpha :: GLfloat
                         } deriving (Show)

fromRGB :: Float -> Float -> Float -> MAGColor
fromRGB r g b = MAGColor { red = (r / 255.0),
                           green = (g / 255.0),
                           blue = (b / 255.0),
                           alpha = 0
                         }

fromRGBA :: Float -> Float -> Float -> Float -> MAGColor
fromRGBA r g b a = (fromRGB r g b){alpha = a}

setColor :: MAGColor -> IO()
setColor (MAGColor r g b a) = color $ Color4 r g b a
