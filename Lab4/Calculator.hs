-- This module is a starting point for implementing the Graph Drawing
-- Calculator as described in Part II of the Standard Lab. You can use this
-- directly, or just study it as an example of how to use threepenny-gui.

import ThreepennyPages
import Expr
import Data.Maybe
import Graphics.UI.Threepenny.Core as UI
import qualified Graphics.UI.Threepenny as UI

canWidth,canHeight :: Num a => a
canWidth  = 300
canHeight = 300

main :: IO ()
main = startGUI defaultConfig setup

setup :: Window -> UI ()
setup window =
  do -- Create them user interface elements
     canvas  <- mkCanvas canWidth canHeight   -- The drawing area
     fx      <- mkHTML "<i>f</i>(<i>x</i>)="  -- The text "f(x)="
     input   <- mkInput 20 "x"                -- The formula input
     draw    <- mkButton "Draw graph"         -- The draw button
     zoom    <- mkSlider (1, 100) 10          -- The zoom slider
     diff    <- mkButton "Differentiate"      -- The differentiate button
       -- The markup "<i>...</i>" means that the text inside should be rendered
       -- in italics.

     -- Add the user interface elements to the page, creating a specific layout
     formula <- row [pure fx,pure input, pure diff]
     getBody window #+ [column [pure canvas,pure formula,pure draw, pure zoom]]

     -- Styling
     getBody window # set style [("backgroundColor","lightblue"),
                                 ("textAlign","center")]
     pure input # set style [("fontSize","14pt")]

     -- Interaction (install event handlers)
     on UI.click     draw  $ \ _ -> readAndDraw input canvas zoom False
     on UI.click     diff  $ \ _ -> readAndDraw input canvas zoom True
     on valueChange' input $ \ _ -> readAndDraw input canvas zoom False
     on valueChange' zoom  $ \ _ -> readAndDraw input canvas zoom False


readAndDraw :: Element -> Canvas -> Element -> Bool ->UI ()
readAndDraw input canvas zoom d =
  do -- Get the current formula (a String) from the input element
     formula <- get value input
     
     let formula'
            | d         = differentiate $ fromJust (readExpr formula)
            | otherwise = fromJust $ readExpr formula
     -- Clear the canvas
     clearCanvas canvas
     set value input (showExpr formula') 
     -- The following code draws the formula text in the canvas and a blue line.
     -- It should be replaced with code that draws the graph of the function.
     scale <- get value zoom
     let scale' = read scale

     set UI.fillStyle (UI.solidColor (UI.RGB 0 0 0)) (pure canvas)
     UI.fillText formula (10,canHeight/2) canvas
     path "blue" (points formula' scale' (300,300)) canvas        -- [(10,10),(canWidth-10,canHeight/2)] canvas


points :: Expr -> Double -> (Int,Int) -> [Point]
points exp scale (width,height) = [(fromIntegral d, realToPix (eval' (inVal d) *scale))| d <- [0..width]]
      where 
          eval'     = eval exp'
          exp'      = simplify exp
          inVal x' = pixToReal (fromIntegral x') / scale


  -- converts a pixel x-coordinate to a real x-coordinate
pixToReal :: Double -> Double
pixToReal x = x-(canWidth/2)

  -- converts a real y-coordinate to a pixel y-coordinate
realToPix :: Double -> Double
realToPix y = canHeight/2-y




