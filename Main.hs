import Control.Applicative
import Control.Monad
import Data.Default
import FRP.Sodium
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import Data.Text.Format
import Text.Read (readMaybe)

import qualified JavaScript.JQuery as J

newHeader textValue parent = do  
  el <- J.select "<h1>HAI</h1>"
  el `J.appendJQuery` parent
  sync $ listen (value textValue) (void . (`J.setText` el))
  return el

newInput initValue parent = do
  el <- J.select "<input>"
  J.setVal initValue el
  J.appendJQuery el parent
  (currValue, writeValue) <- sync $ newBehavior initValue
  let handler _ = sync . writeValue =<< J.getVal el
  J.on handler "keyup change" def el
  return (el, currValue)

newOption :: [(T.Text, a)] -> J.JQuery -> IO (J.JQuery, Behavior a)
newOption options parent = do
  let (defOptKey, defOptVal):_ = options
  el <- J.select "<select size='10'>"
  forM_ (zip [0..] options) $ \ (i :: Int, (key, _)) ->
    let html = format "<option value=\"{}\">{}</select>" (i, key)
     in J.append (L.toStrict html) el
  J.appendJQuery el parent
  (currValue, writeValue) <- sync $ newBehavior defOptVal
  let
    handler _ = do
      i <- read . T.unpack <$> J.getVal el
      sync $ writeValue (snd $ options !! i)
  J.setVal "0" el
  J.on handler "change" def el
  return (el, currValue)

main = do
  (mousePos, writeMousePos) <- sync $ newBehavior (0, 0)
  let
    moveHandler jqEv = do
      x <- J.pageX jqEv
      y <- J.pageY jqEv
      sync $ writeMousePos (x, y)
  body <- J.select "body"
  J.mousemove moveHandler def body

  mapM (\ x -> J.setCss "height" "100%" =<< J.select x) ["body", "html"]

  let
    addTuple (x, y) (a, b) = (x + a, y + b)
    showText = T.pack . show
    accumPos = do
      rec s <- hold (0, 0) $ snapshot addTuple (updates mousePos) s
      return s

  newHeader (showText <$> mousePos) body
  newHeader (fmap (showText . uncurry (flip (,))) mousePos) body
  lolPos <- sync accumPos
  newHeader (showText <$> lolPos) body

  J.append "<hr />" body

  (_, lhsVal) <- newInput "0" body
  (_, rhsVal) <- newInput "0" body
  (_, opVal)  <- newOption [("+", (+)), ("-", (-))] body
  let
    toInt :: T.Text -> Maybe Int
    toInt = readMaybe . T.unpack
    sumVal = (liftA2 <$> opVal) <*> (toInt <$> lhsVal) <*> (toInt <$> rhsVal)
  newHeader (fmap (maybe "invalid input" (T.pack . show)) sumVal) body

  return ()
