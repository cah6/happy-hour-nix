{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE NamedFieldPuns #-}
module Frontend where

import Control.Lens.Getter
import Control.Monad (forM_)
import Control.Monad.Trans (liftIO)
import qualified Data.Map.Lazy as M
import Data.Monoid ((<>))
import qualified Data.Text as T
import Reflex.Dom 
import Reflex.Dom.Core

import Common.Api
import Static

frontend :: (StaticWidget x (), Widget x ())
frontend = (head', body)
  where
    head' = do
      el "title" $ text "Happy Hour Listings!"
      elAttr "link" ("href" =: "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css" 
                  <> "rel" =: "stylesheet" 
                  <> "type" =: "text/css") $ return ()
      --   do 
      -- el "title" $ text "Happy Hour Listings!"
      -- styleSheet "css/bootstra-grid.css"
    -- body = do
    --   text "Happy hour times:"
    --   el "p" $ text $ T.pack "this is a string"
    --   elAttr "img" ("src" =: static @"obelisk.jpg") blank

body :: MonadWidget t m => m ()
body = panel $ grid $ mdo
  eHHs <- liftIO loadHHs
  case eHHs of
    (Left error) -> text $ "Could not load list due to: " <> T.pack error
    (Right res)  -> do 
      elAttr "table" ("class" =: "table") $ do 
        el "thead" $ el "tr" $ do
          mapM_ (\name -> elAttr "th" ("scope" =: "col") $ text name) cols
        el "tbody" $ mapM_ mkRow res 

cols :: [T.Text]
cols = ["Restaurant", "Time", "Description"]

mkRow :: MonadWidget t m 
       => HappyHour
       -> m ()
mkRow hh = forM_ (_schedule hh) $ \schedule ->
  let
    c1 = text $ _restaurant hh
    c2 = text $ times schedule
    c3 = text $ _scheduleDescription schedule
  in 
    row c1 c2 c3

times :: Schedule -> T.Text
times Schedule{ _days, _time } = 
  let 
    days = printDays _days
    time = printTimeRange _time
  in 
    days <> " " <> time

flattenHH :: HappyHour -> [HappyHour]
flattenHH hh@HappyHour{_schedule, ..} = map (\s -> hh & schedule .~ [s]) $ _schedule

test :: (MonadWidget t m)
  => [HappyHour]
  -> m (Dynamic t (M.Map Int (El t, [()])))
test xs = let
  hhMap :: M.Map Int HappyHour
  hhMap = M.fromList $ zip [1..] (concatMap flattenHH xs) 
  in 
  tableDynAttr "container" 
    [ ("Restaurant", restaurantEntry)
    , ("Time", restaurantEntry)
    , ("Description", restaurantEntry)
    ]
    (constDyn hhMap)
    rowKeyToValue

rowKeyToValue :: (MonadWidget t m, Show k, Ord k)
  => k 
  -> m (Dynamic t (M.Map T.Text T.Text))
rowKeyToValue key = return $ constDyn mempty

restaurantEntry :: MonadWidget t m 
  => Int
  -> Dynamic t HappyHour 
  -> m ()
restaurantEntry k dynHH = elClass "div" "col-md-2" $ (text "Restaurant") -- dynText $ mapDyn (\hh -> hh ^. restaurant) dHH

getRestaurant :: MonadWidget t m => Dynamic t HappyHour -> m (Dynamic t T.Text)
getRestaurant dHH = mapDyn (\hh -> hh ^. restaurant) dHH

grid ::
  MonadWidget t m =>
  m a ->
  m a
grid =
  elClass "div" "container"

row ::
  MonadWidget t m =>
  m a ->
  m b ->
  m c ->
  m c
row ma mb mc = elClass "div" "row" $ do
    elClass "div" "col-md-2" ma
    elClass "div" "col-md-3" mb
    elClass "div" "col-md-4" mc

panel ::
  MonadWidget t m =>
  m a ->
  m a
panel =
  divClass "panel panel-default" . divClass "panel-body"