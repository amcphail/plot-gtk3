-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.Gtk.Plot
-- Copyright   :  (c) A. V. H. McPhail 2014
-- License     :  BSD3
--
-- Maintainer  :  haskell.vivian.mcphail <at> gmail <dot> com
-- Stability   :  provisional
-- Portability :  portable
--
-- A 'DrawingArea' widget that displays 'Figure's
--
-----------------------------------------------------------------------------

module Graphics.UI.Gtk.Plot (
                            FigureHandle()
                            -- * Drawing Area
                            , plotNew
                            -- * Attributes
                            , figure
                            ) where

-----------------------------------------------------------------------------

import System.IO.Unsafe

import Control.Concurrent.MVar

import Control.Monad.Trans

import System.Glib.GObject

import Graphics.UI.Gtk

import Graphics.Rendering.Plot.Figure

import Graphics.Rendering.Plot.Render

-----------------------------------------------------------------------------

type FigureHandle = MVar FigureState

-----------------------------------------------------------------------------

-- | create a new 'Figure' plot
--     click on the window to save
plotNew :: FigureHandle -> IO DrawingArea
plotNew f = do
   canvas <- drawingAreaNew
   
   set canvas [maybeFigure := (Just f)]

   _ <- on canvas draw $ liftIO $ do
           (Just drw) <- widgetGetWindow canvas
           fig <- get canvas figure 
           sx  <- widgetGetAllocatedWidth canvas
           sy  <- widgetGetAllocatedHeight canvas
           renderWithDrawWindow drw (renderFigureState fig (sx, sy))

   return canvas

-----------------------------------------------------------------------------

-- | the figure attribute
figure :: Attr DrawingArea FigureState
figure = newAttr getFigure setFigure
   where getFigure o = do
                       Just f <- get o maybeFigure 
                       readMVar f 
         setFigure o f = set o [maybeFigure :~>
                                (\(Just h) -> do
                                    modifyMVar_ h (\_ -> return f)
                                    return $ Just h)]
                                                     
-----------------------------------------------------------------------------

maybeFigure :: Attr DrawingArea (Maybe FigureHandle)
maybeFigure = unsafePerformIO $ objectCreateAttribute
{-# NOINLINE maybeFigure #-}

-----------------------------------------------------------------------------
