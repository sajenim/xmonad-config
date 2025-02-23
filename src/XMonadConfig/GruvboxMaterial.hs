-- |
-- Module: XMonadConfig.GruvboxMaterial
-- Author: Jasmine Marie Wilson
--
-- Creates xmobar escape codes for use in our status bar,
-- Based on Gruvbox Material Dark and Hard by sainnhe.
module XMonadConfig.GruvboxMaterial
  ( background
  , foreground
  , bg0
  , bg1
  , fg0
  , fg1
  , red
  , orange
  , yellow
  , green
  , aqua
  , blue
  , purple
  , grey0
  , grey1
  , grey2
  ) where

import XMonad.Hooks.StatusBar.PP (xmobarColor)

background :: String
background = "#1d2021"

foreground :: String
foreground = "#d4be98"

bg0, bg1, fg0, fg1, red, orange, yellow, green, aqua, blue, purple, grey0, grey1, grey2 :: String -> String
{- FOURMOLU_DISABLE -}
bg0    = xmobarColor "#1d2021" ""
bg1    = xmobarColor "#282828" ""
fg0    = xmobarColor "#d4be98" ""
fg1    = xmobarColor "#ddc7a1" ""
red    = xmobarColor "#ea6962" ""
orange = xmobarColor "#e78a4e" ""
yellow = xmobarColor "#d8a657" ""
green  = xmobarColor "#a9b665" ""
aqua   = xmobarColor "#89b482" ""
blue   = xmobarColor "#7daea3" ""
purple = xmobarColor "#d3869b" ""
grey0  = xmobarColor "#7c6f64" ""
grey1  = xmobarColor "#928374" ""
grey2  = xmobarColor "#a89984" ""
{- FOURMOLU_ENABLE -}
