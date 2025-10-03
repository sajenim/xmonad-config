import Data.List as L
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.CycleWindows
import XMonad.Actions.RotSlaves
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.StatusBar
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.Spacing
import XMonad.StackSet qualified as W
import XMonad.Util.EZConfig
import XMonad.Util.Loggers

import Graphics.X11.ExtraTypes.XF86

import XMonadConfig.GruvboxMaterial

{- FOURMOLU_DISABLE -}

--
-- The main function
--

main :: IO ()
main = xmonad
     . docks
     . ewmhFullscreen
     . ewmh
     . withSB myXmobar
     $ myConfig

myConfig = def
    { modMask            = myModMask
    , layoutHook         = myLayouts
    , terminal           = myTerminal
    , borderWidth        = myBorderWidth
    , normalBorderColor  = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor
    , workspaces         = myWorkspaces
    , manageHook         = myManageHook
    , keys               = (`mkKeymap` myKeymap)
    } 

--
-- Configuration
--

myTerminal           = "wezterm"
myModMask            = mod4Mask
myBorderWidth        = 5
myNormalBorderColor  = "#32302f"
myFocusedBorderColor = "#32302f"
myWorkspaces         = ["code", "chat", "web", "games", "misc"]
myLauncher           = "dmenu_run -fn 'Fisa Code-10'"
myFileManager        = "thunar"
myScrot              = "scrot -s '%Y%m%d_%H%M%S.png' -e 'mv $f ~/Pictures/scrots/'"


--
-- Keybindings
--

myKeymap =
    --
    -- LeaderKey
    --

    -- spawning programs
    [ ("M-a n", spawn myTerminal   )
    , ("M-a e", spawn myLauncher   )
    , ("M-a t", spawn myFileManager)
    , ("M-a s", spawn myScrot      )

    -- toggling layouts
    , ("M-a d", sendMessage $ JumpToLayout "dynamic tiling")
    , ("M-a m", sendMessage $ JumpToLayout "maximised"     )
    , ("M-a f", sendMessage $ JumpToLayout "fullscreen"    )

    -- quit window
    , ("M-a q", kill)


    --
    -- Navigation
    --

    -- cycle windows
    , ("M-<Up>"  , windows W.focusUp  )
    , ("M-<Down>", windows W.focusDown)

    -- cycle workspaces
    , ("M-<Left>" , moveTo Prev hiddenWS)
    , ("M-<Right>", moveTo Next hiddenWS)

    -- window rotation
    , ("M-S-<Up>",   rotAllUp)
    , ("M-S-<Down>", rotAllDown)

    -- focus screens
    , ("M-S-<Left>",  prevScreen)
    , ("M-S-<Right>", nextScreen)

    -- switch workspaces
    , ("M-1", windows $ W.greedyView "code" )
    , ("M-2", windows $ W.greedyView "chat" )
    , ("M-3", windows $ W.greedyView "web"  )
    , ("M-4", windows $ W.greedyView "games")
    , ("M-5", windows $ W.greedyView "misc" )

    -- send window to workspace
    , ("M-S-1", windows $ W.shift "code" )
    , ("M-S-2", windows $ W.shift "chat" )
    , ("M-S-3", windows $ W.shift "web"  )
    , ("M-S-4", windows $ W.shift "games")
    , ("M-S-5", windows $ W.shift "misc" )

    -- master pane manipulation
    , ("M-<Home>",      sendMessage Shrink)            -- shrink master pane width
    , ("M-<End>",       sendMessage Expand)            -- expand master pane width
    , ("M-<Page_Up>",   sendMessage (IncMasterN 1))    -- more windows in master
    , ("M-<Page_Down>", sendMessage (IncMasterN (-1))) -- fewer windows in master

    -- master window operations
    , ("M-<Delete>",   windows W.focusMaster)
    , ("M-S-<Delete>", windows W.swapMaster )


    --
    -- Multimedia
    --

    , ("<XF86AudioPlay>"       , spawn "mpc toggle")
    , ("<XF86AudioStop>"       , spawn "mpc stop"  )
    , ("<XF86AudioNext>"       , spawn "mpc next"  )
    , ("<XF86AudioPrev>"       , spawn "mpc prev"  )
    , ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ -10%")
    , ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ +10%")
    ]


--
-- Layouts
--

myLayouts = myTile ||| myMax ||| myFull
  where
    -- our layouts
    myTile     = renamed [Replace "dynamic tiling"] . avoidStruts . myGaps $ Tall nmaster delta ratio
    myMax      = renamed [Replace "maximised"     ] . avoidStruts . myGaps $ Full
    myFull     = renamed [Replace "fullscreen"    ] . noBorders            $ Full
    -- add a configurable amount of space around windows.
    myGaps     = spacingRaw False (Border 10 10 10 10) True (Border 10 10 10 10) True
    -- layout configuration
    nmaster    = 1      -- Default number of windows in the master pane
    ratio      = 2/3    -- Default proportion of screen occupied by master pane
    delta      = 3/100  -- Percent of screen to increment by when resizing panes


--
-- Statusbar
--

myXmobar = statusBarPropTo "_XMONAD_LOG_1" "xmobar" (pure myXmobarPP)

myXmobarPP :: PP
myXmobarPP = def
    { ppSep             = grey0         " "
    , ppCurrent         = blue   . wrap " " ""
    , ppTitle           = grey2  . wrap (grey0 " <fn=1>[</fn> ") (grey0 " <fn=1>]</fn>") . shorten 32
    , ppVisible         = purple . wrap " " ""
    , ppHidden          = grey0  . wrap " " ""
    , ppHiddenNoWindows = grey0  . wrap " " ""
    , ppUrgent          = red    . wrap " " ""
    , ppLayout          = aqua   . wrap (grey0 " <fn=1>[</fn> ") (grey0 " <fn=1>]</fn> ")
    , ppOrder           = \[ws, l, _] -> [ws, l]
    }


--
-- ManageHook
--

(~?) :: Eq a => Query [a] -> [a] -> Query Bool
q ~? x = fmap (x `L.isInfixOf`) q

myManageHook = composeAll
    [ className =? "Thunar"     --> doFloat
    , className =? "Ristretto"  --> doFloat
    , className =? "Tk"         --> doFloat -- gui development (python)
    , className =? "TkFDialog"  --> doFloat -- ^^
    , className ~? "App"        --> doFloat -- gui development (java)
    ]
