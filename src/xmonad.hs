import Data.List as L
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.CycleWindows
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers (doRectFloat)
import XMonad.Hooks.Modal
import XMonad.Hooks.StatusBar
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import XMonad.StackSet qualified as W
import XMonad.Util.EZConfig
import XMonad.Util.Font
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
     . modal [layoutMode, spawnMode]
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
    -- Core operations
    --

    -- spawn/kill programs
    [ ("M-a n", spawn myTerminal)
    , ("M-a e", spawn myLauncher)
    , ("M-a q", kill)

    -- modal modes
    , ("M-a l", setMode "layout")
    , ("M-a s", setMode "spawn")


    --
    -- Navigation
    --

    -- cycle windows
    , ("M-<Up>"  , windows W.focusUp  )
    , ("M-<Down>", windows W.focusDown)

    -- cycle workspaces
    , ("M-<Left>" , moveTo Prev hiddenWS)
    , ("M-<Right>", moveTo Next hiddenWS)

    -- move windows in stack
    , ("M-S-<Page_Up>"  , windows W.swapUp  )
    , ("M-S-<Page_Down>", windows W.swapDown)

    -- focus screens
    , ("M-<Home>", prevScreen)
    , ("M-<End>" , nextScreen)

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

    -- resize window proportions
    , ("M-S-<Up>"   , sendMessage MirrorExpand)
    , ("M-S-<Down>" , sendMessage MirrorShrink)
    , ("M-S-<Left>" , sendMessage Shrink)
    , ("M-S-<Right>", sendMessage Expand)

    -- master window operations
    , ("M-<Delete>"  , windows W.focusMaster)
    , ("M-S-<Delete>", windows W.swapMaster )
    , ("M-S-<Home>"  , sendMessage (IncMasterN 1))
    , ("M-S-<End>"   , sendMessage (IncMasterN (-1)))


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
-- Modal Modes
--

layoutMode :: Mode
layoutMode = mode "layout" $ mkKeysEz
    -- jump to layout (exits immediately)
    [ ("t", sendMessage (JumpToLayout "dynamic tiling") >> exitMode)
    , ("m", sendMessage (JumpToLayout "maximised"     ) >> exitMode)
    , ("f", sendMessage (JumpToLayout "fullscreen"    ) >> exitMode)
    ]

spawnMode :: Mode
spawnMode = mode "spawn" $ mkKeysEz
    -- spawn programs (exits immediately)
    [ ("t", spawn myFileManager >> exitMode)
    , ("s", spawn myScrot       >> exitMode)
    ]


--
-- Layouts
--

myLayouts = myTile ||| myMax ||| myFull
  where
    -- our layouts
    myTile     = renamed [Replace "dynamic tiling"] . avoidStruts . myGaps $ ResizableTall nmaster delta ratio []
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
    , ppVisible         = grey0  . wrap " " ""
    , ppHidden          = grey0  . wrap " " ""
    , ppHiddenNoWindows = grey0  . wrap " " ""
    , ppUrgent          = red    . wrap " " ""
    , ppLayout          = aqua   . wrap (grey0 " <fn=1>[</fn> ") (grey0 " <fn=1>]</fn> ")
    , ppOrder           = \case { [ws, l, title, mode] -> [ws, l, mode, title]; xs -> xs }
    , ppExtras          = [lMode]
    }
  where
    lMode = xmobarColorL "#d8a657" "#282828" . fixedWidthL AlignCenter "-" 6 $ logMode


--
-- ManageHook
--

(~?) :: Eq a => Query [a] -> [a] -> Query Bool
q ~? x = fmap (x `L.isInfixOf`) q

myManageHook = composeAll
    [ className =? "Thunar"     --> doRectFloat (W.RationalRect 0.25 0.25 0.5 0.5)
    , className =? "Ristretto"  --> doFloat
    , className =? "Tk"         --> doFloat -- gui development (python)
    , className =? "TkFDialog"  --> doFloat -- ^^
    , className ~? "App"        --> doFloat -- gui development (java)
    ]
