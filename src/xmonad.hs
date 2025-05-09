import Data.Map qualified as M
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.Navigation2D
import XMonad.Actions.Submap
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.Modal
import XMonad.Hooks.StatusBar
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.Spacing
import XMonad.StackSet qualified as W
import XMonad.Util.EZConfig (additionalKeysP)
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
     . withNavigation2DConfig def 
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
    } `additionalKeysP` myKeybindings


--
-- Configuration
--

myTerminal           = "wezterm"
myModMask            = mod4Mask
myBorderWidth        = 5
myNormalBorderColor  = "#32302f"
myFocusedBorderColor = "#32302f"
myWorkspaces         = ["code", "chat", "web", "games", "misc"]
myLauncher           = "dmenu_run"
myFileManager        = "thunar"
myScrot              = "scrot -s '%Y%m%d_%H%M%S.png' -e 'mv $f ~/Pictures/scrots/'"
volumeDown           = "pactl set-sink-volume @DEFAULT_SINK@ -10%"
volumeUp             = "pactl set-sink-volume @DEFAULT_SINK@ +10%"


--
-- Keybindings
--

myKeybindings =
    --
    -- LeaderKey
    --

    -- spawning programs
    [ ("M-a s t", spawn myTerminal   )
    , ("M-a s d", spawn myLauncher   )
    , ("M-a s f", spawn myFileManager)
    , ("M-a s s", spawn myScrot      )

    -- kill/exit
    , ("M-a c", kill          )
    , ("M-a q", io exitSuccess)

    -- directional navigation of windows
    , ("M-a o", windowGo   U False)
    , ("M-a n", windowGo   L False)
    , ("M-a e", windowGo   D False)
    , ("M-a i", windowGo   R False)

    -- switch workspaces
    , ("M-a j", windows $ W.greedyView "code" )
    , ("M-a v", windows $ W.greedyView "chat" )
    , ("M-a d", windows $ W.greedyView "web"  )
    , ("M-a r", windows $ W.greedyView "games")
    , ("M-a s", windows $ W.greedyView "misc" )

    -- focus master window
    , ("M-a <Space>", windows W.focusMaster)

    -- toggling layouts
    , ("M-a t", sendMessage $ JumpToLayout "dynamic tiling"        )
    , ("M-a b", sendMessage $ JumpToLayout "binary space partition")
    , ("M-a m", sendMessage $ JumpToLayout "maximised"             )
    , ("M-a f", sendMessage $ JumpToLayout "fullscreen"            )


    --
    -- Modifier Keys
    --

    -- directional navigation of windows
    , ("M-<Up>"     , windowGo   U False)
    , ("M-<Left>"   , windowGo   L False)
    , ("M-<Down>"   , windowGo   D False)
    , ("M-<Right>"  , windowGo   R False)

    -- swap adjacent windows
    , ("M-S-<Up>"   , windowSwap U False)
    , ("M-S-<Left>" , windowSwap L False)
    , ("M-S-<Down>" , windowSwap D False)
    , ("M-S-<Right>", windowSwap R False)

    -- enable edit mode
    , ("M1-<Space>", setMode "edit")


    --
    -- Multimedia
    --

    , ("<XF86AudioPlay>"       , spawn "mpc toggle")
    , ("<XF86AudioStop>"       , spawn "mpc stop"  )
    , ("<XF86AudioNext>"       , spawn "mpc next"  )
    , ("<XF86AudioPrev>"       , spawn "mpc prev"  )
    , ("<XF86AudioLowerVolume>", spawn volumeDown  )
    , ("<XF86AudioRaiseVolume>", spawn volumeUp    )
    ]


--
-- Modal keybindings
--

editMode :: Mode
editMode = mode "edit" $ mkKeysEz
    -- directional navigation of windows
    [ ("o", windowGo   U False)
    , ("n", windowGo   L False)
    , ("e", windowGo   D False)
    , ("i", windowGo   R False)

    -- swap adjacent windows
    , ("S-o", windowSwap U False)
    , ("S-n", windowSwap L False)
    , ("S-e", windowSwap D False)
    , ("S-i", windowSwap R False)

    -- expand windows
    , ("M1-o", sendMessage $ ExpandTowardsBy U 0.01)
    , ("M1-n", sendMessage $ ExpandTowardsBy L 0.01)
    , ("M1-e", sendMessage $ ExpandTowardsBy D 0.01)
    , ("M1-i", sendMessage $ ExpandTowardsBy R 0.01)

    -- shrink windows
    , ("M1-C-o", sendMessage $ ShrinkFromBy U 0.01)
    , ("M1-C-n", sendMessage $ ShrinkFromBy L 0.01)
    , ("M1-C-e", sendMessage $ ShrinkFromBy D 0.01)
    , ("M1-C-i", sendMessage $ ShrinkFromBy R 0.01)

    -- shrink/expand the master area
    , ("M-f", sendMessage Shrink)
    , ("M-u", sendMessage Expand)

    -- number of windows in the master area
    , ("S-f", sendMessage (IncMasterN 1)   )
    , ("S-u", sendMessage (IncMasterN (-1)))

    -- swap/rotate
    , ("M1-r", sendMessage Rotate)
    , ("M1-s", sendMessage Swap  )

    -- split shift
    , ("M1-f", sendMessage $ SplitShift Prev)
    , ("M1-u", sendMessage $ SplitShift Next)
    ]


--
-- Layouts
--

myLayouts = myTile ||| myBsp ||| myMax ||| myFull
  where
    -- our layouts
    myTile     = renamed [Replace "dynamic tiling"        ] . avoidStruts . myGaps $ Tall nmaster delta ratio
    myBsp      = renamed [Replace "binary space partition"] . avoidStruts . myGaps $ emptyBSP
    myMax      = renamed [Replace "maximised"             ] . avoidStruts . myGaps $ Full
    myFull     = renamed [Replace "fullscreen"            ] . noBorders            $ Full
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
    , ppVisible         = purple . wrap " " ""
    , ppHidden          = grey0  . wrap " " ""
    , ppHiddenNoWindows = grey0  . wrap " " ""
    , ppUrgent          = red    . wrap " " ""
    , ppLayout          = aqua   . wrap (grey0 " <fn=1>[</fn> ") (grey0 " <fn=1>]</fn> ")
    , ppOrder           = \[ws, l, _] -> [ws, l]
    , ppExtras          = [logMode]
    }


--
-- ManageHook
--

myManageHook = composeAll
    [ className =? "Thunar"    --> doFloat
    , className =? "Ristretto" --> doFloat
    , className =? "Tk"        --> doFloat -- python gui development
    , className =? "TkFDialog" --> doFloat -- ^^
    ]
