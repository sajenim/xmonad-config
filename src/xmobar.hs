import Xmobar

import XMonad.Hooks.StatusBar.PP (wrap)

import XMonadConfig.GruvboxMaterial

-- | Configures how things should be displayed on the bar
config :: Config
config =
  defaultConfig
    { font = "Fisa Code Book Italic 10"
    , additionalFonts =
        [ "Fisa Code Bold 10"
        , "Symbols Nerd Font 2048-em 24"
        ]
    , bgColor = background
    , fgColor = foreground
    , border = BottomB
    , borderColor = "#32302f"
    , borderWidth = 5
    , position = Static {xpos = 1920, ypos = 0, width = 2560, height = 28}
    , commands = myCommands
    , sepChar = "%"
    , alignSep = "}{"
    , --  The output template is how xmobar will end up printing all of our configured commands.
      template =
        myLogo
          ++ wrap " " " " (green "%uname%")
          ++ "%uptime%"
          ++ "%_XMONAD_LOG_1%"
          ++ "} %date% {"
          ++ "%YPJT% %disku%"
    }
 where
  myLogo :: String
  myLogo = wrap " " " " "<fn=2>\59255</fn>"

  -- Commands to run xmobar modules on start
  myCommands :: [Runnable]
  myCommands =
    [ --  XPropertyLog PropName
      Run $ XPropertyLog "_XMONAD_LOG_1"
    , -- Com ProgramName Args Alias RefreshRate
      Run $
        Com
          "uname"
          ["-r", "-s"]
          ""
          (0 `seconds`)
    , --  DateZone Format Locale Zone Alias RefreshRate
      Run $
        DateZone
          (grey2 "%A %B %d %Y " ++ yellow "%H:%M:%S")
          ""
          "America/Los_Angeles"
          "date"
          (1 `seconds`)
    , -- Weather StationID Args RefreshRate
      Run $
        Weather
          "YPJT"
          [ "--template"
          , inWrapper (ppTitle "Temp" ++ green "<tempC>C")
              ++ inWrapper (ppTitle "Wind" ++ purple "<windKmh>km" ++ grey0 "/" ++ purple "h")
              ++ inWrapper (ppTitle "Humidity" ++ blue "<rh>%")
          ]
          (30 `minutes`)
    , -- DiskU Disks Args RefreshRate
      Run $
        DiskU
          [ ("/", inWrapper' (ppTitle "System" ++ ppDiskSpace))
          , ("/home/sajenim", inWrapper' (ppTitle "Home" ++ ppDiskSpace))
          ]
          []
          (30 `minutes`)
    , -- Uptime Args RefreshRate
      Run $
        Uptime
          [ "--template"
          , inWrapper (ppTitle "Uptime" ++ red "<days>d <hours>h <minutes>m")
          ]
          (60 `seconds`)
    ]
   where
    -- Stylistic formatting
    ppDiskSpace :: String
    ppDiskSpace = orange "<used>" ++ grey0 "/" ++ aqua "<free>"

    -- Convenience functions
    seconds :: Int -> Int
    seconds = (* 10)

    minutes :: Int -> Int
    minutes = (60 *) . seconds

    ppTitle :: String -> String
    ppTitle = wrap "" ": " . grey2

    inWrapper :: String -> String
    inWrapper = wrap (grey0 " <fn=1>[</fn> ") (grey0 " <fn=1>]</fn> ")

    inWrapper' :: String -> String
    inWrapper' = wrap (grey0 "<fn=1>[</fn> ") (grey0 " <fn=1>]</fn> ")

main :: IO ()
main = configFromArgs config >>= xmobar
