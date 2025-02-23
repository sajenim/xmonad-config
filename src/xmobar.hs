import Xmobar

import XMonad.Hooks.StatusBar.PP (wrap)

import XMonadConfig.GruvboxMaterial

-- | Configures how things should be displayed on the bar
config :: Config
config =
  defaultConfig
    { font = "Fira Code Semi Bold 9"
    , additionalFonts = ["Symbols Nerd Font 2048-em 24"]
    , bgColor = background
    , fgColor = foreground
    , position = Static {xpos = 1920, ypos = 0, width = 2560, height = 24}
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
  myLogo = wrap " " " " "<fn=1>\59255</fn>"

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
    , --  Date Format Alias RefreshRate
      Run $
        Date
          (grey2 "%a %b %_d %Y " ++ yellow "%H:%M:%S")
          "date"
          (1 `seconds`)
    , -- Weather StationID Args RefreshRate
      Run $
        Weather
          "YPJT"
          [ "--template"
          , inWrapper (ppTitle "Temp" ++ green "<tempC>C")
              ++ inWrapper (ppTitle "Wind" ++ purple "<windKmh>km/h")
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
    inWrapper = wrap (grey0 " [ ") (grey0 " ] ")

    inWrapper' :: String -> String
    inWrapper' = wrap (grey0 "[ ") (grey0 " ] ")

main :: IO ()
main = configFromArgs config >>= xmobar
