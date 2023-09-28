Config { font    = "xft:SauceCodePro Nerd Font Mono:weight=bold:pixelsize=11:antialias=true:hinting=true"
       , bgColor = "#fffaf3"
       , fgColor = "#575279"
       , position = Static { xpos = 4, ypos = 4, width = 2552, height = 24 }
       , lowerOnStart = True
       , hideOnStart = False
       , allDesktops = True
       , persistent = True
       , commands = [ Run Date "%T" "date" 10
                    , Run UnsafeStdinReader
                    ]
       , template = "  %UnsafeStdinReader% } <action=xdotool key super+shift+return>%date%</action> {"
       , sepChar = "%" 
       , alignSep = "}{"
       }
