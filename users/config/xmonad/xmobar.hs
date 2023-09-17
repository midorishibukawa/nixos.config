Config { font    = "xft:JetBrainsMono Nerd Fonts Mono:weight=bold:pixelsize=11:antialias=true:hinting=true"
       , bgColor = "#282c34"
       , fgColor = "#ff6c6b"
       , position = Static { xpos = 0 , ypos = 0, width = 1920, height = 24 }
       , lowerOnStart = True
       , hideOnStart = False
       , allDesktops = True
       , persistent = True
       , iconRoot = "/home/dt/.xmonad/xpm/"  -- default: "."
       , commands = [
                      -- Time and date
                      Run Date "<fn=1>\xf133</fn>  %b %d %Y - (%H:%M) " "date" 50
                      -- Network up and down
                    , Run Network "enp6s0" ["-t", "<fn=1>\xf0aa</fn>  <rx>kb  <fn=1>\xf0ab</fn>  <tx>kb"] 20
                      -- Cpu usage in percent
                    , Run Cpu ["-t", "<fn=1>\xf108</fn>  cpu: (<total>%)","-H","50","--high","red"] 20
                      -- Ram used number and percent
                    , Run Memory ["-t", "<fn=1>\xf233</fn>  mem: <used>M (<usedratio>%)"] 20
                      -- Disk space free
                    , Run DiskU [("/", "<fn=1>\xf0c7</fn>  hdd: <free> free")] [] 60
                      -- Runs a standard shell command 'uname -r' to get kernel version
                    , Run Com "uname" ["-r"] "" 3600
                      -- Prints out the left side items such as workspaces, layout, etc.
                      -- The workspaces are 'clickable' in my configs.
                    , Run UnsafeStdinReader
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       }
