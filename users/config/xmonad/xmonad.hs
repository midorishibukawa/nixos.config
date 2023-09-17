module Main (main) where

import XMonad
import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.MouseResize
import XMonad.Actions.WithAll (sinkAll, killAll)
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks (avoidStruts, docks, manageDocks, ToggleStruts(..))
import XMonad.Hooks.WorkspaceHistory
import XMonad.Layout.LayoutModifier
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile
import XMonad.Layout.Simplest
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spacing
import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowArranger (windowArrange)
import XMonad.Layout.WindowNavigation
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.SpawnOnce
import System.Exit (exitSuccess)
import System.IO (hPutStrLn)
import qualified Data.Map as M
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))
import qualified XMonad.StackSet as W

myFont :: String 
myFont = "xft:JetBrainsMono Nerd Font Mono:regular:size=11:antialias=true:hinting=true"

myTerminal :: String 
myTerminal = "kitty"

myWorkspaces :: [Int]
myWorkspaces = [0..9] 

myWorkspaces' :: [String]
myWorkspaces' = map show myWorkspaces 

switchWorkspace :: Int -> (String, X ())
switchWorkspace i = ("M-" ++ i', windows $ W.greedyView $ myWorkspaces' !! i)
                where i' = show i

sendToWorkspace :: Int -> (String, X ())
sendToWorkspace i = ("M-S-" ++ i', windows $ W.shift $ myWorkspaces' !! i)
                where i' = show i

defaultColor :: String
defaultColor = "#faf4ed"

data Shade  = Base | Surface | Overlay | Muted | Subtle | Text -- grayscale
            | Love | Gold    | Rose    | Pine  | Foam   | Iris -- colors
            deriving (Read, Show, Enum, Eq, Ord)

myColors :: M.Map (Shade) (String)
myColors = M.fromList
        ([(Base   , defaultColor)
        , (Surface, "#fffaf3")
        , (Overlay, "#f2e9e1")
        , (Muted  , "#9893a5")
        , (Subtle , "#797593")
        , (Text   , "#575279")
        , (Love   , "#b4637a")
        , (Gold   , "#ea9d34")
        , (Rose   , "#d7827e")
        , (Pine   , "#286983")
        , (Foam   , "#56949f")
        , (Iris   , "#907aa9")
        ])

myColor :: Shade -> String -> String
myColor colorName = case M.lookup colorName myColors of
    Just color -> xmobarColor color "" 
    Nothing    -> xmobarColor defaultColor ""

myKeys :: [(String, X ())]
myKeys =
    [ ("M-C-r"          , spawn "xmonad --recompile"                                    )
    , ("M-S-r"          , spawn "xmonad --restart"                                      )
    , ("M-S-q"          , io exitSuccess                                                )
    , ("M-S-<Return>"   , spawn "rofi -show drun"                                       )
    , ("M-<Return>"     , spawn myTerminal                                              )
    , ("M-S-c"          , kill1                                                         )
    , ("M-S-a"          , killAll                                                       )
    , ("M-f"            , sendMessage (T.Toggle "floats")                               )
    , ("M-t"            , withFocused $ windows . W.sink                                )
    , ("M-S-t"          , sinkAll                                                       )
    , ("M-<Space>"      , sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts    )
    ] 
    ++ map switchWorkspace myWorkspaces
    ++ map sendToWorkspace myWorkspaces

myLogHook :: X ()
myLogHook = fadeInactiveLogHook 0.8

mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

tall = renamed [Replace "tall"]
        $ windowNavigation
        $ subLayout [] (smartBorders Simplest)
        $ limitWindows 12
        $ mySpacing 2
        $ ResizableTall 1 (3/100) (1/2) []

floats = renamed [Replace "floats"]
        $ windowNavigation
        $ subLayout [] (smartBorders Simplest)
        $ limitWindows 20 simplestFloat

myLayoutHook = avoidStruts 
                $ mouseResize 
                $ windowArrange 
                $ T.toggleLayouts floats
                $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) myDefaultLayout
            where
                myDefaultLayout = tall

myBorderWidth :: Dimension
myBorderWidth = 0

myStartupHook :: X ()
myStartupHook = do
    spawnOnce "picom --config $XDG_CONFIG_HOME/picom/picom.conf"

main = do
    xmproc <- spawnPipe "xmobar $XDG_CONFIG_HOME/xmonad/xmobar.hs"
    xmonad $ ewmh $ docks $ def
        { manageHook = manageDocks
        , layoutHook = myLayoutHook
        , modMask = mod4Mask
        , terminal = myTerminal
        , workspaces = myWorkspaces'
        , borderWidth = myBorderWidth
        , startupHook = myStartupHook
        , logHook = workspaceHistoryHook <+> myLogHook <+> dynamicLogWithPP xmobarPP
            { ppOutput = \x -> hPutStrLn xmproc x
            , ppCurrent = (myColor Text) . wrap "[" "]"
            , ppVisible = (myColor Text) 
            , ppHidden = (myColor Text) . wrap "*" ""
            , ppHiddenNoWindows = (myColor Text)
            , ppTitle = (myColor Text) . shorten 60
            , ppSep =  "<fc=#666666> <fn=1>|</fn> </fc>"
            , ppUrgent = (myColor Text) .  wrap "!" "!"
            , ppExtras  = [windowCount]
            , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]
            }
        } `additionalKeysP` myKeys
