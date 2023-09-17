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
import System.Exit (exitSuccess)
import System.IO (hPutStrLn)
import qualified Data.Map as M
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))
import qualified XMonad.StackSet as W

myTerminal :: String 
myTerminal = "kitty"

myWorkspaces :: [Int]
myWorkspaces = [0..9] 

myWorkspaces' :: [String]
myWorkspaces' = map show myWorkspaces 

switchWorkspace :: Int -> (String, X ())
switchWorkspace i = ("M-" ++ i', windows $ W.greedyView $ myWorkspaces' !! i)
                where i' = show i

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
    ] ++ map switchWorkspace myWorkspaces

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
        $ mySpacing 8
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

main = do
    xmproc <- spawnPipe "xmobar $XDG_CONFIG_HOME/xmonad/xmobar.hs"
    xmonad $ ewmh $ docks $ def
        { manageHook = manageDocks
        , layoutHook = myLayoutHook
        , modMask = mod4Mask
        , terminal = myTerminal
        , workspaces = myWorkspaces'
        , borderWidth = myBorderWidth
        , logHook = workspaceHistoryHook <+> myLogHook <+> dynamicLogWithPP xmobarPP
            { ppOutput = \x -> hPutStrLn xmproc x
            , ppCurrent = xmobarColor "#98be65" "" . wrap "[" "]"
            , ppVisible = xmobarColor "#98be65" "" 
            , ppHidden = xmobarColor "#82AAFF" "" . wrap "*" ""
            , ppHiddenNoWindows = xmobarColor "#c792ea" ""
            , ppTitle = xmobarColor "#b3afc2" "" . shorten 60
            , ppSep =  "<fc=#666666> <fn=1>|</fn> </fc>"
            , ppUrgent = xmobarColor "#C45500" "" . wrap "!" "!"
            , ppExtras  = [windowCount]
            , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]
            }
        } `additionalKeysP` myKeys
