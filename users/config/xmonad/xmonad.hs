module Main (main) where

import XMonad 
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks (avoidStruts, docksEventHook, manageDocks, ToggleStruts(..))
import XMonad.Hooks.WorkspaceHistory
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Run (spawnPipe)
import System.Exit (exitSuccess)
import System.IO (hPutStrLn)
import qualified XMonad.StackSet as W

myTerminal :: String 
myTerminal = "kitty"

myKeys :: [(String, X ())]
myKeys =
        [ ("M-C-r", spawn "xmonad --recompile")
        , ("M-S-r", spawn "xmonad --restart")
        , ("M-S-q", io exitSuccess)
        , ("M-S-<Return>", spawn "rofi -show drun")
        , ("M-<Return>", spawn myTerminal)
        ] ++ map switchWorkspace myWorkspaces

switchWorkspace :: Int -> (String, X ())
switchWorkspace i = ("M-" ++ (show i), windows $ W.greedyView $ myWorkspaces' !! i)

myWorkspaces :: [Int]
myWorkspaces = [0..9] 

myWorkspaces' :: [String]
myWorkspaces' = map show myWorkspaces 

myLogHook :: X ()
myLogHook = fadeInactiveLogHook 0.8

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

main = do
    xmproc <- spawnPipe "xmobar $XDG_CONFIG_HOME/xmonad/xmobar.hs"
    xmonad $ ewmh def
        { modMask = mod4Mask
        , terminal = myTerminal
        , workspaces = myWorkspaces'
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
