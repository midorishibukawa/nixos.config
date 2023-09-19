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
import XMonad.Util.EZConfig (additionalKeysP, removeKeysP)
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.SpawnOnce
import System.Exit (exitSuccess)
import System.IO (hPutStrLn)
import qualified Data.Map as M
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))
import qualified XMonad.StackSet as W
import Data.Time
import Control.Monad.Cont (MonadIO(liftIO))

myFont :: String
myFont = "xft:SauceCodePro Nerd Font:regular:size=11:antialias=true:hinting=true"

myTerminal :: String
myTerminal = "kitty"

myWorkspaces :: [(Int, String)]
myWorkspaces = map (\i -> (i, show $ if i == 10 then 0 else i)) $ [1..3] ++ [8..10]

myWorkspaces' :: [String]
myWorkspaces' = map snd myWorkspaces

switchWorkspace :: (Int, String) -> (String, X ())
switchWorkspace (i, kb) = ("M-" ++ kb, windows $ W.greedyView kb)

sendToWorkspace :: (Int, String) -> (String, X ())
sendToWorkspace (i, kb)  = ("M-S-" ++ kb, windows $ W.shift kb)

defaultColor :: String
defaultColor = "#faf4ed"

data Shade  = Base | Surface | Overlay | Muted | Subtle | Text -- grayscale
            | Love | Gold    | Rose    | Pine  | Foam   | Iris -- colors
            deriving (Read, Show, Enum, Eq, Ord)

myColors :: M.Map Shade String
myColors = M.fromList
        [(Base   , defaultColor)
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
        ]

myColor :: Shade -> String -> String
myColor colorName = case M.lookup colorName myColors of
    Just color -> xmobarColor color ""
    Nothing    -> xmobarColor defaultColor ""

screenshotPath :: String
screenshotPath = "$HOME/media/images/screenshots/$(date +%Y-%m-%d_%H-%M-%S).jpg"

myRemovedKeys :: [String]
myRemovedKeys = concatMap (generateKey . show) [4..6]

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
    , ("M-S-e"          , spawn "rofimoji"                                              )
    , ("M-S-b"          , spawn "rofi-bluetooth"                                        )
    , ("<Print>"        , spawn $ "scrot " ++ screenshotPath                            )
    , ("M-<Print>"      , spawn $ "scrot --select " ++ screenshotPath                            )
    ]
    ++ concatMap (\ws -> switchWorkspace ws : [ sendToWorkspace ws ]) myWorkspaces


generateKey :: String -> [String]
generateKey i = ("M-" ++ i) : ["M-S-" ++ i]


myLogHook :: X ()
myLogHook = fadeInactiveLogHook 0.9

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
                $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) tall

myBorderWidth :: Dimension
myBorderWidth = 0

myStartupHook :: X ()
myStartupHook = do
    spawnOnce "picom --config $XDG_CONFIG_HOME/picom/picom.conf"
    spawnOnce "feh --bg-center $XDG_STATE_HOME/background.png"

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
            { ppOutput          = hPutStrLn xmproc
            , ppCurrent         = \_ -> myColor Love "\xf111 "
            , ppVisible         = \_ -> myColor Text "\xf1db "
            , ppHidden          = \_ -> myColor Muted "\xf111 "
            , ppHiddenNoWindows = \_ -> myColor Muted "\xf1db "
            , ppTitle           = myColor Text . shorten 60
            , ppUrgent          = \_ -> myColor Rose "\xf06a "
            , ppSep             = "\xf460 "
            , ppOrder           = \(ws:_:t:_) -> ws : [t]
            }
        } `additionalKeysP` myKeys
        `removeKeysP` myRemovedKeys
