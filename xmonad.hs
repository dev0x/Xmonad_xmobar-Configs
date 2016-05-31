{-
 -
 = ||+||+||+||+||+||+||+||+||+||+||+||+||+||+||+||
 -
 = Xmonad.hs
 -
 = ||+||+||+||+||+||+||+||+||+||+||+||+||+||+||+||
 -
 -}
-- Language
{-# LANGUAGE DeriveDataTypeable, NoMonomorphismRestriction, TypeSynonymInstances, MultiParamTypeClasses,  ImplicitParams, PatternGuards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

import XMonad
import qualified Data.Map as M
import qualified XMonad.StackSet as W
import System.IO
import System.Exit
import Control.Monad

-- hooks
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.SetWMName
import XMonad.Hooks.Place

--layouts
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.IM
import XMonad.Layout.Tabbed
import XMonad.Layout.PerWorkspace(onWorkspace)
import XMonad.Layout.Grid
import XMonad.Layout.ThreeColumns
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Magnifier

--actions
import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.CopyWindow

--prompt
import XMonad.Prompt
import XMonad.Prompt.Workspace

-- utils
import XMonad.Util.Dmenu
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.NamedScratchpad
import XMonad.Util.SpawnOnce


-- Data.Ratio due to the IM layout
import Data.Ratio ((%))
import Data.Maybe

--------------------------------------------------------------------------------
-- defined function(s)
--------------------------------------------------------------------------------

--Quit with warning prompt at top
quitWithWarning :: X ()
quitWithWarning = do
    let message = "confirm quit"
    s <- dmenu [message]
    when (s == "y") (io exitSuccess)

carryToNamedWs :: XPConfig -> X ()
carryToNamedWs = \xpconf -> withWorkspace xpconf (\ws -> windows $ W.greedyView ws . W.shift ws)

promptedShift :: X ()
promptedShift = workspacePrompt myXPConfig $ windows . W.shift

--Define Terminal
myTerminal :: String
myTerminal = "urxvt"

--Define myModMask
myModMask :: KeyMask
myModMask = mod1Mask -- leave as alt

--Define various colors
myBackground, myForeground, myBorder, myNormalBorderColor, myFocusedBorderColor :: String

myBackground              = "#0c1021"
myForeground              = "#f8f8f8"
myBorder                  = "DarkOrange"
myNormalBorderColor     = "#202020"
myFocusedBorderColor    = "#7C7C7C"

myBorderWidth :: Dimension
myBorderWidth = 1

--------------------------------------------------------------------------------
--Theme for Tabbed layout and window decoration
myTheme :: Theme
myTheme = def { decoHeight = 16
  , activeColor = "#000000"
  , activeBorderColor = "#A6C292"
  , activeTextColor = "#2AA198"
  , inactiveColor = "#000000"
  , inactiveBorderColor = "#7C7C7C"
  , inactiveTextColor = "#006000"
}

--------------------------------------------------------------------------------
--'Themes' for XPconfig
myXPConfig :: XPConfig
myXPConfig = def {
    bgColor             = myBackground
    , fgColor           = myForeground
    , fgHLight          = "#f8f8f8"
    , bgHLight          = "steelblue3"
    , borderColor       = myBorder
    , promptBorderWidth = 1
    , position          = Bottom
    , height            = 24
    , defaultText       = []
}

-- Shell prompt theme
myShellPrompt :: XPConfig
myShellPrompt = def {
    bgColor            = myBackground
    ,fgColor           = myForeground
    ,borderColor       = myBorder
    ,promptBorderWidth = 1
    ,position          = Bottom
    ,height            = 24
    ,defaultText       = []
}

--------------------------------------------------------------------------------
-- Pretty Printing
customPP :: PP
customPP = def {
        ppCurrent = \wsId -> xmobarColor "#AFFF00" (ppMultiColor wsId) . pad . wrap "[" "]" $ wsName wsId
        , ppVisible = xmobarColor "#2AA198" ""
        , ppHidden = xmobarColor "#959595" "" . noScratchPad
        , ppUrgent = xmobarColor "#FF0000" "" . wrap "*" "*"
        , ppLayout = xmobarColor "#2AA198" ""
        , ppTitle = xmobarColor "#00FF00" "" . shorten 100
        , ppSep = "<fc=#0033FF> | </fc>"
    } where
    noScratchPad ws = if ws == "NSP" then "" else ws -- NSP doesn't show up in WS
    ppMultiColor wsId = fromMaybe "#586e75" (M.lookup wsId wsColorMap)
    wsName wsId = case M.lookup wsId wsNameMap of
        Nothing     -> wsId
        Just name   -> wsId ++ "»" ++ name

--------------------------------------------------------------------------------
--Workspaces

myWorkspaces :: [WorkspaceId]
myWorkspaces =
  map show [1,2,3,4,5,6,7 :: Int]
    ++ ["-"]

myWorkspaceNames :: [WorkspaceId]
myWorkspaceNames =
        [ "tmux", "crow", "fire", "code", "music", "log" ]

--wsNameMap - workspaceNameMap
wsNameMap :: M.Map WorkspaceId WorkspaceId
wsNameMap = M.fromList $ zip myWorkspaces myWorkspaceNames

--wsNameToId - workspaceName to ID function
--wsNameToId :: M.Map WorkspaceId WorkspaceId
--wsNameToId = M.fromList $ zip myWorkspaceNames myWorkspaces

-- Workspace colors map
wsColorMap :: M.Map WorkspaceId String
wsColorMap = M.fromList $ zip myWorkspaces
        [ "#b58900" --yellow
        , "#2aa198" --cyan
        , "#859900" --green
        , "#dc322f" --red
        , "#268bd2" --blue
        , "#6c71c4" --violet
        , "#cb4b16" --orange
        , "#d33682" --magenta
        ]
--Define showmenu - dmenu - which I've broken the config/formatting out - why? because fuck you, that's why
showmenu :: MonadIO m => m()
showmenu = spawn ("dmenu_run" ++ dmenuFormatString)

dmenuFormatString :: [Char]
dmenuFormatString = concat
    [     " -nb '#000000' " -- black
        , " -nf '#3289BD' " -- ??
        , " -sb '#42CBF5' " -- blue
        , " -sf '#F46D43' " -- orange
        , " -fn 'envypn' "  -- font -> http://ywstd.fr/me/#envypn
    ]



--------------------------------------------------------------------------------
--ManageHook
myManageHook :: ManageHook
myManageHook = composeAll . concat $
  [
    [namedScratchpadManageHook myScratchPads]
    , [ className =? "Chromium" --> doShift "web" ]
    , [ className =? "Gimp" --> doFloat]
    , [ className =? "Eclipse" --> doFloat <+> doShift "code2"]
    , [ className =? "Steam" --> doFloat]
    , [ (className =? "Firefox" <&&> resource =? "Dialog") --> doFloat]
    , [ appName =? "crx_knipolnnllmklapflnccelgolnpehhpl" --> (placeHook chatPlacement <+> doFloat)] -- hangout
    , [ appName =? "crx_nckgahadagoaajjgafhacjanaoiihapd" --> (placeHook chatPlacement <+> doFloat)] -- hangout
    , [ appName =? "crx_fahmaaghhglfmonjliepjlchgpgfmobi" --> (placeHook chatPlacement <+> doFloat)] -- gmusic
    , [ className =? "Telegram" --> (placeHook telegramPlacement <+> doShift "im" <+> doFloat)]
  ]

--Define chat Placement - used in myManageHook
chatPlacement :: Placement
chatPlacement = withGaps (0,16,5,0) (inBounds (smart (0,1)))

--Define telegram Placement - used in myManageHook
telegramPlacement :: Placement
telegramPlacement = withGaps (10,16,1,10) (inBounds (smart (1,1)))

--Define myScratchPads
myScratchPads :: [NamedScratchpad]
myScratchPads =
    [
      NS "scratchpad"
         "urxvt -name 'NSP-Terminal'"
         (appName =? "NSP-Terminal") $ customFloating scratchWindow
    , NS "terminal"
         "urxvt -name 'NSP-Tmux' -e tmux new-session -s scratch \\; set-option destroy-unattached"
         (appName =? "NSP-Tmux") $ customFloating termWindow
    , NS "notes"
         "gvim --role 'NSP-notes'~/notes.txt"
         (role =? "NSP-notes") $ customFloating notesWindow
    , NS "mpd"
         "urxvt -name 'NSP-mpd' -e ncmpcpp"
         (appName =? "NSP-mpd") $ customFloating fullScreen
    , NS "pidgin" "pidgin"
         (className =? "Pidgin" <&&> role =? "buddy_list") defaultFloating
--
    , NS  "scratchr"
         (myTerminal ++ " -name scratchr")
         (appName =? "scratchr")
         (customFloating $ W.RationalRect (0.65) (0.4) (0.45) (0.60))

    , NS  "scratchl"
         (myTerminal ++ " -name scratchl")
         (appName =? "scratchl")
         (customFloating $ W.RationalRect (0.0) (0.45) (0.45) (0.60))

    , NS  "scratchc"
          (myTerminal ++ " -name scratchc")
         (appName =? "scratchc")
         (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3))
--
    ]
    where
        role = stringProperty "WM_WINDOW_ROLE"
        --wmName = stringProperty "WM_NAME"
        --netWmName = stringProperty "_NET_WM_NAME"
        fullScreen = W.RationalRect 0 0.018 1 (1 - 0.018)
        termWindow = W.RationalRect l t w h
            where h = 1 - t
                  w = 0.45
                  t = 0.018
                  l = 1 - w
        notesWindow = W.RationalRect l t w h
            where h = 1 - t
                  w = 0.45
                  t = 14 / 1080
                  l = 1 - w
        scratchWindow = W.RationalRect l t w h
            where h = 0.45    -- terminal height %
                  w = 0.8     -- terminal width %
                  t = 1 - h   -- distance from top edge %
                  l = 1 - w   -- distance from left edge %


--------------------------------------------------------------------------------
--LogHook -
myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP . namedScratchpadFilterOutWorkspacePP $ customPP { ppOutput = hPutStrLn h }

--------------------------------------------------------------------------------
-- b!tch of a type signature
{-
   myLayoutHook :: onWorkspace
                        (Layout
                           AddRoster
                           (Layout
                              AvoidStruts
                              (Choose
                                 (Layout
                                    SmartBorder ResizableTall)
                                 (Choose
                                    (Layout SmartBorder
                                       (Layout Magnifier Tall))
                                    (Choose
                                       (Mirror
                                          (Layout SmartBorder ResizableTall))
                                       (Choose ThreeCol Full))))))
                        (onWorkspace
                           (Layout
                              AvoidStruts
                              (Choose
                                 (Layout
                                    SmartBorder
                                    (Layout
                                       (Decoration TabbedDecoration DefaultShrinker)
                                       Simplest))
                                 (Choose
                                    (Layout SmartBorder ResizableTall)
                                    (Choose
                                       Grid
                                       (Choose
                                          (Layout SmartBorder
                                             (Layout
                                                Magnifier Tall))
                                          Full)))))
                           (onWorkspace
                              (Layout
                                 AvoidStruts
                                 (Choose
                                    (Layout SmartBorder
                                       (Layout
                                          (Decoration TabbedDecoration DefaultShrinker)
                                          Simplest))
                                    (Choose
                                       (Layout SmartBorder ResizableTall)
                                       (Choose
                                          Grid
                                          (Choose
                                             (Layout SmartBorder
                                                (Layout
                                                   Magnifier Tall))
                                             Full)))))
                              (onWorkspace
                                 (Layout
                                    WithBorder
                                    (Layout WindowChanges SimplestFloat))
                                 (Layout
                                    AvoidStruts
                                    (Choose
                                       (Layout SmartBorder ResizableTall)
                                       (Choose
                                          (Layout SmartBorder
                                             (Layout Magnifier Tall))
                                          (Choose
                                             (Mirror
                                                (Layout SmartBorder ResizableTall))
                                             (Choose ThreeCol Full))))))))

     Window
-}

myLayoutHook =
    onWorkspace "im" imLayout
    $ onWorkspace "crow" webLayout
    $ onWorkspace "fire" webLayout
    $ onWorkspace "steam" steam
    $ standardLayouts
  where
    --Layouts
    standardLayouts     = avoidStruts $ (tiled
                            ||| magLayout
                            ||| Mirror tiled
                            ||| threeLayout
                            ||| Full)
    -- tiled
    tiled               = smartBorders (ResizableTall 1 (2/100) (1/2) [])

    -- tab Layout
    tabLayout           = smartBorders (tabbed shrinkText myTheme)

    -- magnifier Layout
    magLayout           = smartBorders (magnifier (Tall 1 (3/100) (1/2)))

    -- steam Layout
    steam               = noBorders simplestFloat

    -- three Layout
    threeLayout         = ThreeCol 1 (3/100) (1/2)

    --IM Layout
    imLayout            = withIM (1%10) (Role "roster") (standardLayouts)

    --Web Layout
    webLayout           = avoidStruts $(tabLayout ||| tiled ||| Grid ||| magLayout ||| Full)

--------------------------------------------------------------------------------
--StartupHook
myStartupHook :: X()
myStartupHook = do
    setWMName "LG3D"
    spawnOnce "xscreensaver -nosplash"

--------------------------------------------------------------------------------
--Run XMonad with the defaults
main :: IO ()
main = do
  xmproc <- spawnPipe "xmobar -x 0 ~/.xmobarrc"
  xmonad $ def {        --my<stuff>
    terminal            = myTerminal
    ,workspaces         = myWorkspaces
    ,manageHook         = myManageHook
    ,normalBorderColor  = myNormalBorderColor
    ,focusedBorderColor = myFocusedBorderColor
    ,borderWidth        = myBorderWidth
    ,logHook            = myLogHook xmproc
    ,layoutHook         = myLayoutHook
    ,modMask            = myModMask
    ,keys               = myKeys
    ,startupHook        = myStartupHook
}

--------------------------------------------------------------------------------
--Keys
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys x = M.union (M.fromList (newKeys x)) (keys def x) -- default Bindings
--newKeys are the NEW bindings and we union with the DefaultConfig Key Lists favoring our list over default
newKeys :: XConfig Layout -> [((KeyMask, KeySym), X ())]
newKeys (XConfig {XMonad.modMask = modm}) = [
-- keybindings
    ((0                     , xK_Print)         , spawn "sleep 0.2; scrot -s")
    ,((modm                 , xK_p)             , showmenu)
    ,((modm.|.shiftMask     , xK_l)             , spawn "xscreensaver-command --lock")
    ,((modm.|.shiftMask     , xK_q)             , quitWithWarning)
    ,((modm                 , xK_Return)        , spawn "urxvt")
    ,((modm                 , xK_b)             , sendMessage ToggleStruts)
-- Magnifying stuff
    ,((modm.|.controlMask   , xK_equal)         , sendMessage MagnifyMore)
    ,((modm.|.controlMask   , xK_minus)         , sendMessage MagnifyLess)
    ,((modm.|.controlMask   , xK_o)             , sendMessage ToggleOff)
    ,((modm.|.controlMask.|.shiftMask, xK_o)    , sendMessage ToggleOn)
    ,((modm.|.controlMask   , xK_m)             , sendMessage Toggle)
-- Workspaces - switching and moving
    ,((modm                 , xK_0)             , nextWS)   --move to next workspace
    ,((modm.|.shiftMask     , xK_0)             , prevWS)   --move to prev workspace
    ,((modm.|.shiftMask     , xK_BackSpace)     , removeWorkspace)  --removeWorkspace
    ,((modm                 , xK_v)             , selectWorkspace myXPConfig)
    ,((modm                 , xK_m)             , withWorkspace myXPConfig (windows . W.shift)) -- move WS to another
--    ,((modm.|.shiftMask     , xK_m)             , withWorkspace myXPConfig (windows . copy))    -- copy WS to another
    ,((modm.|.shiftMask     , xK_r)             , renameWorkspace myXPConfig)
    ,((modm                 , xK_a)             , addWorkspacePrompt myXPConfig) -- add WS with prompt
    ,((modm                 , xK_grave)         , namedScratchpadAction myScratchPads "volume" ) -- scratchPad
    ,((modm                 , xK_e)             , namedScratchpadAction myScratchPads "scratchc") -- scratchPad
    ,((modm                 , xK_i)             , namedScratchpadAction myScratchPads "scratchr") -- scratchPad
    ,((modm.|.shiftMask     , xK_i)             , namedScratchpadAction myScratchPads "scratchl") -- scratchPad
  ]
  ++ -- mapping switching to different workspaces
  zip (zip (repeat (modm)) [xK_1..xK_9]) (map (withNthWorkspace W.view) [0..])
  ++ -- mapping MOVING windows to different workspaces
  zip (zip (repeat (modm.|.shiftMask)) [xK_1..xK_9]) (map (withNthWorkspace W.shift) [0..])

{-
  -- "M-[1..9,0,-]" -- Switch to workspace N
  -- "M-S-[1..9,0,-]" -- Move client to workspace N
  -- "M-C-[1..9,0,-]" -- Copy client to workspace N
  [("M-" ++ m ++ k, windows $ f i)
      | (i, k) <- zip (XMonad.workspaces conf) myWorkspaces
      , (f, m) <- [ (W.view, ""), (W.shift, "S-"), (copy, "C-") ]
  ]
  ++
  -- "M-C-S-[1..9,0,-]" -- Move client to workspace N and follow
  [("M-C-S-" ++ k, windows (W.shift i) >> windows (W.view i))
      | (i, k) <- zip (XMonad.workspaces conf) myWorkspaces
  ]
-}
--------------------------------------------------------------------------------
