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

-- Import System  
import XMonad
import qualified Data.Map as M
import Data.Maybe
import qualified XMonad.StackSet as W 
import System.IO
import System.Exit
import Control.Monad

-- Actions
import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicWorkspaces

-- Hooks
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.SetWMName
import XMonad.Hooks.Place

-- Layout
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Reflect
import XMonad.Layout.IM
import XMonad.Layout.Tabbed
import XMonad.Layout.PerWorkspace(onWorkspace)
import XMonad.Layout.Grid
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Magnifier

-- Prompt
import XMonad.Prompt

-- Util
import XMonad.Util.SpawnOnce
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.Dmenu
import XMonad.Util.NamedScratchpad

-- Data.Ratio -  IM layout
import Data.Ratio((%))

--------------------------------------------------------------------------------
-- self defined function(s)
--------------------------------------------------------------------------------
-- quit with warning
quitWithWarning :: X ()    
quitWithWarning = do
    let message = "confirm quit"
    s <- dmenu [message]
    when (s == "y") (io exitSuccess)

-------------------------------------------------------------------------------
-- self defined variables
--------------------------------------------------------------------------------
--Define Terminal
myTerminal  :: String
myTerminal = "urxvt"

myFont :: String 
myFont = "xft:Fira Mono For Powerline:size=16"

--Define MyModMask
myModMask   :: KeyMask
myModMask = mod1Mask 

--Define borderWidth - bumping it to 2px to see if that helps with the issue with x2x
myBorderWidth :: Dimension
myBorderWidth = 1
--------------------------------------------------------------------------------
-- Make the bordercolor different here because well...  this is where it is defined.  BAM spice-weasel!
myNormalBorderColor :: String
myNormalBorderColor = "#808080"
myFocusedBorderColor :: String
myFocusedBorderColor = "#009900"

-------------------------------------------------------------------------------
--Workspaces
myWorkspaceNames :: [WorkspaceId]
myWorkspaceNames = ["tmux","web","web1","code","code1","code2","im"]

myWorkspaces :: [WorkspaceId]
myWorkspaces = map show [1..9 :: Int]

--myWorkspaces :: [WorkspaceId]
--myWorkspaces = zipWith (++) (map show [1..]) wsnames
--    where wsnames = map ((:) ':') myWorkspaceNames

--wsNameMap - workspaceNameMap
wsNameMap :: M.Map WorkspaceId WorkspaceId
wsNameMap = M.fromList $ zip myWorkspaces myWorkspaceNames

--wsNameToId - workspaceName to ID function 
wsNameToId :: M.Map WorkspaceId WorkspaceId
wsNameToId = M.fromList $ zip myWorkspaceNames myWorkspaces

-- Workspace colors map
wsColorMap :: M.Map WorkspaceId String
wsColorMap = M.fromList $ zip myWorkspaces
        [
        "#859900" --green
        , "#586e75" --ColorISelected
        , "#268bd2" --blue
        , "#6c71c4" --violet
        , "#2aa198" --cyan
        , "#cb4b16" --orange
        , "#b58900" --yellow
        , "#dc322f" --red
        ]

--XPConfig
myShellPrompt :: XPConfig
myShellPrompt = def {
    borderColor     = "DarkOrange"
    , bgColor       = "#42CBF5" --blue
    , fgColor       = "#F46D43" --orange
    , bgHLight      = "#42CBF5" --blue
    , fgHLight      = "#f8f8f8"
    , position      = Bottom
    , font          = myFont
    , height        = 24
    , defaultText   = []
}
-------------------------------------------------------------------------------
--dev0xPP - Pretty printing for my settings
dev0xPP :: PP
dev0xPP = def {
      ppHidden      = xmobarColor "#4E4E4E" "" . noScratchPad
      , ppCurrent   = \wsId -> xmobarColor "#AFFF00" (ppMultiColor wsId) . pad . wrap "[" "]" $ wsName wsId
      , ppVisible   = xmobarColor "#808080" "" . wrap "-" "-"
      , ppUrgent    = xmobarColor "#FF0000" "" . wrap "*" "*"
      , ppLayout    = xmobarColor "#2AA198" ""
      , ppTitle     = xmobarColor "#00FF00" "" . shorten 80
      , ppSep       = "<fc=#0033FF> Σ </fc>"
    }
   where
   noScratchPad ws = if ws == "NSP" then "" else pad ws
   ppMultiColor wsId = fromMaybe "#586e75" (M.lookup wsId wsColorMap)
   wsName wsId = case M.lookup wsId wsNameMap of
                                Nothing    -> wsId
                                Just wsName  -> wsId ++ ":" ++ wsName


--showmenu - dmenu - which I've broken the config/formatting out - why? because fuck you, that's why
showmenu :: MonadIO m => m()
showmenu = spawn ("dmenu_run" ++ dmenuFormatString)

dmenuFormatString :: String
dmenuFormatString = concat
    [     " -nb '#000000' " -- black
        , " -nf '#3289BD' " -- ?? 
        , " -sb '#42CBF5' " -- blue
        , " -sf '#F46D43' " -- orange 
        , " -fn 'envypn' "  -- fc-match envypn YIELDS envypn7x13.pcf.gz: "envpn" "Regular" 
    ]
--------------------------------------------------------------------------------
-- MangeDocks --> ManageHook
myManageHook :: ManageHook
myManageHook = composeAll . concat $
   [ [ className =? "Chromium" --> doShift "web" ]
       , [ className =? "Firefox" --> doShift "web2"]
       , [ className =? "Eclipse" --> doShift "code2" <+> doFloat]
       , [ appName =? "crx_nckgahadagoaajjgafhacjanaoiihapd"    --> (placeHook chatPlacement <+> doFloat) ]
       , [ appName =? "crx_knipolnnllmklapflnccelgolnpehhpl"    --> (placeHook chatPlacement <+> doShift "im" <+> doFloat) ]
       , [ appName =? "crx_fahmaaghhglfmonjliepjlchgpgfmobi"    --> (placeHook gmusicPlacement <+> doShift "term1" <+> doFloat) ] --gmusic
       , [ (className =? "Firefox" <&&> resource =? "Dialog") --> doFloat]
       , [ manageDocks ]
       , [ manageHookScratchPad ]
   ]

chatPlacement :: Placement
chatPlacement   = withGaps(0,16,5,10) (smart(0,1))

gmusicPlacement :: Placement
gmusicPlacement = withGaps(0,16,1,0) (inBounds (smart(0,1)))

-------------------------------------------------------------------------------
--scratchpads
myScratchpads :: [NamedScratchpad]
myScratchpads =
  [ NS  "htop"
        (myTerminal ++ " -e htop")
        (title =? "htop")
        (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3))

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

  , NS  "ranger"
        (myTerminal ++ " -e ranger")
        (title =? "ranger")
        (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3))

  , NS  "chat"
        (myTerminal ++ " -name 'NSP-chat'")
        (title =? "NSP-chat")
        (customFloating $ W.RationalRect (1/6) (1/10) (0.40) (0.40)) 
  ]

--------------------------------------------------------------------------------
--manageScratchPadHook for scratchPads
manageHookScratchPad :: ManageHook
manageHookScratchPad = namedScratchpadManageHook myScratchpads

--- Theme For Tabbed layout
myTabTheme :: Theme 
myTabTheme = def { decoHeight = 16
            , activeColor = "#000000"
            , activeBorderColor = "#A6C292"
            , activeTextColor = "#CEFFAC"
            , inactiveColor = "#000000"
            , inactiveBorderColor = "#7C7C7C"
            , inactiveTextColor = "#EEEEEE"
            }

--------------------------------------------------------------------------------
-- Layout Specification - this might be a little messy
--------------------------------------------------------------------------------
--  This Type Signature is a bit crazy - a bunch of expletives summarize trying to write this crap out lol
{-
myLayoutHook :: onWorkspace() 
-}
myLayoutHook = onWorkspace "tmux" tmuxLayout
             $ onWorkspace "im" imLayout 
             $ onWorkspace "web" webLayout
             $ onWorkspace "web1" webLayout
             $ standardLayouts 
    where
    --Layouts
    standardLayouts     = avoidStruts $ (reflectTiled ||| magLayout ||| (tabbed shrinkText myTabTheme) ||| Grid ||| Full ||| threeLayout)
    --tiled
    tiled               = smartBorders (ResizableTall 1 (2/100) (1/2) [])
    --reflectTiled
    reflectTiled        = (reflectHoriz tiled)
    --magnifier Layout
    magLayout           = magnifier (Tall 1 (3/100) (1/2))
    --tab Layout
    tabLayout           = (tabbed shrinkText myTabTheme)
    --Three Columnar 
    threeLayout         = ThreeCol 1 (3/100) (1/2)
    --tmux layout
    tmuxLayout          = smartBorders $ avoidStruts (magLayout ||| Grid ||| Full) 
    --Im Layout
    imLayout            = withIM (1%10) (Role "roster") (standardLayouts)
    --Web Layout
    webLayout           = avoidStruts $ (tabLayout ||| magLayout ||| reflectTiled ||| Grid ||| Full ||| threeLayout)
     
-------------------------------------------------------------------------------
--myStartupHook
myStartupHook :: X ()
myStartupHook = do
        setWMName "LG3D" 
        spawnOnce "xscreensaver -nosplash"

-------------------------------------------------------------------------------
--myLogHook
myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ dev0xPP {ppOutput = hPutStrLn h}

-------------------------------------------------------------------------------
--Run XMonad with the defaults
main :: IO ()
main = do
  xmobarHandle     <- spawnPipe "xmobar -x 0 ~/.xmobarrc0" 
  --xmobarRight    <- spawnPipe "xmobar -x 1 ~/.xmobarrc1"
  xmonad $ def {
      focusedBorderColor = myFocusedBorderColor
      ,normalBorderColor = myNormalBorderColor
      ,borderWidth       = myBorderWidth
      ,logHook          = myLogHook xmobarHandle
      ,layoutHook       = myLayoutHook
      ,manageHook       = manageDocks <+> myManageHook <+> manageHookScratchPad 
      ,handleEventHook  = docksEventHook <+> handleEventHook def
      ,modMask          = myModMask
      ,keys             = myKeys 
      ,startupHook      = myStartupHook
      ,terminal         = myTerminal
      ,workspaces       = myWorkspaces
  }

-------------------------------------------------------------------------------
--Keys
myKeys   :: XConfig Layout -> M.Map (KeyMask, KeySym) (X())
myKeys x = M.union(M.fromList (newKeys x)) (keys def x)
-- newKeys is self defined configuration to which then gets merged into myKeys to then be pulled into xmonad conf
newKeys  :: XConfig Layout -> [((KeyMask, KeySym), X())]
newKeys (XConfig {XMonad.modMask = modm}) =
    [
        ((modm                   ,xK_p)          ,showmenu)  --dmenu
        ,((modm                  ,xK_b)          ,sendMessage ToggleStruts) 
        ,((modm                  ,xK_f)          ,spawn "urxvt -e xcalc")
        ,((modm                  ,xK_Return)     ,spawn "urxvt")
        ,((modm                  ,xK_m)          ,spawn "chromium-browser --app='https://mail.google.com'")
        ,((0                     ,xK_Print)      ,spawn "sleep 0.5; scrot -s")
        ,((modm.|.shiftMask      ,xK_l)          ,spawn "xscreensaver-command --lock")
-- Workspace selection
        ,((modm                  ,xK_0)          ,nextWS)
        ,((modm.|.shiftMask      ,xK_0)          ,prevWS)
        ,((modm.|.shiftMask      ,xK_q)          ,quitWithWarning)
        ,((modm.|.shiftMask      ,xK_BackSpace)  ,removeWorkspace)  --removeWorkspace 
        ,((modm                  ,xK_v)          ,selectWorkspace myShellPrompt)
        ,((modm.|.shiftMask      ,xK_m)          ,withWorkspace   myShellPrompt(windows . W.shift)) --MOVE
        ,((modm.|.shiftMask      ,xK_r)          ,renameWorkspace myShellPrompt)
        ,((modm                  ,xK_a)          ,addWorkspacePrompt myShellPrompt)
        ,((modm                  ,xK_e)          ,namedScratchpadAction myScratchpads "ranger")
        ,((modm                  ,xK_i)          ,namedScratchpadAction myScratchpads "scratchc")
        ,((modm                  ,xK_f)          ,namedScratchpadAction myScratchpads "scratchr")
        ,((modm.|.shiftMask      ,xK_f)          ,namedScratchpadAction myScratchpads "scratchl")
    ]
{-
    -- "M-[1..9,0,-]" -- Switch to workspace N
    -- "M-S-[1..9,0,-]" -- Move client to workspace N
    -- "M-C-[1..9,0,-]" -- Copy client to workspace N
    ++
    [(modm ++ m ++ k, windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) myWorkspaces
        , (f, m) <- [ (W.view, "")
            , (W.shift, "S-")
            , (W.copy, "C-") ]
    ]
    ++
    -- "M-C-S-[1..9,0,-]" -- Move client to workspace N and follow
    [("M-C-S-" ++ k, windows (W.shift i) >> windows (W.view i))
        | (i, k) <- zip (XMonad.workspaces conf) myWorkspaces
    ]

-}
    ++ -- http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Actions-DynamicWorkspaces.html 
    zip (zip (repeat (modm)) [xK_1..xK_9]) (map (withNthWorkspace W.greedyView) [0..])
    ++ -- revise for convention? 
    zip (zip (repeat (modm.|.shiftMask)) [xK_1..xK_9]) (map (withNthWorkspace W.shift) [0..])
--------------------------------------------------------------------------------
