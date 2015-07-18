-- Import Statements
import XMonad
import qualified Data.Map as M
import Graphics.X11.Xlib
import System.IO
import System.Exit

-- hooks
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.SetWMName
import XMonad.Hooks.Place
import XMonad.Hooks.FadeInactive

-- utils
import XMonad.Util.SpawnOnce
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.Scratchpad
--import XMonad.Util

--layouts
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Reflect
import XMonad.Layout.IM
import XMonad.Layout.Tabbed
import XMonad.Layout.PerWorkspace(onWorkspace)
import XMonad.Layout.Grid
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Spacing
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Magnifier

--actions
import XMonad.Actions.CycleWS

-- Data.Ratio due to the IM layout
import Data.Ratio ((%))

--------------------------------------------------------------------------------
--Define Terminal
myTerminal :: String
myTerminal = "urxvt"

--------------------------------------------------------------------------------
--Workspaces
myWorkspaces :: [WorkspaceId]
myWorkspaces = ["tmux","web","web2","code","code1","code2","dev server","term","term1","steam"]

--------------------------------------------------------------------------------
-- Make the bordercolor different here because well...	this is where it is defined.  BAM spice-weasel!
myNormalBorderColor = "#202020"
myFocusedBorderColor = "#7C7C7C"
myBorderWidth = 1

--------------------------------------------------------------------------------
myManageHook = composeAll . concat $
  [ [ className =? "Chromium" --> doShift "web" ]
    , [ className =? "Gimp" --> doFloat]
    , [ className =? "steam" --> doFloat]
    , [ className =? "Steam" --> doFloat]
    , [ (className =? "Firefox" <&&> resource =? "Dialog") --> doFloat]
    , [ appName =? "crx_nckgahadagoaajjgafhacjanaoiihapd" --> (placeHook chatPlacement <+> doFloat)]
    , [ appName =? "crx_knipolnnllmklapflnccelgolnpehhpl" --> (placeHook chatPlacement <+> doFloat)]
   ]

chatPlacement :: Placement
chatPlacement = withGaps (0,16,1,0) (inBounds (smart (1,1)))


--------------------------------------------------------------------------------
--logHook
myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ customPP {ppOutput = hPutStrLn h}

--- Theme For Tabbed layout
myTheme = defaultTheme { decoHeight = 16
  , activeColor = "#000000"
  , activeBorderColor = "#A6C292"
  , activeTextColor = "#CEFFAC"
  , inactiveColor = "#000000"
  , inactiveBorderColor = "#7C7C7C"
  , inactiveTextColor = "#006000"
}
--------------------------------------------------------------------------------
myLayoutHook = onWorkspace "im" imLayout 
    $ onWorkspace "web" webL
    $ onWorkspace "web2" webL
    $ onWorkspace "steam" steam 
    $ standardLayouts 
  where
    standardLayouts = avoidStruts $ (tiled ||| Mirror tiled ||| Grid ||| magnifier (Tall 1 (3/100) (1/2)) ||| ThreeCol 1 (3/100) (1/2) ||| Full)
 
    --Layouts
    tiled     = smartBorders (ResizableTall 1 (2/100) (1/2) [])
    reflectTiled = (reflectHoriz tiled)
    tabLayout = (tabbed shrinkText myTheme)
    full      = noBorders Full
    steam     = noBorders simplestFloat
 
    --Im Layout
    imLayout = withIM (1%10) (Role "roster") (standardLayouts)

    --Web Layout
    webL = avoidStruts $ (tabLayout ||| tiled ||| reflectHoriz tiled ||| Grid ||| magnifier (Tall 1 (3/100) (1/2)) ||| Full)

--------------------------------------------------------------------------------
customPP :: PP
customPP = defaultPP {
    ppHidden = xmobarColor "#959595" ""
    , ppCurrent = xmobarColor "#859900" "" . wrap "[" "]" 
    , ppUrgent = xmobarColor "#FF0000" "" . wrap "*" "*"
    , ppLayout = xmobarColor "#2AA198" "" 
    , ppTitle = xmobarColor "#00FF00" "" . shorten 100
    , ppSep = "<fc=#0033FF> | </fc>"
} 

--------------------------------------------------------------------------------
--Run XMonad with the defaults
main = do
  xmproc <- spawnPipe "xmobar -x 0 ~/.xmobarrc" 
  xmonad $ defaultConfig { 
    terminal = myTerminal
    ,workspaces = myWorkspaces
    ,manageHook = myManageHook
    ,normalBorderColor = myNormalBorderColor
    ,focusedBorderColor = myFocusedBorderColor
    ,borderWidth = myBorderWidth
    ,logHook = myLogHook xmproc
    ,layoutHook = myLayoutHook
    ,modMask = myModMask
    ,keys = myKeys
    ,startupHook = setWMName "LG3D" 
} 


myModMask = mod1Mask -- leave as alt

myKeys x = M.union (M.fromList (newKeys x)) (keys defaultConfig x)
--------------------------------------------------------------------------------
newKeys conf@(XConfig {XMonad.modMask = modm}) = [    
    ((modm, xK_p), spawn "dmenu_run -nb '#3F3F3F' -nf '#DCDCCC' -sb '#7F9F7F' -sf '#DCDCCC'")  --Uses a colorscheme with dmenu
    ,((modm, xK_f), spawn "urxvt -e xcalc")
    ,((modm .|. shiftMask , xK_l), spawn "xtrlock -b")
    ,((modm .|. shiftMask , xK_s), spawn "sudo pm-suspend")
    ,((modm , xK_Return), spawn "urxvt")
    ,((modm , xK_m), spawn "chromium --app='https://mail.google.com'")
    ,((modm , xK_0), nextWS)
    ,((0 , xK_Print), spawn "sleep 0.2; scrot -s")
    ,((modm .|. controlMask , xK_equal), sendMessage MagnifyMore)
    ,((modm .|. controlMask , xK_minus), sendMessage MagnifyLess)
    ,((modm .|. controlMask , xK_o), sendMessage ToggleOff)
    ,((modm .|. controlMask .|. shiftMask, xK_o), sendMessage ToggleOn)
    ,((modm .|. controlMask , xK_m), sendMessage Toggle)
  ]
--------------------------------------------------------------------------------
