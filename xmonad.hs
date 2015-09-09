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

-- utils
import XMonad.Util.SpawnOnce
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.Scratchpad

--layouts
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Reflect
import XMonad.Layout.IM
import XMonad.Layout.Tabbed
import XMonad.Layout.PerWorkspace(onWorkspace)
import XMonad.Layout.Grid
import XMonad.Layout.ThreeColumns
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spacing
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
myWorkspaces = ["tmux","web","web2","code","code1","code2","im","term","term1","moo!and!oink"]

--------------------------------------------------------------------------------
-- Make the bordercolor different here because well...  this is where it is defined.  BAM spice-weasel!
myNormalBorderColor = "#808080"
myFocusedBorderColor = "#009900"

--------------------------------------------------------------------------------
-- MangeDocks --> ManageHook 
myManageHook = composeAll . concat $
   [ [ className =? "Chromium" --> doShift "web" ]
   , [ className =? "Firefox-bin" --> doShift "web2"]
   , [ appName =? "crx_nckgahadagoaajjgafhacjanaoiihapd"    --> (placeHook chatPlacement <+> doShift "im" <+> doFloat) ] -- hangouts
   , [ appName =? "crx_knipolnnllmklapflnccelgolnpehhpl"    --> (placeHook chatPlacement <+> doShift "im" <+> doFloat) ] -- hangouts 
   , [ appName =? "crx_fahmaaghhglfmonjliepjlchgpgfmobi"    --> (placeHook gmusicPlacement <+> doShift "term1" <+> doFloat) ] -- gmusic player 
   , [ (className =? "Firefox" <&&> resource =? "Dialog") --> doFloat]
   ]
   -- in a composeAll hook, you'd use: fmap ("VLC" `isInfixOf`) title --> doFloat
  where myFloatsC = ["Hangouts", "donkey-doodle"]
        myMatchAnywhereFloatsC = ["Google","hangouts"]
        myMatchAnywhereFloatsT = ["VLC"] -- this one is silly for only one string!

chatPlacement :: Placement
chatPlacement = withGaps(0,16,1,0) (inBounds (smart(0,1)))

gmusicPlacement :: Placement
gmusicPlacement = withGaps(0,16,1,0) (inBounds (smart(0,1)))
--------------------------------------------------------------------------------
--logHook
myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ dev0xPP {ppOutput = hPutStrLn h}

--- Theme For Tabbed layout
myTheme = defaultTheme { decoHeight = 16
            , activeColor = "#000000"
            , activeBorderColor = "#A6C292"
            , activeTextColor = "#CEFFAC"
            , inactiveColor = "#000000"
            , inactiveBorderColor = "#7C7C7C"
            , inactiveTextColor = "#EEEEEE"
            }
--------------------------------------------------------------------------------
myLayoutHook  =  onWorkspace "im" imLayout 
         $ onWorkspace "web" webL
         $ onWorkspace "web2" webL
         $ standardLayouts 
    where
    threeLayout = ThreeCol 1 (3/100) (1/2)
    standardLayouts =  avoidStruts $ (tiled ||| reflectTiled ||| Mirror tiled ||| Grid ||| Full ||| threeLayout)
 
    --Layouts
    tiled      = smartBorders (ResizableTall 1 (2/100) (1/2) [])
    reflectTiled  = (reflectHoriz tiled)
    tabLayout    = (tabbed shrinkText myTheme)
    full      = noBorders Full
 
    --Im Layout
    imLayout = withIM (1/10) (Role "roster") (standardLayouts)
     
    --Web Layout
    webL = avoidStruts $ (tabLayout  ||| tiled ||| reflectHoriz tiled ||| Grid ||| Full)
    gimpLayout = smartBorders(avoidStruts(ThreeColMid 1 (3/100) (3/4)))
     
--------------------------------------------------------------------------------
myStartupHook :: X ()
myStartupHook = do
        setWMName "LG3D", 
        spawnOnce "xmobar -x 1 ~/.xmobarrc2"

dev0xPP :: PP
dev0xPP = defaultPP {
      ppHidden = xmobarColor "#4e4e4e" ""
      , ppCurrent = xmobarColor "#AFFF00" "" . wrap "[" "]" 
      , ppVisible = xmobarColor "#808080" "" . wrap "-" "-"
      , ppUrgent = xmobarColor "#FF0000" "" . wrap "*" "*"
      , ppLayout = xmobarColor "#2AA198" "" 
      , ppTitle = xmobarColor "#00FF00" "" . shorten 80
      , ppSep = "<fc=#0033FF> | </fc>"
    } 
 
--------------------------------------------------------------------------------
--Run XMonad with the defaults
main = do
  xmproc <- spawnPipe "xmobar -x 0 ~/.xmobarrc" 
  xmonad  $ defaultConfig { 
  terminal = myTerminal
  ,workspaces = myWorkspaces
    ,manageHook = myManageHook
  ,normalBorderColor = myNormalBorderColor
  ,focusedBorderColor = myFocusedBorderColor
  ,logHook = myLogHook xmproc
  ,layoutHook = myLayoutHook
  ,keys = myKeys
  ,startupHook = myStartupHook 
  } 

myKeys x = M.union (M.fromList (newKeys x)) (keys defaultConfig x)
--------------------------------------------------------------------------------
newKeys conf@(XConfig {XMonad.modMask = mod4Mask}) = [    
    ((mod4Mask, xK_p), spawn "dmenu_run -nb '#3F3F3F' -nf '#DCDCCC' -sb '#7F9F7F' -sf '#DCDCCC'")  --Uses a colorscheme with dmenu
    ,((mod4Mask, xK_f), spawn "urxvt -e xcalc")
    ,((mod4Mask.|.shiftMask, xK_l), spawn "xscreensaver-command --lock")
    ,((mod4Mask, xK_Return), spawn "urxvt")
    ,((mod4Mask, xK_m), spawn "chromium-browser --app='https://mail.google.com'")
    ,((0, xK_Print), spawn "sleep 0.2; scrot -s")
    ,((mod4Mask, xK_0), nextWS)
  ]
--------------------------------------------------------------------------------
