-- Import System  
import XMonad
import qualified Data.Map as M
import qualified XMonad.StackSet as W 
import System.IO
import System.Exit
import Control.Monad

-- actions
import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.CopyWindow
-- hooks
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers()
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.SetWMName
import XMonad.Hooks.Place

--layouts
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Reflect
import XMonad.Layout.IM
import XMonad.Layout.Tabbed
import XMonad.Layout.PerWorkspace(onWorkspace)
import XMonad.Layout.Grid
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Magnifier


-- prompt
import XMonad.Prompt

-- utils
import XMonad.Util.SpawnOnce
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.Dmenu


-- Data.Ratio -  IM layout
import Data.Ratio((%))

--------------------------------------------------------------------------------
-- self defined functions
--------------------------------------------------------------------------------
-- Quit with warning
quitWithWarning :: X ()    
quitWithWarning = do
    let message = "confirm quit"
    s <- dmenu [message]
    when (s == "y") (io exitSuccess)
--------------------------------------------------------------------------------
-- self defined variables
--------------------------------------------------------------------------------
--Define Terminal
myTerminal  :: String
myTerminal = "urxvt"

--Define MyModMask
myModMask   :: KeyMask
myModMask = mod1Mask 

--------------------------------------------------------------------------------
--Workspaces
myWorkspaces :: [WorkspaceId]
myWorkspaces = ["tmux","web","web2","code","code1","code2","im","term","term1","moo!and!oink"]

-------------------------------------------------------------------------------
-- Make the bordercolor different here because well...  this is where it is defined.  BAM spice-weasel!
myNormalBorderColor :: String
myNormalBorderColor = "#808080"
myFocusedBorderColor :: String
myFocusedBorderColor = "#009900"

--Define to def...
myXPConfig :: XPConfig
myXPConfig = defaultXPConfig

--------------------------------------------------------------------------------
-- MangeDocks --> ManageHook 
myManageHook :: ManageHook 
myManageHook = composeAll . concat $
   [ [ className =? "Chromium" --> doShift "web" ]
       , [ className =? "Firefox" --> doShift "web2"]
       , [ className =? "Eclipse" --> doShift "code2" <+> doFloat]
       , [ appName =? "crx_nckgahadagoaajjgafhacjanaoiihapd"    --> (placeHook chatPlacement <+> doShift "im" <+> doFloat) ] 
       , [ appName =? "crx_knipolnnllmklapflnccelgolnpehhpl"    --> (placeHook chatPlacement <+> doFloat) ] 
       , [ appName =? "crx_fahmaaghhglfmonjliepjlchgpgfmobi"    --> (placeHook gmusicPlacement <+> doShift "term1" <+> doFloat) ] --gmusic 
       , [ (className =? "Firefox" <&&> resource =? "Dialog") --> doFloat]
   ]

chatPlacement :: Placement
chatPlacement = withGaps(0,16,1,0) (inBounds (smart(0,1)))

gmusicPlacement :: Placement
gmusicPlacement = withGaps(0,16,1,0) (inBounds (smart(0,1)))
--------------------------------------------------------------------------------
--logHook
myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ dev0xPP {ppOutput = hPutStrLn h} 

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

--- Theme For Tabbed layout
myTabTheme :: Theme 
myTabTheme = defaultTheme { decoHeight = 16
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
myLayoutHook = onWorkspace "im" imLayout 
             $ onWorkspace "web" webLayout
             $ onWorkspace "web1" webLayout
             $ standardLayouts 
    where
    standardLayouts     = avoidStruts $ (tiled ||| magLayout ||| reflectTiled ||| Grid ||| Full ||| threeLayout)
    tiled               = smartBorders (ResizableTall 1 (2/100) (1/2) [])
    reflectTiled        = (reflectHoriz tiled)

    --magnifier Layout
    magLayout           = magnifier (Tall 1 (3/100) (1/2))

    --tab Layout
    tabLayout           = (tabbed shrinkText myTabTheme)

    --Three Columnar window layout
    threeLayout         = ThreeCol 1 (3/100) (1/2)

    --Im Layout
    imLayout            = withIM (1%10) (Role "roster") (standardLayouts)
     
    --Web Layout
    webLayout           = avoidStruts $ (tabLayout ||| magLayout ||| reflectTiled ||| Grid ||| Full ||| threeLayout)
     
-------------------------------------------------------------------------------
myStartupHook :: X ()
myStartupHook = do
        setWMName "LG3D" 
        spawnOnce "xmobar -x 1 ~/.xmobarrc2"
-------------------------------------------------------------------------------
--Run XMonad with the defaults
main :: IO ()
main = do
  xmobarProc <- spawnPipe "xmobar -x 0 ~/.xmobarrc" 
  xmonad $ defaultConfig { 
      focusedBorderColor = myFocusedBorderColor
      ,normalBorderColor = myNormalBorderColor
      ,logHook = myLogHook xmobarProc
      ,layoutHook = myLayoutHook
      ,manageHook = myManageHook
      ,modMask = myModMask
      ,keys = myKeys 
      ,startupHook = myStartupHook
      ,terminal = myTerminal
      ,workspaces = myWorkspaces
  } 

-------------------------------------------------------------------------------
--Keys
myKeys   :: XConfig Layout -> M.Map (KeyMask, KeySym) (X())
myKeys x = M.union(M.fromList (newKeys x)) (keys defaultConfig x)
-- newKeys is self defined configuration to which then gets merged into myKeys to then be pulled into xmonad conf
newKeys  :: XConfig Layout -> [((KeyMask, KeySym), X())]
newKeys (XConfig {XMonad.modMask = modm}) =
    [    
        ((modm                   ,xK_p)          ,spawn "dmenu_run -nb '#3F3F3F' -nf '#DCDCCC' -sb '#7F9F7F' -sf '#DCDCCC'")  --Uses colorscheme with dmenu
        ,((modm                  ,xK_f)          ,spawn "urxvt -e xcalc")
        ,((modm                  ,xK_Return)     ,spawn "urxvt")
        ,((modm                  ,xK_m)          ,spawn "chromium-browser --app='https://mail.google.com'")
        ,((0                     ,xK_Print)      ,spawn "sleep 0.5; scrot -s")
        ,((modm                  ,xK_0)          ,nextWS)
        ,((modm.|.shiftMask      ,xK_0)          ,prevWS)
        ,((modm.|.shiftMask      ,xK_q)          ,quitWithWarning)
        ,((modm.|.shiftMask      ,xK_l)          ,spawn "xscreensaver-command --lock")
        ,((modm.|.shiftMask      ,xK_BackSpace)  ,removeWorkspace)
        ,((modm.|.shiftMask      ,xK_v)          ,selectWorkspace myXPConfig)

        ,((modm                  ,xK_m)          ,withWorkspace myXPConfig (windows . W.shift))
        ,((modm.|.shiftMask      ,xK_m)          ,withWorkspace myXPConfig (windows . copy))

        ,((modm.|.shiftMask      ,xK_r)          ,renameWorkspace myXPConfig)
        ,((modm                  ,xK_a)          ,addWorkspacePrompt myXPConfig)
    ]
    ++
    zip (zip (repeat (modm)) [xK_1..xK_9]) (map (withNthWorkspace W.greedyView) [0..])
    ++
    zip (zip (repeat (modm .|. shiftMask)) [xK_1..xK_9]) (map (withNthWorkspace W.shift) [0..])
--------------------------------------------------------------------------------
