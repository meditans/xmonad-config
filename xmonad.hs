-- -*- dante-repl-command-line: ("nix-shell" "-p" "with import <nixpkgs> {}; pkgs.haskellPackages.ghcWithPackages (p: [p.pretty-show p.xmonad p.xmonad-contrib])" "--run" "ghci") -*-

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName

import XMonad.Util.Run        (spawnPipe)
import XMonad.Util.EZConfig   (additionalKeys)
import XMonad.Util.SpawnOnce

import qualified XMonad.StackSet as W

import XMonad.Layout.Spacing
import XMonad.Layout.ResizableTile
import XMonad.Layout.LayoutModifier
import XMonad.Layout.WindowArranger
import XMonad.Layout.NoBorders
import XMonad.Layout.Fullscreen
import XMonad.Layout.Circle
import XMonad.Layout.Gaps

import XMonad.Actions.CycleWS (prevWS, nextWS)

import System.IO
import Data.Semigroup


-- namd options ----------------------------------------------------------------
-- colours
normBord, focdBord, fore, back, winType :: String
normBord = "#343C48"
focdBord = "#6986a0"
fore     = "#DEE3E0"
back     = "#282c34"
winType  = "#c678dd"

-- programs
dmenu :: String
dmenu = "dmenu_run -fn 'xft:Fira Mono:pixelsize=20' -p 'Run: '"
-----------

myWorkspaces    :: [String]
myWorkspaces    = click $ [" 1 "," 2 "," 3 "," 4 "," 5 "," 6 "," 7 "," 8 "," 9 "]
                  where click l = [ "^ca(1, xdotool key super+"
                                  ++ show (n) ++ ")" ++ ws ++ "^ca()" |
                                  (i,ws) <- zip [(1::Int)..] l,
                                  let n = i]

myManageHook :: Query (Endo (W.StackSet String (Layout Window) Window ScreenId ScreenDetail))
myManageHook = composeAll
    [ className =? "Firefox"   --> doF(W.shift(myWorkspaces !! 2))
    , className =? "Nautilus"  --> doFloat
    , className =? "Gimp"      --> doFloat
    ]

-- keys
mKeys :: [((KeyMask, KeySym), X ())]
mKeys = [ ((modm, xK_p), spawn $ dmenu)
        , ((modm, xK_f), spawn $ "firefox")
        , ((modm, xK_c), spawn $ "chromium")
        , ((modm, xK_e), spawn $ "emacs")
        , ((modm .|. controlMask .|. shiftMask, xK_l), spawn $ "systemctl suspend")
        , ((modm, xK_Left), prevWS)
        , ((modm, xK_Right), nextWS)
        , ((modm                 .|. shiftMask, xK_z    ), spawn "slock"                 )
        , ((modm .|. controlMask              , xK_s    ), sendMessage  Arrange          )
        , ((modm .|. controlMask .|. shiftMask, xK_s    ), sendMessage  DeArrange        )
        , ((modm .|. controlMask              , xK_Left ), sendMessage (MoveLeft      10))
        , ((modm .|. controlMask              , xK_Right), sendMessage (MoveRight     10))
        , ((modm .|. controlMask              , xK_Down ), sendMessage (MoveDown      10))
        , ((modm .|. controlMask              , xK_Up   ), sendMessage (MoveUp        10))
        , ((modm                 .|. shiftMask, xK_Left ), sendMessage (IncreaseLeft  10))
        , ((modm                 .|. shiftMask, xK_Right), sendMessage (IncreaseRight 10))
        , ((modm                 .|. shiftMask, xK_Down ), sendMessage (IncreaseDown  10))
        , ((modm                 .|. shiftMask, xK_Up   ), sendMessage (IncreaseUp    10))
        , ((modm .|. controlMask .|. shiftMask, xK_Left ), sendMessage (DecreaseLeft  10))
        , ((modm .|. controlMask .|. shiftMask, xK_Right), sendMessage (DecreaseRight 10))
        , ((modm .|. controlMask .|. shiftMask, xK_Down ), sendMessage (DecreaseDown  10))
        , ((modm .|. controlMask .|. shiftMask, xK_Up   ), sendMessage (DecreaseUp    10))
        , ((modm, xK_KP_Add), sequence_ [ sendMessage (IncreaseLeft 10)
                    , sendMessage (IncreaseRight 10)
                    , sendMessage (IncreaseUp 10)
                    , sendMessage (IncreaseDown 10)
                    ])
        , ((modm, xK_KP_Subtract), sequence_ [ sendMessage (DecreaseLeft 10)
                         , sendMessage (DecreaseRight 10)
                         , sendMessage (DecreaseUp 10)
                         , sendMessage (DecreaseDown 10)
                         ])
    ] where modm = mod4Mask

startUp :: X ()
startUp = do
    spawnOnce "compton"
    spawnOnce "conky -c ~/.conky/bottom.conky | dzen2 -y 1056 -w 1920 -h 24"
    setWMName "LG3D"

logbar :: Handle -> X ()
logbar h = dynamicLogWithPP $ tryPP h

tryPP :: Handle -> PP
tryPP h = def
    { ppOutput             = hPutStrLn h
    , ppCurrent            = dzenColor (fore) (normBord) . pad
    , ppVisible            = dzenColor (fore) (back) . pad
    , ppHidden             = dzenColor (fore) (back) . pad
    , ppHiddenNoWindows    = dzenColor (fore) (back) . pad
    , ppUrgent             = dzenColor (fore) (focdBord) . pad
    , ppOrder              = \(ws:l:_:_) -> [ws,l]
    , ppSep                = ""
    , ppLayout             = dzenColor (fore) (winType) .
                ( \t -> case t of
                    "Spacing 2 ResizableTall" -> " " ++ i ++ "tile.xbm) TALL "
                    "Full" -> " " ++ i ++ "dice1.xbm) FULL "
                    "Circle" -> " " ++ i ++ "dice2.xbm) CIRC "
                    _ -> " " ++ i ++ "tile.xbm) TALL "
                )
    } where i = "^i(/home/carlo/.icons/stlarch/"



-- layout --

res :: ResizableTall a
res = ResizableTall 1 (2/100) (1/2) []

ful :: ModifiedLayout WithBorder (ModifiedLayout FullscreenFull Full) Window
ful = noBorders (fullscreenFull Full)

-- useless gap --

layout ::
  Choose
    (ModifiedLayout
       Gaps
       (ModifiedLayout
          AvoidStruts (ModifiedLayout Spacing ResizableTall)))
    (Choose
       Circle
       (ModifiedLayout WithBorder (ModifiedLayout FullscreenFull Full)))
    Window
layout = (gaps [(U, 32), (R, 8), (L, 8), (D, 32)] $ avoidStruts (spacingRaw False (uniformBorder 0) False (uniformBorder 2) True res))
     ||| Circle
     ||| ful
  where
    uniformBorder i = Border i i i i

------------

main :: IO ()
main = do
    bar  <- spawnPipe panel
    _    <- spawnPipe "conky -c ~/.conky/top.conky |\
                      \dzen2 -x 400 -y 0 -h 24 -w 1520 -p -ta r -e''"
    xmonad $ def
        { manageHook = manageDocks <+> manageHook def
        , layoutHook = windowArrange layout
        , startupHook = startUp
        , workspaces = myWorkspaces
        , terminal = "urxvt"
        , modMask = mod4Mask
        , borderWidth = 2
        , focusedBorderColor = focdBord
        , normalBorderColor = normBord
        , logHook = logbar bar
        } `additionalKeys` mKeys
        where panel = "dzen2 -ta l -p -w 400 -y 0 -x 0 -h 24 -e ''"

