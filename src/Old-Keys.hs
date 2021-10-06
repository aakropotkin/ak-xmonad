{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module XMonadKeys where
{-------------------------------------------------------------------------------
    Imports                                                                 {{{1
-------------------------------------------------------------------------------}

import Graphics.X11.ExtraTypes.XF86
import XMonad
import XMonad.Actions.WindowGo
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.XMonad
import XMonad.Prompt.Window
import XMonad.Prompt.AppendFile
import qualified XMonad.StackSet as W
import XMonad.Util.Scratchpad
import XMonad.Util.NamedScratchpad
import XMonad.Util.Paste
import XMonad.Util.Spotify
import XMonad.Util.Ungrab
import qualified Data.Text as T
import NeatInterpolation
import XMonad.Actions.FloatSnap
import System.Environment

-- My Modules:
import XMonadTheme
import XMonadCommon

{-------------------------------------------------------------------------------
    End Imports                                                             }}}1
-------------------------------------------------------------------------------}
{-------------------------------------------------------------------------------
    Keybinds                                                                {{{1
-------------------------------------------------------------------------------}
{-    Masks {{{8
      mod1Mask  = Alt
      mod4Mask  = Super
      shiftMask = Shift
}}}8 -}
-- Boiler Plate {{{3
myKeys :: Paths -> [ ((KeyMask, KeySym), X ()) ]
myKeys paths = -- Workspaces
    [ ((mask .|. mod1Mask, key), windows $ f sc)
        | (key, sc) <- zip [xK_1..xK_9] myWorkspaces
        , (f, mask) <- [ (W.greedyView, 0), (W.shift, shiftMask) ]
    ]
    ++ -- Fixes W,R,E swaps
    [ ((mask .|. mod1Mask, key)
        , screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [ xK_w, xK_r, xK_e ] [0..]
        , (f, mask) <- [ (W.view, 0), (W.shift, shiftMask) ]
    ] ++ -- End Boiler Plate }}}3

-- Custom Mappings -------------------------------------------------------- {{{2
-- ===============

-- Launch Terminals and Applications -------------------------------------- {{{3
-- * * * * * * * * * * * * * * * * *

-- ``If it moves, kill it.''
    [ m1 xK_grave   kill
    , m1 xK_Escape  kill

-- GUI
    -- Ranger and Nautilus
    , m1 xK_y $     spawn $ termWith "ranger"
    , s1 xK_y $     spawn "nautilus"
    -- Browsers
    , m1 xK_g $     spawn myBrowser
    , s1 xK_g $     spawn $ myBrowser ++ " --incognito"
    , m1 xK_f $     spawn "firefox"
    -- Audio Players and Mixers
    , m1 xK_d $     spawn "pavucontrol"
    , m1 xK_s $     spawn $ myBrowser ++ " https://open.spotify.com"

    {- !FIXME Spotify's audio doesn't work :( {{{8
    , m1 xK_s $     runOrRaise (scripts paths ++ "/spawn-spotify.sh")
    -- See Spotify Track
    , m1 xK_F5 $    spawn $ "notify-send \"$(" ++ scripts paths ++
                              "/spotinfo)\""    -- }}}8 -}
-- Terminal Based
    -- New Term
    , m1 xK_f $     spawn myTermFull
    -- New Term in previously opened folder
    , s1 xK_f $     spawn $ myTermFull ++ " -cd $(cat $HOME/.latestdir " ++ 
                              "2&>/dev/null || echo $HOME)"
    -- Quake Term
    , m1 xK_u $     scratchpadSpawnActionTerminal myTermFull
    , s1 xK_u $     spawn "${MY_XMO_SCRIPT_DIR}/releasePad"
    -- Shell Prompt
    , m1 xK_p $     shellPrompt myXPConfig
    -- IRC
    , m1 xK_i $     namedScratchpadAction myPads "IRC"
-- Queries
    -- NixOS and NixPkgs Queries
    , m1 xK_o $     namedScratchpadAction myPads "nixpkgs-search"
    , s1 xK_o $     namedScratchpadAction myPads "nixopts-search"
    , m1m4 xK_o $   namedScratchpadAction myPads "home-manager-search"
    -- Dictionary, Translations, and Wiki Queries
    , m1m4 xK_l $   namedScratchpadAction myPads "dictionary"
    , m1m4 xK_t $   namedScratchpadAction myPads "translate"
    , m1m4 xK_w $   namedScratchpadAction myPads "wiki"
-- Prompts
    -- XMonad Command Prompt
    , m1 xK_x $     flip xmonadPromptC myXPConfig =<< allCommands
    -- Write a note
    , m1 xK_n $     appendFilePrompt myXPConfig (note paths)
    -- Open
    , s1 xK_n $     spawn $ termWith $ "vim " ++ note paths
    -- Raise window by name
    , m1 xK_period $ windowPrompt myXPConfig Bring allWindows

-- End Launch Terminals and Applications ---------------------------------- }}}3

-- Control and Information ------------------------------------------------ {{{3
-- * * * * * * * * * * * * 

-- Info Panels and Tools
    -- Show window info
    , s1 xK_i $     spawn $ "xmessage \"$(" ++ bin paths ++
                              "/winfo -default)\""
    -- Pretty System Info display 
    , m1 xK_bracketright $ spawn $ myTermFull ++ " -title XScreenfetch " ++
                            "-g 110x20 -hold -e zsh -ic 'screenfetch'"
    -- Show Mouse Coords
    , s1 xK_period $ spawn $ "xmessage \"$(xdotool " ++ 
                                "getmouselocation --shell)\""
    -- Screenshots
    , nm xK_Print . spawn $ screenShot " && notify-send 'SCREENSHOT: ~/scrns'"
    , sm xK_Print . spawn $ screenShot
                              "-s && && notify-send 'SCREENSHOT ~/scrns'"

    , s1 xK_slash . spawn $ keyHelper

-- Volume Controls
    , m1 xK_F6      audioPrev
    , m1 xK_F7      audioPlayPause
    , m1 xK_F8      audioNext
    , nm xF86XK_AudioLowerVolume $  spawn $ vol "decrease"
    , nm xF86XK_AudioRaiseVolume $  spawn $ vol "increase"
    , nm xF86XK_AudioMute $         spawn $ vol "toggle"

-- End Control and Information -------------------------------------------- }}}3

-- SUPER Mappings (Capslock) ---------------------------------------------- {{{3
-- ==============

-- Arrows                 : <Super> + <[hjkl]>
-- Pg(Up|Down), Home, End : <Shift> + <Super> + <[hjkl]>
    , m4 xK_h $ sendKey 0 xK_Left
    , s4 xK_h $ sendKey 0 xK_Home
    , m4 xK_j $ sendKey 0 xK_Down
    , s4 xK_j $ sendKey 0 xK_Page_Down
    , m4 xK_k $ sendKey 0 xK_Up
    , s4 xK_k $ sendKey 0 xK_Page_Up
    , m4 xK_l $ sendKey 0 xK_Right
    , s4 xK_l $ sendKey 0 xK_End

-- Print, Insert, Delete : <Super> + <[ip]>
    , m4 xK_i $ sendKey 0 xK_Insert
    , s4 xK_i $ sendKey 0 xK_Delete
    , m4 xK_p $ sendKey 0 xK_Print

-- Toggle Mute : <Super> + < f >
    , m4 xK_f . spawn $ vol "toggle"
-- Volume Down/Down <Super> + < [ds] >
    , m4 xK_s . spawn $ vol "decrease"
    , m4 xK_d . spawn $ vol "increase"


-- F1..F12 : <Super> + <[1-90-=]>
    , m4 xK_1     $ sendKey 0 xK_F1
    , m4 xK_2     $ sendKey 0 xK_F2
    , m4 xK_3     $ sendKey 0 xK_F3
    , m4 xK_4     $ sendKey 0 xK_F4
    , m4 xK_5     $ sendKey 0 xK_F5
    , m4 xK_6     $ sendKey 0 xK_F6
    , m4 xK_7     $ sendKey 0 xK_F7
    , m4 xK_8     $ sendKey 0 xK_F8
    , m4 xK_9     $ sendKey 0 xK_F9
    , m4 xK_0     $ sendKey 0 xK_F10
    , m4 xK_minus $ sendKey 0 xK_F11
    , m4 xK_equal $ sendKey 0 xK_F12

-- Symbols
    -- Backtick (`) : <Super> + < ' >
    , m4 xK_apostrophe $ sendKey 0 xK_grave
    -- Tilde (~) : <Shift> + <Super> + < ' >
    , s4 xK_apostrophe $ sendKey shiftMask xK_asciitilde
    , s4 xK_apostrophe $ sendKey shiftMask xK_asciitilde
    -- Lambda (λ) : <Super> + < \ >
    , m4 xK_backslash  $ sendKey 0 xK_Lambda


-- End SUPER Mappings ----------------------------------------------------- }}}3

{- FloatSnapping (unfinished test) {{{3
    , m1 xK_Left  . withFocused $ snapMove L Nothing
    , m1 xK_Right . withFocused $ snapMove R Nothing
    , m1 xK_Up    . withFocused $ snapMove U Nothing
    , m1 xK_Down  . withFocused $ snapMove D Nothing

    , s1 xK_Left  . withFocused $ snapShrink R Nothing
    , s1 xK_Right . withFocused $ snapGrow   R Nothing
    , s1 xK_Up    . withFocused $ snapShrink D Nothing
    , s1 xK_Down  . withFocused $ snapGrow   D Nothing
}}}3 -}

-- End Custom Mappings ---------------------------------------------------- }}}2

-- Basic Mappings --------------------------------------------------------- {{{2
-- ==============

-- Remapped Defaults ------------------------------------------------------ {{{3
-- * * * * * * * * *

    -- Launch terminal
    , s1 xK_Return $  spawn myTermFull
    
    , m1 xK_z $     spawn $ "xmonad --recompile && notify-send " ++
                            "'XMONAD: Compiled Successfully'"
    , s1 xK_z $     spawn "xmonad --recompile && xmonad --restart"

    -- Increment the number of windows in the master area (was comma)
    , m1 xK_equal $  sendMessage (IncMasterN 1)
    , m1 xK_plus $  sendMessage (IncMasterN 1)

    -- Deincrement the number of windows in the master area (was period)
    , m1 xK_minus $ sendMessage (IncMasterN (-1))

    -- Resize viewed windows to the correct size
    , s1 xK_t refresh

-- End Remapped Defaults }}}3

-- Default Keybindings ---------------------------------------------------- {{{3
-- * * * * * * * * * *

    -- Close the focused window
    , s1 xK_c kill

    --, s1 xK_space $   setLayout myLayoutHook --FIXME

    -- Reset the layouts on the current workspace to default
    --, s1 xK_space $   setLayout $ XMonad.layoutHook conf

-- Window Focus ----------------------------------------------------------- {{{4

    -- Rotate through the available layout algorithms
    , m1 xK_space $   sendMessage NextLayout

    -- move focus up or down the window stack
    , m1 xK_Tab $     windows W.focusDown
    , m1 xK_j   $     windows W.focusDown
    , s1 xK_Tab $     windows W.focusUp
    , m1 xK_k   $     windows W.focusUp

    -- Move focus to the master window
    , m1 xK_m   $     windows W.focusMaster
    , m1 xK_Return $  windows W.swapMaster

    -- Swap the focused window with the next window
    , s1 xK_j $       windows W.swapDown

    -- Swap the focused window with the previous window
    , s1 xK_k $       windows W.swapUp

-- End Window Focus ------------------------------------------------------- }}}4

-- Resizing
    , m1 xK_h $       sendMessage Shrink
    -- Shrink the master area
    , m1 xK_l $       sendMessage Expand
    -- Expand the master area

-- Floating layer support
    , m1 xK_t $ withFocused $ windows . W.sink
    -- Push window back into tiling

    -- quit, or restart
    --, s1 xK_q $ io (exitWith ExitSuccess)

-- End Default Keybindings ------------------------------------------------ }}}3
-- End Basic Mappings ----------------------------------------------------- }}}2

-- Where Block ------------------------------------------------------------ {{{3
-- * * * * * *
    ] where myXPConfig = mkXPConfig myTheme

{-  Mapping Shorthands
    nm   ->  No Mask
    sm   ->  Shift Mask
    m1   ->  Alt Mask
    s1   ->  Alt-Shift Mask
    m4   ->  Super (Caps-Lock) Mask
    s4   ->  Super-Shift Mask
    m1m4 ->  Alt+Super Mask
-}
            nm k a        = ((0, k), a)
            sm k a        = ((shiftMask, k), a)
            m1 k a        = ((mod1Mask, k), a)
            s1 k a        = ((shiftMask .|. mod1Mask, k), a)
            m4 k a        = ((mod4Mask, k), a)
            s4 k a        = ((shiftMask .|. mod4Mask, k), a)
            m1m4 k a      = ((mod1Mask .|. mod4Mask, k), a)
-- Helpers
            spawnMaybe s  = raiseMaybe (spawn $ myTermFull ++ " -e " ++ s)
                              (title =? s)

            vol c         = pulsecmd ++
                              " && notify-send 'Volume " ++ c ++ "d'" ++
                              " || notify-send 'Volume Command `" ++
                              pulsecmd ++ "` failed!'"
              where pulsecmd = "${MY_XMO_SCRIPT_DIR}//pulse-volume.sh "
                                ++ c

            keyHelper     = myTermFull ++ " -e 'vim " ++ "${MY_XMO_CONF_DIR}" ++
                              "/src/keys/KeyHelp.txt'"

            screenShot o  = "scrot " ++ o ++ "${MY_SCROT_DIR}" ++
                              "/%Y-%m-%d-%T-screenshot.png"

-- End Where Block -------------------------------------------------------- }}}3

-- Misc. Globals

myRemoveKeys =  [ (mod1Mask, xK_q) ] :: [ (KeyMask, KeySym) ]

toggleStrutsKey XConfig { XMonad.modMask = modMask } = (modMask, xK_b)
toggleStrutsKey :: (XConfig Layout -> (KeyMask, KeySym))

xK_Lambda = 007235

{-------------------------------------------------------------------------------
    End Keybinds                                                            }}}1
-------------------------------------------------------------------------------}

-- vim: set foldmethod=marker :
