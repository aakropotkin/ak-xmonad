
{ pkgs, config, nixosConfig, lib, ... }:

let

  inherit (lib) optionalAttrs recursiveUpdate;

  hostMachine = nixosConfig.networking.hostName;

  myBackground = ../nixpkgs/themes/wallpapers/hex.jpg; # FIXME as Option.

  pulse-switch = ./src/scripts/pulse-switch.sh;
  pulse-volume = ./src/scripts/pulse-volume.sh;

  homeDir = "/home/camus";

in {

  config = {

    programs.autorandr = {
      enable = true;
      hooks = {
        postswitch = {
          "resetBackground" = "systemctl --user restart fehBackground.service";
        };
      };
      profiles."vhh" = {
        fingerprint =
          let
            benq_fingerprint = "00ffffffffffff0009d1e77845540000" +
                               "1b1d0104a5351e783a0565a756529c27" +
                               "0f5054a56b80d1c0b300a9c081808100" +
                               "81c001010101023a801871382d40582c" +
                               "45000f282100001e000000ff0035374b" +
                               "30303239313031510a20000000fd0032" +
                               "4c1e5311010a202020202020000000fc" +
                               "0042656e51204757323438300a200182" +
                               "02031cf14f901f041303120211011406" +
                               "071516052309070783010000023a8018" +
                               "71382d40582c45000f282100001f011d" +
                               "8018711c1620582c25000f282100009f" +
                               "011d007251d01e206e2855000f282100" +
                               "001e8c0ad08a20e02d10103e96000f28" +
                               "21000018000000000000000000000000" +
                               "0000000000000000000000000000008d";
          in {
          "DP-0" = benq_fingerprint;
          "DP-2" = benq_fingerprint;
          "DP-4" = benq_fingerprint;
        };
        config = {
          DP-4 = {
            crtc     = 0;
            enable   = true;
            primary  = true;
            position = "0x0";
            mode     = "1920x1080";
            rotate   = "right";
            rate     = "60.00";
          };
          DP-0 = {
            crtc     = 1;
            enable   = true;
            position = "1080x420";
            mode     = "1920x1080";
            rotate   = "normal";
            rate     = "60.00";
          };
          DP-2 = {
            crtc     = 2;
            enable   = true;
            position = "3000x420";
            mode     = "1920x1080";
            rotate   = "normal";
            rate     = "60.00";
          };
        };
      };
    };

    xsession = {
      enable = true;
      windowManager.command = ''
        export PATH="${homeDir}/bin:''${PATH}"
        ${pkgs.xmonad-configured}/bin/xmonad
      '';

      initExtra = with lib; ''
        ${optionalString ( hostMachine == "alonso" ) "${pulse-switch} init;"}
        ${pulse-volume} reset;
        ${optionalString ( hostMachine == "petr" ) "${pulse-volume} mute;"}
      '';
    }; # END XSession

    home.packages = with pkgs; [
      #volumebar
      xmonad-configured
      set_wm_class
      #myXmonad
      #dwm-configured
    ] ++ ( if ( hostMachine != "kant" ) then [myXmobars] else [] );

    services = {

      stalonetray = {
        enable = true;
        config = {
          geometry =
            if      ( hostMachine == "petr"   ) then "3x1-650+8"
            else if ( hostMachine == "angela" ) then "5x1-400+8"
            else if ( hostMachine == "alonso" ) then "5x1+680+8"
            else if ( hostMachine == "kant"   ) then "3x1-423+8"
            else                                     null;
          decorations = null;
          icon_size = 24;
          sticky = true;
          background = config.colors.background;
        };
      };

    }; # END Services

    systemd.user.services =
    let
      serviceTemplate = {
        Unit = {
          Description = "Spawns a program alongside a graphical session";
          After       = ["graphical-session-pre.target"];
          PartOf      = ["graphical-session.target"];
        };
        Service = {
          Type              = "simple";
          ExecStart         = "echo Hello, World";
          IOSchedulingClass = "idle"; # Lowest priority
        };
        Install.WantedBy = ["graphical-session.target"];
      };

      mkGraphicalService = desc: cmd:
        recursiveUpdate serviceTemplate {
            Unit.Description  = desc;
            Service.ExecStart = cmd;
          };

      mkOneShotGraphicalService = desc: cmd:
        recursiveUpdate ( mkGraphicalService desc cmd ) {
            Service.Type = "oneshot";
          };

    in {

      backgroundFeh = mkOneShotGraphicalService
        "Sets the User's Background Image."
        "${pkgs.feh}/bin/feh --bg-fill ${myBackground}";

    } // ( optionalAttrs ( hostMachine == "alonso" ) {

      autorandr = mkOneShotGraphicalService
        "Apply autorandr config."
        "${pkgs.autorandr}/bin/autorandr -c";
      
      myXmobarsCenter = recursiveUpdate
        ( mkGraphicalService
          "Spawns Xmobar for Alonso's center monitor."
          "${pkgs.myXmobars}/bin/myXmobars center"
        ) { Service.Environment = [ "PATH=${pkgs.hostname}/bin" ]; };

      myXmobarsRight = recursiveUpdate
        ( mkGraphicalService
          "Spawns Xmobar for Alonso's right monitor."
          "${pkgs.myXmobars}/bin/myXmobars right"
        ) { Service.Environment = [ "PATH=${pkgs.hostname}/bin" ]; };

      analogMeters = mkGraphicalService
        "Controls analog meters."
        "${reportScript}";

    } ); # END Systemd.User.Services

  }; # END Config
}
