{ config, pkgs, musnix, ... }:

{
    imports = [ ];

    console = {
        useXkbConfig = true;
    };
    environment = {
        binsh = "${pkgs.zsh}/bin/zsh";
        localBinInPath = true;

        sessionVariables = rec {
            NIX_DATA_DIR = "$HOME/.local/etc/nix";
            XDG_BIN_HOME = "$HOME/.local/bin";
            XDG_CACHE_HOME = "$HOME/.local/cache";
            XDG_CONFIG_HOME = "$HOME/.local/config";
            XDG_DATA_HOME = "$HOME/.local/data";
            XDG_STATE_HOME = "$HOME/.local/state";
            XINITRC="$XDG_CONFIG_HOME/X11/xinitrc";
            XSERVERRC="$XDG_CONFIG_HOME/X11/xserverrc";
            STARSHIP_CACHE = "$XDG_CACHE_HOME/starship/cache";
            ZDOTDIR = "$XDG_CONFIG_HOME/zsh";
            USERXSESSION="$XDG_CACHE_HOME/X11/xsession";
            USERXSESSIONRC="$XDG_CACHE_HOME/X11/xsessionrc";
            ALTUSERXSESSION="$XDG_CACHE_HOME/X11/Xsession";
            ERRFILE="$XDG_CACHE_HOME/X11/xsession-errors";
        };

        systemPackages = with pkgs; [
            libjack2 jack2 qjackctl pavucontrol jack2Full jack_capture
            feh
            cudatoolkit
            htop-vim
            greetd.tuigreet
            xmonad-with-packages
            xorg.xmodmap
        ];
    };
    
    fonts = {
        fontconfig = {
            defaultFonts = {
                monospace = [
                    "JetBrainsMono"
                    "SourceCodePro"
                ];
                sansSerif = [
                    "SourceSansPro"
                    "IPAPGothic"
                ];
                serif = [
                    "SourceSerifPro"
                    "IPAPMincho"
                ];
            };
        };
        # packages = with pkgs; [
        fonts = with pkgs; [
            ipafont
            kochi-substitute
            (nerdfonts.override { fonts = [ "JetBrainsMono" "SourceCodePro" ]; })
        ];
    };

    i18n = {
        defaultLocale = "en_GB.UTF-8";

        inputMethod = {
            enabled = "fcitx5";
            fcitx5.addons = with pkgs; [
                fcitx5-mozc
                fcitx5-gtk
            ];
        };

        extraLocaleSettings = {
            LC_ADDRESS = "pt_BR.UTF-8";
            LC_IDENTIFICATION = "pt_BR.UTF-8";
            LC_MEASUREMENT = "pt_BR.UTF-8";
            LC_MONETARY = "pt_BR.UTF-8";
            LC_NAME = "pt_BR.UTF-8";
            LC_NUMERIC = "pt_BR.UTF-8";
            LC_PAPER = "pt_BR.UTF-8";
            LC_TELEPHONE = "pt_BR.UTF-8";
            LC_TIME = "pt_BR.UTF-8";
        };
    };

    networking.firewall = {
        allowedTCPPorts = [ 3000 ];
        allowedUDPPorts = [ 53 ];
    };

    nix = {
        optimise.automatic = true;
        settings.experimental-features = [ "nix-command" "flakes" ];
    };

    nixpkgs.config.allowUnfree = true;

    programs = {
        dconf.enable = true;
        steam = {
            enable = true;
            remotePlay.openFirewall = true;
            dedicatedServer.openFirewall = true;
        };
        zsh = {
            enable = true;
            shellAliases = {
                ls = "ls -a --color=auto";
                ll = "ls -lah --color=auto";
            };
        };
    };

    security.rtkit.enable = true;

    services = {
        adguardhome = {
            enable = true;
            openFirewall = true;
            settings = {
                bind_port = 3000;
                schema_version = 20;
            };
        };
        blueman.enable = true;
        flatpak.enable = true;
        printing.enable = true;

        pipewire = {
            enable = true;

            pulse.enable = true;
            alsa = {
                enable = true;
                support32Bit = true;
            };
        };

        xserver = {
            displayManager = {
  		        lightdm.enable = true;
            };
            enable = true;
            layout = "br";
            libinput.mouse.naturalScrolling = true;
            videoDrivers = ["nvidia"];
            xkbVariant = "";
            xkbOptions = "caps:escape";

            windowManager.xmonad = {
                config = builtins.readFile ../users/config/xmonad/xmonad.hs;
                enable = true;
                enableContribAndExtras = true;
            };
        };
    };

    boot.kernelModules = [ "snd-seq" "snd-rawmidi" ];
    hardware.pulseaudio.package = pkgs.pulseaudio.override { jackaudioSupport = true; };
    security.sudo.extraConfig = ''
        moritz ALL=(ALL) NOPASSWD: ${pkgs.systemd}/bin/systemctl
    '';

    musnix = {
        enable = true;
        alsaSeq.enable = false;
        rtirq = {
            resetAll = 1;
            prioLow = 0;
            enable = true;
            nameList = "rtc9 snd";
        };
    };

    sound.enable = true;

    time.timeZone = "America/Sao_Paulo";

    xdg.portal = {
        # config.common.default = "*";
        enable = true;
        extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
    };

    users = {
        defaultUserShell = pkgs.zsh;

        users.midori = {
            isNormalUser = true;
            description = "midori";
            extraGroups = [ "networkmanager" "wheel" "scanner" "docker" "jackaudio" "audio" ];
        };
    };

    system.stateVersion = "23.05";
}
