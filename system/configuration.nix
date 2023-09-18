{ config, pkgs, ... }:

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
            STARSHIP_CACHE = "$XDG_CACHE_HOME/starship/cache";
            ZDOTDIR = "$XDG_CONFIG_HOME/zsh";
        };

        systemPackages = with pkgs; [
            bitwarden
            bitwarden-cli
            xmonad-with-packages
            xorg.xmodmap
        ];
    };
    
    fonts.fonts = with pkgs; [
        (nerdfonts.override { fonts = [ "JetBrainsMono" ]; })
    ];

    i18n = {
        defaultLocale = "en_GB.UTF-8";

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

    nix.settings.experimental-features = [ "nix-command" "flakes" ];

    nixpkgs.config.allowUnfree = true;

    programs = {
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
        blueman.enable = true;
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
                sessionCommands = "xmodmap -e \"keycode 9 = apostrophe quotedbl apostrophe quotedbl\"";
            };
            enable = true;
            layout = "br";
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

    sound.enable = true;

    time.timeZone = "America/Sao_Paulo";

    users = {
        defaultUserShell = pkgs.zsh;

        users.midori = {
            isNormalUser = true;
            description = "midori";
            extraGroups = [ "networkmanager" "wheel" ];
        };
    };

    system.stateVersion = "23.05";
}
