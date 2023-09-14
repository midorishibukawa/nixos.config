{ config, pkgs, ... }:

{
  imports = [ 
  ];

  console.keyMap = "br-abnt2";

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
    };

    systemPackages = with pkgs; [
      git
      neovim
    ];
  };

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
    
    neovim = {
      enable = true;
      defaultEditor = true;
    };
    
    steam = {
      enable = true;
      remotePlay.openFirewall = true;
      dedicatedServer.openFirewall = true;
    };

    zsh = {
      enable = true;
      histSize = 10000;
      
      shellAliases = {
        ll = "ls -l";
	update = "sudo nixos-rebuild switch";
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
      displayManager.lightdm.enable = true;
      enable = true;
      layout = "br";
      videoDrivers = ["nvidia"];
      xkbVariant = "";

      windowManager.xmonad = {
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

      packages = with pkgs; [
        firefox
	git
	gnupg
	kitty
        lutris
        pavucontrol
	pinentry
        vulkan-tools
      	wine
      	winetricks
        wineWowPackages.stable
        wineWowPackages.staging
        (wine.override { wineBuild = "wine64"; })
      ];
    };
  };

  system.stateVersion = "23.05";
}
