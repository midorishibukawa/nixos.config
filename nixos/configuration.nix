{ config, pkgs, ... }:

{
  imports =
    [ 
      ./hardware-configuration.nix
    ];

  boot = {

    initrd = {
      luks.devices."luks-b618c3bf-eb2d-4eee-a974-856d52962886" = {
        device = "/dev/disk/by-uuid/b618c3bf-eb2d-4eee-a974-856d52962886";
	keyFile = "/crypto_keyfile.bin";
      };
      secrets."/crypto_keyfile.bin" = null;
    };

    loader = {
      efi.canTouchEfiVariables = true;
      systemd-boot.enable = true;
    };
  };

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
      neovim
      wine
      winetricks
      wineWowPackages.stable
      wineWowPackages.staging
      (wine.override { wineBuild = "wine64"; })
    ];
  };

  hardware = {
    bluetooth.enable = true;

    nvidia = {
      modesetting.enable = true;
      nvidiaSettings = true;
      open = false;
      package = config.boot.kernelPackages.nvidiaPackages.stable;
      powerManagement.enable = true;
    };

    opengl = {
      driSupport = true;
      driSupport32Bit = true;
      enable = true;
    };

    pulseaudio = {
      enable = false;
      support32Bit = true;
    };
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
  

  networking = {
    hostName = "midori-nixos";
    networkmanager.enable = true;
  };

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
        lutris
        pavucontrol
        vulkan-tools
      ];
    };
  };

  system.stateVersion = "23.05";
}
