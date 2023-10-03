{ config, lib, pkgs, modulesPath, ... }:

{
  imports = [ 
    (modulesPath + "/installer/scan/not-detected.nix")
  ];

  boot = {
    extraModulePackages = [ ];
    kernelModules = [ "kvm-intel" ];
    
    initrd = {
      availableKernelModules = [ "xhci_pci" "ahci" "nvme" "usbhid" "usb_storage" "sd_mod" ];
      kernelModules = [ ];
      secrets."/crypto_keyfile.bin" = null;
      luks.devices = {
        "luks-4e682116-e39e-49e7-b3d7-89fa66922f3a" = {
          device = "/dev/disk/by-uuid/4e682116-e39e-49e7-b3d7-89fa66922f3a";
  	  keyFile = "/crypto_keyfile.bin";
        };
  	"luks-84ed79c3-bb5a-41ec-bb22-d0aa4a846a55" = {
          device = "/dev/disk/by-uuid/84ed79c3-bb5a-41ec-bb22-d0aa4a846a55";
        };
      };
    };

    loader = {
      efi.canTouchEfiVariables = true;
      systemd-boot.enable = true;
    };
  };
  
  fileSystems = {
    "/" = 
    { device = "/dev/disk/by-uuid/934b7216-e38b-4606-ade0-63ab4deeee24";
      fsType = "xfs";
    };
    "/boot" =  
    { device = "/dev/disk/by-uuid/15E3-834B";
      fsType = "vfat";
    };
  };


  hardware = {
    bluetooth.enable = true;
    cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;

    nvidia = {
      modesetting.enable = true;
      nvidiaSettings = true;
      open = false;
      package = config.boot.kernelPackages.nvidiaPackages.stable;
      powerManagement.enable = true;
      prime = {
        sync.enable = true;
	    intelBusId = "PCI:0:0:0";
	    nvidiaBusId = "PCI:1:0:0";
      };
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
  
  networking = {
    hostName = "nixos";
    networkmanager.enable = true;
    useDHCP = lib.mkDefault true;
  };
  
  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
  
  swapDevices =
    [ { device = "/dev/disk/by-uuid/1a61ccfd-408d-44ad-b637-93cd35792f7a"; }
    ];

}
