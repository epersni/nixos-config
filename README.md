# nixos-config
My reproducible NixOS and Home Manager configurations using flakes.

### Rebuild NixOS system
```bash
sudo nixos-rebuild switch --flake path/to/nixos-config#desktop
```

### Rebuild home-manager configuration
```bash
home-manager switch --flake .#nicklas@desktop
```

