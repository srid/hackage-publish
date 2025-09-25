{ inputs, ... }:
{
  perSystem = { pkg, system, lib, ... }: {
    _module.args.pkgs = import inputs.nixpkgs {
      inherit system;
      config = {
        allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [
          "1password-cli"
        ];
      };
    };
  };
}
