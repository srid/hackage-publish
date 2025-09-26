# haskell-flake configuration goes in this module.

{ root, inputs, ... }:
{
  imports = [
    inputs.haskell-flake.flakeModule
  ];
  perSystem = { self', lib, config, pkgs, ... }: {
    haskellProjects.default = { config, ... }: {
      projectRoot = builtins.toString (lib.fileset.toSource {
        inherit root;
        fileset = lib.fileset.unions [
          (root + /src)
          (root + /hackage-publish.cabal)
          (root + /LICENSE)
          (root + /README.md)
        ];
      });

      # Add your package overrides here
      settings = {
        hackage-publish = {
          extraBuildDepends = [
            pkgs.cabal-install
            pkgs.coreutils
            pkgs._1password-cli
          ];
          stan = true;
        };
      };

      devShell.tools = _:
        # Bring all the `extraBuildDepends` (above) into the devShell, so
        # cabal/ghcid can resolve `staticWhich`.
        lib.flip lib.concatMapAttrs config.outputs.packages (_: v:
          lib.listToAttrs (lib.map (p: lib.nameValuePair p.name p) v.package.getCabalDeps.buildDepends)
        );

      # What should haskell-flake add to flake outputs?
      autoWire = [ "packages" "apps" "checks" ]; # Wire all but the devShell
    };

    # Default package & app.
    packages.default = self'.packages.hackage-publish;
    apps.default = self'.apps.hackage-publish;
  };
}
