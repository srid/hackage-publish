-- Pipeline configuration for Vira <https://vira.nixos.asia/>
\ctx pipeline ->
  pipeline
    { signoff.enable = True
    , cachix.enable = False
    , attic.enable = False
    }
