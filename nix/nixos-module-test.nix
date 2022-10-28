{ pkgs
, upcheck-nixos-module
}:
pkgs.nixosTest (
  { lib, pkgs, ... }: {
    name = "upcheck-module-test";
    nodes.machine = {
      imports = [ upcheck-nixos-module ];
      services.upcheck.enable = true;
    };
    testScript = ''
      machine.start()
      machine.wait_for_unit("multi-user.target")
      machine.systemctl("start upcheck.service --wait")
    '';
  }
)
