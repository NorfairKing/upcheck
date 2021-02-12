{ lib, pkgs, config, ... }:
with lib;

let
  upcheck = (import ../.).upcheck;
  upCheckConfig = ./up-check-config.yaml;
  cfg = config.services.up-checker;
  upcheckName = "up-checker";
  upcheckTimer = {
    wantedBy = [ "timers.target" ];
    timerConfig = {
      OnCalendar = "minutely";
      Persistent = true;
    };
  };
  configFile = pkgs.writeText "upcheck-config-file" ''
    ${cfg.extraVerbatimConfig}
  '';
  upcheckService = {
    description = "Website Up Checker";
    path = with pkgs; [
      upcheck
    ];
    script = ''
      set +e # We need error codes to work this way.

      tmpfile=~/.up-checker
      ${upcheck}/bin/upcheck ${configFile} 2>&1 > "$tmpfile"
      if [[ "$?" != "0" ]]
      then
        cat "$tmpfile" | ${cfg.notifyCommand}
      fi
      rm -f "$tmpfile"
    '';
  };

in
{
  options.services.up-checker = {
    enable = mkEnableOption "Website Up Checker";
    extraVerbatimConfig = mkOption {
      type = types.str;
      default = "";
      description = "Extra configuration, verbatim";
    };
    notifyCommand = mkOption {
      type = types.str;
      default = "false";
      description = "The command to pass the output to in case of a failure";
    };
  };
  config.systemd = mkIf cfg.enable {
    timers = { "${upcheckName}" = upcheckTimer; };
    services = { "${upcheckName}" = upcheckService; };
  };
}
