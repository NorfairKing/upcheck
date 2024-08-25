{ upcheck }:
{ lib, pkgs, config, ... }:
with lib;

{
  options.services.upcheck = {
    enable = mkEnableOption "Website Up Checker";
    config = mkOption {
      default = { };
      type = import ../upcheck/options.nix { inherit lib; };
    };
    onCalendar = mkOption {
      type = types.str;
      default = "hourly";
      description = "The OnCalendar part of the systemd timer";
    };
    notifyCommand = mkOption {
      type = types.str;
      default = "false";
      description = "The command to pass the output to in case of a failure";
    };
  };
  config =
    let
      cfg = config.services.upcheck;
      upcheckName = "upcheck";
      upcheckTimer = {
        wantedBy = [ "timers.target" ];
        timerConfig = {
          OnCalendar = cfg.onCalendar;
          Persistent = true;
        };
      };
      configFile = (pkgs.formats.yaml { }).generate "upcheck-config.yaml" cfg.config;
      settingsCheck = pkgs.runCommand "upcheck-settings-check" { } ''
        ${upcheck}/bin/upcheck ${configFile} --help > $out
      '';
      upcheckService = {
        description = "Website Up Checker";
        path = [ upcheck ];
        script = ''
          # We need error codes to work this way.
          # We also need this to make sure that the service still fails
          # correctly when the notification command fails.
          set +e
          set -x # See what's happening.

          tmpfile=~/.upcheck
          ${upcheck}/bin/upcheck ${configFile} 2>&1 > "$tmpfile"
          exitcode="$?"
          if [[ "$exitcode" != "0" ]]
          then
            cat "$tmpfile" | ${cfg.notifyCommand}
            rm -f "$tmpfile"
            # Make sure the service still fails.
            exit "$exitcode"
          fi
        '';
        documentation = [ "${settingsCheck}" ];
      };

    in
    mkIf cfg.enable {
      environment.systemPackages = [ upcheck ];
      systemd = {
        timers = { "${upcheckName}" = upcheckTimer; };
        services = { "${upcheckName}" = upcheckService; };
      };
    };
}
