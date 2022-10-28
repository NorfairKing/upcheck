{ upcheck }:
{ lib, pkgs, config, ... }:
with lib;

{
  options.services.upcheck = {
    enable = mkEnableOption "Website Up Checker";
    checks = mkOption {
      default = [ ];
      type = types.listOf (types.submodule {
        options = {
          uri = mkOption {
            type = types.str;
            example = "a";
            description = "The uri to check";
          };
          status = mkOption {
            type = types.nullOr types.int;
            default = null;
            example = "200";
            description = "The status to expect";
          };
          location = mkOption {
            type = types.nullOr types.str;
            default = null;
            description = "The redirect to expect";
          };
        };
      });
    };
    verbatimConfig = mkOption {
      type = types.str;
      default = "";
      description = "Extra configuration, verbatim";
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
  config.systemd =
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
      configFile =
        if cfg.checks == [ ] && !builtins.isNull cfg.verbatimConfig
        then pkgs.writeText "upcheck-config-file" cfg.verbatimConfig
        else
          (pkgs.formats.yaml { }).generate "upcheck-config.yaml" {
            checks = cfg.checks;
          };
      upcheckService = {
        description = "Website Up Checker";
        path = [ upcheck ];
        script = ''
          set +e # We need error codes to work this way.

          tmpfile=~/.upcheck
          ${upcheck}/bin/upcheck ${configFile} 2>&1 > "$tmpfile"
          if [[ "$?" != "0" ]]
          then
            cat "$tmpfile" | ${cfg.notifyCommand}
          fi
          rm -f "$tmpfile"
        '';
      };

    in
    mkIf cfg.enable {
      timers = { "${upcheckName}" = upcheckTimer; };
      services = { "${upcheckName}" = upcheckService; };
    };
}
