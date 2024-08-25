{ lib }:
lib.types.submodule {
  options = {
    checks = lib.mkOption {
      default = [];
      description = "The checks to perform";
      type = lib.types.nullOr (lib.types.listOf (lib.types.submodule {
        options = {
          get = lib.mkOption {
            description = "The URL to GET";
            type = lib.types.str;
          };
          location = lib.mkOption {
            default = null;
            description = "The expected location for a redirect";
            type = lib.types.nullOr lib.types.str;
          };
          status = lib.mkOption {
            default = null;
            description = "The expected status code. If this is not supplied, any status code will pass the test, as long as the server replied.";
            type = lib.types.nullOr lib.types.int;
          };
        };
      }));
    };
    retry-policy = lib.mkOption {
      default = {
        base-delay = null;
        max-retries = null;
      };
      description = "The retry policy for flaky checks due to network failures etc";
      type = lib.types.nullOr (lib.types.submodule {
        options = {
          base-delay = lib.mkOption {
            default = 100000;
            description = "The delay between the first and second try, in microseconds";
            type = lib.types.nullOr lib.types.ints.unsigned;
          };
          max-retries = lib.mkOption {
            default = 10;
            description = "The maximum number of retries";
            type = lib.types.nullOr lib.types.ints.unsigned;
          };
        };
      });
    };
  };
}
