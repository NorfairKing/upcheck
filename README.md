# UpCheck

A declarative checker for website uptime to run continuously for monitoring.

This way you can be notified if any of your sites are not up.


1. Write a spec file `spec.yaml` like this:

```
checks:
- get: "https://cs-syd.eu"
  status: 200
- get: "http://cs-syd.eu"
  status: 301
  location: "https://cs-syd.eu/"
```

2. Run `upcheck spec.yaml`

   The exit code will be `1` if any of the specified checks fail and the output will look like this:

   ![Example output](/example.png)


See the 'examples' directory for more example specifications.


## Running on NixOS

Have a look at [the provided nixos module](/nix/module.nix)
