{ config, lib, ... }:
let
  cfg = config.custom.services.xmrig;
in
{
  options.custom.services.xmrig.numberOfThreads = lib.mkOption {
    type = lib.types.ints.between 1 256; # You can raise the max as needed
    default = 1;
    description = "Number of threads for XMRig to use (via rx array)";
  };
  config = lib.mkIf config.services.xmrig.enable {
    services.xmrig = {
      settings = {
        # without nixos on first run miner would look at the config file, find the messing parts,
        # generate them into a file and write them to disk, But as nixos responsible to generate the
        # configuration file and "point" miner explicitly to use them even if miner generates a file
        # it wil be ignored.
        autosave = false;
        # xmrig provides an official client for the API at http://workers.xmrig.info/
        # The http server acts in two modes:
        #
        # restrected, which provides read only informatoin which is set by:
        # access-token = null;
        # restrected = true;
        # and can be tested by exectuing:
        #
        # curl http://127.0.0.1:18643/1/summary
        #
        # unrestricted, which provides a way to access and maybe even edit the config, which
        # is not relevant in nixos generated config.
        # access-token = "<your token>";
        # restrected = false;
        # and can be tested by executing:
        #
        # curl -H "Authorization: Bearer <your token>" http://127.0.0.1:18643/1/summary

        http = {
          enabled = true;
          host = config.custom.shared.anyIPv4;
          port = 18643;
          access-token = null;
          restricted = true;
        };
        cpu = {
          enabled = true;
          # Not relevant as in the case of nixos there is always config file, meaning this hint will be
          # ignored. the only reason I left this config(commented out) is as a reminder for myserlf to to use it.
          # max-threads-hint = 8;
          yield = true; # Prefer better system response/stability (defaults to true)
          # I need to look into this.
          # priority = 1;

          # Linux only!
          # without setting this option at all my system enabled it on runtime. meaning
          # my system can use them. The optoin itself provides 1-3% speedup.
          huge-pages = true;

          # execute nproc to find number of threads.
          # RandomX profile (used for Monero): 8 threads, no affinity, intensity = 1 (default)
          # intensity = 1, which is the max at the moment for all RandomX variants and all Argon2 variants.
          rx = lib.genList (_: -1) cfg.numberOfThreads;
          #
        };
        opencl = false;
        cuda = false;
        pools = [
          {
            url = "${config.custom.shared.localHostIPv4}:${toString config.custom.services.p2pool.stratumPort}";
            keepalive = true;
            tls = false;
          }
        ];
      };
    };
  };
}
