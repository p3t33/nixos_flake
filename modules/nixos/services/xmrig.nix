{ config, lib, ... }:
{
  config = lib.mkIf config.services.xmrig.enable {
    services.xmrig = {
      settings = {
        # without nixos on first run miner would look at the config file, find the messing parts,
        # generate them into a file and write them to disk, But as nixos responsible to generate the
        # configuration file and "point" miner explicitly to use them even if miner generates a file
        # it wil be ignored.
        autosave = false;
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
          rx = lib.genList (_: -1) 24;
          #
        };
        opencl = false;
        cuda = false;
        pools = [
          {
            url = "${config.customGlobal.localHostIPv4}:${toString config.custom.services.p2pool.stratumPort}";
            keepalive = true;
            tls = false;
          }
        ];
      };
    };
  };
}
