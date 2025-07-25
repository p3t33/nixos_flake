{
  services.xmrig = {
    enable = true;

    settings = {
      autosave = true;
      cpu = true;
      opencl = false;
      cuda = false;
      pools = [
        {
          url = "127.0.0.1:3333";
          keepalive = true;
          tls = false;
        }
      ];
    };
  };
}
