{
  config,
  hostSpecific,
  lib,
  ...
}:

let
  swayCommand =
    if config.programs.sway.package == null then "sway" else lib.getExe config.programs.sway.package;
in
{
  config = lib.mkIf config.services.greetd.enable {
    assertions = [
      {
        assertion = config.programs.sway.enable && config.programs.sway.package != null;
        message = "services.greetd requires programs.sway with a non-null package";
      }
      {
        assertion = config.services.getty.autologinUser == null;
        message = "services.greetd requires services.getty.autologinUser to be disabled";
      }
    ];

    services.greetd = {
      useTextGreeter = true;

      settings = {
        initial_session = {
          command = swayCommand;
          user = hostSpecific.primeUsername;
        };

        default_session = {
          command = "${lib.getExe' config.services.greetd.package "agreety"} --cmd ${swayCommand}";
          user = "greeter";
        };
      };
    };
  };
}
