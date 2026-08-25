{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.custom.programs.glabels;
in
{
  options.custom.programs.glabels.enable = lib.mkEnableOption "gLabels label designer";

  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.glabels-qt ];

    # There is a delta between how the cups-driver and glabels deal with size of
    # "stickers"(glabels term is a template). This causes misalignment between what
    # you see on the gui and what actually gets printed on the sticker.
    #
    # By creating custom stickers(based on the cups-driver dimensions, as it is the source of truth) and
    # placing them in a path glabels looks for user stickers this misalignment can be solved.
    #
    # gLabels only discovers manual XML templates named *-templates.xml.
    #
    # To see the dimensions of the sticker in glabels you can use the gui, while to get the dimensions
    # from the driver I used(nas been the host where the printer is connected to):
    #
    # lpoptions -h nas.local:631 -p Dymo -l
    # curl http://nas.local:631/printers/Dymo.ppd
    #
    # From there it is possible to calculate the delta and do the alignment.

    home.file.".glabels/dymo-30327-cups-templates.xml".text = ''
      <?xml version="1.0"?>

      <Glabels-templates>
        <Template brand="Custom DYMO" part="30327 CUPS-aligned" description="File folder labels"
                  size="other" width="19.9mm" height="87.29mm">
          <Meta category="label"/>
          <Meta category="rectangle-label"/>
          <Meta category="filing"/>
          <Label-rectangle id="0" width="14.29mm" height="87.29mm" round="0mm"
                           x_waste="0mm" y_waste="0mm">
            <Markup-margin size="0mm"/>
            <Layout nx="1" ny="1" x0="2.805mm" y0="0mm" dx="19.9mm" dy="87.29mm"/>
          </Label-rectangle>
        </Template>
      </Glabels-templates>
    '';
  };
}
