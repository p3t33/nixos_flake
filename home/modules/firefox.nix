{config, pkgs, ...}:
{
    programs.firefox = {
        enable = true;

        profiles.${config.userDefinedGlobalVariables.username} = {
            # to verify inter into the search bar about:profiles
            id = 0;
            name = "${config.userDefinedGlobalVariables.username}";
            isDefault = true;
            # The list bellow reflects the configuration that are available from
            # the search bar using about:config
            settings = {
                # Disable Pocket Integration
                "browser.newtabpage.activity-stream.section.highlights.includePocket" = false;
                "extensions.pocket.enabled" = false;
                "extensions.pocket.api" = "";
                "extensions.pocket.oAuthConsumerKey" = "";
                "extensions.pocket.showHome" = false;

                # Disable firefox from asking to save passwords
                "extensions.pocket.site" = "";
                "signon.rememberSignons" = false;
                "signon.autofillForms" = false;
                "signon.formlessCapture.enabled" = false;

                # Enable HTTPS-Only Mode
                "dom.security.https_only_mode" = true;
                "dom.security.https_only_mode_ever_enabled" = true;

                # Privacy settings
                "privacy.donottrackheader.enabled" = true;
                "privacy.trackingprotection.enabled" = true;
                "privacy.trackingprotection.socialtracking.enabled" = true;
                "privacy.partition.network_state.ocsp_cache" = true;

                # Disable all sorts of telemetry
                "browser.newtabpage.activity-stream.feeds.telemetry" = false;
                "browser.newtabpage.activity-stream.telemetry" = false;
                "browser.ping-centre.telemetry" = false;
                "toolkit.telemetry.archive.enabled" = false;
                "toolkit.telemetry.bhrPing.enabled" = false;
                "toolkit.telemetry.enabled" = false;
                "toolkit.telemetry.firstShutdownPing.enabled" = false;
                "toolkit.telemetry.hybridContent.enabled" = false;
                "toolkit.telemetry.newProfilePing.enabled" = false;
                "toolkit.telemetry.reportingpolicy.firstRun" = false;
                "toolkit.telemetry.shutdownPingSender.enabled" = false;
                "toolkit.telemetry.unified" = false;
                "toolkit.telemetry.updatePing.enabled" = false;

                # As well as Firefox 'experiments'
                "experiments.activeExperiment" = false;
                "experiments.enabled" = false;
                "experiments.supported" = false;
                "network.allow-experiments" = false;

            };

            search.engines = {
                "Nix Packages" = {
                    urls = [{
                        template = "https://search.nixos.org/packages";
                        params = [
                        { name = "type"; value = "packages"; }
                        { name = "query"; value = "{searchTerms}"; }
                        ];
                    }];

                    icon = "${pkgs.nixos-icons}/share/icons/hicolor/scalable/apps/nix-snowflake.svg";
                    definedAliases = [ "@np" ];
                };
            };
            search.force = true;

            bookmarks = [
                {
                    name = "wikipedia";
                    tags = [ "wiki" ];
                    keyword = "wiki";
                    url = "https://en.wikipedia.org/wiki/Special:Search?search=%s&go=Go";
                }
            ];


        };


    };
}
