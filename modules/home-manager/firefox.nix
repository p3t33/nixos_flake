{config, pkgs, ...}:
{
    programs.firefox = {
        enable = true;
        policies = {
            ExtensionSettings = with builtins;
            let extension = shortId: uuid: {
              name = uuid;
              value = {
                install_url = "https://addons.mozilla.org/firefox/downloads/latest/${shortId}/latest.xpi";
                installation_mode = "normal_installed";
              };
            };
            # They way to get uuid is to install the plugin in a regular manner the plugin you would like to declare
            # and then from the firefox url execute about:debugging#/runtime/this-firefox
            # copy the Extention ID(and not the internal UUID) and uninstal the plugin.
            #
            # to get the  uuid/name value you will need to do some trial and error and see that the url is valild and when used from
            # firefox url it will trigger installtion(replace ${shortid} with
            # the uuid in the url address..
            # In most cases if the name of the plugin is more then one word you will use - between them.
            # Its best to go the the page of the addond and copy its url and from there to try and find out its shortId.
            #
            # shortId that are email do not go into curly brackets. while the
            # hash looking shortId do. Failing to do so will prevent the
            # installtion of the plugin.
            #
            # it is important to undertand that failng to provide a valid
            # shorId(with or wihoht the curly brackets) or uuid/name will not
            # fail the build of homw manger but you won't get the plugin inside
            # of firefox one you restart it.

            in listToAttrs [
              (extension "ublock-origin" "uBlock0@raymondhill.net")
              (extension "privacy-badger17" "jid1-MnnxcxisBPnSXQ@jetpack")
              (extension "mooltipass-extension" "{da36041e-5ce1-4c8a-9dc5-406cc98f0a23}")
              (extension "yet-another-speed-dial" "yet_another_speed_dial@conceptualspace.net")
              (extension "tree-style-tab" "treestyletab@piro.sakura.ne.jp")
              (extension "vimium-ff" "{d7742d87-e61d-4b78-b8a1-b469842139fa}")
              (extension "hover-zoom-plus" "{92e6fe1c-6e1d-44e1-8bc6-d309e59406af}")
              (extension "shortkeys" "Shortkeys@Shortkeys.com")
              (extension "simple-tab-groups" "simple-tab-groups@drive4ik") # conflicts with "Tab Session Manager"
              (extension "session-boss" "sessionboss@william.wong")
            ];
        };

        profiles.${config.userDefinedGlobalVariables.primeUsername} = {
            # to verify inter into the search bar about:profiles
            id = 0;
            name = "${config.userDefinedGlobalVariables.primeUsername}";
            isDefault = true;
            # The list bellow reflects the configuration that are available from
            # the search bar using about:config
            settings = {
                "browser.startup.page" = 3; # This line is added to automatically restore the session
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
