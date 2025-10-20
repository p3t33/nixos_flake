{config, lib, hostSpecific, ...}:
let
  pathToUsenetDirectory = "${config.customHostSpecificGlobalOptions.pathToMediaDirectory}/usenet";
in
{
  options.custom.services.sabnzbd = {
    httpPort = lib.mkOption {
      type = lib.types.int;
      default = 8080;
      description = "sabnzbd port";
    };
  };

  config = lib.mkIf config.services.sabnzbd.enable {
    services.sabnzbd = {
      group = config.customGlobal.mediaGroup;
      # configFile = "/var/lib/sabnzbd/sabnzbd.ini"; # will be created if not exist
      openFirewall = true;     # expose web interface port (8080 by default)
      # log_dir and admin_dir needs to be defined with absolute paths or they will try to write
      # relative to the path of the secrets file and fail.
      configFile = config.sops.templates."sabnzbd.ini".path;
    };

    systemd.tmpfiles.rules = [
      "d ${pathToUsenetDirectory} 0770 ${config.services.sabnzbd.user} ${config.customGlobal.mediaGroup} -"
      "d ${pathToUsenetDirectory}/complete 0770 ${config.services.sabnzbd.user} ${config.customGlobal.mediaGroup} -"
      "d ${pathToUsenetDirectory}/incomplete 0770 ${config.services.sabnzbd.user} ${config.customGlobal.mediaGroup} -"
    ];


    sops.secrets."sabnzbd/api_key" = { };
    sops.secrets."sabnzbd/nzb_key" = { };
    sops.secrets."sabnzbd/servers/zero/name" = { };
    sops.secrets."sabnzbd/servers/zero/displayname" = { };
    sops.secrets."sabnzbd/servers/zero/host" = { };
    sops.secrets."sabnzbd/servers/zero/username" = { };
    sops.secrets."sabnzbd/servers/zero/password" = { };
    sops.secrets."sabnzbd/servers/zero/port" = { };



    sops.templates."sabnzbd.ini" = {
      owner = config.services.sabnzbd.user;
      group = config.services.sabnzbd.group;
      mode = "0640";
      restartUnits = [ config.systemd.services.sabnzbd.name ];


      content = ''
        __version__ = 19
        __encoding__ = utf-8
        [misc]
        helpful_warnings = 1
        queue_complete = ""
        queue_complete_pers = 0
        bandwidth_perc = 100
        refresh_rate = 1
        interface_settings = ""
        queue_limit = 20
        config_lock = 0
        fixed_ports = 1
        notified_new_skin = 0
        direct_unpack_tested = 1
        sorters_converted = 1
        check_new_rel = 1
        auto_browser = 0
        language = en
        enable_https_verification = 1
        host = "${config.customGlobal.anyIPv4}"
        port = "${toString config.custom.services.sabnzbd.httpPort}"
        https_port = ""
        username = ""
        password = ""
        bandwidth_max = ""
        cache_limit = 1G
        web_dir = Glitter
        web_color = Auto
        https_cert = server.cert
        https_key = server.key
        https_chain = ""
        enable_https = 0
        inet_exposure = 0
        api_key = "${config.sops.placeholder."sabnzbd/api_key"}"
        nzb_key = "${config.sops.placeholder."sabnzbd/nzb_key"}"
        socks5_proxy_url = ""
        permissions = 775
        download_dir = "${pathToUsenetDirectory}/incomplete"
        download_free = ""
        complete_dir = "${pathToUsenetDirectory}/complete"
        complete_free = ""
        fulldisk_autoresume = 0
        script_dir = ""
        nzb_backup_dir = ""
        admin_dir = /var/lib/sabnzbd/admin
        backup_dir = ""
        dirscan_dir = ""
        dirscan_speed = 5
        password_file = ""
        log_dir = /var/lib/sabnzbd/logs
        max_art_tries = 3
        top_only = 0
        sfv_check = 1
        script_can_fail = 0
        enable_recursive = 1
        flat_unpack = 0
        par_option = ""
        pre_check = 0
        nice = ""
        win_process_prio = 3
        ionice = ""
        fail_hopeless_jobs = 1
        fast_fail = 1
        auto_disconnect = 1
        pre_script = None
        end_queue_script = None
        no_dupes = 0
        no_series_dupes = 0
        no_smart_dupes = 0
        dupes_propercheck = 1
        pause_on_pwrar = 1
        ignore_samples = 0
        deobfuscate_final_filenames = 1
        auto_sort = ""
        direct_unpack = 0
        propagation_delay = 0
        folder_rename = 1
        replace_spaces = 0
        replace_underscores = 0
        replace_dots = 0
        safe_postproc = 1
        pause_on_post_processing = 0
        enable_all_par = 0
        sanitize_safe = 0
        cleanup_list = ,
        unwanted_extensions = ,
        action_on_unwanted_extensions = 0
        unwanted_extensions_mode = 0
        new_nzb_on_failure = 0
        history_retention = ""
        history_retention_option = all
        history_retention_number = 1
        quota_size = ""
        quota_day = ""
        quota_resume = 0
        quota_period = m
        enable_tv_sorting = 0
        tv_sort_string = ""
        tv_categories = tv,
        enable_movie_sorting = 0
        movie_sort_string = ""
        movie_sort_extra = -cd%1
        movie_categories = movies,
        enable_date_sorting = 0
        date_sort_string = ""
        date_categories = tv,
        schedlines = ,
        rss_rate = 60
        ampm = 0
        start_paused = 0
        preserve_paused_state = 0
        enable_par_cleanup = 1
        process_unpacked_par2 = 1
        enable_multipar = 1
        enable_unrar = 1
        enable_7zip = 1
        enable_filejoin = 1
        enable_tsjoin = 1
        overwrite_files = 0
        ignore_unrar_dates = 0
        backup_for_duplicates = 0
        empty_postproc = 0
        wait_for_dfolder = 0
        rss_filenames = 0
        api_logging = 1
        html_login = 1
        disable_archive = 0
        warn_dupl_jobs = 0
        keep_awake = 1
        tray_icon = 1
        allow_incomplete_nzb = 0
        enable_broadcast = 1
        ipv6_hosting = 0
        ipv6_staging = 0
        api_warnings = 1
        no_penalties = 0
        x_frame_options = 1
        allow_old_ssl_tls = 0
        enable_season_sorting = 1
        verify_xff_header = 0
        rss_odd_titles = nzbindex.nl/, nzbindex.com/, nzbclub.com/
        quick_check_ext_ignore = nfo, sfv, srr
        req_completion_rate = 100.2
        selftest_host = self-test.sabnzbd.org
        movie_rename_limit = 100M
        episode_rename_limit = 20M
        size_limit = 0
        direct_unpack_threads = 3
        history_limit = 10
        wait_ext_drive = 5
        max_foldername_length = 246
        nomedia_marker = ""
        ipv6_servers = 1
        url_base = /sabnzbd
        host_whitelist = homelab, sabnzbd.homelab
        local_ranges = ,
        max_url_retries = 10
        downloader_sleep_time = 10
        receive_threads = 2
        switchinterval = 0.005
        ssdp_broadcast_interval = 15
        ext_rename_ignore = ,
        email_server = ""
        email_to = ,
        email_from = ""
        email_account = ""
        email_pwd = ""
        email_endjob = 0
        email_full = 0
        email_dir = ""
        email_rss = 0
        email_cats = *,
        config_conversion_version = 4
        unrar_parameters = ""
        [logging]
        log_level = 1
        max_log_size = 5242880
        log_backups = 5
        [ncenter]
        ncenter_enable = 0
        ncenter_cats = *,
        ncenter_prio_startup = 0
        ncenter_prio_download = 0
        ncenter_prio_pause_resume = 0
        ncenter_prio_pp = 0
        ncenter_prio_complete = 1
        ncenter_prio_failed = 1
        ncenter_prio_disk_full = 1
        ncenter_prio_new_login = 0
        ncenter_prio_warning = 0
        ncenter_prio_error = 0
        ncenter_prio_queue_done = 0
        ncenter_prio_other = 1
        [acenter]
        acenter_enable = 0
        acenter_cats = *,
        acenter_prio_startup = 0
        acenter_prio_download = 0
        acenter_prio_pause_resume = 0
        acenter_prio_pp = 0
        acenter_prio_complete = 1
        acenter_prio_failed = 1
        acenter_prio_disk_full = 1
        acenter_prio_new_login = 0
        acenter_prio_warning = 0
        acenter_prio_error = 0
        acenter_prio_queue_done = 0
        acenter_prio_other = 1
        [ntfosd]
        ntfosd_enable = 1
        ntfosd_cats = *,
        ntfosd_prio_startup = 0
        ntfosd_prio_download = 0
        ntfosd_prio_pause_resume = 0
        ntfosd_prio_pp = 0
        ntfosd_prio_complete = 1
        ntfosd_prio_failed = 1
        ntfosd_prio_disk_full = 1
        ntfosd_prio_new_login = 0
        ntfosd_prio_warning = 0
        ntfosd_prio_error = 0
        ntfosd_prio_queue_done = 0
        ntfosd_prio_other = 1
        [prowl]
        prowl_enable = 0
        prowl_cats = *,
        prowl_apikey = ""
        prowl_prio_startup = -3
        prowl_prio_download = -3
        prowl_prio_pause_resume = -3
        prowl_prio_pp = -3
        prowl_prio_complete = 0
        prowl_prio_failed = 1
        prowl_prio_disk_full = 1
        prowl_prio_new_login = -3
        prowl_prio_warning = -3
        prowl_prio_error = -3
        prowl_prio_queue_done = -3
        prowl_prio_other = 0
        [pushover]
        pushover_token = ""
        pushover_userkey = ""
        pushover_device = ""
        pushover_emergency_expire = 3600
        pushover_emergency_retry = 60
        pushover_enable = 0
        pushover_cats = *,
        pushover_prio_startup = -3
        pushover_prio_download = -2
        pushover_prio_pause_resume = -2
        pushover_prio_pp = -3
        pushover_prio_complete = -1
        pushover_prio_failed = -1
        pushover_prio_disk_full = 1
        pushover_prio_new_login = -3
        pushover_prio_warning = 1
        pushover_prio_error = 1
        pushover_prio_queue_done = -3
        pushover_prio_other = -1
        [pushbullet]
        pushbullet_enable = 0
        pushbullet_cats = *,
        pushbullet_apikey = ""
        pushbullet_device = ""
        pushbullet_prio_startup = 0
        pushbullet_prio_download = 0
        pushbullet_prio_pause_resume = 0
        pushbullet_prio_pp = 0
        pushbullet_prio_complete = 1
        pushbullet_prio_failed = 1
        pushbullet_prio_disk_full = 1
        pushbullet_prio_new_login = 0
        pushbullet_prio_warning = 0
        pushbullet_prio_error = 0
        pushbullet_prio_queue_done = 0
        pushbullet_prio_other = 1
        [apprise]
        apprise_enable = 0
        apprise_cats = *,
        apprise_urls = ""
        apprise_target_startup = ""
        apprise_target_startup_enable = 0
        apprise_target_download = ""
        apprise_target_download_enable = 0
        apprise_target_pause_resume = ""
        apprise_target_pause_resume_enable = 0
        apprise_target_pp = ""
        apprise_target_pp_enable = 0
        apprise_target_complete = ""
        apprise_target_complete_enable = 1
        apprise_target_failed = ""
        apprise_target_failed_enable = 1
        apprise_target_disk_full = ""
        apprise_target_disk_full_enable = 0
        apprise_target_new_login = ""
        apprise_target_new_login_enable = 1
        apprise_target_warning = ""
        apprise_target_warning_enable = 0
        apprise_target_error = ""
        apprise_target_error_enable = 0
        apprise_target_queue_done = ""
        apprise_target_queue_done_enable = 0
        apprise_target_other = ""
        apprise_target_other_enable = 1
        [nscript]
        nscript_enable = 0
        nscript_cats = *,
        nscript_script = ""
        nscript_parameters = ""
        nscript_prio_startup = 0
        nscript_prio_download = 0
        nscript_prio_pause_resume = 0
        nscript_prio_pp = 0
        nscript_prio_complete = 1
        nscript_prio_failed = 1
        nscript_prio_disk_full = 1
        nscript_prio_new_login = 0
        nscript_prio_warning = 0
        nscript_prio_error = 0
        nscript_prio_queue_done = 0
        nscript_prio_other = 1
        [servers]
        [["${config.sops.placeholder."sabnzbd/servers/zero/name"}"]]
        name = "${config.sops.placeholder."sabnzbd/servers/zero/name"}"
        displayname = "${config.sops.placeholder."sabnzbd/servers/zero/displayname"}"
        host = "${config.sops.placeholder."sabnzbd/servers/zero/host"}"
        port = ${config.sops.placeholder."sabnzbd/servers/zero/port"}
        timeout = 60
        username = "${config.sops.placeholder."sabnzbd/servers/zero/username"}"
        password = "${config.sops.placeholder."sabnzbd/servers/zero/password"}"
        connections = 50
        ssl = 1
        ssl_verify = 3
        ssl_ciphers = ""
        enable = 1
        required = 0
        optional = 0
        retention = 0
        expire_date = ""
        quota = ""
        usage_at_start = 0
        priority = 0
        notes = ""
        [categories]
        [[*]]
        name = *
        order = 0
        pp = 3
        script = None
        dir = ""
        newzbin = ""
        priority = 0
        [[movies]]
        name = movies
        order = 1
        pp = ""
        script = Default
        dir = ""
        newzbin = ""
        priority = -100
        [[tv]]
        name = tv
        order = 2
        pp = ""
        script = Default
        dir = ""
        newzbin = ""
        priority = -100
        [[audio]]
        name = audio
        order = 3
        pp = ""
        script = Default
        dir = ""
        newzbin = ""
        priority = -100
        [[software]]
        name = software
        order = 4
        pp = ""
        script = Default
        dir = ""
        newzbin = ""
        priority = -100
        [[books]]
        name = books
        order = 5
        pp = ""
        script = Default
        dir = ""
        newzbin = ""
        priority = -100
      '';
      };
  };
}
