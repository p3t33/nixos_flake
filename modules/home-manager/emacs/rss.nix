{ config, ... }:

{
  programs.emacs = {
    extraPackages = epkgs: with epkgs; [
      elfeed
      elfeed-goodies
    ];

    extraConfig = ''
      ;; ====================
      ;; RSS feed
      ;; ====================
      (use-package elfeed
          :ensure nil
          :config
          (setq elfeed-search-feed-face ":foreground #ffffff :weight bold"
              elfeed-feeds (quote
                  (("https://www.reddit.com/r/linux.rss" reddit linux)
                   ("https://www.reddit.com/r/commandline.rss" reddit commandline)
                   ("https://www.reddit.com/r/emacs.rss" reddit emacs)
                   ("https://hackaday.com/blog/feed/" hackaday linux)
                   ("https://opensource.com/feed" opensource linux)
                   ("https://linux.softpedia.com/backend.xml" softpedia linux)
                   ("https://www.computerworld.com/index.rss" computerworld linux)
                   ("https://www.networkworld.com/category/linux/index.rss" networkworld linux)
                   ("https://www.techrepublic.com/rssfeeds/topic/open-source/" techrepublic linux)
                   ("http://lxer.com/module/newswire/headlines.rss" lxer linux)))))

      (use-package elfeed-goodies
          :ensure nil
          :after elfeed
          :init
          (elfeed-goodies/setup)
          :config
          (setq elfeed-goodies/entry-pane-size 0.5))
      ;; ====================
    '';
  };
}

