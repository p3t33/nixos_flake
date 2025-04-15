let
  defaultWebBrowser = "firefox.desktop";
  defaultPdfReader = "org.pwmt.zathura.desktop";
in
{
  # Used to define defaults for the GUI based on file suffix.
  # test E.g xdg-mime query default application/pdf
  xdg.mimeApps = {
    enable = true;

    associations.added = {
      "x-scheme-handler/http"  = [ defaultWebBrowser ];
      "x-scheme-handler/https" = [ defaultWebBrowser ];
      "x-scheme-handler/chrome" = [ defaultWebBrowser ];
      "text/html" = [ defaultWebBrowser ];
      "application/x-extension-htm" = [ defaultWebBrowser ];
      "application/x-extension-html" = [ defaultWebBrowser ];
      "application/x-extension-shtml" = [ defaultWebBrowser ];
      "application/xhtml+xml" = [ defaultWebBrowser ];
      "application/x-extension-xhtml" = [ defaultWebBrowser ];
      "application/x-extension-xht" = [ defaultWebBrowser ];
      "application/pdf" = [ defaultPdfReader ];
    };

    defaultApplications = {
      "text/html" = [ defaultWebBrowser ];
      "x-scheme-handler/http" = [ defaultWebBrowser ];
      "x-scheme-handler/https" = [ defaultWebBrowser ];
      "x-scheme-handler/about" = [ defaultWebBrowser ];
      "x-scheme-handler/unknown" = [ defaultWebBrowser ];
      "x-scheme-handler/chrome" = [ defaultWebBrowser ];
      "application/x-extension-htm" = [ defaultWebBrowser ];
      "application/x-extension-html" = [ defaultWebBrowser ];
      "application/x-extension-shtml" = [ defaultWebBrowser ];
      "application/xhtml+xml" = [ defaultWebBrowser ];
      "application/x-extension-xhtml" = [ defaultWebBrowser ];
      "application/x-extension-xht" = [ defaultWebBrowser ];
      "application/pdf" = [ defaultPdfReader ];
    };
  };
}
