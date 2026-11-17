{ lib, pkgs, ... }:

let
  script = pkgs.writeShellScript "transcript" ''
    set -euo pipefail

    video_id="''${1:-}"

    if [ -z "$video_id" ]; then
      echo "Usage: transcript <video-id-or-url>" >&2
      echo "Example: transcript dQw4w9WgXcQ" >&2
      echo "Example: transcript https://www.youtube.com/watch?v=dQw4w9WgXcQ" >&2
      exit 1
    fi

    tmpdir=$(mktemp -d)
    trap 'rm -rf "$tmpdir"' EXIT

    ${lib.getExe pkgs.yt-dlp} \
      --write-auto-sub \
      --sub-lang "en.*" \
      --skip-download \
      --convert-subs srt \
      -o "$tmpdir/%(id)s" \
      "$video_id" 2>/dev/null

    sub_file=$(ls "$tmpdir"/*.srt 2>/dev/null | head -1)

    if [ -z "$sub_file" ]; then
      echo "Error: No English subtitles found. Trying any available language..." >&2
      ${lib.getExe pkgs.yt-dlp} \
        --write-auto-sub \
        --skip-download \
        --convert-subs srt \
        -o "$tmpdir/%(id)s" \
        "$video_id" 2>/dev/null
      sub_file=$(ls "$tmpdir"/*.srt 2>/dev/null | head -1)
    fi

    if [ -z "$sub_file" ]; then
      echo "Error: No subtitles available for this video" >&2
      exit 1
    fi

    ${lib.getExe pkgs.gawk} '
      /^[0-9]+$/ { next }
      /^[0-9]{2}:[0-9]{2}:[0-9]{2}/ {
        split($1, t, /[:,]/)
        h = int(t[1]); m = int(t[2]); s = int(t[3])
        if (h > 0) ts = h ":" sprintf("%02d", m) ":" sprintf("%02d", s)
        else ts = m ":" sprintf("%02d", s)
        getline text
        if (text != "" && text != prev) {
          printf "[%s] %s\n", ts, text
          prev = text
        }
        next
      }
    ' "$sub_file"
  '';

  skill = ''
    ---
    name: youtube-transcript
    description: "Fetch transcripts from YouTube videos for summarization and analysis. Use when user shares a YouTube URL or asks about a YouTube video's content."
    ---

    # YouTube Transcript

    Fetch transcripts from YouTube videos.

    ## Usage

    ```bash
    ./transcript <video-id-or-url>
    ```

    Accepts video ID or full URL:
    - `dQw4w9WgXcQ`
    - `https://www.youtube.com/watch?v=dQw4w9WgXcQ`
    - `https://youtu.be/dQw4w9WgXcQ`

    ## Output

    Timestamped transcript entries:

    ```
    [0:00] All right. So, I got this UniFi Theta
    [0:15] I took the camera out, painted it
    [1:23] And here's the final result
    ```

    ## Notes

    - Requires the video to have captions/transcripts available
    - Works with auto-generated and manual transcripts
    - Falls back to any available language if English subtitles are not found
  '';
in
{
  inherit skill script;
}
