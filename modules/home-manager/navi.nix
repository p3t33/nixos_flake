{ config, lib, ... }:
{
  config = lib.mkIf config.programs.navi.enable {
    programs.navi = {
      # Disable Home Manager’s built-in Zsh integration (it injects an early `eval "$(navi widget zsh)"`)
      # so we can initialize the Navi widget manually and at the correct time in our own zsh.nix initContent.
      enableZshIntegration = false;
      settings = {
        style = {

          tag = {
            width_percentage = 10;
            min_width = 10;
          };

          comment = {
            width_percentage = 35;
            min_width = 35;
          };

          snippet = {
            width_percentage = 45;
            min_width = 45;
          };
        };
      };
    };

    home.file = {
      ".local/share/navi/cheats/custom/git.cheat".text = ''
       % git
       # Set git user name
       git config --global user.name <name>

       # Set git user email
       git config user.email <email>

       # create worktree tacking remote
       git worktree add -b <name> <path> <remote>/<branch>

       # Search git log comment message for a string
       git log --grep="<string>"

       # Search code changes for string
       git log -S "<sting>"

       # create anointed tag to HEAD
       git tag -a <tag> -m "<message>"

       # create anoinated and signed tag to HEAD
       git tag -s <tag> -m "<message>"

       # create anoinated and signed tag to commit
       git tag -s <tag> <commit-hash> -m "<message>"

       # remove tag from local
       git tag -d <tag>

       # remove tag from remote
       git push origin --delete <tag>

       # push all local tags to remote
       git push --tags

       # fetch tags from remote
       git fetch --tags

       # read tag message
       git show <tag>

       #
      '';

      ".local/share/navi/cheats/custom/buku.cheat".text = ''
       % buku, cheat
       # Print all bookmarks
       buku -p
      '';

      ".local/share/navi/cheats/custom/taskwarrior.cheat".text = ''
       % task, taskwarrior
       # Print all bookmarks
       buku -p
      '';
    };

    home.file = {
      ".local/share/navi/cheats/custom/ssh.cheat".text = ''
       % ssh
       # generate ed25519 key
       ssh-keygen -f ~/.ssh/<key_name> -t ed25519 -C "<comment>"

       # local port forwarding
       ssh -N -L 127.0.0.1:<local_port>:127.0.0.1:<remote_port> <user>@<remote_ip>

       # copy key to remote
       ssh-copy-id -i ~/.ssh/<privare_key_name> <user>@<remote_ip>

       # remove old host key
       ssh-keygen -R <ip-of-host>

       # send command over ssh
       ssh <user>@<remote_ip> -t <command to send>
      '';
    };

    home.file = {
      ".local/share/navi/cheats/custom/compression.cheat".text = ''
       % compression, tar
       # create tar archive
       tar -cvf <archive_name> <path_to_compress>

       # Extract tar archive
       tar -xvf <path_to_archive>

       # create compressed tar archive with gzip
       tar -czvf <archive_name> <path_to_compress>

       # Extract compressed tar archive
       tar -xzvf <path_to_archive>
      '';
    };

    home.file = {
      ".local/share/navi/cheats/custom/network_manager.cheat".text = ''
       % networkmanager, nmcli

       # list available wifi networks
       nmcli d wifi list | more

       # connect to wireless netwrok
       nmcli d wifi connect <SSID> password <password>

      '';
    };

    home.file = {
      ".local/share/navi/cheats/custom/systemctl.cheat".text = ''
       % systemctl
       # List running services
       systemctl list-units --type=service --state=running

       # List user running services
       systemctl list-units --user --type=service --state=running

       # cat unit file
       systemctl cat <unit_name>

       # show unit details
       systemctl show <unit_name>

      '';
    };

    home.file = {
      ".local/share/navi/cheats/custom/journalctl.cheat".text = ''
       % journalctl
       # List running services

       # show unit log
       journalctl -u <unit_name>

       # follow unit log
       journalctl -uf <unit_name>

       # list log generations
       journalctl --list-boots

       # show log for specific boot
       journalctl -b <boot_number>

      '';
    };
  };
}

