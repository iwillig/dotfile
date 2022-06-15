if status is-interactive
   # Commands to run in interactive sessions can go here
   starship init fish | source
   source ~/.asdf/asdf.fish
end

set -x GRAALVM_HOME "/home/ivan/opt/graalvm-ce-java11-21.2.0"

set -x TERM xterm-256color
##  Internal Workspace
set -x SHORTCUT_API_TOKEN 627bffae-ba62-417d-9ca8-4acde11654c0
##
##
##
## stuffstuff
# set -x SHORTCUT_API_TOKEN 62a0d71b-52f9-4cc9-89b1-3055d18cb0b2

fish_add_path  -m /home/ivan/bin
fish_add_path  /home/ivan/development/clubhouse/backend/bin
fish_add_path  /home/ivan/.cargo/bin
fish_add_path  /home/ivan/opt/shortcut-cli
fish_add_path  /home/ivan/.local/bin
