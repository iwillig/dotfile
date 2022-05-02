if status is-interactive
   # Commands to run in interactive sessions can go here
   starship init fish | source
   source ~/.asdf/asdf.fish
end

set -x GRAALVM_HOME "/home/ivan/opt/graalvm-ce-java11-21.2.0"

set -x TERM xterm-256color

fish_add_path  -m /home/ivan/bin
fish_add_path  /home/ivan/development/clubhouse/backend/bin
fish_add_path  /home/ivan/.cargo/bin
