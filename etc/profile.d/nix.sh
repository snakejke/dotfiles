
# Nix
if [ -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh' ]; then
    . '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
fi
# End Nix

        

# Nix
if [ -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh' ]; then
    . '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
fi
# End Nix

        # Only execute this file once per shell.
# This file is tested by tests/installer/default.nix.
# if [ -n "${__ETC_PROFILE_NIX_SOURCED:-}" ]; then return; fi
# export __ETC_PROFILE_NIX_SOURCED=1

# NIX_LINK=$HOME/.nix-profile
# if [ -n "${XDG_STATE_HOME-}" ]; then
#     NIX_LINK_NEW="$XDG_STATE_HOME/nix/profile"
# else
#     NIX_LINK_NEW=$HOME/.local/state/nix/profile
# fi
# if [ -e "$NIX_LINK_NEW" ]; then
#     if [ -t 2 ] && [ -e "$NIX_LINK" ]; then
#         warning="\033[1;35mwarning:\033[0m"
#         printf "$warning Both %s and legacy %s exist; using the former.\n" "$NIX_LINK_NEW" "$NIX_LINK" 1>&2
#         if [ "$(realpath "$NIX_LINK")" = "$(realpath "$NIX_LINK_NEW")" ]; then
#             printf "         Since the profiles match, you can safely delete either of them.\n" 1>&2
#         else
#             # This should be an exceptionally rare occasion: the only way to get it would be to
#             # 1. Update to newer Nix;
#             # 2. Remove .nix-profile;
#             # 3. Set the $NIX_LINK_NEW to something other than the default user profile;
#             # 4. Roll back to older Nix.
#             # If someone did all that, they can probably figure out how to migrate the profile.
#             printf "$warning Profiles do not match. You should manually migrate from %s to %s.\n" "$NIX_LINK" "$NIX_LINK_NEW" 1>&2
#         fi
#     fi
#     NIX_LINK="$NIX_LINK_NEW"
# fi

# export NIX_PROFILES="@localstatedir@/nix/profiles/default $NIX_LINK"

# # Populate bash completions, .desktop files, etc
# if [ -z "${XDG_DATA_DIRS-}" ]; then
#     # According to XDG spec the default is /usr/local/share:/usr/share, don't set something that prevents that default
#     export XDG_DATA_DIRS="/usr/local/share:/usr/share:$NIX_LINK/share:/nix/var/nix/profiles/default/share"
# else
#     export XDG_DATA_DIRS="$XDG_DATA_DIRS:$NIX_LINK/share:/nix/var/nix/profiles/default/share"
# fi

# # Set $NIX_SSL_CERT_FILE so that Nixpkgs applications like curl work.
# if [ -n "${NIX_SSL_CERT_FILE:-}" ]; then
#     : # Allow users to override the NIX_SSL_CERT_FILE
# elif [ -e /etc/ssl/certs/ca-certificates.crt ]; then # NixOS, Ubuntu, Debian, Gentoo, Arch
#     export NIX_SSL_CERT_FILE=/etc/ssl/certs/ca-certificates.crt
# elif [ -e /etc/ssl/certs.pem ]; then # Void Linux 
#     export NIX_SSL_CERT_FILE=/etc/ssl/ca-bundle.pem
# elif [ -e /etc/ssl/certs/ca-bundle.crt ]; then # Old NixOS
#     export NIX_SSL_CERT_FILE=/etc/ssl/certs/ca-bundle.crt
# elif [ -e /etc/pki/tls/certs/ca-bundle.crt ]; then # Fedora, CentOS
#     export NIX_SSL_CERT_FILE=/etc/pki/tls/certs/ca-bundle.crt
# else
#   # Fall back to what is in the nix profiles, favouring whatever is defined last.
#   check_nix_profiles() {
#     if [ -n "${ZSH_VERSION:-}" ]; then
#       # Zsh by default doesn't split words in unquoted parameter expansion.
#       # Set local_options for these options to be reverted at the end of the function
#       # and shwordsplit to force splitting words in $NIX_PROFILES below.
#       setopt local_options shwordsplit
#     fi
#     for i in $NIX_PROFILES; do
#       if [ -e "$i/etc/ssl/certs/ca-bundle.crt" ]; then
#         export NIX_SSL_CERT_FILE=$i/etc/ssl/certs/ca-bundle.crt
#       fi
#     done
#   }
#   check_nix_profiles
#   unset -f check_nix_profiles
# fi

# export PATH="$NIX_LINK/bin:@localstatedir@/nix/profiles/default/bin:$PATH"
# unset NIX_LINK NIX_LINK_NEW
