#!/usr/bin/env sh
set -e

CURRENT_USER="$(id -un)"
FLAKE_DIR="$HOME/.config/home-manager"

if [ -f /run/current-system/system ]; then
    KIND="nixos"; CONF_NAME="$CURRENT_USER"
elif grep -qi "microsoft" /proc/version 2>/dev/null; then
    KIND="wsl"; CONF_NAME="${CURRENT_USER}@wsl"
else
    KIND="linux"; CONF_NAME="$CURRENT_USER"
fi

echo "[INFO] Running as $CURRENT_USER (kind: $KIND, conf: $CONF_NAME)"

[ -d "$FLAKE_DIR" ] || { echo "[ERROR] Flake dir not found: $FLAKE_DIR" >&2; exit 1; }

ATTR="$FLAKE_DIR#homeConfigurations.\"$CONF_NAME\".config.nix.settings.substituters"
DATA=$(nix eval "$ATTR" --json 2>/dev/null || true)

if [ -z "$DATA" ] || [ "$DATA" = "null" ]; then
    echo "[ERROR] Could not find: $ATTR" >&2
    exit 1
fi

echo "[INFO] Checking substituters for $CONF_NAME..."
echo "$DATA" | jq -r '.[]' | while read -r url; do
    if curl -fsL --max-time 5 "${url}/nix-cache-info" >/dev/null 2>&1; then
        printf "[ \033[32mOK\033[0m ]  %s\n" "$url"
    else
        printf "[\033[31mFAIL\033[0m]  %s\n" "$url"
    fi
done
