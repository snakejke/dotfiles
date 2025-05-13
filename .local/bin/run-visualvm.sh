#!/bin/bash

exec 1> >(tee -a "/tmp/visualvm-launcher.log")
exec 2>&1
echo "Script started at $(date)"
env | sort >> "/tmp/visualvm-launcher.log"

# Script to launch VisualVM with optimized display settings
# Dependencies: VisualVM must be installed via SDKMAN

# Exit on any error
set -e

# Configuration variables
VISUALVM_HOME="${HOME}/.local/devjava/sdkman/candidates/visualvm/current"
VISUALVM_USER_DIR="${HOME}/.local/share/visualvm"
FONT_SIZE=17
UI_SCALE=1.3

# Verify VisualVM installation
if [ ! -d "$VISUALVM_HOME" ]; then
    echo "Error: VisualVM not found in $VISUALVM_HOME"
    echo "Please ensure VisualVM is installed via SDKMAN"
    exit 1
fi

# Set display optimization options
export _JAVA_OPTIONS='-Dawt.useSystemAAFontSettings=on -Dswing.aatext=true'

# Launch VisualVM with configured parameters
"$VISUALVM_HOME/bin/visualvm" \
    --fontsize "$FONT_SIZE" \
    -J-Dsun.java2d.uiScale="$UI_SCALE" \
    -J-Dnetbeans.plaf.disable.ui.customizations=true
