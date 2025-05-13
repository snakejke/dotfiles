#!/bin/bash

# ======================================================
# Script for Debugging in IntelliJ IDEA (Java)
# 
# This script toggles kernel settings to facilitate debugging in IntelliJ IDEA.
# It adjusts two kernel parameters:
# 1. `kptr_restrict`: Controls access to kernel pointers in `/proc/kallsyms`.
# 2. `perf_event_paranoid`: Controls access to performance monitoring.
#
# Debugging in IntelliJ IDEA may require these settings to be relaxed.
# After debugging, the script can restore the original secure settings.
# ======================================================

# Check current values
current_kptr=$(cat /proc/sys/kernel/kptr_restrict)
current_perf=$(cat /proc/sys/kernel/perf_event_paranoid)

if [[ $current_kptr -eq 0 && $current_perf -eq 1 ]]; then
    echo "Restoring secure settings..."
    sudo sh -c 'echo 1 > /proc/sys/kernel/kptr_restrict'
    sudo sh -c 'echo 2 > /proc/sys/kernel/perf_event_paranoid'
    echo "Secure settings applied."
else
    echo "Enabling debug-friendly settings..."
    sudo sh -c 'echo 0 > /proc/sys/kernel/kptr_restrict'
    sudo sh -c 'echo 1 > /proc/sys/kernel/perf_event_paranoid'
    echo "Debug-friendly settings applied."
fi
