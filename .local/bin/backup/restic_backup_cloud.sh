#!/bin/bash

# Usage modes:
# ./script.sh -i → output duplicated to terminal, progress visible
# ./script.sh -i --no-progress → output duplicated to terminal, progress disabled
# ./script.sh (cron/job/systemd timer) → all output goes to log, no progress

# Main settings
readonly USER_HOME="${HOME:-/home/snake}"
readonly RCLONE_REPO="mailru:restic-repo"
readonly REPO_PATH="rclone:$RCLONE_REPO"
readonly PASSWORD_FILE="$USER_HOME/.password-store/.restic"
readonly LOG_FILE="$USER_HOME/.local/var/log/restic_cloud_backup.log"
readonly LOG_MAX_SIZE=10485760  # 10MB in bytes
readonly LOG_MAX_BACKUPS=5
readonly LOCK_FILE="/tmp/restic_cloud_backup.lock"
readonly LOCK_TIMEOUT=10
readonly RESTIC_TIMEOUT=3600  # 1 hour timeout for restic operations
readonly KEEP_LAST=3
readonly EXCLUDE_FILE="$USER_HOME/.local/bin/backup/restic-ignore.txt"
readonly NOTIFICATION_EMAIL=""  # Set email for error notifications (empty = disabled)
readonly NOTIFICATION_TELEGRAM=""  # Set telegram chat_id for notifications (empty = disabled)

# Options
DRY_RUN=""
VERBOSE=""
INTERACTIVE=false
NO_PROGRESS=false

# Command line arguments processing
while [[ "$#" -gt 0 ]]; do
    case $1 in
        --dry-run) DRY_RUN="--dry-run";;
        --verbose|-v) VERBOSE="-v";;
        -vv) VERBOSE="-vv";;
        -vvv) VERBOSE="-vvv";;
        --interactive|-i) INTERACTIVE=true;;
        --no-progress) NO_PROGRESS=true;;
        *) echo "Unknown argument: $1" >&2; exit 1;;
    esac
    shift
done

# Logging setup
rotate_log_if_needed() {
    if [ -f "$LOG_FILE" ]; then
        local log_size=$(stat -c%s "$LOG_FILE" 2>/dev/null || echo 0)
        if [ "$log_size" -ge "$LOG_MAX_SIZE" ]; then
            # Rotate logs
            for i in $(seq $((LOG_MAX_BACKUPS - 1)) -1 1); do
                [ -f "$LOG_FILE.$i" ] && mv "$LOG_FILE.$i" "$LOG_FILE.$((i + 1))"
            done
            mv "$LOG_FILE" "$LOG_FILE.1"
            touch "$LOG_FILE"
        fi
    fi
}

rotate_log_if_needed

if $INTERACTIVE; then
    # In interactive mode, duplicate output to terminal
    exec > >(tee -a "$LOG_FILE") 2>&1
else
    # In cron mode, everything goes to log only
    exec >> "$LOG_FILE" 2>&1
fi

# Restic progress setup
if $NO_PROGRESS; then
    export RESTIC_PROGRESS_FPS=0
else
    if $INTERACTIVE; then
        # Force enable progress even with tee
        export RESTIC_PROGRESS_FPS=0.5   # Update 2 times per second
    fi
    # In cron mode, don't set the variable → restic will decide, no progress needed
fi

# Function to handle restic exit codes correctly
handle_restic_exit() {
    local exit_code=$1
    local operation_name=$2
    
    case $exit_code in
        0)
            echo "✓ $operation_name completed successfully"
            return 0
            ;;
        1)
            echo "✗ Critical error during $operation_name" >&2
            return 1
            ;;
        3)
            echo "⚠ $operation_name completed with warnings (some files unavailable)" >&2
            return 0  # Continue on warnings
            ;;
        *)
            echo "✗ Unexpected error ($exit_code) during $operation_name" >&2
            return 1
            ;;
    esac
}

# Send notification on critical errors
send_notification() {
    local message="$1"
    local subject="Restic Backup Error"
    
    # Email notification
    if [ -n "$NOTIFICATION_EMAIL" ] && command -v mail &>/dev/null; then
        echo "$message" | mail -s "$subject" "$NOTIFICATION_EMAIL" 2>/dev/null || true
    fi
    
    # Telegram notification (requires curl and bot token in environment)
    if [ -n "$NOTIFICATION_TELEGRAM" ] && [ -n "$TELEGRAM_BOT_TOKEN" ] && command -v curl &>/dev/null; then
        curl -s -X POST "https://api.telegram.org/bot${TELEGRAM_BOT_TOKEN}/sendMessage" \
            -d chat_id="$NOTIFICATION_TELEGRAM" \
            -d text="$subject: $message" &>/dev/null || true
    fi
}

# Check dependencies
check_dependencies() {
    local missing_deps=()
    
    for cmd in restic rclone; do
        if ! command -v "$cmd" &> /dev/null; then
            missing_deps+=("$cmd")
        fi
    done
    
    if [ ${#missing_deps[@]} -gt 0 ]; then
        echo "✗ Missing required programs: ${missing_deps[*]}" >&2
        exit 1
    fi
    
    # Check password file exists
    if [ ! -f "$PASSWORD_FILE" ]; then
        echo "✗ Password file not found: $PASSWORD_FILE" >&2
        exit 1
    fi
    
    # Check password file permissions (should be 600)
    local perms=$(stat -c "%a" "$PASSWORD_FILE" 2>/dev/null)
    if [ "$perms" != "600" ]; then
        echo "⚠ Password file has insecure permissions: $perms (should be 600)" >&2
        echo "Fixing permissions..."
        chmod 600 "$PASSWORD_FILE" || {
            echo "✗ Failed to fix permissions on $PASSWORD_FILE" >&2
            exit 1
        }
    fi
    
    echo "✓ All dependencies found"
}

# Check rclone configuration
check_rclone_config() {
    echo "Checking rclone configuration..."
    if ! timeout 30 rclone lsd mailru: &>/dev/null; then
        echo "✗ rclone is not configured for Mail.ru Cloud or connection timeout" >&2
        echo "Configure rclone using: rclone config" >&2
        exit 1
    fi
    echo "✓ rclone configured correctly"
}

# Create lock file using flock
create_lock() {
    exec 200>"$LOCK_FILE"
    if ! flock -n -w "$LOCK_TIMEOUT" 200; then
        echo "✗ Another backup process is already running or lock timeout reached" >&2
        exit 1
    fi
    trap 'flock -u 200; rm -f "$LOCK_FILE"' EXIT
    echo "✓ Lock created"
}

# Check repository integrity
check_repository() {
    echo "Checking cloud repository integrity..."
    timeout "$RESTIC_TIMEOUT" restic -r "$REPO_PATH" --password-file "$PASSWORD_FILE" check
    handle_restic_exit $? "repository check" || exit 1
}

# Initialize repository if needed
init_repository_if_needed() {
    echo "Checking repository existence..."
    if ! timeout 30 restic -r "$REPO_PATH" --password-file "$PASSWORD_FILE" snapshots &>/dev/null; then
        echo "Initializing cloud Restic repository..."
        timeout "$RESTIC_TIMEOUT" restic -r "$REPO_PATH" --password-file "$PASSWORD_FILE" init
        handle_restic_exit $? "repository initialization" || exit 1
    else
        echo "✓ Repository already exists"
    fi
}

# Prepare backup paths
prepare_backup_paths() {
    # Example: "Documents", "Downloads", ".ssh"
    local -a home_files=(
        ".gnupg"
        ".password-store"
        ".ssh"
        "Documents"
        "Downloads"
        "Mail"
        "OrgFiles"
    )
    
    # Example: "nvim", "emacs", "fish"
    local -a config_files=()
    
    # Example: "hosts", "fstab", "nginx"
    local -a etc_files=()
    
    backup_paths=()
    
    # Add existing paths from home_files
    for file in "${home_files[@]}"; do
        if [ -e "$USER_HOME/$file" ]; then
            backup_paths+=("$USER_HOME/$file")
        else
            echo "⚠ $USER_HOME/$file does not exist, skipping"
        fi
    done
    
    # Add paths from config_files
    for file in "${config_files[@]}"; do
        if [ -e "$USER_HOME/.config/$file" ]; then
            backup_paths+=("$USER_HOME/.config/$file")
        else
            echo "⚠ $USER_HOME/.config/$file does not exist, skipping"
        fi
    done
    
    # Add paths from etc_files
    for file in "${etc_files[@]}"; do
        if [ -e "/etc/$file" ]; then
            backup_paths+=("/etc/$file")
        else
            echo "⚠ /etc/$file does not exist, skipping"
        fi
    done
    
    if [ ${#backup_paths[@]} -eq 0 ]; then
        echo "✗ No existing paths for backup" >&2
        exit 1
    fi
}

# Prepare exclude arguments
prepare_exclude_args() {
    if [ -f "$EXCLUDE_FILE" ]; then
        EXCLUDE_ARGS=("--exclude-file=$EXCLUDE_FILE")
        echo "✓ Using exclude file: $EXCLUDE_FILE"
    else
        EXCLUDE_ARGS=("--exclude=Documents/SVALKA")
        echo "✓ Using default exclusions"
    fi
}

# Create backup
create_backup() {
    echo "$(date '+%Y-%m-%d %H:%M:%S') Starting cloud backup"
    echo "Creating cloud backup of the following paths:"
    printf '  %s\n' "${backup_paths[@]}"
    
    # Create backup with proper error handling and timeout
    local backup_output
    backup_output=$(timeout "$RESTIC_TIMEOUT" restic -r "$REPO_PATH" \
        --password-file "$PASSWORD_FILE" \
        backup $DRY_RUN $VERBOSE "${backup_paths[@]}" \
        "${EXCLUDE_ARGS[@]}" 2>&1)
    
    local backup_exit_code=$?
    echo "$backup_output"
    
    # Check for timeout (exit code 124)
    if [ $backup_exit_code -eq 124 ]; then
        echo "✗ Backup operation timed out after $RESTIC_TIMEOUT seconds" >&2
        return 1
    fi
    
    # Log warnings for exit code 3 (some files unavailable)
    if [ $backup_exit_code -eq 3 ]; then
        echo "⚠ Some files were unavailable during backup:" >&2
        echo "$backup_output" | grep -i "error\|warning\|unable\|failed" | head -20 >&2
    fi
    
    handle_restic_exit $backup_exit_code "backup creation"
    
    # Return code only for critical errors
    if [ $backup_exit_code -eq 1 ]; then
        return 1
    fi
    
    return 0
}

# Cleanup old backups with automatic stale lock removal
cleanup_old_backups() {
    echo "Cleaning up old backups..."

    # Attempt to run forget/prune with timeout
    timeout "$RESTIC_TIMEOUT" restic -r "$REPO_PATH" --password-file "$PASSWORD_FILE" forget --keep-last $KEEP_LAST --prune
    local cleanup_exit_code=$?

    # Check for timeout
    if [ $cleanup_exit_code -eq 124 ]; then
        echo "⚠ Cleanup operation timed out after $RESTIC_TIMEOUT seconds" >&2
        return 0
    fi

    # If error 11 (repository is locked), try to unlock and retry
    if [ $cleanup_exit_code -eq 11 ]; then
        echo "⚠ Repository is locked, attempting to remove stale lock..."
        timeout 60 restic -r "$REPO_PATH" --password-file "$PASSWORD_FILE" unlock || echo "✗ Failed to remove lock"
        echo "Retrying cleanup of old backups..."
        timeout "$RESTIC_TIMEOUT" restic -r "$REPO_PATH" --password-file "$PASSWORD_FILE" forget --keep-last $KEEP_LAST --prune
        cleanup_exit_code=$?
    fi

    # Handle results
    if ! handle_restic_exit $cleanup_exit_code "cleanup of old backups"; then
        echo "⚠ Continuing despite cleanup issues"
    fi
}

# Print statistics
print_stats() {
    echo "Cloud backup statistics:"
    timeout 60 restic -r "$REPO_PATH" --password-file "$PASSWORD_FILE" stats latest
    handle_restic_exit $? "statistics retrieval" || echo "⚠ Failed to retrieve statistics"
}

# Main function
main() {
    echo "=== Starting cloud backup script ==="
    
    check_dependencies
    check_rclone_config
    create_lock
    init_repository_if_needed
    prepare_backup_paths
    prepare_exclude_args
    
    if create_backup; then
        cleanup_old_backups
        print_stats
        echo "$(date '+%Y-%m-%d %H:%M:%S') ✓ Cloud backup completed successfully"
    else
        local error_msg="Cloud backup failed on $(hostname) at $(date '+%Y-%m-%d %H:%M:%S')"
        echo "$(date '+%Y-%m-%d %H:%M:%S') ✗ Cloud backup completed with error"
        send_notification "$error_msg"
        exit 1
    fi
}

# Run main function
main
