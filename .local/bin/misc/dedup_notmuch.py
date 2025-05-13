#!/usr/bin/env python3
"""
This script deduplicates email files managed by the Notmuch email client. It identifies
duplicate messages sharing the same Message-ID and retains only the oldest file (by
modification time), removing all other duplicates. This helps reduce storage usage while
preserving the original message.

Usage: Ensure Notmuch is installed and configured. Run the script to clean up duplicates.
"""

import subprocess
import os

# Retrieve all Message-IDs from Notmuch database and strip the 'id:' prefix
message_ids = [line[3:] for line in subprocess.check_output(
    ["notmuch", "search", "--output=messages", "*"]).decode().splitlines()]

print(f"Found Message-IDs: {len(message_ids)}")  # Debug output

# Map each Message-ID to its associated files
message_files = {}
for msg_id in message_ids:
    files = subprocess.check_output(
        ["notmuch", "search", "--output=files", f"id:{msg_id}"]).decode().splitlines()
    
    if not files:
        print(f"⚠️  No files found for message {msg_id}!")
    else:
        message_files[msg_id] = files

print(f"Messages with files detected: {len(message_files)}")  # Debug output

# Identify the oldest file for each Message-ID to retain
files_to_keep = set()

for msg_id, files in message_files.items():
    if files:
        oldest_file = min(files, key=os.path.getmtime)  # Select oldest by modification time
        files_to_keep.add(oldest_file)

print(f"Files to retain: {len(files_to_keep)}")  # Debug output

# Get all files currently tracked by Notmuch
all_indexed_files = set(subprocess.check_output(["notmuch", "search", "--output=files", "*"]).decode().splitlines())

# Calculate files marked for deletion (all files minus those to keep)
files_to_delete = all_indexed_files - files_to_keep

print(f"Files scheduled for deletion: {len(files_to_delete)}")  # Debug output

# Test mode: Print files that would be deleted without actual removal
# WARNING: The next line ACTUALLY DELETES FILES. Uncomment with caution.
for file in files_to_delete:
    print(f"❌ Would delete: {file}")  # Simulated deletion
    # os.remove(file)  # Uncomment to enable actual deletion
