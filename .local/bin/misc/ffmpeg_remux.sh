#!/bin/bash

# Directory containing the MKV files
INPUT_DIR="/home/snake/Documents/Projects/Python/YouTubeAPI/videos_to_upload"
#INPUT_DIR="/home/snake/Documents/interviews"

# Check if ffmpeg is installed
if ! command -v ffmpeg &> /dev/null; then
    echo "Error: ffmpeg is not installed. Please install it first."
    exit 1
fi

# Create a log file
LOG_FILE="$INPUT_DIR/conversion.log"
touch "$LOG_FILE"

# Function to log messages
log_message() {
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] $1" | tee -a "$LOG_FILE"
}

# Counter for processed files
processed_count=0
failed_count=0

# Process each MKV file in the directory
for input_file in "$INPUT_DIR"/*.mkv; do
    # Check if any MKV files exist
    if [ ! -f "$input_file" ]; then
        log_message "No MKV files found in $INPUT_DIR"
        exit 0
    fi

    # Generate output filename
    output_file="${input_file%.mkv}.mp4"
    
    log_message "Starting remux of: $(basename "$input_file")"
    
    # Simple remux without re-encoding
    if ffmpeg -i "$input_file" -c copy "$output_file" 2>> "$LOG_FILE"; then
        # If remux successful, remove original file
        if [ -f "$output_file" ]; then
            rm "$input_file"
            log_message "Successfully remuxed and removed: $(basename "$input_file")"
            ((processed_count++))
        else
            log_message "Error: Output file not created for: $(basename "$input_file")"
            ((failed_count++))
        fi
    else
        log_message "Error processing: $(basename "$input_file")"
        ((failed_count++))
    fi
done

# Print summary
log_message "Processing complete. Successfully processed: $processed_count files. Failed: $failed_count files."
