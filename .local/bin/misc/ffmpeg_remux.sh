#!/bin/bash

DEFAULT_INPUT_DIR="/home/snake/Documents/Projects/Python/YouTubeAPI/videos_to_upload"

# Check if ffmpeg is installed
if ! command -v ffmpeg &> /dev/null; then
    echo "Error: ffmpeg is not installed. Please install it first."
    exit 1
fi

# Function to process a single MKV file
process_file() {
    local input_file="$1"
    local output_file="${input_file%.mkv}.mp4"
    
    echo "Processing: $(basename "$input_file")"
    
    if ffmpeg -i "$input_file" -c copy "$output_file" 2>/dev/null; then
        if [ -f "$output_file" ]; then
            rm "$input_file"
            echo "Successfully converted: $(basename "$input_file")"
            return 0
        else
            echo "Error: Output file not created for: $(basename "$input_file")"
            return 1
        fi
    else
        echo "Error processing: $(basename "$input_file")"
        return 1
    fi
}

# Function to process directory
process_directory() {
    local dir="$1"
    local processed_count=0
    local failed_count=0
    local found_files=false
    
    for input_file in "$dir"/*.mkv; do
        if [ ! -f "$input_file" ]; then
            continue
        fi
        found_files=true
        
        if process_file "$input_file"; then
            ((processed_count++))
        else
            ((failed_count++))
        fi
    done
    
    if [ "$found_files" = false ]; then
        echo "No MKV files found in $dir"
        exit 0
    fi
    
    echo "Processing complete. Successfully processed: $processed_count files. Failed: $failed_count files."
}

# Main logic
if [ $# -eq 0 ]; then
    # No arguments - use default directory
    process_directory "$DEFAULT_INPUT_DIR"
elif [ $# -eq 1 ]; then
    if [ -f "$1" ] && [[ "$1" == *.mkv ]]; then
        # Single file processing
        process_file "$1"
    elif [ -d "$1" ]; then
        # Directory processing
        process_directory "$1"
    else
        echo "Error: '$1' is not a valid MKV file or directory"
        exit 1
    fi
else
    echo "Usage: $0 [file.mkv|directory]"
    echo "  No arguments: process default directory ($DEFAULT_INPUT_DIR)"
    echo "  file.mkv: process single MKV file"
    echo "  directory: process all MKV files in directory"
    exit 1
fi
