#!/bin/bash

show_help() {
    echo "Usage: $0 <input_image> [options]"
    echo "Options:"
    echo "  -o, --output NAME    Set output filename base (default: input filename)"
    echo "  -t, --type TYPE      Set output type: square or wide (default: square)"
    echo "  -b, --background COLOR  Set background color for transparent images (default: none)"
    echo "  -h, --help          Show this help message"
    echo ""
    echo "Example: $0 input.png -t square -b white"
}

# Default values
output_base=""
type="square"
background="none"

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        -o|--output)
            output_base="$2"
            shift 2
            ;;
        -t|--type)
            type="$2"
            shift 2
            ;;
        -b|--background)
            background="$2"
            shift 2
            ;;
        -h|--help)
            show_help
            exit 0
            ;;
        *)
            if [ -z "$input_file" ]; then
                input_file="$1"
            else
                echo "Error: Unknown parameter $1"
                show_help
                exit 1
            fi
            shift
            ;;
    esac
done

# Validate input file
if [ -z "$input_file" ]; then
    echo "Error: Input file is required"
    show_help
    exit 1
fi

if [ ! -f "$input_file" ]; then
    echo "Error: File '$input_file' not found"
    exit 1
fi

# Set output base name if not specified
if [ -z "$output_base" ]; then
    output_base=$(basename "${input_file%.*}")
fi

# Set dimensions based on type
if [ "$type" = "square" ]; then
    dimensions="1280x1280"
    suffix="_square"
else
    dimensions="1280x720"
    suffix="_wide"
fi

echo "Converting '$input_file' to ${type} format..."

# Prepare background option
background_opt=""
if [ "$background" != "none" ]; then
    background_opt="-background $background"
fi

# Main conversion with error checking
if ! convert "$input_file" \
    -gravity center \
    $background_opt \
    -resize "${dimensions}^" \
    -extent "$dimensions" \
    -define png:compression-level=9 \
    -strip \
    "${output_base}${suffix}.png"; then
    echo "Error: Image conversion failed"
    exit 1
fi

# Check if the output file was created
if [ ! -f "${output_base}${suffix}.png" ]; then
    echo "Error: Output file was not created"
    exit 1
fi

# Optimize with pngquant if available
if command -v pngquant >/dev/null 2>&1; then
    echo "Optimizing PNG..."
    if ! pngquant --force --quality=65-80 --skip-if-larger \
        --output "${output_base}${suffix}.png" \
        "${output_base}${suffix}.png"; then
        echo "Warning: PNG optimization failed, using original converted file"
    fi
fi

# Show final file size
if [ -f "${output_base}${suffix}.png" ]; then
    final_size=$(stat -f%z "${output_base}${suffix}.png" 2>/dev/null || stat -c%s "${output_base}${suffix}.png")
    echo "Conversion complete! Output saved as ${output_base}${suffix}.png"
    echo "Final file size: $(numfmt --to=iec-i --suffix=B --format="%.2f" $final_size)"
else
    echo "Error: Cannot find output file to calculate size"
    exit 1
fi
