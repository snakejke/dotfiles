#!/bin/bash

upload_file() {
    local file_path="$1"
    local url=$(curl -F "file=@$file_path" https://0x0.st)
    echo "Uploaded $file_path to $url"
}

delete_file() {
    local file_url="$1"
    local token=$(curl -si "$file_url" | grep -oP 'X-Token: \K\S+')
    if [ -n "$token" ]; then
        curl -F "token=$token" -F "delete=" "$file_url"
        echo "Deleted $file_url"
    else
        echo "Failed to get token for $file_url"
    fi
}

#./script.sh upload /home/snake/example.md
#./script.sh delete https://0x0.st/iR8q.md
case "$1" in
    upload)
        upload_file "$2"
        ;;
    delete)
        delete_file "$2"
        ;;
    *)
        echo "Usage: $0 [upload|delete] [file_path|file_url]"
        ;;
esac
