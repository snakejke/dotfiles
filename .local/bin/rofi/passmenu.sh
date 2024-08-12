#!/usr/bin/env bash

shopt -s nullglob globstar

typeit=0
if [[ $1 == "--type" ]]; then
    typeit=1
    shift
fi

prefix=${PASSWORD_STORE_DIR-~/.password-store}

password_files=$(rg --files "$prefix" -g '*.gpg')
password_files=$(printf '%s\n' "$password_files" | sed -e "s|^$prefix/||" -e 's|\.gpg$||')

if [[ -z "$password_files" ]]; then
    echo "No password files found."
    exit 1
fi

password=$(printf '%s\n' "$password_files" | dmenu -i -p "Select password:")

[[ -n $password ]] || exit

if [[ $typeit -eq 0 ]]; then
    pass show -c "$password" 2>/dev/null
else
    pass show "$password" | { read -r pass; printf %s "$pass"; } |
        xdotool type --clearmodifiers --file -
fi

