#!/usr/bin/env bash
shopt -s nullglob globstar

prefix=${PASSWORD_STORE_DIR-~/.password-store}
password_files=$(rg --files "$prefix" -g '*.gpg')
password_files=$(printf '%s\n' "$password_files" | sed -e "s|^$prefix/||" -e 's|\.gpg$||')

if [[ -z "$password_files" ]]; then
    echo "No password files found."
    exit 1
fi

password=$(printf '%s\n' "$password_files" | rofi -dmenu -i -p "Select password:")
[[ -n $password ]] || exit

content=$(pass show "$password" 2>/dev/null)
if [[ $? -ne 0 ]]; then
    rofi -e "Error: Cannot decrypt password file"
    exit 1
fi

password_line=$(echo "$content" | head -n1)
metadata=$(echo "$content" | tail -n +2)

display_msg="Password: [HIDDEN]\n"
if [[ -n "$metadata" ]]; then
    display_msg+="\nMetadata:\n$metadata"
fi
display_msg+="\n\nPress Enter to copy password to clipboard"

echo -e "$display_msg" | rofi -dmenu -p "$password" -no-custom
exit_code=$?

if [[ $exit_code -eq 0 ]]; then
    pass show -c "$password" 2>/dev/null
fi


# shopt -s nullglob globstar

# typeit=0
# if [[ $1 == "--type" ]]; then
#     typeit=1
#     shift
# fi

# prefix=${PASSWORD_STORE_DIR-~/.password-store}

# password_files=$(rg --files "$prefix" -g '*.gpg')
# password_files=$(printf '%s\n' "$password_files" | sed -e "s|^$prefix/||" -e 's|\.gpg$||')

# if [[ -z "$password_files" ]]; then
#     echo "No password files found."
#     exit 1
# fi

# password=$(printf '%s\n' "$password_files" | dmenu -i -p "Select password:")

# [[ -n $password ]] || exit

# if [[ $typeit -eq 0 ]]; then
#     pass show -c "$password" 2>/dev/null
# else
#     pass show "$password" | { read -r pass; printf %s "$pass"; } |
#         xdotool type --clearmodifiers --file -
# fi

