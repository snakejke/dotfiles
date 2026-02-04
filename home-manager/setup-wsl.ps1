# Setup-WSL.ps1
# Run this script from PowerShell as Administrator to configure your WSL instance automatically.

param (
    [string]$DistroName = "Ubuntu",
    [string]$TargetUser = "snake",
    [string]$TargetHostname = "wsl"
)

Write-Host "Configuring WSL Distro: $DistroName" -ForegroundColor Cyan

# 1. Check if Distro exists
$wslList = wsl -l -q
if ($wslList -notmatch $DistroName) {
    Write-Host "Error: Distribution '$DistroName' not found." -ForegroundColor Red
    Write-Host "Installed distributions:"
    wsl -l -v
    exit 1
}

# 2. Check if the user exists inside WSL
# We attempt to look for the user in /etc/passwd
$userExists = wsl -d $DistroName bash -c "id -u $TargetUser > /dev/null 2>&1 && echo 'yes' || echo 'no'"

if ($userExists -eq "no") {
    Write-Host "User '$TargetUser' does not exist yet." -ForegroundColor Yellow
    Write-Host "Please complete the initial WSL setup and create the user '$TargetUser' manually,"
    Write-Host "OR the script can attempt to create it (requires you to set a password manually later)."
    # For safety, we usually exit here or ask prompt. 
    # But to fully automate, we rely on the user having created the account during install OOBE.
    Write-Host "Aborting config to prevent locking you out. Please create user '$TargetUser' first."
    exit 1
}

# 3. Write /etc/wsl.conf
Write-Host "Writing /etc/wsl.conf..." -ForegroundColor Green

# We construct the content. 
# We use 'printf' inside bash to avoid newline issues between Windows/Linux
$wslConfContent = "[network]\nhostname=$TargetHostname\ngenerateHosts=false\n\n[user]\ndefault=$TargetUser\n"

wsl -d $DistroName -u root bash -c "printf '$wslConfContent' > /etc/wsl.conf"

if ($LASTEXITCODE -eq 0) {
    Write-Host "Configuration written successfully." -ForegroundColor Green
} else {
    Write-Host "Failed to write configuration." -ForegroundColor Red
    exit 1
}

# 4. Restart WSL
Write-Host "Restarting WSL to apply changes..." -ForegroundColor Cyan
wsl --shutdown

Write-Host "Done! You can now start Ubuntu." -ForegroundColor Green
Write-Host "It should start as user '$TargetUser' with hostname '$TargetHostname'."
