set windows-shell := ["powershell.exe", "-NoLogo", "-Command"]

[private]
default:
    @just --list

# Backup exercism solutions to root directory
backup *args:
    auxiliaire backup -s published {{args}} .
