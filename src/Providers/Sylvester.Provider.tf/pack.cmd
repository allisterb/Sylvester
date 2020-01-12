@echo off
dotnet paket pack --build-config Debug --version %1 --release-notes %2 lib