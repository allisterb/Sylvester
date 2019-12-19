@echo off
echo Building AnyCPU Debug
dotnet build -c debug
echo Building AnyCPU Release
dotnet build -c release