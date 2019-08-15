@echo %off
setlocal enabledelayedexpansion
echo Searching for R installation...
for /f "delims=" %%a in ('dir /s /b /a-d-s-h C:\.\R.exe ^| findstr bin') do set RDir=%%~a
set R="%RDir%"
echo R found at: %R%
echo Loading eTBS Visualiser...
set mypath=%~dp0
%R% --slave -f "%mypath%\run.R" --args "%mypath:~0,-1%"
