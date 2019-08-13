@echo %off
setlocal enabledelayedexpansion
echo Loading eTBS Visualiser, please wait...
for /f "delims=" %%a in ('dir /s /b C:\.\R.exe ^| findstr bin') do set RDir=%%~a
set R="%RDir%"
set mypath=%~dp0
echo Checking package dependencies...
%R% --slave -f "%mypath%\run.R" --args "%mypath:~0,-1%"
pause
