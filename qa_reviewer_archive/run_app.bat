@echo off
REM ============================================================
REM run_app.bat - Launch QA Reviewer on Windows
REM
REM Prerequisites:
REM   1. R installed (https://cran.r-project.org)
REM   2. Ollama installed and running (https://ollama.com)
REM   3. Packages installed once via: Rscript install_dependencies.R
REM
REM Usage:
REM   Double-click this file, or run from cmd:
REM       run_app.bat
REM ============================================================

setlocal enabledelayedexpansion

REM --- Locate Rscript -----------------------------------------
where Rscript >nul 2>nul
if errorlevel 1 (
  echo [ERROR] Rscript was not found on PATH.
  echo Install R from https://cran.r-project.org and try again.
  pause
  exit /b 1
)

REM --- Ensure Ollama is running -------------------------------
echo Checking Ollama at http://localhost:11434 ...
curl -s -o nul -w "%%{http_code}" http://localhost:11434/api/tags > "%TEMP%\ollama_status.txt" 2>nul
set /p OLLAMA_STATUS=<"%TEMP%\ollama_status.txt"
del "%TEMP%\ollama_status.txt" >nul 2>nul

if not "%OLLAMA_STATUS%"=="200" (
  echo [WARN] Ollama not reachable. Attempting to start "ollama serve" in the background...
  start "" /B ollama serve
  timeout /t 3 /nobreak >nul
)

REM --- Launch Shiny app ---------------------------------------
cd /d "%~dp0"
echo Starting QA Reviewer on http://127.0.0.1:3838 ...
Rscript -e "options(shiny.host='127.0.0.1', shiny.port=3838); shiny::runApp('.', launch.browser=TRUE)"

endlocal
