@echo off
cd /d "%~dp0.."
start "" http://localhost:8888/viewer/
python -m http.server 8888
