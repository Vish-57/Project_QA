@echo off
REM Pull the recommended default Ollama models for QA Reviewer.
REM Adjust the list if you prefer different ones.

echo Pulling llama3.2:3b ...
ollama pull llama3.2:3b

echo Pulling qwen2.5:3b ...
ollama pull qwen2.5:3b

echo Pulling phi3.5:3.8b ...
ollama pull phi3.5:3.8b

echo Done. Verify with:  ollama list
pause
