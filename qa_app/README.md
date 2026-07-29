# QA Reviewer - Simple, Local, Single-File

One file (`app.R`) that uploads a Protocol / Report / SOP, sends it to a local
Ollama model, and shows a QA review: score, risk level, executive summary, and
a table of issues with concrete suggested fixes. Plus download as Markdown or
JSON.

## First-time setup (Windows)

1. Install **R** from https://cran.r-project.org
2. Install **Ollama** from https://ollama.com
3. Pull a small fast model. In Command Prompt:
   ```
   ollama pull llama3.2:3b
   ```
4. Install the R packages. In R / RStudio:
   ```r
   install.packages(c("shiny", "httr2", "jsonlite",
                      "pdftools", "officer", "DT"))
   ```

## Run it

Open `app.R` in RStudio and click **Run App**, or from the R console:

```r
setwd("C:/Users/USER/OneDrive - Mascot Universal Pvt Ltd/Documents/Claude/Projects/Project_QA/qa_app")
shiny::runApp("app.R", launch.browser = TRUE)
```

Make sure Ollama is running (open Ollama Desktop, or `ollama serve` in another
terminal) before you click Analyse.

## Using it

1. Upload a `.pdf`, `.docx`, or `.txt` file.
2. Pick the document type (Protocol, CSR, SOP, etc.).
3. Pick an installed Ollama model from the dropdown.
4. Click **Analyse document**.
5. Read the score / summary / issues table. Download Markdown or JSON.

## Speed tips

- Use a small model: `llama3.2:3b` or `qwen2.5:3b` (~2 GB, very fast on CPU).
- Documents over ~24,000 characters are auto-trimmed to head + tail to keep
  the model context bounded and analysis fast.
- For maximum quality on a beefy machine: `qwen2.5:7b` or `llama3.1:8b`.

## If something doesn't work

- **"no models installed" in the dropdown** -> Ollama isn't running, or you
  haven't pulled a model. Run `ollama pull llama3.2:3b`.
- **"Model did not return valid JSON"** -> the model produced text the parser
  couldn't read; the raw text is shown so nothing is lost. Try a different
  model (qwen2.5:3b is good at JSON).
- **Slow** -> drop to a 2-3 GB model, or make sure Ollama is using your GPU
  if you have one (`ollama ps` to check).
