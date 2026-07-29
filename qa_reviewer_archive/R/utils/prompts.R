# =============================================================================
# prompts.R
# Loads prompt templates from prompts/*.txt and fills them in. Keeping prompts
# in plain text files (rather than inline) makes them easy to iterate on
# without touching R code.
# =============================================================================

PROMPTS_DIR <- "prompts"

# Cache loaded prompts in package-local env
.prompt_cache <- new.env(parent = emptyenv())

load_prompt <- function(name, prompts_dir = PROMPTS_DIR) {
  if (!is.null(.prompt_cache[[name]])) return(.prompt_cache[[name]])
  path <- file.path(prompts_dir, paste0(name, ".txt"))
  if (!file.exists(path))
    stop("Prompt template not found: ", path)
  txt <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"),
               collapse = "\n")
  .prompt_cache[[name]] <- txt
  txt
}

#' Fill {placeholders} in a template with values from `vars`.
fill_prompt <- function(template, vars) {
  for (k in names(vars)) {
    template <- gsub(paste0("\\{", k, "\\}"),
                     fixed_replace(as.character(vars[[k]])),
                     template, perl = TRUE)
  }
  template
}

# gsub w/ perl=TRUE treats $ and \\ specially in replacement; escape them.
fixed_replace <- function(x) {
  x <- gsub("\\\\", "\\\\\\\\", x)
  x <- gsub("\\$",  "\\\\$",   x)
  x
}

#' Standard system prompt used for ALL QA passes — keeps tone & format stable.
SYSTEM_QA_REVIEWER <- "You are a senior Quality Assurance reviewer with deep experience in clinical trial protocols, clinical study reports (CSRs), standard operating procedures (SOPs), and regulatory submissions (ICH-GCP, FDA, EMA). You are precise, conservative, and never invent issues. When you are unsure, you say so explicitly. You always respond with valid JSON exactly matching the schema requested."
