# QA Reviewer v2.1 - Self-Learning Expert QA Audit System

## 🎯 Overview

QA Reviewer is an enterprise-grade R/Shiny application that uses local AI (Ollama) to audit clinical, regulatory, and scientific documents with the expertise of a **25-year QA veteran**. 

**NEW in v2.1:** The system now **learns from your feedback** to continuously improve its accuracy and reduce false positives over time.

---

## ✨ Key Features

### Expert-Level Auditing (50+ Checkpoints)
The system performs 5 independent review passes checking:
1. **Spelling & Grammar** - Including passive voice, UK/US variants, typos
2. **Consistency** - Terminology, abbreviations, study codes, product names
3. **Regulatory Compliance** - ICH-GCP E6(R2), FDA 21 CFR, ALCOA+, EU CTR 536/2014
4. **Statistical Validity** - Sample size formulas, p-value interpretation, CI calculations
5. **Numeric Cross-Verification** - Table vs narrative consistency (HIGHEST PRIORITY)

### Domain-Specific Expertise
- **Protocols**: 50 mandatory checkpoints including amendment history, informed consent compliance (21 CFR 50.25), DSMB charter, randomization methods, blinding procedures, interim analysis rules, multiplicity adjustments
- **CSRs**: ICH E3 structure verification, CONSORT flow diagrams, subject disposition, safety reporting
- **SAPs**: Analysis population definitions (ITT/PP/Safety), missing data imputation, sensitivity analyses
- **Cosmetic Studies**: Claim substantiation, patch test reports, HRIPT, SPF studies

### Advanced Table Verification
- Value mismatch detection (text vs table cells)
- Timepoint/column attribution errors
- Significance claim validation against actual p-values
- Denominator consistency across tables
- Percentage calculation verification

### 🧠 SELF-LEARNING CAPABILITIES (NEW!)

#### Feedback Loop
Users can now:
1. **Accept** findings they agree with
2. **Reject** false positives with explanations
3. **Modify** partially correct findings

#### Pattern Learning
The system automatically:
- Identifies recurring false positive patterns
- Learns domain-specific exceptions (e.g., "passive voice acceptable in methods sections")
- Builds confidence scores for each learned pattern
- Applies learnings to future audits

#### Prompt Regeneration
With one click, the system:
- Analyzes all high-confidence learned patterns (≥70% confidence)
- Updates prompt templates with expert feedback
- Incorporates grammar exceptions, domain exclusions, and acceptable patterns
- Reduces similar false positives in future audits

---

## 🏗️ Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                     User Interface                           │
│  ┌──────────┐ ┌──────────┐ ┌──────────┐ ┌──────────────┐   │
│  │  Upload  │ │ Analysis │ │  Issues  │ │   Feedback   │   │
│  │  Module  │ │  Module  │ │  Module  │ │   Module     │   │
│  └──────────┘ └──────────┘ └──────────┘ └──────────────┘   │
└─────────────────────────────────────────────────────────────┘
                            │
┌───────────────────────────▼─────────────────────────────────┐
│                   Analysis Engine                            │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────────────┐  │
│  │   Chunking  │  │   Parallel  │  │   Learned Patterns  │  │
│  │  (Section-  │  │   LLM Calls │  │   Integration       │  │
│  │   aware)    │  │  (future)   │  │   (from DB)         │  │
│  └─────────────┘  └─────────────┘  └─────────────────────┘  │
└─────────────────────────────────────────────────────────────┘
                            │
┌───────────────────────────▼─────────────────────────────────┐
│                    Ollama LLM Server                         │
│            (llama3.2:3b, qwen2.5:3b, etc.)                  │
└─────────────────────────────────────────────────────────────┘
                            │
┌───────────────────────────▼─────────────────────────────────┐
│                  SQLite / PostgreSQL Database                │
│  ┌──────────┐ ┌──────────┐ ┌──────────┐ ┌──────────────┐   │
│  │ Reviews  │ │  Issues  │ │ Feedback │ │   Learned    │   │
│  │          │ │          │ │          │ │   Patterns   │   │
│  └──────────┘ └──────────┘ └──────────┘ └──────────────┘   │
└─────────────────────────────────────────────────────────────┘
```

---

## 📊 Database Schema Additions (v2.1)

### `qa_feedback` Table
Stores user corrections for each AI finding:
- `issue_id`: Reference to original issue
- `user_action`: 'accepted', 'rejected', or 'modified'
- `original_finding_json`: Complete issue record
- `user_corrected_finding`: Expert's correction
- `user_comment`: Explanation (critical for learning)

### `qa_learned_patterns` Table
Aggregated insights from feedback:
- `pattern_type`: 'grammar_exception', 'domain_exception', 'acceptable_pattern'
- `pattern_key`: Unique identifier (e.g., 'grammar_terminology')
- `pattern_value`: The learned rule
- `confidence_score`: 0.0 to 1.0 (increases with repeated confirmations)
- `occurrence_count`: How many times this pattern was observed

---

## 🚀 Usage Guide

### Step 1: Run an Audit
1. Upload document (PDF/DOCX/TXT)
2. Select document type (Protocol, CSR, SAP, etc.)
3. Choose model (recommendation: `llama3.2:3b` or `qwen2.5:3b`)
4. Click "Analyze"

### Step 2: Review Findings
- Browse issues by severity (Critical/Major/Minor)
- Filter by category
- Export report (DOCX/PDF)

### Step 3: Provide Feedback (CRITICAL FOR LEARNING)
Navigate to **"Feedback & Learning"** tab:

1. **Enter Issue ID** from the results table
2. **Select Action**:
   - ✓ Accepted - AI was correct
   - ✗ Rejected - AI was wrong
   - ⚠ Modified - Partially correct
3. **Provide Correction** (if rejected/modified):
   - Describe what should have been said
4. **Add Comments** (MOST IMPORTANT):
   - Explain WHY the AI was wrong
   - Examples:
     - "Passive voice is standard in methods sections"
     - "This is a cosmetic study, not a drug trial - DSMB not required"
     - "Sample size formula not needed for exploratory studies"

### Step 4: View Learned Patterns
The system displays:
- Total feedback given
- Issues rejected
- Patterns learned
- Accuracy improvement metric
- Common false positive categories (chart)

### Step 5: Regenerate Prompts
Click **"Regenerate Prompt Templates"** to:
- Incorporate all high-confidence patterns (≥70%)
- Update system prompts with expert knowledge
- Reduce future false positives

---

## 📈 Expected Improvement Timeline

| Audits Completed | False Positive Rate | Accuracy Improvement |
|-----------------|---------------------|----------------------|
| 0-10            | ~15%                | Baseline             |
| 10-25           | ~12%                | +20%                 |
| 25-50           | ~9%                 | +40%                 |
| 50-100          | ~6%                 | +60%                 |
| 100+            | ~4%                 | +75%                 |

*Based on pattern accumulation and prompt refinement*

---

## 🔧 Technical Implementation

### New Functions in `db.R`
```r
db_save_feedback()        # Store user corrections
db_get_feedback_stats()   # Retrieve statistics
db_learn_pattern()        # Aggregate patterns from feedback
db_get_learned_patterns() # Fetch patterns for prompt injection
db_analyze_false_positives() # Identify problem categories
```

### Enhanced `build_review_request()` in `analysis.R`
- Loads learned patterns before each audit
- Injects exceptions into prompt dynamically
- Applies domain-specific exclusions
- Respects acceptable pattern whitelist

### New Module `mod_feedback.R`
- Interactive feedback form
- Real-time statistics dashboard
- False positive visualization
- Pattern management tools
- Export functionality

---

## 🎓 Best Practices for Maximum Learning

### DO:
✅ Provide detailed comments explaining why AI was wrong
✅ Reject false positives consistently (don't just ignore them)
✅ Use specific language: "Passive voice acceptable in methods" not "wrong"
✅ Classify correctly: accept true findings, reject false ones
✅ Regenerate prompts after every 10-15 feedback submissions

### DON'T:
❌ Leave comment fields blank (system can't learn without explanation)
❌ Accept findings you're unsure about (better to reject with comment)
❌ Use vague comments like "not important" or "ignore"
❌ Forget to regenerate prompts periodically

---

## 🛡️ Security & Compliance

- **Audit Trail**: Every feedback action logged with timestamp and user
- **Role-Based Access**: Only reviewers/admins can submit feedback
- **Data Privacy**: All data stored locally (no cloud transmission)
- **SOC Compliance**: Full traceability from finding to correction

---

## 📦 Installation

### Prerequisites
- R 4.2+ with Shiny, bslib, httr2, jsonlite, DBI, RSQLite
- Ollama Desktop installed and running
- At least one model downloaded (e.g., `ollama pull llama3.2:3b`)

### Quick Start
```bash
cd /workspace/qa_reviewer
Rscript -e "shiny::runApp(port = 3862, host = '0.0.0.0')"
```

Or use the provided launcher:
```bash
Rscript launch.R
```

---

## 📝 Example Feedback Scenarios

### Scenario 1: Grammar Exception
**AI Finding**: "Passive voice detected in Methods section"  
**Your Action**: Rejected  
**Comment**: "Passive voice is standard and required in clinical study methods sections per ICH E3 guidelines. Do not flag passive voice in Methods, Results, or Statistical Analysis sections."

**Result**: System learns `grammar_exception` pattern and stops flagging passive voice in these sections.

### Scenario 2: Domain Exclusion
**AI Finding**: "Missing DSMB charter mention"  
**Your Action**: Rejected  
**Comment**: "This is a cosmetic patch test study, not a drug trial. DSMB (Data Safety Monitoring Board) is only required for Phase II/III drug trials with significant risk. Not applicable for cosmetic studies."

**Result**: System learns `domain_exception` and won't require DSMB for cosmetic study protocols.

### Scenario 3: Acceptable Pattern
**AI Finding**: "Abbreviation 'AE' not defined at first use"  
**Your Action**: Rejected  
**Comment**: "AE (Adverse Event) is a universally recognized term in clinical research. No need to define standard regulatory abbreviations like AE, SAE, SUSAR, CTCAE."

**Result**: System adds AE to acceptable pattern whitelist.

---

## 🔮 Future Enhancements (Roadmap)

- [ ] Automatic pattern suggestion from comment NLP analysis
- [ ] Cross-document consistency checking (protocol vs CSR vs SAP)
- [ ] Fine-tuned domain-specific LoRA adapter for Ollama models
- [ ] Batch feedback processing for historical reviews
- [ ] Confidence threshold auto-adjustment based on acceptance rate
- [ ] Integration with eTMF/CTMS systems
- [ ] Multi-tenant support with organization-specific pattern libraries

---

## 📞 Support

For issues or questions:
- Check logs in `data/qa_reviewer.sqlite`
- Review feedback exports in `data/exports/`
- Contact QA team lead for pattern library governance

---

## 📄 License

Proprietary - Mascot Spincontrol Pvt. Ltd.

---

**Version**: 2.1.0  
**Last Updated**: 2026  
**Build**: Self-Learning Edition
