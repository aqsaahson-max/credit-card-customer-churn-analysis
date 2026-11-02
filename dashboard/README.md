# Customer Retention Decision-Support Dashboard

**IT3081 Statistical Modelling — Group Assignment**

A working portfolio prototype demonstrating how statistical findings from a credit card customer churn analysis could support a banking team's customer-review process.

## Business Problem

How can a retention team use customer activity patterns, model scores, and available staff capacity to decide which records to review — and then evaluate whether its actions are useful?

## Dataset

- **Source:** [BankChurners](https://www.kaggle.com/datasets/sakshigoyal7/credit-card-customers) (Kaggle)
- **Records:** 10,127 credit card holders with a recorded attrition label
- **Training subset:** 8,101 records (model fitting and 10-fold cross-validation)
- **Internal test subset:** 2,026 records (held-out evaluation, used once)

## Methods

| Stage | Approach |
|---|---|
| Descriptive analysis | Summary statistics, visualisations, correlation analysis |
| Statistical inference | Welch t-test, proportion test, Brown–Forsythe, Welch ANOVA (Holm-adjusted) |
| Predictive modelling | Logistic regression with natural spline terms |
| Model comparison | Baseline logistic, Ridge, LASSO, Elastic Net (α = 0.25, 0.50, 0.75) |
| Selection criterion | Training 10-fold cross-validated log loss |
| Threshold | Maximum F1 on training out-of-fold predictions (0.3392) |

**Selected model test performance:** ROC-AUC 0.960, Log Loss 0.180, Average Precision 0.847

## Dashboard Features

| Screen | Description |
|---|---|
| **Project Overview** | Business context, dataset populations, project status |
| **Customer Activity** | Descriptive charts and comparison tables from Task 3 |
| **Model Evidence** | Model comparison, selected model metrics, calibration, threshold analysis |
| **Review Queue** | Searchable, sortable table of 2,026 anonymised test records |
| **Workload & Capacity** | Capacity slider and threshold exploration |
| **Review Tracking** | Simulated review decision summary |
| **Pilot Proposal** | Proposed controlled evaluation framework and expert feedback section |
| **Methodology** | PCA, Bayesian, and time series evaluations; known limitations |

## Running the Dashboard

### Prerequisites

- A web browser (Chrome, Firefox, Edge, or Safari)
- One of: Python, Node.js, or PowerShell (for the local server)

### Option 1: PowerShell (Windows, no installation needed)

```powershell
cd dashboard
powershell -ExecutionPolicy Bypass -File serve.ps1
```

Then open **http://localhost:8000** in your browser.

### Option 2: Python

```bash
cd dashboard
python -m http.server 8000
```

### Option 3: Node.js

```bash
cd dashboard
npx -y serve -p 8000
```

> **Note:** The dashboard cannot be opened directly as a file (`file://`) because browsers block `fetch()` requests to local JSON/CSV files. A local server is required.

## Project Structure

```
Credit Card Customer Churn Analysis/
├── BankChurners.csv                    # Source dataset
├── prepare_dashboard_data.R            # Data preparation script
├── SM- Credit Card Customer Churn Analysis/
│   └── DS-WD-77.R                      # Complete R analysis (Tasks 3–5)
├── Task3_Manual_R_Outputs/             # Descriptive analysis outputs
├── Task4_Manual_R_Outputs/             # Statistical inference outputs
├── Task5_Manual_R_Outputs/             # Modelling outputs and predictions
└── dashboard/
    ├── index.html                      # Dashboard application
    ├── styles.css                      # Design system
    ├── app.js                          # Application logic
    ├── serve.ps1                       # Local development server
    └── data/                           # Dashboard-ready data files
        ├── test_records.json           # 2,026 merged test records
        ├── summary.json                # Dataset summary statistics
        └── *.csv                       # Copied analysis outputs
```

## Limitations

- **Retrospective labels:** The dataset records who left, not who will leave
- **No dated history:** Only aggregated transaction totals, not timestamped records
- **Unknown sampling frame:** Selection criteria for inclusion are undocumented
- **Prototype scope:** Simulated review workflow; no production banking system
- **Expert consultation:** Pending (section prepared for actual feedback)

This prototype does not claim to have saved customers or money. It demonstrates a defensible decision process that would require further validation before operational deployment.

## Data Preparation

The `prepare_dashboard_data.R` script:
1. Loads the raw BankChurners dataset and test predictions
2. Merges predictions with customer features by source row index
3. Creates anonymous demonstration IDs
4. Exports dashboard-ready JSON and copies analysis CSVs

To regenerate: `Rscript prepare_dashboard_data.R` (requires R 4.x, base packages only)

## Technology

- **Analysis:** R 4.6.1 (base, stats, splines, glmnet)
- **Dashboard:** HTML, CSS, vanilla JavaScript
- **Charts:** Chart.js 4.4.4 (loaded from CDN)
- **Server:** PowerShell HttpListener (zero dependencies)

## Acknowledgements

- Dataset: [Sakshi Goyal, Kaggle](https://www.kaggle.com/datasets/sakshigoyal7/credit-card-customers)
- Chart library: [Chart.js](https://www.chartjs.org/)
- Fonts: [Inter](https://fonts.google.com/specimen/Inter), [JetBrains Mono](https://fonts.google.com/specimen/JetBrains+Mono)

## Recording checks (16 October 2026)

- Verified 2,026 records: 360 flagged and 1,666 below the saved threshold 0.339210811098011. Saved prediction precision is preserved; only displayed scores are rounded.
- Amounts use dataset monetary units because national currency is unverified.
- Queue starts with the highest score; the active sort is visible. Reset Demo restores that order and clears decisions and notes.
- Tracking covers all 2,026 records, including below-threshold records. All decisions are simulated and session-only.
- At 100 hypothetical slots: 100 within capacity, 260 beyond capacity. Zero and 500 slots are also checked.
- Threshold outcome tables are under Model Evidence. Individual actual labels are absent from queue, details, and tracking.
- Suggested recording note: Additional customer context needed before considering contact. Choose Further information needed, then save. Refreshing clears this session note.
- Run functional checks with: node dashboard/verify.cjs (from the project folder). Browser checks also verified finite averages, descending queue order, saving a note, capacity totals, and no console errors.
