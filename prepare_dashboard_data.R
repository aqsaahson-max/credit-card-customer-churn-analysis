## prepare_dashboard_data.R
## Merges the saved R analysis outputs with the raw BankChurners dataset
## and exports dashboard-ready JSON files for the HTML frontend.
## Run once: Rscript prepare_dashboard_data.R

# ── paths ────────────────────────────────────────────────────────────────
raw_csv   <- "BankChurners.csv"
pred_csv  <- "Task5_Manual_R_Outputs/Final_Test_Predictions.csv"
assign_csv <- "Task5_Manual_R_Outputs/Training_Test_Row_Assignment.csv"

out_dir <- "dashboard/data"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ── 1. Load and clean the raw dataset ────────────────────────────────────
bank <- read.csv(raw_csv, stringsAsFactors = FALSE)
cat("Raw dataset rows:", nrow(bank), "\n")
cat("Raw dataset cols:", ncol(bank), "\n")

# Drop Naive Bayes leakage columns
nb_cols <- grep("^Naive_Bayes", names(bank), value = TRUE)
bank <- bank[, !(names(bank) %in% nb_cols)]

# Add a 1-indexed Source_Row to match predictions
bank$Source_Row <- seq_len(nrow(bank))

# ── 2. Load test predictions ────────────────────────────────────────────
preds <- read.csv(pred_csv, stringsAsFactors = FALSE)
cat("Prediction rows:", nrow(preds), "\n")

# ── 3. Merge predictions with customer features ─────────────────────────
test_merged <- merge(preds, bank, by = "Source_Row")
cat("Merged test rows:", nrow(test_merged), "\n")

# Verify expected counts
stopifnot(nrow(test_merged) == 2026)
stopifnot(sum(test_merged$Predicted_Churn == 1) == 360) # flagged at threshold

# ── 4. Create anonymous demonstration IDs ────────────────────────────────
set.seed(42)
test_merged$Demo_ID <- paste0("DEMO-", sprintf("%04d", sample(seq_len(nrow(test_merged)))))

# ── 5. Select dashboard fields ──────────────────────────────────────────
dashboard_records <- data.frame(
  Demo_ID                  = test_merged$Demo_ID,
  Attrition_Score          = test_merged$Predicted_Probability,
  Predicted_Churn          = test_merged$Predicted_Churn,
  Actual_Churn             = test_merged$Actual_Churn,
  Customer_Age             = test_merged$Customer_Age,
  Gender                   = test_merged$Gender,
  Dependent_count          = test_merged$Dependent_count,
  Education_Level          = test_merged$Education_Level,
  Marital_Status           = test_merged$Marital_Status,
  Income_Category          = test_merged$Income_Category,
  Card_Category            = test_merged$Card_Category,
  Months_on_book           = test_merged$Months_on_book,
  Total_Relationship_Count = test_merged$Total_Relationship_Count,
  Months_Inactive_12_mon   = test_merged$Months_Inactive_12_mon,
  Contacts_Count_12_mon    = test_merged$Contacts_Count_12_mon,
  Credit_Limit             = test_merged$Credit_Limit,
  Total_Revolving_Bal      = test_merged$Total_Revolving_Bal,
  Avg_Open_To_Buy          = test_merged$Avg_Open_To_Buy,
  Total_Amt_Chng_Q4_Q1     = test_merged$Total_Amt_Chng_Q4_Q1,
  Total_Trans_Amt          = test_merged$Total_Trans_Amt,
  Total_Trans_Ct           = test_merged$Total_Trans_Ct,
  Total_Ct_Chng_Q4_Q1      = test_merged$Total_Ct_Chng_Q4_Q1,
  Avg_Utilization_Ratio    = test_merged$Avg_Utilization_Ratio,
  stringsAsFactors = FALSE
)

# ── 6. Write as JSON (no extra dependencies needed) ──────────────────────
# Simple JSON array writer using base R
to_json_value <- function(x) {
  if (is.na(x)) return("null")
  if (is.numeric(x)) return(sprintf("%.17g", x))
  # Escape quotes in strings
  x <- gsub("\\\\", "\\\\\\\\", x)
  x <- gsub("\"", "\\\\\"", x)
  paste0("\"", x, "\"")
}

to_json_row <- function(row, nms) {
  vals <- sapply(seq_along(row), function(i) {
    paste0("\"", nms[i], "\":", to_json_value(row[[i]]))
  })
  paste0("{", paste(vals, collapse = ","), "}")
}

write_json_array <- function(df, filepath) {
  nms <- names(df)
  lines <- sapply(seq_len(nrow(df)), function(i) {
    to_json_row(as.list(df[i, ]), nms)
  })
  writeLines(
    c("[", paste(lines, collapse = ",\n"), "]"),
    filepath
  )
  cat("Wrote", filepath, "-", nrow(df), "records\n")
}

# Main test records for the review queue
write_json_array(dashboard_records, file.path(out_dir, "test_records.json"))

# ── 7. Summary statistics for dashboard cards ────────────────────────────
# Full dataset summary
full_summary <- list(
  total_records   = nrow(bank),
  training_count  = 8101,
  test_count      = 2026,
  total_existing  = sum(bank$Attrition_Flag == "Existing Customer"),
  total_attrited  = sum(bank$Attrition_Flag == "Attrited Customer"),
  attrition_rate  = round(mean(bank$Attrition_Flag == "Attrited Customer") * 100, 2),
  test_flagged    = sum(test_merged$Predicted_Churn == 1),
  test_attrited   = sum(test_merged$Actual_Churn == 1),
  threshold       = 0.339210811098011,
  tp = 259, fp = 101, tn = 1599, fn = 67
)

# Write as a simple JSON object
summary_json <- paste0("{\n",
  paste(sapply(names(full_summary), function(n) {
    v <- full_summary[[n]]
    if (is.character(v)) paste0('  "', n, '": "', v, '"')
    else paste0('  "', n, '": ', v)
  }), collapse = ",\n"),
"\n}")
writeLines(summary_json, file.path(out_dir, "summary.json"))
cat("Wrote summary.json\n")

# ── 8. Copy pre-computed CSV outputs for model evidence screens ──────────
files_to_copy <- c(
  "Task5_Manual_R_Outputs/OOF_Model_Comparison.csv",
  "Task5_Manual_R_Outputs/Final_Test_Probability_Performance.csv",
  "Task5_Manual_R_Outputs/Final_Test_Metric_Intervals.csv",
  "Task5_Manual_R_Outputs/Final_Test_Confusion_Matrix.csv",
  "Task5_Manual_R_Outputs/Final_Test_Calibration_Table.csv",
  "Task5_Manual_R_Outputs/Final_Test_Threshold_Comparison.csv",
  "Task5_Manual_R_Outputs/Final_Model_Selection_Record.csv",
  "Task5_Manual_R_Outputs/Penalized_Logistic_Tuning_Summary.csv",
  "Task3_Manual_R_Outputs/Numeric_Comparison_By_Attrition.csv",
  "Task3_Manual_R_Outputs/Attrition_By_Category.csv",
  "Task3_Manual_R_Outputs/Numeric_Descriptive_Statistics.csv",
  "Task4_Manual_R_Outputs/Task4_Primary_Tests_Holm_Adjusted.csv"
)

for (f in files_to_copy) {
  if (file.exists(f)) {
    file.copy(f, file.path(out_dir, basename(f)), overwrite = TRUE)
    cat("Copied", basename(f), "\n")
  } else {
    cat("WARNING: missing", f, "\n")
  }
}

cat("\n=== Data preparation complete ===\n")
cat("Dashboard data written to:", normalizePath(out_dir), "\n")
