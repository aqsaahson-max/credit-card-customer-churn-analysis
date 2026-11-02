# Task 3: Credit Card Customer Churn Analysis
#Identify the dataset

data_path <- "BankChurners.csv"

#Check that R can find it
file.exists(data_path)

#Read the CSV
bank <- read.csv(
  file = data_path,
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  na.strings = c("", "NA", "N/A", "NULL")
)

#Check the dimensions
dim(bank)

#inspect them separately
nrow(bank)
ncol(bank)

#Examine the column names and a few records
names(bank)

#inspect a manageable subset
head(bank[, 1:6])

#open the dataset in RStudio’s table viewer
View(bank)

#Inspect the structure
str(bank)

#Check the recorded outcome categories
table(bank$Attrition_Flag, useNA = "ifany")

status_counts <- table(
  bank$Attrition_Flag,
  useNA = "ifany"
)

#Calculate percentages
status_percentages <- prop.table(status_counts) * 100

round(status_percentages, 2)

#Identify the columns we should exclude from predictors
# Identify the supplied classifier-output columns
nb_columns <- grep(
  pattern = "^Naive_Bayes",
  x = names(bank),
  value = TRUE
)

nb_columns
length(nb_columns)

#Define the outcome and identifier
outcome_column <- "Attrition_Flag"
id_column <- "CLIENTNUM"

#define the eligible customer predictors
predictor_columns <- setdiff(
  names(bank),
  c(outcome_column, id_column, nb_columns)
)

predictor_columns
length(predictor_columns)

#We have only created lists of column names. 
#Nothing has been deleted from bank. 
#Also, eligibility does not mean we will necessarily include all 19 predictors together in a final model.

#Separate numeric and categorical predictors
#Identify numeric predictors
numeric_columns <- predictor_columns[
  sapply(bank[predictor_columns], is.numeric)
]

numeric_columns
length(numeric_columns)

#remaining predictors are categorical
categorical_columns <- setdiff(
  predictor_columns,
  numeric_columns
)

categorical_columns

#Inspect their recorded categories
lapply(
  bank[categorical_columns],
  function(x) sort(unique(x))
)

#Why inspect categories before converting them to factors? 
#It lets you notice unexpected spellings, Unknown labels and sparse categories first. 
#For now, leave them unchanged. Later, we can choose reference categories and factor coding deliberately

#Check for duplicate records
#Check whether any entire rows are repeated
# Number of rows that repeat an earlier complete row
sum(duplicated(bank))

#check customer identifiers
sum(duplicated(bank$CLIENTNUM))

#Confirm the number of distinct identifiers
length(unique(bank$CLIENTNUM))

#Why perform both checks? 
#Two rows could share a customer identifier but contain different values. They would not be exact duplicate rows, but they would still require investigation.

#check for identical customer predictor profiles
sum(duplicated(bank[predictor_columns]))

#Identical profiles would not automatically mean duplicate customers: different customers can share the same characteristics. This check would identify records to investigate, not justify automatic deletion.

#Organizational interpretation: no repeated customer identifiers were found, supporting the use of one recorded customer per row in this file. This does not independently verify that the underlying customer records are authentic.

#Check for missing values recognised by R
#Count missing values in every column
missing_counts <- colSums(is.na(bank))

missing_counts

#Check the total
sum(is.na(bank))

#Do not conclude that customer information is complete yet. "Unknown" is text, so is.na() does not count it.

#Count Unknown categories separately
unknown_counts <- sapply(
  bank[categorical_columns],
  function(x) sum(x == "Unknown", na.rm = TRUE)
)

unknown_counts

#na.rm = TRUE prevents an actual missing value from making the count unavailable.

#Create a table with percentages
unknown_summary <- data.frame(
  Variable = names(unknown_counts),
  Unknown_Count = as.numeric(unknown_counts),
  Unknown_Percent = round(
    as.numeric(unknown_counts) / nrow(bank) * 100,
    2
  )
)

unknown_summary

#count distinct customers with at least one unknown field
has_unknown <- rowSums(
  bank[categorical_columns] == "Unknown",
  na.rm = TRUE
) > 0

sum(has_unknown)

#Calculate the percentage
round(mean(has_unknown) * 100, 2)

#Why not add the three unknown counts? \
#One customer can have unknown education, marital status and income. Adding column counts would count that customer more than once.
#Preparation decision: retain Unknown as a separate category for descriptive analysis. 
#Do not delete these customers or replace their unknown information with the most common category. Neither action is justified merely by finding an unknown value.

#finish the data-quality assessment by checking value ranges and relationships between financial variables

#Inspect the minimum and maximum values

# Minimum and maximum for each numeric predictor
range_summary <- data.frame(
  Variable = numeric_columns,
  Minimum = sapply(
    bank[numeric_columns],
    min,
    na.rm = TRUE
  ),
  Maximum = sapply(
    bank[numeric_columns],
    max,
    na.rm = TRUE
  ),
  row.names = NULL
)

range_summary

#Ranges help identify values that need investigation. 
#For example, negative transaction counts would be inconsistent with their meaning. A high credit limit, however, may be legitimate.
#Do not label a value erroneous simply because it is unusually large.

#Check for negative values
negative_counts <- sapply(
  bank[numeric_columns],
  function(x) sum(x < 0, na.rm = TRUE)
)

negative_counts

#For these recorded variables, negative values would require investigation because they represent ages, counts, balances, limits or nonnegative ratios.
#This rule is specific to these variables. Negative values can be valid for other financial variables, such as profit or net cash flow.

#Check logical bounds and relationships
#Create a small table of checks
range_checks <- data.frame(
  Check = c(
    "Utilization outside 0 to 1",
    "Inactive months outside 0 to 12",
    "Credit limit zero or negative",
    "Revolving balance above credit limit"
  ),
  
  Number_of_records = c(
    sum(
      bank$Avg_Utilization_Ratio < 0 |
        bank$Avg_Utilization_Ratio > 1,
      na.rm = TRUE
    ),
    
    sum(
      bank$Months_Inactive_12_mon < 0 |
        bank$Months_Inactive_12_mon > 12,
      na.rm = TRUE
    ),
    
    sum(
      bank$Credit_Limit <= 0,
      na.rm = TRUE
    ),
    
    sum(
      bank$Total_Revolving_Bal > bank$Credit_Limit,
      na.rm = TRUE
    )
  )
)

range_checks

#Check whether available credit is a derived variable
#We want to examine this relationship: Available Credit = Credit Limit - Revolving Balance
#Calculate the expected available credit
calculated_open_to_buy <- bank$Credit_Limit -
  bank$Total_Revolving_Bal

#Compare it with the recorded column
open_to_buy_difference <- bank$Avg_Open_To_Buy -
  calculated_open_to_buy

max(abs(open_to_buy_difference))

#This means approximately 0.000000000000057, effectively zero for this comparison. Tiny differences arise from how computers represent decimal numbers.

#Use a small tolerance to count meaningful discrepancies
sum(abs(open_to_buy_difference) > 0.000001)

#What does this establish?
#In your file, Avg_Open_To_Buy is determined by the other two columns. These are not three independent pieces of information.

#Why does this matter later? Including all three together as ordinary linear terms in logistic regression creates exact linear dependence between predictors, so their separate coefficients cannot be uniquely estimated.

#For now:
#- Keep all three in the descriptive analysis.
#- Record the redundancy.
#- Plan to omit Avg_Open_To_Buy from a regression specification that already includes credit limit and revolving balance.
#Do not delete it from bank at this stage.

#Check the utilization ratio
#Examine whether utilization corresponds to revolving balance divided by credit limit
calculated_utilization <- bank$Total_Revolving_Bal /
  bank$Credit_Limit

utilization_difference <- bank$Avg_Utilization_Ratio -
  calculated_utilization

max(abs(utilization_difference))

#The supplied utilization values are rounded to three decimal places. Check that all discrepancies fall within that rounding allowance
sum(abs(utilization_difference) > 0.0005001)

#The slightly larger tolerance avoids treating a tiny computer-rounding difference as a violation.
#Interpretation: utilization contains information derived from the balance and credit limit. Unlike the available-credit identity, this is a division relationship, so it does not automatically create the same exact linear dependence. However, it still matters when interpreting overlapping financial predictors.

#Investigate the repeated maximum credit limit
maximum_credit_limit <- max(bank$Credit_Limit)

maximum_credit_limit

#Count how many customers have that value
customers_at_maximum <- sum(
  bank$Credit_Limit == maximum_credit_limit
)

customers_at_maximum

#Calculate the percentage
round(
  customers_at_maximum / nrow(bank) * 100,
  2
)

#Organizational interpretation: 508 customers share the highest recorded limit. This could reflect a product ceiling, a data cap or another recording convention. The CSV cannot establish which explanation is correct.
#Keep these observations. Record the pattern as something to clarify through documentation or expert consultation.

#Understand one variable first
#Start with credit limit

# Descriptive statistics for credit limit
mean(bank$Credit_Limit)
median(bank$Credit_Limit)
sd(bank$Credit_Limit)

quantile(
  bank$Credit_Limit,
  probs = c(0.25, 0.50, 0.75),
  type = 7
)

#The mean credit limit is much higher than the median. This suggests that relatively high credit limits pull the mean upward. We will examine the distribution visually before describing its shape more fully.
#Organizational meaning: using only the mean would give an incomplete picture of a typical customer’s credit limit. Reporting the median and quartiles shows the substantial variation across customers.

#Create a table for all 14 numeric predictors
#Instead of calculating each variable separately, apply the same functions to all numeric predictors

# Overall descriptive statistics
numeric_summary <- data.frame(
  Variable = numeric_columns,
  
  N = sapply(
    bank[numeric_columns],
    function(x) sum(!is.na(x))
  ),
  
  Mean = sapply(
    bank[numeric_columns],
    mean,
    na.rm = TRUE
  ),
  
  SD = sapply(
    bank[numeric_columns],
    sd,
    na.rm = TRUE
  ),
  
  Minimum = sapply(
    bank[numeric_columns],
    min,
    na.rm = TRUE
  ),
  
  Q1 = sapply(
    bank[numeric_columns],
    function(x) quantile(
      x,
      probs = 0.25,
      na.rm = TRUE,
      type = 7,
      names = FALSE
    )
  ),
  
  Median = sapply(
    bank[numeric_columns],
    median,
    na.rm = TRUE
  ),
  
  Q3 = sapply(
    bank[numeric_columns],
    function(x) quantile(
      x,
      probs = 0.75,
      na.rm = TRUE,
      type = 7,
      names = FALSE
    )
  ),
  
  Maximum = sapply(
    bank[numeric_columns],
    max,
    na.rm = TRUE
  ),
  
  row.names = NULL
)

View(numeric_summary)

#We include both mean/SD and median/quartiles because they describe different aspects of a distribution. A large upper tail can strongly affect the mean and SD, while the median and quartiles are less sensitive to extreme values.

#Add the interquartile range
numeric_summary$IQR <- numeric_summary$Q3 -
  numeric_summary$Q1

#It measures the spread of the middle half of the observations. We will also use it during outlier screening.

#Create a rounded presentation copy
#Keep the original calculations at full precision. Make a separate copy for display
numeric_summary_display <- numeric_summary

numeric_summary_display[, -1] <- round(
  numeric_summary_display[, -1],
  digits = 3
)

View(numeric_summary_display)

#[, -1] means “all rows and all columns except the first.” We exclude the first column because it contains variable names rather than numbers.
#Why three decimal places? Several predictors are ratios. Rounding a ratio too aggressively can conceal useful detail. When preparing your final report, you can show monetary values to two decimal places and ratios to three.

#Save your table
#Create an output folder beside the CSV

output_folder <- file.path(
  dirname(data_path),
  "Task3_Manual_R_Outputs"
)

dir.create(
  output_folder,
  showWarnings = FALSE,
  recursive = TRUE
)

#Save the full-precision table
write.csv(
numeric_summary,
file = file.path(
  output_folder,
  "Numeric_Descriptive_Statistics.csv"
),
row.names = FALSE
)

#This saves your results without modifying the source dataset. Running the command again updates that results file.

#create frequency tables for the five categorical predictors
#Start with one categorical variable
#Count customers in each card category
# Customer counts by card category
card_counts <- table(
  bank$Card_Category,
  useNA = "ifany"
)

card_counts

#Convert these counts into percentages:
  card_percentages <- prop.table(card_counts) * 100

round(card_percentages, 2)

#These are percentages of all customer records, not attrition percentages

#Why counts and percentages? Category labels do not have meaningful numerical distances. Calculating an average of arbitrary codes such as Blue = 1 and Gold = 2 would not describe a meaningful customer characteristic.

#Create a reusable frequency-table function
#Rather than repeating the same calculations five times, write a small function:

# Function to summarize one categorical variable
make_frequency_table <- function(variable_name) {
  
  counts <- table(
    bank[[variable_name]],
    useNA = "ifany"
  )
  
  data.frame(
    Variable = variable_name,
    Category = names(counts),
    Count = as.numeric(counts),
    Percentage = as.numeric(counts) / nrow(bank) * 100,
    row.names = NULL
  )
}

#Test it on card category:
  make_frequency_table("Card_Category")
  
#Apply it to all five categorical predictors
# Generate one table per categorical predictor
  frequency_tables <- lapply(
    categorical_columns,
    make_frequency_table
  )
  
#Combine them into one table
  categorical_summary <- do.call(
    rbind,
    frequency_tables
  )
  
  rownames(categorical_summary) <- NULL
  
  View(categorical_summary)
  
#You should obtain 24 rows:
    nrow(categorical_summary)
    
#Check that the counts reconcile
#Each variable should account for all 10,127 customers:
    aggregate(
      Count ~ Variable,
      data = categorical_summary,
      FUN = sum
    )
    
#Check percentages:
    aggregate(
      Percentage ~ Variable,
      data = categorical_summary,
      FUN = sum
    )
    
#Why perform these checks? They help detect accidentally omitted categories or incorrect denominators. Do not add counts across all five variables and interpret that as the number of customers: each customer appears once in each variable’s frequency table.

#Make a rounded display copy and save the results
    categorical_summary_display <- categorical_summary
    
    categorical_summary_display$Percentage <- round(
      categorical_summary_display$Percentage,
      2
    )
    
    View(categorical_summary_display)
    
#Save the unrounded results using the output folder you already created
    write.csv(
      categorical_summary,
      file = file.path(
        output_folder,
        "Categorical_Frequency_Tables.csv"
      ),
      row.names = FALSE
    )
    
#Rounded category percentages can add to slightly above or below 100%. That is ordinary rounding, not necessarily a calculation error.

#Why highlight small categories? A percentage based on 20 customers can change considerably with only a few observations. Later comparisons should always show the number of customers behind each rate.
#Do not merge or remove categories yet. Descriptive tables should preserve the recorded categories. Any later modelling decision to combine categories needs a substantive justification.
    
#we’ll create two bar charts:
#1. Recorded attrition status.
#2. Card-category distribution.
#these show the outcome imbalance and product concentration.
    
#Create a folder for your charts
# Folder for saved figures
    chart_folder <- file.path(
      output_folder,
      "Charts"
    )
    
    dir.create(
      chart_folder,
      showWarnings = FALSE,
      recursive = TRUE
    )
    
#Prepare the attrition chart data
#You already calculated status_counts. Arrange the categories in a deliberate order
    
# Display existing customers first
    status_plot_counts <- status_counts[
      c("Existing Customer", "Attrited Customer")
    ]
    
    status_plot_percentages <- prop.table(
      status_plot_counts
    ) * 100
    
    status_plot_counts
    
#Changing the display order does not change any observations.

#Create labels containing both the count and percentage:
    status_labels <- paste0(
      format(
        as.numeric(status_plot_counts),
        big.mark = ",",
        trim = TRUE
      ),
      " (",
      sprintf("%.2f", as.numeric(status_plot_percentages)),
      "%)"
    )
    
    status_labels
    
#Draw the attrition bar chart
    # Draw the bars and store their horizontal positions
    status_bar_positions <- barplot(
      height = as.numeric(status_plot_counts),
      names.arg = c("Existing", "Attrited"),
      col = c("#26718B", "#CC6230"),
      border = NA,
      main = "Recorded customer attrition status",
      xlab = "Customer status",
      ylab = "Number of customer records",
      ylim = c(0, max(status_plot_counts) * 1.18),
      las = 1
    )
    
    # Add a count and percentage above each bar
    text(
      x = status_bar_positions,
      y = as.numeric(status_plot_counts),
      labels = status_labels,
      pos = 3,
      cex = 0.9
    )
    
#Why a bar chart? It makes the difference between the two category counts easy to compare. A histogram would be inappropriate because the outcome is categorical, rather than a numeric measurement.

#Save the completed chart
    dev.copy(
      device = png,
      filename = file.path(
        chart_folder,
        "Figure_1_Attrition_Status.png"
      ),
      width = 1800,
      height = 1200,
      res = 180
    )
    
    dev.off()
    
#Interpretation for this figure
#The existing-customer class is much larger than the attrited class. A model assigning every customer to “Existing” would achieve 83.93% accuracy but identify no attrited customers.
#This supports evaluating later models using churn-class recall, precision and other appropriate measures—not accuracy alone. It does not mean we should automatically balance the dataset before analysis.
    
#Prepare the card-category chart
#Use the counts you calculated earlier and arrange them in product-tier order
    
    card_plot_counts <- card_counts[
      c("Blue", "Silver", "Gold", "Platinum")
    ]
    
    card_plot_percentages <- prop.table(
      card_plot_counts
    ) * 100
    
    card_labels <- paste0(
      format(
        as.numeric(card_plot_counts),
        big.mark = ",",
        trim = TRUE
      ),
      " (",
      sprintf("%.2f", as.numeric(card_plot_percentages)),
      "%)"
    )    
    
#Now draw the chart:
    card_bar_positions <- barplot(
      height = as.numeric(card_plot_counts),
      names.arg = names(card_plot_counts),
      col = "#26718B",
      border = NA,
      main = "Distribution of recorded card categories",
      xlab = "Card category",
      ylab = "Number of customer records",
      ylim = c(0, max(card_plot_counts) * 1.18),
      las = 1
    )
    
    text(
      x = card_bar_positions,
      y = as.numeric(card_plot_counts),
      labels = card_labels,
      pos = 3,
      cex = 0.85
    )
    
#The small premium-card bars are not a plotting error. Their size correctly reflects how few observations those groups contain. The labels make their counts visible without distorting the scale.
    
#Save this chart:
    dev.copy(
      device = png,
      filename = file.path(
        chart_folder,
        "Figure_2_Card_Categories.png"
      ),
      width = 1800,
      height = 1200,
      res = 180
    )
    
    dev.off()
    
#Organizational interpretation
#The dataset predominantly describes Blue-card customers. Overall findings therefore provide much more evidence about that group than about premium-card customers.
#The chart does not show that Blue-card customers are more likely to leave. It shows how many customers hold each card category. To compare attrition, we must later calculate:
    #Attrition percentage within a card category = (Attrited customers in that category / All customers in that category) * 100
#Keeping these denominators distinct prevents misleading conclusions.

#Confirm that both files were saved
    file.exists(
      file.path(
        chart_folder,
        c(
          "Figure_1_Attrition_Status.png",
          "Figure_2_Card_Categories.png"
        )
      )
    )
    
#examine numeric distributions and outlier flags. These help explain why some means differ substantially from medians and whether unusual observations need investigation.
    
#Draw a histogram of credit limits
#Distribution of credit limits
    hist(
      bank$Credit_Limit,
      breaks = 30,
      col = "#26718B",
      border = "white",
      main = "Distribution of recorded credit limits",
      xlab = "Credit limit (supplied monetary units)",
      ylab = "Number of customer records",
      las = 1
    )
    
#How to read it: each bar counts customers whose credit limits fall within an interval. Unlike a categorical bar chart, neighbouring bars represent adjacent numerical intervals.
#breaks = 30 requests approximately 30 intervals; R may adjust the boundaries to convenient values.
    
#Add lines showing the mean and median:
    abline(
      v = mean(bank$Credit_Limit),
      col = "#CC6230",
      lwd = 2,
      lty = 2
    )
    
    abline(
      v = median(bank$Credit_Limit),
      col = "#222222",
      lwd = 2
    )
    
    legend(
      "topright",
      legend = c("Mean", "Median"),
      col = c("#CC6230", "#222222"),
      lwd = 2,
      lty = c(2, 1),
      bty = "n"
    )
    
#Expected pattern: many customers have relatively low limits, with a long tail towards higher limits. The mean, approximately 8,632, lies to the right of the median, 4,549.
#This is evidence of right-skewness. It does not mean the high-limit observations are incorrect.
    
#Save the chart:
    dev.copy(
      device = png,
      filename = file.path(
        chart_folder,
        "Figure_3_Credit_Limit_Histogram.png"
      ),
      width = 1800,
      height = 1200,
      res = 180
    )
    
    dev.off()
    
#2. Examine transaction amounts
    hist(
      bank$Total_Trans_Amt,
      breaks = 30,
      col = "#26718B",
      border = "white",
      main = "Distribution of recorded transaction amounts",
      xlab = "Total transaction amount (supplied monetary units)",
      ylab = "Number of customer records",
      las = 1
    )
    
    abline(
      v = mean(bank$Total_Trans_Amt),
      col = "#CC6230",
      lwd = 2,
      lty = 2
    )
    
    abline(
      v = median(bank$Total_Trans_Amt),
      col = "#222222",
      lwd = 2
    )
    
    legend(
      "topright",
      legend = c("Mean", "Median"),
      col = c("#CC6230", "#222222"),
      lwd = 2,
      lty = c(2, 1),
      bty = "n"
    )
    
#save it
    dev.copy(
      device = png,
      filename = file.path(
        chart_folder,
        "Figure_4_Transaction_Amount_Histogram.png"
      ),
      width = 1800,
      height = 1200,
      res = 180
    )
    
    dev.off()
    
#Organizational interpretation: transaction amounts vary considerably across customers. The upper tail may contain legitimate high-activity customers. Management should consider this variation rather than describe all customers using one average.
#The plot may also show concentrations in different value ranges. Those are patterns to investigate—not proof of distinct customer segments without further analysis.

#Draw a credit-limit boxplot
    boxplot(
      bank$Credit_Limit,
      horizontal = TRUE,
      col = "#26718B",
      main = "Spread of recorded credit limits",
      xlab = "Credit limit (supplied monetary units)",
      las = 1
    )
    
#A boxplot provides a compact view of location, spread and unusually distant observations
    dev.copy(
      device = png,
      filename = file.path(
        chart_folder,
        "Figure_5_Credit_Limit_Boxplot.png"
      ),
      width = 1800,
      height = 900,
      res = 180
    )
    
    dev.off()
    
#4. Calculate the credit-limit outlier fences
#We will flag values outside:
    #Q1 - 1.5 * IQR and Q3 + 1.5 * IQR
    
    credit_q1 <- quantile(
      bank$Credit_Limit,
      probs = 0.25,
      type = 7,
      names = FALSE
    )
    
    credit_q3 <- quantile(
      bank$Credit_Limit,
      probs = 0.75,
      type = 7,
      names = FALSE
    )
    
    credit_iqr <- credit_q3 - credit_q1
    
    credit_lower_fence <- credit_q1 - 1.5 * credit_iqr
    credit_upper_fence <- credit_q3 + 1.5 * credit_iqr
    
    credit_lower_fence
    credit_upper_fence
    
#The negative lower fence is a calculated screening threshold, not a suggestion that negative credit limits are valid.

#Identify flagged records:
    credit_outlier_flag <- (
      bank$Credit_Limit < credit_lower_fence |
        bank$Credit_Limit > credit_upper_fence
    )
    
    sum(credit_outlier_flag)
    
#calculate percentage
    round(mean(credit_outlier_flag) * 100, 2)
#Interpretation: 984 records fall outside the pooled credit-limit IQR fences. They are statistical flags, not 984 confirmed data errors.
    
#Apply the same rule to all numeric predictors
#You already have Q1, Q3 and IQR in numeric_summary, so reuse those results
    outlier_summary <- numeric_summary[
      ,
      c("Variable", "Q1", "Q3", "IQR")
    ]
    
    outlier_summary$Lower_Fence <- (
      outlier_summary$Q1 -
        1.5 * outlier_summary$IQR
    )
    
    outlier_summary$Upper_Fence <- (
      outlier_summary$Q3 +
        1.5 * outlier_summary$IQR
    )

#Now create a flag column for each predictor:
    outlier_flags <- sapply(
      seq_along(numeric_columns),
      
      function(i) {
        
        values <- bank[[numeric_columns[i]]]
        
        values < outlier_summary$Lower_Fence[i] |
          values > outlier_summary$Upper_Fence[i]
      }
    )
    
    colnames(outlier_flags) <- numeric_columns
    
#This uses the existing matching order of numeric_columns and numeric_summary. You can confirm it:
    identical(
      outlier_summary$Variable,
      numeric_columns
    )
    
#Count the flags:
    outlier_summary$Flagged_Count <- colSums(outlier_flags)
    
    outlier_summary$Flagged_Percent <- (
      outlier_summary$Flagged_Count / nrow(bank) * 100
    )
    
    View(outlier_summary)
    
#Count customers with at least one flag:
    has_outlier_flag <- rowSums(outlier_flags) > 0
    
    sum(has_outlier_flag)
    
    round(mean(has_outlier_flag) * 100, 2)
    
#Do not add the per-variable counts to calculate affected customers. One customer can be flagged on several variables.
#Save the table:
    write.csv(
      outlier_summary,
      file = file.path(
        output_folder,
        "Outlier_Assessment.csv"
      ),
      row.names = FALSE
    )
    
#Decide what to do with the flags
#Retain the observations at this stage.
#The IQR rule identifies values distant from the central distribution. It does not determine whether they are incorrect. For example:
#- High credit limits may be valid product allocations.
#- High transaction amounts may represent active customers.
#- Zero contacts can be flagged when the central distribution is narrow, even though zero is logically possible.
#Automatically removing all flagged customers would discard 32.84% of the sample and could substantially change the population being analysed.
    
#Next, we’ll compare existing and attrited customers. This is where the descriptive analysis begins to address the churn question directly.
#We are comparing recorded groups—not yet testing statistical significance or identifying causes.
    
#Create two customer groups
    # Separate records by recorded attrition status
    existing_customers <- bank[
      bank$Attrition_Flag == "Existing Customer",
    ]
    
    attrited_customers <- bank[
      bank$Attrition_Flag == "Attrited Customer",
    ]
    
#The condition before the comma selects matching rows.
#The empty position after the comma keeps all columns.
    
#Check the sizes:
    nrow(existing_customers)
    nrow(attrited_customers)
    
#These are separate objects for comparison. Your original bank object remains unchanged.

#Compare one variable first
#Start with transaction count:
    mean(existing_customers$Total_Trans_Ct)
    mean(attrited_customers$Total_Trans_Ct)
    
    median(existing_customers$Total_Trans_Ct)
    median(attrited_customers$Total_Trans_Ct)
    
#Interpretation: recorded transaction activity is lower among attrited customers.
#However, the timing is not verified. Lower activity may precede departure, accompany it, or reflect account closure. This comparison alone cannot distinguish those explanations.
    
#Build a comparison table for all numeric predictors
    # Compare numeric predictors between the two groups
    group_comparison <- data.frame(
      Variable = numeric_columns,
      
      Existing_Mean = sapply(
        existing_customers[numeric_columns],
        mean,
        na.rm = TRUE
      ),
      
      Attrited_Mean = sapply(
        attrited_customers[numeric_columns],
        mean,
        na.rm = TRUE
      ),
      
      Existing_Median = sapply(
        existing_customers[numeric_columns],
        median,
        na.rm = TRUE
      ),
      
      Attrited_Median = sapply(
        attrited_customers[numeric_columns],
        median,
        na.rm = TRUE
      ),
      
      Existing_SD = sapply(
        existing_customers[numeric_columns],
        sd,
        na.rm = TRUE
      ),
      
      Attrited_SD = sapply(
        attrited_customers[numeric_columns],
        sd,
        na.rm = TRUE
      ),
      
      row.names = NULL
    )
    
#Create a rounded display copy
    group_comparison_display <- group_comparison
    
    group_comparison_display[, -1] <- round(
      group_comparison_display[, -1],
      digits = 3
    )
    
    View(group_comparison_display)
    
#Why include both means and medians? Some financial measures are skewed. Comparing both helps determine whether the group difference appears in the centre of the distribution or is strongly influenced by large observations.
#The SD columns describe variation within each group. They do not, by themselves, test whether a difference is statistically significant.
    
#Save the full-precision table:
    write.csv(
      group_comparison,
      file = file.path(
        output_folder,
        "Numeric_Comparison_By_Attrition.csv"
      ),
      row.names = FALSE
    )
    
#Visualize transaction counts with side-by-side boxplots
#Use a list to put the groups in an explicit order:
    
    boxplot(
      list(
        Existing = existing_customers$Total_Trans_Ct,
        Attrited = attrited_customers$Total_Trans_Ct
      ),
      col = c("#26718B", "#CC6230"),
      main = "Transaction counts by recorded attrition status",
      xlab = "Customer status",
      ylab = "Recorded transaction count",
      las = 1
    )
    
#What to look for
    #- The attrited group’s median should be lower.
    #- Compare the boxes and whiskers to examine variation.
    #- Notice that the distributions overlap: a transaction count does not perfectly identify status.
#Boxplots summarize each group separately, so the larger existing-customer group does not automatically produce a taller box. Remember to report the group sizes alongside the figure.
    
#save it
    dev.copy(
      device = png,
      filename = file.path(
        chart_folder,
        "Figure_6_Transaction_Count_By_Status.png"
      ),
      width = 1800,
      height = 1200,
      res = 180
    )
    
    dev.off()
    
#Visualize transaction amounts
    boxplot(
      list(
        Existing = existing_customers$Total_Trans_Amt,
        Attrited = attrited_customers$Total_Trans_Amt
      ),
      col = c("#26718B", "#CC6230"),
      main = "Transaction amounts by recorded attrition status",
      xlab = "Customer status",
      ylab = "Recorded transaction amount (supplied monetary units)",
      las = 1
    )
    
#Expected medians:
    #- Existing customers: 4,100
    #- Attrited customers: 2,329
#Keep the upper-tail observations visible. They are part of the observed distribution.
    
#save it
    dev.copy(
      device = png,
      filename = file.path(
        chart_folder,
        "Figure_7_Transaction_Amount_By_Status.png"
      ),
      width = 1800,
      height = 1200,
      res = 180
    )
    
    dev.off()
   
#7. Interpret the comparisons carefully 
#Do not call these differences “statistically significant” yet. Task 3 establishes descriptive patterns. Formal inference belongs in the later analysis.
 
#Correct Figure 7’s formatting
    png(
      filename = file.path(
        chart_folder,
        "Figure_7_Transaction_Amount_By_Status.png"
      ),
      width = 1800,
      height = 1200,
      res = 180
    )
    
    # Margins: bottom, left, top, right
    par(mar = c(5, 7, 4, 2))
    
    boxplot(
      list(
        Existing = existing_customers$Total_Trans_Amt,
        Attrited = attrited_customers$Total_Trans_Amt
      ),
      col = c("#26718B", "#CC6230"),
      main = "Transaction amounts by recorded attrition status",
      xlab = "Customer status",
      ylab = "",
      las = 1
    )
    
    mtext(
      "Transaction amount (supplied units)",
      side = 2,
      line = 5
    )
    
    dev.off()
    
#Next: attrition percentages within categories
#Start with a card-category cross-tabulation
    card_status_table <- table(
      Card_Category = bank$Card_Category,
      Attrition_Status = bank$Attrition_Flag
    )
    
    card_status_table
    
#Calculate percentages within rows:
    card_status_percentages <- prop.table(
      card_status_table,
      margin = 1
    ) * 100
    
    round(card_status_percentages, 2)
    
#For Platinum: (5/ 5 +15) * 100 = 25%
#That means 25% of the 20 Platinum customers are attrited. It does not mean 25% of all attrited customers hold Platinum cards.

#Build a reusable category-comparison function
    make_attrition_table <- function(variable_name) {
      
      counts <- table(
        bank[[variable_name]],
        bank$Attrition_Flag
      )
      
      totals <- rowSums(counts)
      
      data.frame(
        Variable = variable_name,
        Category = rownames(counts),
        Total_Customers = as.numeric(totals),
        Existing_Customers = as.numeric(
          counts[, "Existing Customer"]
        ),
        Attrited_Customers = as.numeric(
          counts[, "Attrited Customer"]
        ),
        Attrition_Percent = as.numeric(
          counts[, "Attrited Customer"] / totals * 100
        ),
        row.names = NULL
      )
    }
    
#This function:
  #1. Counts customers by category and status.
  #2. Calculates each category’s total.
  #3. Divides its attrited count by that total.
  #4. Returns a readable table.
#The explicit column names prevent us from accidentally extracting the existing-customer percentage instead.
    
#test
    card_attrition <- make_attrition_table("Card_Category")
    
    card_attrition
    
#Interpretation: Platinum has the highest observed percentage, but only 20 records. One customer’s outcome changes that percentage by five percentage points. Do not interpret this as strong evidence that Platinum customers generally have the greatest attrition risk.

#4. Apply the function to all categorical predictors and selected counts
    category_comparison_variables <- c(
      categorical_columns,
      "Months_Inactive_12_mon",
      "Contacts_Count_12_mon",
      "Total_Relationship_Count"
    )
    
    attrition_tables <- lapply(
      category_comparison_variables,
      make_attrition_table
    )
    
    category_attrition_summary <- do.call(
      rbind,
      attrition_tables
    )
    
    rownames(category_attrition_summary) <- NULL
    
#Create a display copy
    category_attrition_display <- category_attrition_summary
    
    category_attrition_display$Attrition_Percent <- round(
      category_attrition_display$Attrition_Percent,
      2
    )
    
    View(category_attrition_display)
    
#Treating each observed count as a group here does not change the variable’s type in bank. It simply lets us inspect whether attrition varies across its values without assuming a straight-line relationship.

#5. Check the totals
    aggregate(
      cbind(
        Total_Customers,
        Existing_Customers,
        Attrited_Customers
      ) ~ Variable,
      data = category_attrition_summary,
      FUN = sum
    )
    
#For each of the eight variables, expect:
    #- Total customers: 10,127
    #- Existing customers: 8,500
    #- Attrited customers: 1,627
    #Do not sum the category attrition percentages. Each uses a different denominator, so their sum has no useful interpretation.
    
#6. Examine the contact-count results
    contact_attrition <- make_attrition_table(
      "Contacts_Count_12_mon"
    )
    
    contact_attrition
    
#This is an important finding to investigate, not a retention rule.

#All 54 customers with six recorded contacts are attrited in this file. We do not know whether those contacts occurred before departure, during closure, or for another reason. Therefore, do not conclude that contacts cause attrition or recommend reducing customer contact.
    
#7. Examine inactivity and relationship count
    inactivity_attrition <- make_attrition_table(
      "Months_Inactive_12_mon"
    )
    
    relationship_attrition <- make_attrition_table(
      "Total_Relationship_Count"
    )
    
    inactivity_attrition
    relationship_attrition
    
#Look for these patterns:
    #- Inactivity is not uniformly increasing: recorded attrition rises from 4.48% at one inactive month to 29.89% at four, but falls at five and six.
    #- Zero inactive months is unusual: this group has 29 customers, including 15 attrited customers—51.72%. Its small size and unexpected pattern warrant investigation.
    #- Relationship count: attrition is 25.60% for one relationship, 27.84% for two and 10.50% for six.
#These comparisons are unadjusted. They do not establish that additional products prevent attrition or that each additional inactive month increases risk.
    
#Save the complete comparison table
    write.csv(
      category_attrition_summary,
      file = file.path(
        output_folder,
        "Attrition_By_Category.csv"
      ),
      row.names = FALSE
    )
    
#plot the category-specific attrition percentages
    #create three figures showing:
    #- Contact count.
    #- Inactive months.
    #- Relationship count.
    #These are useful because they reveal patterns that group averages can conceal.
#1. Calculate the overall reference percentage
    overall_attrition_percent <- mean(
      bank$Attrition_Flag == "Attrited Customer"
    ) * 100
    
    overall_attrition_percent
    
#We will draw a dashed horizontal line at this percentage. It provides a sample-wide reference—not a statistical significance threshold.
    
#Write one plotting function
    
    plot_attrition_percentages <- function(
    summary_table,
    plot_title,
    x_axis_label
    ) {
      
      bar_positions <- barplot(
        height = summary_table$Attrition_Percent,
        
        names.arg = paste0(
          summary_table$Category,
          "\n(n=",
          format(
            summary_table$Total_Customers,
            big.mark = ",",
            trim = TRUE
          ),
          ")"
        ),
        
        col = "#26718B",
        border = NA,
        ylim = c(0, 110),
        yaxt = "n",
        main = plot_title,
        xlab = x_axis_label,
        ylab = "Recorded attrition (%)",
        las = 1,
        cex.names = 0.85
      )
      
      # Percentage axis, labelled from 0 to 100
      axis(
        side = 2,
        at = seq(0, 100, by = 20),
        las = 1
      )
      
      # Overall attrition percentage
      abline(
        h = overall_attrition_percent,
        col = "#CC6230",
        lty = 2,
        lwd = 2
      )
      
      # Percentage above each bar
      text(
        x = bar_positions,
        y = summary_table$Attrition_Percent,
        labels = sprintf(
          "%.2f%%",
          summary_table$Attrition_Percent
        ),
        pos = 3,
        cex = 0.85
      )
      
      legend(
        "topleft",
        legend = sprintf(
          "Overall recorded attrition: %.2f%%",
          overall_attrition_percent
        ),
        col = "#CC6230",
        lty = 2,
        lwd = 2,
        bty = "n",
        cex = 0.85
      )
    }
    
#Draw the contact-count chart
    plot_attrition_percentages(
      summary_table = contact_attrition,
      plot_title = "Recorded attrition by contact count",
      x_axis_label = "Recorded contacts in the 12-month window"
    )
    
#save it
    png(
      filename = file.path(
        chart_folder,
        "Figure_8_Attrition_By_Contacts.png"
      ),
      width = 1800,
      height = 1200,
      res = 180
    )
    
    par(mar = c(6, 5, 4, 2))
    
    plot_attrition_percentages(
      summary_table = contact_attrition,
      plot_title = "Recorded attrition by contact count",
      x_axis_label = "Recorded contacts in the 12-month window"
    )
    
    dev.off()
    
#Interpretation: the observed attrition percentage increases across contact-count categories, reaching 100% among the 54 customers with six contacts.
#This warrants investigating contact timing and purpose. It does not support reducing contact with customers: the contacts might have occurred during attempts to resolve problems or close accounts.
    
#Draw and save the inactivity chart
    
    plot_attrition_percentages(
      summary_table = inactivity_attrition,
      plot_title = "Recorded attrition by inactive months",
      x_axis_label = "Inactive months in the 12-month window"
    )
    
    png(
      filename = file.path(
        chart_folder,
        "Figure_9_Attrition_By_Inactivity.png"
      ),
      width = 1800,
      height = 1200,
      res = 180
    )
    
    par(mar = c(6, 5, 4, 2))
    
    plot_attrition_percentages(
      summary_table = inactivity_attrition,
      plot_title = "Recorded attrition by inactive months",
      x_axis_label = "Inactive months in the 12-month window"
    )
    
    dev.off()
    
#Interpretation: the pattern is not consistently increasing. Attrition rises between one and four inactive months, then falls at five and six.
#The zero-month category is especially important: 15 of 29 customers are attrited. Its small size and unexpected pattern need investigation. Do not remove it simply because it contradicts an anticipated trend.

#Draw and save the relationship-count chart
    plot_attrition_percentages(
      summary_table = relationship_attrition,
      plot_title = "Recorded attrition by relationship count",
      x_axis_label = "Recorded relationship count"
    )
    
    png(
      filename = file.path(
        chart_folder,
        "Figure_10_Attrition_By_Relationships.png"
      ),
      width = 1800,
      height = 1200,
      res = 180
    )
    
    par(mar = c(6, 5, 4, 2))
    
    plot_attrition_percentages(
      summary_table = relationship_attrition,
      plot_title = "Recorded attrition by relationship count",
      x_axis_label = "Recorded relationship count"
    )
    
    dev.off()
    
#Interpretation: customers with one or two relationships have higher recorded attrition percentages than those with four to six.
#This supports further investigation of relationship breadth. It does not establish that selling another product would prevent attrition: customer selection, other characteristics and timing could explain the association.
    
#Confirm that all three files exist
    file.exists(
      file.path(
        chart_folder,
        c(
          "Figure_8_Attrition_By_Contacts.png",
          "Figure_9_Attrition_By_Inactivity.png",
          "Figure_10_Attrition_By_Relationships.png"
        )
      )
    )
    
#Next is correlation analysis between numeric predictors. This is the final analytical check we planned before assembling Task 3.
#Why examine correlations?
    #We want to identify variables that contain overlapping information.
    #For example:
      #- Are transaction amount and transaction count closely related?
      #- Are customer age and relationship tenure related?
      #- How strongly are the financial measures associated?
      #This is not a test of which variables cause attrition. We are examining relationships between predictors, not ranking their importance for predicting churn.
    #We’ll use Spearman correlation because several variables are skewed and some are counts. It measures whether higher values of one variable generally accompany higher or lower values of another, using ranks.
    #Pearson correlation would answer a different question about linear association. Skewness does not automatically invalidate Pearson, but Spearman is a reasonable descriptive choice here.
    
# Calculate the correlation matrix
# Confirm that the selected numeric predictors have no missing values
    sum(is.na(bank[numeric_columns]))   
    
#calculate
    spearman_matrix <- cor(
      bank[numeric_columns],
      method = "spearman",
      use = "everything"
    )
    
    round(spearman_matrix, 3)
    
#For easier viewing:
    View(
      as.data.frame(
        round(spearman_matrix, 3)
      )
    )
    
#How to read the matrix
    #Value	             Meaning
    #Close to +1	  Higher values of one variable generally accompany higher values of the other
    #Close to −1	  Higher values of one generally accompany lower values of the other
    #Close to 0	    Little monotonic association; other forms of relationship may still exist
    
#The diagonal contains 1 because every variable is perfectly correlated with itself. The matrix is symmetric, so each pair appears twice.

#Inspect three useful pairs
    spearman_matrix[
      "Customer_Age",
      "Months_on_book"
    ]
    
    spearman_matrix[
      "Total_Trans_Amt",
      "Total_Trans_Ct"
    ]
    
    spearman_matrix[
      "Credit_Limit",
      "Avg_Open_To_Buy"
    ]
    
#Interpretation
    #- Age and tenure: older customers tend to have longer recorded relationships in this sample. This does not establish that age determines loyalty.
    #- Transaction amount and count: customers with more transactions generally have higher total transaction amounts. The variables describe related aspects of activity.
    #- Credit limit and available credit: their strong association is consistent with the financial identity you already verified.
#Do not automatically delete variables because their correlation is high. Retention or exclusion depends on meaning, redundancy and the intended model.
    
#Also, correlation alone does not capture every dependency. You already established:
    #Avg_Open_To_Buy = Credit_Limit - Total_Revolving_Bal
#That exact relationship involves three variables and remains important even though neither pairwise correlation has to equal 1.
 
#Create a readable table of highly correlated pairs
    #Instead of interpreting all 196 matrix cells, identify pairs with absolute correlation of at least 0.70:   
    
    # Descriptive screening threshold, not a significance cutoff
    correlation_threshold <- 0.70
    
    pair_positions <- which(
      upper.tri(spearman_matrix) &
        abs(spearman_matrix) >= correlation_threshold,
      arr.ind = TRUE
    )
    
#Build the table:
    strong_correlations <- data.frame(
      Variable_1 = rownames(spearman_matrix)[
        pair_positions[, 1]
      ],
      
      Variable_2 = colnames(spearman_matrix)[
        pair_positions[, 2]
      ],
      
      Spearman_rho = spearman_matrix[pair_positions],
      
      row.names = NULL
    )
    
    strong_correlations <- strong_correlations[
      order(
        abs(strong_correlations$Spearman_rho),
        decreasing = TRUE
      ),
    ]
    
    rownames(strong_correlations) <- NULL
    
    strong_correlations
    
#Why 0.70? It is a practical screening threshold for focusing the discussion. It is not a universal boundary, a significance test or an automatic variable-removal rule.

#Inspect one relationship visually
#A scatterplot helps reveal structure that a single correlation number can conceal.
    
    plot(
      x = bank$Total_Trans_Ct,
      y = bank$Total_Trans_Amt,
      pch = 16,
      cex = 0.45,
      col = adjustcolor("#26718B", alpha.f = 0.20),
      main = "Transaction count and transaction amount",
      xlab = "Recorded transaction count",
      ylab = "Transaction amount (supplied units)",
      las = 1
    )
    
#Each point represents a customer.
#Look for an overall increasing relationship, concentrations and departures from a simple straight line. Do not interpret visible concentrations as confirmed customer segments.
    
#save figure
    png(
      filename = file.path(
        chart_folder,
        "Figure_11_Transaction_Count_vs_Amount.png"
      ),
      width = 1800,
      height = 1200,
      res = 180
    )
    
    par(mar = c(5, 7, 4, 2))
    
    plot(
      x = bank$Total_Trans_Ct,
      y = bank$Total_Trans_Amt,
      pch = 16,
      cex = 0.45,
      col = adjustcolor("#26718B", alpha.f = 0.20),
      main = "Transaction count and transaction amount",
      xlab = "Recorded transaction count",
      ylab = "",
      las = 1
    )
    
    mtext(
      "Transaction amount (supplied units)",
      side = 2,
      line = 5
    )
    
    dev.off()
    
#save correlation results
    write.csv(
      spearman_matrix,
      file = file.path(
        output_folder,
        "Spearman_Correlation_Matrix.csv"
      ),
      row.names = TRUE
    )
    
    write.csv(
      strong_correlations,
      file = file.path(
        output_folder,
        "Strong_Predictor_Correlations.csv"
      ),
      row.names = FALSE
    )
    
#For the matrix, we deliberately retain row names because they identify the variables corresponding to each row.
    
#Save the quality-check tables
    
    # Save the missing-information summary
    write.csv(
      unknown_summary,
      file = file.path(
        output_folder,
        "Unknown_Category_Summary.csv"
      ),
      row.names = FALSE
    )
    
    # Save observed ranges
    write.csv(
      range_summary,
      file = file.path(
        output_folder,
        "Numeric_Ranges.csv"
      ),
      row.names = FALSE
    )
    
    # Save logical-range checks
    write.csv(
      range_checks,
      file = file.path(
        output_folder,
        "Logical_Range_Checks.csv"
      ),
      row.names = FALSE
    )

#Create one compact audit summary:
    
    quality_summary <- data.frame(
      Check = c(
        "Customer records",
        "Original columns",
        "Exact duplicate rows",
        "Duplicate customer identifiers",
        "Missing cells recognised by R",
        "Customers with at least one Unknown category",
        "Customers with at least one IQR outlier flag"
      ),
      
      Result = c(
        nrow(bank),
        ncol(bank),
        sum(duplicated(bank)),
        sum(duplicated(bank$CLIENTNUM)),
        sum(is.na(bank)),
        sum(has_unknown),
        sum(has_outlier_flag)
      )
    )
    
    quality_summary
#save it
    write.csv(
      quality_summary,
      file = file.path(
        output_folder,
        "Data_Quality_Summary.csv"
      ),
      row.names = FALSE
    )
    
#Record the preparation decisions
#A preparation record explains what you decided and why. It is different from silently changing the data.
    
    preparation_decisions <- data.frame(
      Issue = c(
        "Customer identifier",
        "Supplied Naive Bayes outputs",
        "Unknown demographic categories",
        "IQR outlier flags",
        "Available-credit redundancy",
        "Small category sizes",
        "Predictor and outcome timing"
      ),
      
      Decision = c(
        "Retain for traceability; exclude from model predictors.",
        "Exclude from model predictors.",
        "Retain as explicit categories in descriptive analysis.",
        "Retain observations; investigate rather than automatically delete.",
        "Avoid including available credit with both credit limit and revolving balance as ordinary linear predictors.",
        "Report denominators; do not automatically merge or remove categories.",
        "Describe recorded attrition; do not claim validated advance warning."
      ),
      
      Reason = c(
        "Identifier magnitude has no established customer-behaviour meaning.",
        "These are model outputs rather than original customer characteristics.",
        "Replacing or deleting unknown information requires additional justification.",
        "Statistical flags are not proof of erroneous records.",
        "Available credit equals credit limit minus revolving balance.",
        "Percentages based on small samples can be unstable.",
        "The observation-to-outcome sequence is not verified."
      )
    )
    
    View(preparation_decisions)
    
    write.csv(
      preparation_decisions,
      file = file.path(
        output_folder,
        "Preparation_Decisions.csv"
      ),
      row.names = FALSE
    )
    
#This table records decisions. It does not delete columns, remove customers or impute values.
#Save the R environment information
    capture.output(
      sessionInfo(),
      file = file.path(
        output_folder,
        "R_Session_Information.txt"
      )
    )
    
#This records the R version and loaded packages. It helps someone reproduce your work.


# Task 4: Statistical Inference
    
    alpha <- 0.05
    
    task4_folder <- file.path(
      dirname(data_path),
      "Task4_Manual_R_Outputs"
    )
    
    dir.create(
      task4_folder,
      showWarnings = FALSE,
      recursive = TRUE
    )
    
#Here, alpha is the planned significance threshold. It is not the probability that a particular conclusion is wrong.
    
#Select the observations
    
    existing_transactions <- bank$Total_Trans_Ct[
      bank$Attrition_Flag == "Existing Customer"
    ]
    
    attrited_transactions <- bank$Total_Trans_Ct[
      bank$Attrition_Flag == "Attrited Customer"
    ]
    
#Check the sample sizes and missing values:
    
    length(existing_transactions)
    length(attrited_transactions)
    
    sum(is.na(existing_transactions))
    sum(is.na(attrited_transactions))
    
#Reconfirm the descriptive evidence
    
    mean_test_summary <- data.frame(
      Group = c("Existing", "Attrited"),
      
      N = c(
        length(existing_transactions),
        length(attrited_transactions)
      ),
      
      Mean = c(
        mean(existing_transactions),
        mean(attrited_transactions)
      ),
      
      SD = c(
        sd(existing_transactions),
        sd(attrited_transactions)
      )
    )
    
    mean_test_summary
    
#Calculate their difference:
    mean_difference <- mean(existing_transactions) -
      mean(attrited_transactions)
    
    mean_difference
    
#This is already an interpretable effect estimate in the original units.
    
#Run Welch’s t-test
    transaction_mean_test <- t.test(
      x = existing_transactions,
      y = attrited_transactions,
      alternative = "two.sided",
      mu = 0,
      var.equal = FALSE,
      conf.level = 0.95
    )
    
    transaction_mean_test
    
#a small p-value does not establish that reduced activity causes departure or that increasing activity would retain customers.
#5. Save the results
    mean_test_results <- data.frame(
      Comparison = "Transaction count: Existing minus Attrited",
      Mean_Difference = mean_difference,
      CI_Lower = transaction_mean_test$conf.int[1],
      CI_Upper = transaction_mean_test$conf.int[2],
      t_statistic = unname(transaction_mean_test$statistic),
      Degrees_of_Freedom = unname(transaction_mean_test$parameter),
      Raw_p_value = transaction_mean_test$p.value
    )
    
    mean_test_results
    
#save both tables
    write.csv(
      mean_test_summary,
      file = file.path(
        task4_folder,
        "Transaction_Count_Group_Summary.csv"
      ),
      row.names = FALSE
    )
    
    write.csv(
      mean_test_results,
      file = file.path(
        task4_folder,
        "Welch_Transaction_Count_Test.csv"
      ),
      row.names = FALSE
    )
    
#The interval is an individual 95% interval, not a simultaneous interval adjusted for all four analyses. We will label it accordingly and calculate the Holm-adjusted p-values once the planned tests are complete.
    
#Correct the p-value reporting
#Your exported table shows Raw_p_value = 0. Here, the calculation is so extreme that the ordinary numerical representation underflows to zero—not merely a rounding issue in the printed table.
#Do not report “p = 0.” Add a reporting column:
    
    mean_test_results$Reported_p_value <- format.pval(
      transaction_mean_test$p.value,
      digits = 3,
      eps = 2.2e-16
    )
    
    mean_test_results
    
#save updated table
    write.csv(
      mean_test_results,
      file = file.path(
        task4_folder,
        "Welch_Transaction_Count_Test.csv"
      ),
      row.names = FALSE
    )
    
#For later numerical work, we can also retain the log p-value without underflow:
    mean_test_log_p <- log(2) + pt(
      q = -abs(unname(transaction_mean_test$statistic)),
      df = unname(transaction_mean_test$parameter),
      log.p = TRUE
    )
    
    mean_test_log_p
    
#3. Prepare the counts in R
# Task 4: Comparison of two proportions
    
    gender_status_table <- table(
      Gender = bank$Gender,
      Status = bank$Attrition_Flag
    )
    
    gender_status_table
    
#Extract the number attrited and the total in each group:
    gender_order <- c("F", "M")
    
    gender_attrited <- gender_status_table[
      gender_order,
      "Attrited Customer"
    ]
    
    gender_totals <- rowSums(
      gender_status_table
    )[gender_order]
    
    gender_attrited
    gender_totals
    
#Calculate the observed proportions:
    gender_proportions <- gender_attrited / gender_totals
    
    round(gender_proportions * 100, 2)
    
#Calculate the difference in percentage points:
    
    gender_difference_pp <- (
      gender_proportions["F"] -
        gender_proportions["M"]
    ) * 100
    
    gender_difference_pp
    
#This is not the same as a 2.74% relative increase.
    
#4. Run the proportion test
    gender_proportion_test <- prop.test(
      x = gender_attrited,
      n = gender_totals,
      alternative = "two.sided",
      conf.level = 0.95,
      correct = FALSE
    )
    
    gender_proportion_test
    
#Why correct = FALSE? This uses the uncorrected large-sample test corresponding to the lecture’s pooled two-proportion z-test. With two groups, R reports a chi-squared statistic equal to \(z^2\), with the same two-sided p-value.
#The confidence interval describes female-labelled minus male-labelled attrition proportion, because we explicitly supplied that order.
    
#Connect the R result to the lecture formula
#Calculate the pooled proportion:
    pooled_proportion <- sum(gender_attrited) /
      sum(gender_totals)
    
#Calculate the standard error under the null:
    pooled_standard_error <- sqrt(
      pooled_proportion *
        (1 - pooled_proportion) *
        (
          1 / gender_totals["F"] +
            1 / gender_totals["M"]
        )
    )
    
#Calculate the z-statistic and two-sided p-value:
    gender_z <- (
      gender_proportions["F"] -
        gender_proportions["M"]
    ) / pooled_standard_error
    
    gender_z
    
    gender_p_manual <- 2 * pnorm(
      -abs(gender_z)
    )
    
    gender_p_manual
    
#Check the equivalence:
    c(
      Z_squared = unname(gender_z^2),
      R_chi_squared = unname(
        gender_proportion_test$statistic
      )
    )
    
    c(
      Manual_p = unname(gender_p_manual),
      R_p = gender_proportion_test$p.value
    )
    
#The corresponding values should agree apart from numerical precision. These are two calculations of the same test, not separate tests to count in the multiple-testing adjustment.
    
#save the result
    proportion_test_results <- data.frame(
      Comparison = "Recorded attrition: F minus M",
      F_Total = unname(gender_totals["F"]),
      M_Total = unname(gender_totals["M"]),
      F_Attrited = unname(gender_attrited["F"]),
      M_Attrited = unname(gender_attrited["M"]),
      Difference_Percentage_Points = unname(
        gender_difference_pp
      ),
      CI_Lower_Percentage_Points =
        gender_proportion_test$conf.int[1] * 100,
      CI_Upper_Percentage_Points =
        gender_proportion_test$conf.int[2] * 100,
      Z_statistic = unname(gender_z),
      Raw_p_value = gender_proportion_test$p.value
    )
    
    proportion_test_results
    
    write.csv(
      proportion_test_results,
      file = file.path(
        task4_folder,
        "Gender_Attrition_Proportion_Test.csv"
      ),
      row.names = FALSE
    )
    
#At the individual 5% level, there is evidence of an unadjusted difference under the test assumptions. We will include this p-value in the planned Holm adjustment later.
#Organizationally: this identifies a disparity worth investigating alongside other customer characteristics. It does not explain the difference, establish discrimination or justify gender-based targeting.
    
#Next: compare variability
#Our third question is:
#Does recorded transaction amount have different variability among existing and attrited customers?

#We’ll first calculate the variances and inspect the distributions.
#Your lecture’s classical F-test assumes normal populations. The large sample sizes do not make that variance test automatically robust to the skewness already seen in Task 3.

#1. Select transaction amounts
    # Task 4: Comparison of variability
    
    existing_amounts <- bank$Total_Trans_Amt[
      bank$Attrition_Flag == "Existing Customer"
    ]
    
    attrited_amounts <- bank$Total_Trans_Amt[
      bank$Attrition_Flag == "Attrited Customer"
    ]
    
#Confirm the available observations:
    length(existing_amounts)
    length(attrited_amounts)
    
    sum(is.na(existing_amounts))
    sum(is.na(attrited_amounts))
    
#2. Calculate several measures of spread
    spread_summary <- data.frame(
      Group = c("Existing", "Attrited"),
      
      N = c(
        length(existing_amounts),
        length(attrited_amounts)
      ),
      
      Mean = c(
        mean(existing_amounts),
        mean(attrited_amounts)
      ),
      
      Variance = c(
        var(existing_amounts),
        var(attrited_amounts)
      ),
      
      SD = c(
        sd(existing_amounts),
        sd(attrited_amounts)
      ),
      
      IQR = c(
        IQR(existing_amounts, type = 7),
        IQR(attrited_amounts, type = 7)
      )
    )
    
    spread_summary
    
#Variance has squared monetary units, so SD and IQR are usually easier to explain to management.

#Calculate the sample variance ratio:
    sample_variance_ratio <- var(existing_amounts) /
      var(attrited_amounts)
    
    sample_variance_ratio
    
#Interpret it as:
  #- Above 1: existing customers have the larger sample variance.
  #- Below 1: attrited customers have the larger sample variance.
  #- Close to 1: similar sample variances.
  #This ratio is descriptive. It does not yet establish statistical significance.

#3. Inspect normal Q–Q plots
#A normal Q–Q plot compares the observed distribution with the shape expected under normality.
    
#Create a reusable plotting function:
    plot_amount_qq <- function() {
      
      old_settings <- par(
        mfrow = c(1, 2),
        mar = c(5, 5, 4, 2)
      )
      
      on.exit(par(old_settings))
      
      qqnorm(
        existing_amounts,
        main = "Existing customers",
        xlab = "Theoretical normal quantiles",
        ylab = "Observed transaction amount",
        pch = 16,
        cex = 0.4,
        col = adjustcolor("#26718B", alpha.f = 0.3)
      )
      
      qqline(
        existing_amounts,
        col = "#CC6230",
        lwd = 2
      )
      
      qqnorm(
        attrited_amounts,
        main = "Attrited customers",
        xlab = "Theoretical normal quantiles",
        ylab = "Observed transaction amount",
        pch = 16,
        cex = 0.4,
        col = adjustcolor("#26718B", alpha.f = 0.3)
      )
      
      qqline(
        attrited_amounts,
        col = "#CC6230",
        lwd = 2
      )
    }
    
#display the plots
    plot_amount_qq()

#4. Save the diagnostic plots and table
    png(
      filename = file.path(
        task4_folder,
        "Transaction_Amount_Normal_QQ_Plots.png"
      ),
      width = 2400,
      height = 1200,
      res = 180
    )
    
    plot_amount_qq()
    
    dev.off()
    
    write.csv(
      spread_summary,
      file = file.path(
        task4_folder,
        "Transaction_Amount_Spread_Summary.csv"
      ),
      row.names = FALSE
    )
    
#Why use a robust test here?
#Your Q–Q plots show substantial departures from normality. The classical two-sample variance F-test covered in your lectures relies on normally distributed populations, so it is unsuitable as our primary test here.
#We will use the median-centred Levene test, also called the Brown–Forsythe test for spread. This is an extension beyond the classical lecture method. NIST recommends the median-based version for robustness against many forms of nonnormality

#The method compares customers’ absolute distances from their own group’s median. Because your groups also differ in distributional shape, we will interpret it as evidence about spread, rather than an assumption-free test of the variance ratio.

#2. Calculate distances from each group’s median
    # Task 4: Robust comparison of transaction-amount spread
    
    existing_deviations <- abs(
      existing_amounts - median(existing_amounts)
    )
    
    attrited_deviations <- abs(
      attrited_amounts - median(attrited_amounts)
    )
    
    # Combine the deviations and their group labels
    spread_data <- data.frame(
      Absolute_Deviation = c(
        existing_deviations,
        attrited_deviations
      ),
      
      Status = factor(
        c(
          rep("Existing", length(existing_deviations)),
          rep("Attrited", length(attrited_deviations))
        ),
        levels = c("Existing", "Attrited")
      )
    )
    
    # Compare average distances from the group medians
    aggregate(
      Absolute_Deviation ~ Status,
      data = spread_data,
      FUN = mean
    )
    
#3. Run the Brown–Forsythe test (median-centred Levene test)
#The standard calculation applies one-way ANOVA to those absolute deviations:
    
    spread_model <- aov(
      Absolute_Deviation ~ Status,
      data = spread_data
    )
    
    spread_test_table <- summary(spread_model)[[1]]
    
    spread_test_table
    
#4. Extract and save the result
    
    spread_test_results <- data.frame(
      Test = "Median-centred Levene (Brown-Forsythe)",
      
      Variable = "Total_Trans_Amt",
      
      F_statistic = unname(
        spread_test_table[1, "F value"]
      ),
      
      Numerator_df = unname(
        spread_test_table[1, "Df"]
      ),
      
      Denominator_df = unname(
        spread_test_table[2, "Df"]
      ),
      
      P_value = unname(
        spread_test_table[1, "Pr(>F)"]
      ),
      
      Existing_mean_absolute_deviation = mean(
        existing_deviations
      ),
      
      Attrited_mean_absolute_deviation = mean(
        attrited_deviations
      ),
      
      Sample_variance_ratio_existing_to_attrited =
        sample_variance_ratio
    )
    
    spread_test_results$Reported_p_value <- format.pval(
      spread_test_results$P_value,
      digits = 4,
      eps = 2.2e-16
    )
    
    print(spread_test_results, row.names = FALSE)
    
    write.csv(
      spread_test_results,
      file = file.path(
        task4_folder,
        "Transaction_Amount_Brown_Forsythe_Test.csv"
      ),
      row.names = FALSE
    )
    
#final planned analysis: Welch ANOVA comparing transaction counts across the six relationship-count groups.
#Next: compare transaction counts across relationship-count groups
    # Task 4: Welch ANOVA
    # Transaction count by number of product relationships
    #prepare data and inspect the groups
    
    relationship_data <- data.frame(
      Transaction_Count = bank$Total_Trans_Ct,
      
      Relationship_Group = factor(
        bank$Total_Relationship_Count,
        levels = 1:6
      )
    )
    
    # Check for missing values
    colSums(is.na(relationship_data))
    
    # Check the number of customers in each group
    table(relationship_data$Relationship_Group)
    
#Treating relationship count as a factor tells R to compare six group means. Using it as a numerical predictor in a regression would answer a different question about a specified trend.

#3. Produce the group summary
    relationship_groups <- split(
      relationship_data$Transaction_Count,
      relationship_data$Relationship_Group
    )
    
    relationship_summary <- do.call(
      rbind,
      lapply(names(relationship_groups), function(group_name) {
        
        x <- relationship_groups[[group_name]]
        
        data.frame(
          Relationship_Count = group_name,
          N = length(x),
          Mean = mean(x),
          SD = sd(x),
          Median = median(x),
          IQR = IQR(x)
        )
      })
    )
    
    rownames(relationship_summary) <- NULL
    
    print(relationship_summary, digits = 5)
    
    write.csv(
      relationship_summary,
      file = file.path(
        task4_folder,
        "Transaction_Count_By_Relationship_Summary.csv"
      ),
      row.names = FALSE
    )
    
#The means answer the ANOVA question. The medians, SDs, and IQRs help explain whether differences in averages coexist with substantial variation within groups.
#Run Welch ANOVA
    relationship_anova <- oneway.test(
      Transaction_Count ~ Relationship_Group,
      data = relationship_data,
      var.equal = FALSE
    )
    
    relationship_anova
    
#This method still assumes independent observations and requires care with extreme distributions. It does not resolve the dataset’s uncertain sampling or establish causality

#save the result
    relationship_anova_results <- data.frame(
      Test = "Welch one-way ANOVA",
      
      Outcome = "Total_Trans_Ct",
      
      Grouping_variable = "Total_Relationship_Count",
      
      F_statistic = unname(
        relationship_anova$statistic
      ),
      
      Numerator_df = unname(
        relationship_anova$parameter[1]
      ),
      
      Denominator_df = unname(
        relationship_anova$parameter[2]
      ),
      
      P_value = relationship_anova$p.value
    )
    
    print(relationship_anova_results, row.names = FALSE)
    
    write.csv(
      relationship_anova_results,
      file = file.path(
        task4_folder,
        "Welch_ANOVA_Transaction_Count_By_Relationship.csv"
      ),
      row.names = FALSE
    )
    
#All four planned primary tests are now complete. Next, we will adjust their p-values together and save your consolidated Task 4 results.
#ANOVA shows : This provides strong evidence that at least one relationship-count group has a different mean transaction count.

#Next: apply the Holm adjustment
#We conducted four primary tests. Testing each separately at 5% increases the opportunity for at least one false-positive conclusion across the set. The Holm procedure controls this family-wide error rate, subject to the validity of the individual tests.
#It adjusts p-values, not means, confidence intervals, or effect sizes. It also does not remove the exploratory nature of questions informed by earlier data inspection.
    
#collect 4 p-values
    # Task 4: Combine the four primary tests
    
    primary_p_values <- c(
      Mean_transaction_count =
        transaction_mean_test$p.value,
      
      Attrition_proportion_by_gender =
        gender_proportion_test$p.value,
      
      Transaction_amount_spread =
        spread_test_results$P_value,
      
      Transaction_count_by_relationship =
        relationship_anova$p.value
    )
    
    primary_p_values
    
#Your transaction-count t-test may display 0. As discussed earlier, that is numerical underflow, not a genuinely zero p-value.
#2. Adjust the p-values and build the table
    holm_p_values <- p.adjust(
      primary_p_values,
      method = "holm"
    )
    
    task4_results <- data.frame(
      Analysis = c(
        "Mean transaction count by attrition status",
        "Attrition proportion by recorded gender",
        "Transaction amount spread by attrition status",
        "Mean transaction count by relationship count"
      ),
      
      Method = c(
        "Welch independent-samples t-test",
        "Two-sample proportion test, no continuity correction",
        "Median-centred Levene / Brown-Forsythe",
        "Welch one-way ANOVA"
      ),
      
      Raw_p_value = unname(primary_p_values),
      
      Holm_adjusted_p_value = unname(holm_p_values),
      
      stringsAsFactors = FALSE
    )
    
    task4_results$Decision_at_5_percent <- ifelse(
      task4_results$Holm_adjusted_p_value < 0.05,
      "Reject H0",
      "Do not reject H0"
    )
    
#3. Add readable p-values
#We will retain the numerical columns for reproducibility and add display columns that avoid reporting an underflowed value as zero.
    
    format_p_value <- function(p) {
      if (p == 0) {
        return("< 2.2e-16 (numerical underflow)")
      }
      
      formatC(
        p,
        format = "e",
        digits = 4
      )
    }
    
    task4_results$Raw_p_display <- vapply(
      task4_results$Raw_p_value,
      format_p_value,
      character(1)
    )
    
    task4_results$Holm_p_display <- vapply(
      task4_results$Holm_adjusted_p_value,
      format_p_value,
      character(1)
    )
    
    print(
      task4_results[
        c(
          "Analysis",
          "Raw_p_display",
          "Holm_p_display",
          "Decision_at_5_percent"
        )
      ],
      row.names = FALSE
    )
    
#All four should remain statistically significant. The proportion-test p-value remains unchanged because it is the largest p-value in this family; that is expected under Holm’s procedure.

#save the consolidated table
    write.csv(
      task4_results,
      file = file.path(
        task4_folder,
        "Task4_Primary_Tests_Holm_Adjusted.csv"
      ),
      row.names = FALSE
    )
    
    # Record the R environment used for reproducibility
    capture.output(
      sessionInfo(),
      file = file.path(
        task4_folder,
        "Task4_R_Session_Info.txt"
      )
    )
    
#create task 5 folder and outcome
    # Task 5: Predictive Statistical Modelling
    
    task5_folder <- file.path(
      dirname(data_path),
      "Task5_Manual_R_Outputs"
    )
    
    dir.create(
      task5_folder,
      showWarnings = FALSE,
      recursive = TRUE
    )
    
    # Check the original outcome labels
    table(bank$Attrition_Flag, useNA = "ifany")
    
    stopifnot(
      !anyNA(bank$Attrition_Flag),
      all(
        bank$Attrition_Flag %in%
          c("Existing Customer", "Attrited Customer")
      )
    )
    
    # Create a separate modelling dataset
    model_data <- bank
    
    # Numeric binary outcome
    model_data$Churn <- ifelse(
      model_data$Attrition_Flag == "Attrited Customer",
      1L,
      0L
    )
#It creates a separate modelling dataset and leaves bank unchanged.

#Remove the identifier and supplied classifier outputs
#The identifier has no justified predictive role. The supplied Naive Bayes output columns could expose outcome-related information and are not original customer characteristics. The original text outcome must also be removed after creating Churn.
    # Identify the supplied Naive Bayes output columns
    nb_columns <- grep(
      "^Naive_Bayes",
      names(model_data),
      value = TRUE
    )
    
    nb_columns
    
    stopifnot(length(nb_columns) == 2L)
    
    excluded_columns <- c(
      "CLIENTNUM",
      "Attrition_Flag",
      nb_columns
    )
    
    model_data <- model_data[
      ,
      !names(model_data) %in% excluded_columns,
      drop = FALSE
    ]
    
    # Inspect the modelling dataset
    dim(model_data)
    names(model_data)
    
    table(model_data$Churn, useNA = "ifany")
#These are candidate predictors, not our final model specification. For example, we already know that Avg_Open_To_Buy is determined by credit limit and revolving balance. We will address that redundancy before fitting.

#Create a stratified 80/20 split
    # Reproducible stratified split
    set.seed(3081)
    
    existing_rows <- which(model_data$Churn == 0L)
    attrited_rows <- which(model_data$Churn == 1L)
    
    train_existing_rows <- sample(
      existing_rows,
      size = floor(0.80 * length(existing_rows))
    )
    
    train_attrited_rows <- sample(
      attrited_rows,
      size = floor(0.80 * length(attrited_rows))
    )
    
    train_rows <- sort(
      c(train_existing_rows, train_attrited_rows)
    )
    
    test_rows <- setdiff(
      seq_len(nrow(model_data)),
      train_rows
    )
    
    train_data <- model_data[train_rows, , drop = FALSE]
    test_data <- model_data[test_rows, , drop = FALSE]
    
    # Verify that every row belongs to exactly one subset
    stopifnot(
      length(intersect(train_rows, test_rows)) == 0L,
      nrow(train_data) + nrow(test_data) == nrow(model_data),
      !anyNA(train_data$Churn),
      !anyNA(test_data$Churn)
    )
    
#set.seed() makes this split reproducible. It is not a value to change repeatedly until performance looks better.

#check and save split
    split_summary <- data.frame(
      Dataset = c("Training", "Test"),
      
      N = c(
        nrow(train_data),
        nrow(test_data)
      ),
      
      Existing = c(
        sum(train_data$Churn == 0L),
        sum(test_data$Churn == 0L)
      ),
      
      Attrited = c(
        sum(train_data$Churn == 1L),
        sum(test_data$Churn == 1L)
      ),
      
      Attrition_Percent = 100 * c(
        mean(train_data$Churn),
        mean(test_data$Churn)
      )
    )
    
    print(split_summary, row.names = FALSE)
    
    write.csv(
      split_summary,
      file = file.path(
        task5_folder,
        "Training_Test_Split_Summary.csv"
      ),
      row.names = FALSE
    )
    
    # Save the exact row assignment for this source dataset
    split_assignment <- data.frame(
      Source_Row = seq_len(nrow(model_data)),
      Dataset = ifelse(
        seq_len(nrow(model_data)) %in% train_rows,
        "Training",
        "Test"
      )
    )
    
    write.csv(
      split_assignment,
      file = file.path(
        task5_folder,
        "Training_Test_Row_Assignment.csv"
      ),
      row.names = FALSE
    )
    
    saveRDS(
      list(
        train_data = train_data,
        test_data = test_data,
        train_rows = train_rows,
        test_rows = test_rows,
        seed = 3081
      ),
      file = file.path(
        task5_folder,
        "Task5_Data_Split.rds"
      )
    )
    
#The similar percentages confirm that stratification worked. Keep this split fixed.
    
#Next, we will prepare the training predictors and fit the first, ordinary logistic regression model. This is a baseline model, not yet our final recommendation.
    
#Make the preparation decisions explicit
#Use the initial decisions consistently across our model comparison
#Avg_Utilization_Ratio is also derived from financial variables, but its relationship is nonlinear. It does not create the same exact linear dependence as Avg_Open_To_Buy. We will initially retain it and assess stability.
    
#2. Create a training dataset for modelling
    # Task 5: Prepare training predictors
    
    train_model <- train_data[
      ,
      names(train_data) != "Avg_Open_To_Buy",
      drop = FALSE
    ]
    
    # Expected: 8,101 rows and 19 columns
    # 18 predictors plus Churn
    dim(train_model)
    
#This leaves your saved split and original data unchanged.
    
#convert categorial variables into factors
    categorical_predictors <- c(
      "Gender",
      "Education_Level",
      "Marital_Status",
      "Income_Category",
      "Card_Category"
    )
    
    for (variable in categorical_predictors) {
      train_model[[variable]] <- factor(
        train_model[[variable]]
      )
    }
    
    # Inspect training category counts
    lapply(
      train_model[categorical_predictors],
      table,
      useNA = "ifany"
    )
    
#For this baseline, education and income are treated as unordered categories. This avoids imposing equally spaced effects or assigning an artificial position to "Unknown".
    
#choose reference categories
#Each categorical coefficient compares a category against a reference category. Use explicit references so your interpretation is reproducible:
    reference_levels <- c(
      Gender = "F",
      Education_Level = "Graduate",
      Marital_Status = "Married",
      Income_Category = "Less than $40K",
      Card_Category = "Blue"
    )
    
    for (variable in names(reference_levels)) {
      
      stopifnot(
        reference_levels[[variable]] %in%
          levels(train_model[[variable]])
      )
      
      train_model[[variable]] <- relevel(
        train_model[[variable]],
        ref = reference_levels[[variable]]
      )
    }
    
    # Confirm references: the first level is the reference
    lapply(
      train_model[categorical_predictors],
      levels
    )
    
#These are interpretable comparison categories, not “best” customer categories. Changing the reference changes how coefficients are expressed; it does not change fitted probabilities.
    
#check missing values and linear dependence
    # Check missing values before fitting
    colSums(is.na(train_model))
    
    stopifnot(
      !anyNA(train_model),
      all(train_model$Churn %in% c(0L, 1L))
    )
    
    # Construct the predictor matrix
    # R creates indicator columns for categorical variables
    training_matrix <- model.matrix(
      Churn ~ .,
      data = train_model
    )
    
    matrix_check <- data.frame(
      Matrix_Columns = ncol(training_matrix),
      Matrix_Rank = qr(training_matrix)$rank
    )
    
    matrix_check
    
    stopifnot(
      matrix_check$Matrix_Rank ==
        matrix_check$Matrix_Columns
    )
    
#The matrix has more columns than the original predictor count because a categorical variable with several categories needs several indicator columns.
#Equal rank and column count indicate no exact linear dependence detected. This does not rule out strong correlations or unstable estimates; we will examine those later.
    
#fit the baseline logistic regression
    # Model 1: Ordinary binary logistic regression
    
    logistic_baseline <- glm(
      Churn ~ .,
      data = train_model,
      family = binomial(link = "logit"),
      na.action = na.fail
    )
    
    summary(logistic_baseline)
    
#A positive coefficient indicates higher estimated log-odds of recorded attrition, holding the other model terms constant. A negative coefficient indicates lower estimated log-odds.
#Do not remove variables simply because their coefficient p-values exceed 0.05. Our objective here is predictive modelling, and correlated predictors can contribute jointly.
    
#check that fitting completed successfully and save the model
    baseline_fit_check <- data.frame(
      Converged = logistic_baseline$converged,
      Iterations = logistic_baseline$iter,
      Observations_Used = nobs(logistic_baseline),
      Aliased_Coefficients = sum(
        is.na(coef(logistic_baseline))
      )
    )
    
    baseline_fit_check
    
    saveRDS(
      logistic_baseline,
      file = file.path(
        task5_folder,
        "Logistic_Baseline_Model.rds"
      )
    )
    
    capture.output(
      summary(logistic_baseline),
      file = file.path(
        task5_folder,
        "Logistic_Baseline_Summary.txt"
      )
    )
    
    write.csv(
      baseline_fit_check,
      file = file.path(
        task5_folder,
        "Logistic_Baseline_Fit_Check.csv"
      ),
      row.names = FALSE
    )
    
#Convergence means the fitting algorithm completed; it does not establish that the model is well specified or predicts well
    
#Next, we should examine sparse categories and influential observations before proceeding to cross-validation. Successful convergence alone does not establish stability or predictive performance.
#Your AIC of 3791.5 and residual deviance of 3727.5 describe training fit. They are not test accuracy or evidence that the model is suitable for deployment.

#check attrition counts within card categories
    # Task 5: Check sparse outcome-category combinations
    
    card_outcome_counts <- with(
      train_model,
      table(
        Card_Category,
        Churn
      )
    )
    
    card_outcome_counts
    
    # Within-category percentages
    round(
      100 * prop.table(card_outcome_counts, margin = 1),
      2
    ) 
    
#The first table is essential: percentages can look dramatic when based on only a few customers.
#We will not automatically merge Gold and Platinum. Combining categories needs a substantive justification, not merely a desire for smaller standard errors.
    
#create an odds-ratio table with confidence intervals
#Exponentiating a logistic coefficient produces an odds ratio. The following calculates approximate Wald 95% confidence intervals, which we will treat cautiously for sparse categories.
    # Extract coefficient estimates and standard errors
    coefficient_matrix <- summary(
      logistic_baseline
    )$coefficients
    
    coefficient_table <- data.frame(
      Term = rownames(coefficient_matrix),
      
      Estimate = coefficient_matrix[, "Estimate"],
      
      Standard_Error = coefficient_matrix[, "Std. Error"],
      
      row.names = NULL
    )
    
    coefficient_table$Odds_Ratio <- exp(
      coefficient_table$Estimate
    )
    
    coefficient_table$OR_Lower_95 <- exp(
      coefficient_table$Estimate -
        qnorm(0.975) * coefficient_table$Standard_Error
    )
    
    coefficient_table$OR_Upper_95 <- exp(
      coefficient_table$Estimate +
        qnorm(0.975) * coefficient_table$Standard_Error
    )
    
    # Inspect selected coefficients
    terms_to_review <- c(
      "Card_CategoryGold",
      "Card_CategoryPlatinum",
      "Card_CategorySilver",
      "Total_Relationship_Count",
      "Months_Inactive_12_mon",
      "Contacts_Count_12_mon",
      "Total_Trans_Ct"
    )
    
    print(
      coefficient_table[
        coefficient_table$Term %in% terms_to_review,
      ],
      row.names = FALSE,
      digits = 4
    )
    
    write.csv(
      coefficient_table,
      file = file.path(
        task5_folder,
        "Logistic_Baseline_Odds_Ratios.csv"
      ),
      row.names = FALSE
    )
    
#For example, your inactivity coefficient is approximately 0.5104, giving an odds ratio of approximately 1.67 per additional recorded inactive month, conditional on the other terms and the assumed linear effect.
#That means approximately 67% higher odds, not a 67-percentage-point increase in probability. These are model-dependent associations, not causal effects.
    
#screen for influential obseravtions
#An influential record is one that may materially affect the fitted model. It is not necessarily erroneous.

    # Influence diagnostics for the training model
    cook_values <- cooks.distance(logistic_baseline)
    leverage_values <- hatvalues(logistic_baseline)
    
    n_training <- nobs(logistic_baseline)
    n_parameters <- logistic_baseline$rank
    
    # Screening thresholds, not automatic deletion rules
    cook_threshold <- 4 / n_training
    leverage_threshold <- 2 * n_parameters / n_training
    
    influence_summary <- data.frame(
      Diagnostic = c(
        "Cook's distance",
        "Leverage"
      ),
      
      Screening_Threshold = c(
        cook_threshold,
        leverage_threshold
      ),
      
      Number_Flagged = c(
        sum(cook_values > cook_threshold),
        sum(leverage_values > leverage_threshold)
      ),
      
      Maximum = c(
        max(cook_values),
        max(leverage_values)
      )
    )
    
    influence_summary
    
#These thresholds are screening heuristics. With thousands of records and rare categories, some flags are expected. Their presence does not invalidate the model

#inspect the 10 most influential records
    top_influence_rows <- head(
      order(cook_values, decreasing = TRUE),
      10
    )
    
    top_influence_records <- data.frame(
      Training_Row = top_influence_rows,
      
      Source_Row = train_rows[top_influence_rows],
      
      Churn = train_model$Churn[top_influence_rows],
      
      Card_Category =
        train_model$Card_Category[top_influence_rows],
      
      Total_Trans_Ct =
        train_model$Total_Trans_Ct[top_influence_rows],
      
      Total_Trans_Amt =
        train_model$Total_Trans_Amt[top_influence_rows],
      
      Cooks_Distance =
        unname(cook_values[top_influence_rows]),
      
      Leverage =
        unname(leverage_values[top_influence_rows])
    )
    
    print(
      top_influence_records,
      row.names = FALSE
    )
    
    write.csv(
      influence_summary,
      file = file.path(
        task5_folder,
        "Logistic_Baseline_Influence_Summary.csv"
      ),
      row.names = FALSE
    )
    
    write.csv(
      top_influence_records,
      file = file.path(
        task5_folder,
        "Logistic_Baseline_Top_Influential_Records.csv"
      ),
      row.names = FALSE
    )
    
#Keep all records for now. These checks help us decide whether sensitivity analysis is needed; they are not instructions to remove customers.

#The main concern is the small Platinum group, rather than a fitting failure.

#fit a flexible logistic model using training data only
    # Task 5: Targeted check of numerical functional form
    
    # Replace three linear terms with natural spline terms
    flexible_formula <- update(
      formula(logistic_baseline),
      
      . ~ . -
        Total_Trans_Ct -
        Total_Trans_Amt -
        Total_Revolving_Bal +
        
        splines::ns(Total_Trans_Ct, df = 3) +
        splines::ns(Total_Trans_Amt, df = 3) +
        splines::ns(Total_Revolving_Bal, df = 3)
    )
    
    logistic_flexible <- glm(
      formula = flexible_formula,
      data = train_model,
      family = binomial(link = "logit"),
      na.action = na.fail
    )
    
    flexible_fit_check <- data.frame(
      Converged = logistic_flexible$converged,
      Observations_Used = nobs(logistic_flexible),
      Model_Rank = logistic_flexible$rank,
      Aliased_Coefficients = sum(
        is.na(coef(logistic_flexible))
      )
    )
    
    flexible_fit_check
    
#Each selected variable now uses three coefficients instead of one. Therefore, we expect six additional parameters, giving a model rank of 38 if the design remains full rank.
#The spline coefficients themselves are not simple “per-unit” odds ratios. If we retain this model, we will explain those effects using predicted-probability plots.
    
#compare the baseline and flexible model
    # Ensure both models used the same observations
    stopifnot(
      identical(
        rownames(model.frame(logistic_baseline)),
        rownames(model.frame(logistic_flexible))
      )
    )
    
    # Likelihood-ratio comparison of nested logistic models
    functional_form_test <- anova(
      logistic_baseline,
      logistic_flexible,
      test = "LRT"
    )
    
    functional_form_test
    
#This is a likelihood-ratio test for logistic models, not the Welch ANOVA used in Task 4. Comparing nested binomial models on the same observations is supported by R’s analysis-of-deviance procedure
#A small p-value would indicate that the baseline’s linear terms miss some structure. It would not establish improved prediction on new records; cross-validation must assess that.
#This is an exploratory Task 5 diagnostic, separate from the four primary Task 4 hypotheses.
    
#save a compact comparison
    functional_form_summary <- data.frame(
      Model = c(
        "Baseline linear-logit model",
        "Targeted spline logistic model"
      ),
      
      Parameters = c(
        logistic_baseline$rank,
        logistic_flexible$rank
      ),
      
      Residual_Deviance = c(
        deviance(logistic_baseline),
        deviance(logistic_flexible)
      ),
      
      AIC = c(
        AIC(logistic_baseline),
        AIC(logistic_flexible)
      )
    )
    
    print(
      functional_form_summary,
      row.names = FALSE
    )
    
    write.csv(
      functional_form_summary,
      file = file.path(
        task5_folder,
        "Logistic_Functional_Form_Comparison.csv"
      ),
      row.names = FALSE
    )
    
    capture.output(
      functional_form_test,
      file = file.path(
        task5_folder,
        "Logistic_Functional_Form_LRT.txt"
      )
    )
    
    saveRDS(
      logistic_flexible,
      file = file.path(
        task5_folder,
        "Logistic_Targeted_Spline_Model.rds"
      )
    )
    
#Why do this before penalized regression? Ridge, LASSO, and elastic net shrink coefficients; they do not automatically fix an incorrectly specified straight-line effect. This check helps distinguish the need for shrinkage from the need for curved relationships.
#When evaluating a spline model through cross-validation, we will fit its spline transformations separately inside each training fold.
    
#We will now compare the two unpenalized models using the same five stratified cross-validation folds. Later, we will reuse those folds for ridge, LASSO, and elastic net.
#recover the complete likelihood ratio result
    # Display the functional-form comparison explicitly
    lrt_statistic <- deviance(logistic_baseline) -
      deviance(logistic_flexible)
    
    lrt_df <- logistic_flexible$rank -
      logistic_baseline$rank
    
    functional_form_result <- data.frame(
      LR_Statistic = lrt_statistic,
      Degrees_of_Freedom = lrt_df,
      P_value = pchisq(
        lrt_statistic,
        df = lrt_df,
        lower.tail = FALSE
      )
    )
    
    print(functional_form_result, row.names = FALSE)
    
    write.csv(
      functional_form_result,
      file.path(task5_folder, "Functional_Form_LRT_Result.csv"),
      row.names = FALSE
    )
    
#This comparison remains an exploratory training-data diagnostic. It is not another primary test to add retrospectively to your Task 4 table.

#Create five stratified folds inside the training set
#Each training customer will belong to one validation fold. In each iteration, the model learns from the other four folds.
    
    # Task 5: Shared cross-validation folds
    
    set.seed(3082)
    
    number_of_folds <- 5L
    fold_id <- integer(nrow(train_model))
    
    for (outcome in c(0L, 1L)) {
      
      group_rows <- which(train_model$Churn == outcome)
      
      fold_id[group_rows] <- sample(
        rep(
          seq_len(number_of_folds),
          length.out = length(group_rows)
        )
      )
    }
    
    # Check outcome counts within each fold
    fold_counts <- table(
      Fold = fold_id,
      Churn = train_model$Churn
    )
    
    fold_counts
    
    saveRDS(
      fold_id,
      file.path(task5_folder, "Shared_CV_Fold_ID.rds")
    )
    
#3. Generate out-of-fold predictions for both models
#An out-of-fold prediction comes from a model that was not fitted using that customer’s record.
#The spline model is fitted afresh inside every fold. Its knots therefore come only from that fold’s fitting data.
    
    # Store one held-out prediction per training customer
    oof_predictions <- data.frame(
      Churn = train_model$Churn,
      Fold = fold_id,
      Baseline = NA_real_,
      Spline = NA_real_
    )
    
    cv_fit_checks <- data.frame()
    
    for (fold in seq_len(number_of_folds)) {
      
      fitting_rows <- which(fold_id != fold)
      validation_rows <- which(fold_id == fold)
      
      fitting_data <- train_model[
        fitting_rows, , drop = FALSE
      ]
      
      validation_data <- train_model[
        validation_rows, , drop = FALSE
      ]
      
      baseline_fold <- glm(
        formula(logistic_baseline),
        data = fitting_data,
        family = binomial(link = "logit"),
        na.action = na.fail
      )
      
      spline_fold <- glm(
        flexible_formula,
        data = fitting_data,
        family = binomial(link = "logit"),
        na.action = na.fail
      )
      
      oof_predictions$Baseline[validation_rows] <- predict(
        baseline_fold,
        newdata = validation_data,
        type = "response"
      )
      
      oof_predictions$Spline[validation_rows] <- predict(
        spline_fold,
        newdata = validation_data,
        type = "response"
      )
      
      cv_fit_checks <- rbind(
        cv_fit_checks,
        data.frame(
          Fold = fold,
          Baseline_Converged = baseline_fold$converged,
          Spline_Converged = spline_fold$converged,
          Baseline_Aliased = sum(is.na(coef(baseline_fold))),
          Spline_Aliased = sum(is.na(coef(spline_fold)))
        )
      )
    }
    
    cv_fit_checks
    
    stopifnot(
      !anyNA(oof_predictions),
      all(oof_predictions$Baseline >= 0 &
            oof_predictions$Baseline <= 1),
      all(oof_predictions$Spline >= 0 &
            oof_predictions$Spline <= 1)
    )
    
#4. Compare probability performance
#Start with two measures that do not require choosing a classification threshold:
#- Log loss: penalizes confident incorrect predictions strongly.
#- Brier score: measures the average squared difference between predicted probability and the observed 0/1 outcome.
#Lower is better for both. Neither measure alone establishes good calibration, so calibration plots will follow later.
    
    # Probability-based performance measures
    probability_metrics <- function(actual, probability) {
      
      # Clipping is used only to make logarithms finite
      epsilon <- 1e-15
      
      clipped_probability <- pmin(
        pmax(probability, epsilon),
        1 - epsilon
      )
      
      c(
        Log_Loss = -mean(
          actual * log(clipped_probability) +
            (1 - actual) * log(1 - clipped_probability)
        ),
        
        Brier_Score = mean(
          (probability - actual)^2
        )
      )
    }
    
    baseline_metrics <- probability_metrics(
      oof_predictions$Churn,
      oof_predictions$Baseline
    )
    
    spline_metrics <- probability_metrics(
      oof_predictions$Churn,
      oof_predictions$Spline
    )
    
    cv_probability_comparison <- data.frame(
      Model = c(
        "Baseline logistic",
        "Spline logistic"
      ),
      
      Log_Loss = c(
        baseline_metrics["Log_Loss"],
        spline_metrics["Log_Loss"]
      ),
      
      Brier_Score = c(
        baseline_metrics["Brier_Score"],
        spline_metrics["Brier_Score"]
      ),
      
      row.names = NULL
    )
    
    print(
      cv_probability_comparison,
      row.names = FALSE,
      digits = 6
    )
    
#We will use cross-validated log loss as the primary tuning criterion, supported by discrimination, calibration, and practical considerations. This suits our aim of estimating useful probabilities rather than optimizing accuracy at an arbitrary threshold.
#save outputs
    write.csv(
      oof_predictions,
      file.path(task5_folder, "Logistic_Out_Of_Fold_Predictions.csv"),
      row.names = FALSE
    )
    
    write.csv(
      cv_fit_checks,
      file.path(task5_folder, "Logistic_CV_Fit_Checks.csv"),
      row.names = FALSE
    )
    
    write.csv(
      cv_probability_comparison,
      file.path(task5_folder, "Logistic_CV_Probability_Comparison.csv"),
      row.names = FALSE
    )
    
#These are internal model-development results. Because the spline candidate was motivated using the whole training set, they should not be described as a fully independent assessment of the entire model-selection process. The test set remains reserved for the final evaluation.

#The spline model reduced log loss by approximately 24.9% and Brier score by 25.9%, relative to the baseline. These are reductions in prediction error—not percentage-point improvements in accuracy.
#All ten fold-specific models converged, none had aliased coefficients, and the folds preserved the class distribution. The spline model is currently the stronger candidate, but we have not selected the final model.
    
#Next, we will fit ridge, LASSO, and elastic-net logistic regression using the original linear predictor specification. This separates two questions:
    #- Does allowing curved effects help? Your spline comparison suggests it does.
    #- Does shrinking coefficients improve the original linear model? The next comparison will assess this.
#We must not interpret a spline model beating a linear penalized model as evidence that penalization itself is ineffective.
    
#1. Install and load glmnet
    
    # Allow more time for downloads
    options(timeout = max(300, getOption("timeout")))
    
    # Install a precompiled Windows package and required dependencies
    install.packages(
      "glmnet",
      repos = "https://cran.r-project.org",
      type = "binary"
    )
    
    library(glmnet)
    packageVersion("glmnet")
    
#2. Prepare the predictor matrix
    # Use the same linear terms as the baseline logistic model
    x_penalized <- model.matrix(
      formula(logistic_baseline),
      data = train_model
    )
    
    # glmnet supplies its own intercept
    x_penalized <- x_penalized[
      ,
      colnames(x_penalized) != "(Intercept)",
      drop = FALSE
    ]
    
    y_penalized <- train_model$Churn
    
    dim(x_penalized)
    
    stopifnot(
      nrow(x_penalized) == length(y_penalized),
      length(fold_id) == length(y_penalized),
      !anyNA(x_penalized),
      !anyNA(y_penalized)
    )

#These columns include category indicators. Therefore, the number of nonzero coefficients later will not necessarily equal the number of original variables.
#We will let glmnet standardize predictors within each fit. Do not manually standardize the entire dataset first.
    
#3. Tune the penalized models
#We will examine three elastic-net mixtures: 0.25, 0.50, and 0.75. This is a modest, explicitly defined search.
    
    # Candidate penalty mixtures
    alpha_values <- c(
      Ridge = 0,
      ElasticNet_025 = 0.25,
      ElasticNet_050 = 0.50,
      ElasticNet_075 = 0.75,
      LASSO = 1
    )
    
    penalized_cv_fits <- list()
    
    for (model_name in names(alpha_values)) {
      
      penalized_cv_fits[[model_name]] <- cv.glmnet(
        x = x_penalized,
        y = y_penalized,
        
        family = "binomial",
        alpha = unname(alpha_values[model_name]),
        
        foldid = fold_id,
        type.measure = "deviance",
        
        standardize = TRUE,
        intercept = TRUE,
        keep = TRUE
      )
    }
    
#We are still using training data only.

#4. Summarize tuning results
    #Two lambda choices are useful:
    #- lambda.min: minimizes cross-validated deviance.
    #- lambda.1se: uses the strongest penalty within one estimated standard error of the minimum. It offers a more strongly regularized alternative, not a guarantee of equivalent performance.
    
    penalized_tuning_summary <- do.call(
      rbind,
      
      lapply(names(penalized_cv_fits), function(model_name) {
        
        fit <- penalized_cv_fits[[model_name]]
        
        index_min <- which.min(
          abs(fit$lambda - fit$lambda.min)
        )
        
        index_1se <- which.min(
          abs(fit$lambda - fit$lambda.1se)
        )
        
        data.frame(
          Model = model_name,
          
          Alpha = unname(alpha_values[model_name]),
          
          Lambda_Min = fit$lambda.min,
          
          CV_Log_Loss_Min = fit$cvm[index_min] / 2,
          
          Nonzero_Coefficients_Min = fit$nzero[index_min],
          
          Lambda_1SE = fit$lambda.1se,
          
          CV_Log_Loss_1SE = fit$cvm[index_1se] / 2,
          
          Nonzero_Coefficients_1SE = fit$nzero[index_1se],
          
          Minimum_At_Grid_Edge =
            index_min %in% c(1L, length(fit$lambda))
        )
      })
    )
    
    rownames(penalized_tuning_summary) <- NULL
    
    print(
      penalized_tuning_summary,
      row.names = FALSE,
      digits = 6
    )
    
#The grid-edge indicator helps identify whether we should inspect the lambda search range before accepting the tuning result.
    
#5. Save the fitted objects and table
    
    saveRDS(
      penalized_cv_fits,
      file.path(
        task5_folder,
        "Penalized_Logistic_CV_Models.rds"
      )
    )
    
    write.csv(
      penalized_tuning_summary,
      file.path(
        task5_folder,
        "Penalized_Logistic_Tuning_Summary.csv"
      ),
      row.names = FALSE
    )
    
#How to interpret the next output
#Lower cross-validated log loss is better. However, because these scores also select the penalty settings, they are model-development estimates, not an independent final assessment. We will use the reserved test set after choosing the modelling procedure.
#Also, ordinary LASSO can remove individual category indicators rather than an entire categorical variable. We must describe that accurately if we discuss “variable selection.”

#Next we will consolidate the candidate models’ out-of-fold predictions and compare discrimination, calibration, and complexity
    
#There is one issue to resolve before finalizing the comparison: ridge selected a lambda at the edge of its search range.

#The spline model remains the strongest candidate. The small differences between baseline logistic, LASSO, and elastic net do not establish a meaningful advantage for one over the others.
#LASSO retains 30 of 31 predictor coefficients at lambda.min, so that choice produces little simplification. Its lambda.1se alternative retains 16 coefficients, with somewhat higher log loss. Remember that these are coefficient counts, including category indicators—not counts of original variables.
    
#Why the ridge result needs another check
#Minimum_At_Grid_Edge = TRUE means the best ridge setting was at an endpoint of the lambda values searched. If it was the smallest lambda, weaker shrinkage might improve performance.
#We should investigate that before concluding ridge performs poorly. Only ridge needs this follow-up; keep the other models and folds unchanged.
    
#1. Identify the selected endpoint
    
    # Inspect the original ridge search
    ridge_original <- penalized_cv_fits[["Ridge"]]
    
    ridge_search_check <- data.frame(
      Smallest_Lambda = min(ridge_original$lambda),
      Selected_Lambda = ridge_original$lambda.min,
      Largest_Lambda = max(ridge_original$lambda),
      
      Selected_Smallest = isTRUE(all.equal(
        ridge_original$lambda.min,
        min(ridge_original$lambda)
      )),
      
      Selected_Largest = isTRUE(all.equal(
        ridge_original$lambda.min,
        max(ridge_original$lambda)
      ))
    )
    
    ridge_search_check
    
#2. Extend the ridge search toward weaker penalties
#The following preserves the original range and adds smaller positive lambda values. Ridge approaches unpenalized logistic regression as lambda approaches zero.
    
    # Preserve the original fit for comparison
    saveRDS(
      ridge_original,
      file.path(task5_folder, "Ridge_Original_CV_Model.rds")
    )
    
    # Add lambda values down to 1/10,000 of the original minimum
    smaller_lambdas <- exp(
      seq(
        from = log(min(ridge_original$lambda)),
        to = log(min(ridge_original$lambda) / 10000),
        length.out = 100
      )
    )
    
    ridge_lambda_extended <- sort(
      unique(c(
        ridge_original$lambda,
        smaller_lambdas
      )),
      decreasing = TRUE
    )
    
    ridge_extended <- cv.glmnet(
      x = x_penalized,
      y = y_penalized,
      
      family = "binomial",
      alpha = 0,
      
      lambda = ridge_lambda_extended,
      foldid = fold_id,
      type.measure = "deviance",
      
      standardize = TRUE,
      intercept = TRUE,
      keep = TRUE
    )
    
#This is a justified extension of an incomplete search, not a change to the data split or an attempt to improve results by trying different random seeds.

#3. Compare the original and extended ridge searches
    summarize_ridge <- function(fit, label) {
      
      selected_index <- which.min(
        abs(fit$lambda - fit$lambda.min)
      )
      
      data.frame(
        Search = label,
        Smallest_Lambda = min(fit$lambda),
        Selected_Lambda = fit$lambda.min,
        CV_Log_Loss = fit$cvm[selected_index] / 2,
        Nonzero_Coefficients = fit$nzero[selected_index],
        
        Minimum_At_Grid_Edge =
          selected_index %in% c(1L, length(fit$lambda))
      )
    }
    
    ridge_search_comparison <- rbind(
      summarize_ridge(ridge_original, "Original"),
      summarize_ridge(ridge_extended, "Extended")
    )
    
    print(
      ridge_search_comparison,
      row.names = FALSE,
      digits = 7
    )
    
    write.csv(
      ridge_search_comparison,
      file.path(task5_folder, "Ridge_Search_Comparison.csv"),
      row.names = FALSE
    )
    
    saveRDS(
      ridge_extended,
      file.path(task5_folder, "Ridge_Extended_CV_Model.rds")
    )
    
#The extended search resolved the ridge boundary issue. Its selected lambda is now inside the search range, and its log loss is close to ordinary logistic regression:
#This suggests that only weak shrinkage is beneficial for this linear specification. The improvement over baseline logistic regression—0.234402—is tiny, so we should not claim a meaningful predictive advantage.

#The warnings need one small correction
#“Collapsing to unique ‘x’ values” concerns repeated interpolation coordinates; it does not mean R removed customer records. R’s interpolation routines combine repeated coordinates
    
#The likely cause is my grid-building code: it included the original minimum lambda and recreated the same endpoint using exp(log(...)). Floating-point rounding can leave nearly identical values that unique() does not remove.
#Let’s rebuild the grid without that overlapping endpoint and refit ridge once.

#1. Correct the lambda grid
    # Generate the extension
    ridge_extension <- exp(
      seq(
        from = log(min(ridge_original$lambda)),
        to = log(min(ridge_original$lambda) / 10000),
        length.out = 100
      )
    )
    
    # Remove the first value: that endpoint is already in the original grid
    ridge_extension <- ridge_extension[-1L]
    
    ridge_lambda_clean <- sort(
      c(
        ridge_original$lambda,
        ridge_extension
      ),
      decreasing = TRUE
    )
    
    # Verify positive, strictly decreasing lambda values
    stopifnot(
      all(ridge_lambda_clean > 0),
      all(diff(ridge_lambda_clean) < 0)
    )
    
#2. Refit using the same data and folds
    ridge_clean <- cv.glmnet(
      x = x_penalized,
      y = y_penalized,
      
      family = "binomial",
      alpha = 0,
      
      lambda = ridge_lambda_clean,
      foldid = fold_id,
      type.measure = "deviance",
      
      standardize = TRUE,
      intercept = TRUE,
      keep = TRUE
    )
    
    ridge_clean_summary <- summarize_ridge(
      ridge_clean,
      "Extended, corrected grid"
    )
    
    print(
      ridge_clean_summary,
      row.names = FALSE,
      digits = 7
    )
    
#The result should be very similar to your extended fit. If the same warnings persist, share them rather than suppressing them.
    
#3. Update the model collection
#If the corrected fit completes without warnings:
    # Replace the original restricted ridge search
    penalized_cv_fits[["Ridge"]] <- ridge_clean
    
    saveRDS(
      penalized_cv_fits,
      file.path(
        task5_folder,
        "Penalized_Logistic_CV_Models.rds"
      )
    )
    
    write.csv(
      ridge_clean_summary,
      file.path(
        task5_folder,
        "Ridge_Corrected_Search_Summary.csv"
      ),
      row.names = FALSE
    )
    
#ridge model now fits without the interpolation warnings, its selected lambda is inside the search range, and its log loss remains 0.2343594. No further ridge search is needed.
#Next, we will create one comparison table from the models’ out-of-fold probabilities. This ensures that all metrics use the same customers and outcome coding.
#We will compare baseline logistic, spline logistic, ridge, the three elastic-net settings, and LASSO. We will also retain the simpler LASSO lambda.1se option.
    
#1. Combine the out-of-fold probabilities
#For binomial glmnet models, the saved prevalidated predictions are on the log-odds scale. We convert them to probabilities using plogis(). These are predictions from cross-validation, not predictions from fitting and evaluating on the same records. The package documents using saved prevalidated predictions for model assessment.

    # Task 5: Consolidated out-of-fold predictions
    
    comparison_oof <- oof_predictions[
      , c("Churn", "Fold", "Baseline", "Spline")
    ]
    
    # Add penalized models at lambda.min
    for (model_name in names(penalized_cv_fits)) {
      
      fit <- penalized_cv_fits[[model_name]]
      
      stopifnot(
        all(fit$foldid == fold_id)
      )
      
      selected_index <- which.min(
        abs(fit$lambda - fit$lambda.min)
      )
      
      comparison_oof[[model_name]] <- plogis(
        fit$fit.preval[, selected_index]
      )
    }
    
    # Add the more strongly regularized LASSO alternative
    lasso_fit <- penalized_cv_fits[["LASSO"]]
    
    lasso_1se_index <- which.min(
      abs(lasso_fit$lambda - lasso_fit$lambda.1se)
    )
    
    comparison_oof$LASSO_1SE <- plogis(
      lasso_fit$fit.preval[, lasso_1se_index]
    )
    
    prediction_columns <- setdiff(
      names(comparison_oof),
      c("Churn", "Fold")
    )
    
    stopifnot(
      !anyNA(comparison_oof),
      
      all(vapply(
        comparison_oof[prediction_columns],
        function(p) all(is.finite(p) & p >= 0 & p <= 1),
        logical(1)
      ))
    )
    
#Do not substitute predict(fit, newx = x_penalized) here: that would produce fitted training predictions rather than out-of-fold predictions.
#2. Add ROC-AUC
#ROC-AUC assesses ranking: how often an attrited record receives a higher score than an existing record. The following base-R calculation gives tied scores half credit.
    
    roc_auc <- function(actual, probability) {
      
      n_positive <- sum(actual == 1L)
      n_negative <- sum(actual == 0L)
      
      stopifnot(
        n_positive > 0,
        n_negative > 0
      )
      
      score_ranks <- rank(
        probability,
        ties.method = "average"
      )
      
      (
        sum(score_ranks[actual == 1L]) -
          n_positive * (n_positive + 1) / 2
      ) / (n_positive * n_negative)
    }
    
#Higher AUC is better. It does not establish that probabilities are calibrated or that a particular targeting threshold is useful.

#3. Create the consolidated performance table
#This reuses your existing probability_metrics() function.
    
    model_comparison <- do.call(
      rbind,
      
      lapply(prediction_columns, function(model_name) {
        
        p <- comparison_oof[[model_name]]
        actual <- comparison_oof$Churn
        
        probability_results <- probability_metrics(
          actual,
          p
        )
        
        data.frame(
          Model = model_name,
          
          Log_Loss = unname(
            probability_results["Log_Loss"]
          ),
          
          Brier_Score = unname(
            probability_results["Brier_Score"]
          ),
          
          ROC_AUC = roc_auc(actual, p),
          
          Mean_Predicted_Probability = mean(p),
          
          Observed_Attrition_Proportion = mean(actual)
        )
      })
    )
    
    # Sort by our primary criterion
    model_comparison <- model_comparison[
      order(model_comparison$Log_Loss),
    ]
    
    rownames(model_comparison) <- NULL
    
    print(
      model_comparison,
      row.names = FALSE,
      digits = 6
    )
    
    write.csv(
      model_comparison,
      file.path(task5_folder, "OOF_Model_Comparison.csv"),
      row.names = FALSE
    )
    
    write.csv(
      comparison_oof,
      file.path(task5_folder, "All_Model_OOF_Probabilities.csv"),
      row.names = FALSE
    )
    
#The mean predicted probability should be considered alongside the observed proportion. However, matching overall averages can hide overprediction in some probability ranges and underprediction in others.
#4. Examine calibration for the spline candidate
#We will group its out-of-fold predictions into approximately ten probability groups, keeping identical probabilities together.
    
    # Calibration groups based on predicted-probability quantiles
    calibration_breaks <- unique(
      quantile(
        comparison_oof$Spline,
        probs = seq(0, 1, by = 0.1),
        names = FALSE
      )
    )
    
    calibration_group <- cut(
      comparison_oof$Spline,
      breaks = calibration_breaks,
      include.lowest = TRUE,
      labels = FALSE
    )
    
    spline_calibration <- do.call(
      rbind,
      
      lapply(
        sort(unique(calibration_group)),
        function(group_number) {
          
          rows <- which(calibration_group == group_number)
          
          data.frame(
            Group = group_number,
            N = length(rows),
            
            Mean_Predicted = mean(
              comparison_oof$Spline[rows]
            ),
            
            Observed_Proportion = mean(
              comparison_oof$Churn[rows]
            )
          )
        }
      )
    )
    
    print(
      spline_calibration,
      row.names = FALSE,
      digits = 4
    )
    
    write.csv(
      spline_calibration,
      file.path(task5_folder, "Spline_OOF_Calibration_Table.csv"),
      row.names = FALSE
    )
    
#For example, a group with an average predicted probability of 0.20 should have an observed attrition proportion near 0.20 if predictions are well calibrated in that range. Some variation is expected; these grouped comparisons are descriptive.
    
#5. Save a calibration plot
png(
    file.path(task5_folder, "Spline_OOF_Calibration_Plot.png"),
    width = 1400,
    height = 1200,
    res = 160
    )

plot(
  spline_calibration$Mean_Predicted,
  spline_calibration$Observed_Proportion,
  
  xlim = c(0, 1),
  ylim = c(0, 1),
  
  pch = 19,
  col = "#26718B",
  
  xlab = "Mean predicted attrition probability",
  ylab = "Observed attrition proportion",
  main = "Spline logistic: out-of-fold calibration"
)

abline(
  a = 0,
  b = 1,
  lty = 2,
  col = "grey40"
)

dev.off()

#Points close to the diagonal indicate agreement within those groups. This plot does not by itself validate deployment.

#We will then assess precision–recall performance and threshold choices before fixing the final candidate and evaluating the reserved test set. 
#The current comparisons remain internal development results because the same folds helped select the penalty settings.

#The spline logistic model is the leading candidate among the models evaluated.
#The spline model performs better on all three measures. This suggests that allowing curved relationships was more useful here than applying shrinkage to the original linear specification. It does not establish that a penalized spline model would be inferior; we have not evaluated that combination.

#Next: assess precision–recall performance and choose a documented classification threshold.

#A probability model does not automatically decide which records to flag. Lower thresholds generally identify more attrited records but also flag more existing records.
#For this academic comparison, we will examine 0.50 and an out-of-fold F1-maximizing threshold. F1 balances precision and recall mathematically; it is not a profit-optimal retention policy.

#1. Calculate average precision for every model
#Average precision summarizes precision across changes in recall. We will label it explicitly rather than call it a trapezoidal PR-AUC, because those calculations differ.

#The following function keeps tied scores together:
# Task 5: Precision-recall assessment

precision_recall_curve <- function(actual, probability) {
  
  stopifnot(
    length(actual) == length(probability),
    all(actual %in% c(0L, 1L)),
    all(is.finite(probability)),
    sum(actual == 1L) > 0
  )
  
  ordering <- order(probability, decreasing = TRUE)
  
  y <- actual[ordering]
  p <- probability[ordering]
  
  cumulative_tp <- cumsum(y == 1L)
  cumulative_fp <- cumsum(y == 0L)
  
  # Evaluate after including all records with each tied score
  endpoints <- cumsum(rle(p)$lengths)
  
  tp <- cumulative_tp[endpoints]
  fp <- cumulative_fp[endpoints]
  fn <- sum(y == 1L) - tp
  
  data.frame(
    Threshold = p[endpoints],
    TP = tp,
    FP = fp,
    FN = fn,
    Precision = tp / (tp + fp),
    Recall = tp / sum(y == 1L),
    F1 = 2 * tp / (2 * tp + fp + fn)
  )
}

average_precision <- function(actual, probability) {
  
  curve <- precision_recall_curve(actual, probability)
  
  recall_increments <- diff(c(0, curve$Recall))
  
  sum(recall_increments * curve$Precision)
}

model_comparison$Average_Precision <- vapply(
  model_comparison$Model,
  
  function(model_name) {
    average_precision(
      comparison_oof$Churn,
      comparison_oof[[model_name]]
    )
  },
  
  numeric(1)
)

print(
  model_comparison[
    c(
      "Model",
      "Log_Loss",
      "Brier_Score",
      "ROC_AUC",
      "Average_Precision"
    )
  ],
  row.names = FALSE,
  digits = 6
)

#Higher average precision is better. Its interpretation depends on class prevalence; your training attrition proportion is approximately 16.06%.

#2. Select an illustrative threshold using training predictions only
spline_pr_curve <- precision_recall_curve(
  comparison_oof$Churn,
  comparison_oof$Spline
)

# If F1 values tie exactly, choose the highest threshold
# The curve is already ordered by decreasing threshold
best_f1_row <- which.max(spline_pr_curve$F1)

selected_threshold <- spline_pr_curve$Threshold[
  best_f1_row
]

selected_threshold

spline_pr_curve[
  best_f1_row,
  c("Threshold", "Precision", "Recall", "F1")
]

#This uses only out-of-fold training predictions. Its reported F1 will be optimistic to some extent because the same predictions select the threshold. We will evaluate the fixed threshold later on the test set.

#3. Compare the selected threshold with 0.50
threshold_metrics <- function(actual, probability, threshold) {
  
  predicted <- as.integer(probability >= threshold)
  
  tp <- sum(predicted == 1L & actual == 1L)
  fp <- sum(predicted == 1L & actual == 0L)
  tn <- sum(predicted == 0L & actual == 0L)
  fn <- sum(predicted == 0L & actual == 1L)
  
  data.frame(
    Threshold = threshold,
    TP = tp,
    FP = fp,
    TN = tn,
    FN = fn,
    
    Precision = if ((tp + fp) > 0) {
      tp / (tp + fp)
    } else {
      NA_real_
    },
    
    Recall = tp / (tp + fn),
    Specificity = tn / (tn + fp),
    F1 = 2 * tp / (2 * tp + fp + fn),
    Accuracy = (tp + tn) / length(actual),
    Flagged_Percent = 100 * (tp + fp) / length(actual)
  )
}

threshold_comparison <- rbind(
  cbind(
    Rule = "Reference threshold 0.50",
    threshold_metrics(
      comparison_oof$Churn,
      comparison_oof$Spline,
      0.50
    )
  ),
  
  cbind(
    Rule = "OOF F1-maximizing threshold",
    threshold_metrics(
      comparison_oof$Churn,
      comparison_oof$Spline,
      selected_threshold
    )
  )
)

print(
  threshold_comparison,
  row.names = FALSE,
  digits = 5
)

#These describe classification workload, not the number of customers an intervention would retain.

#4. Save the outputs
write.csv(
  model_comparison,
  file.path(task5_folder, "OOF_Model_Comparison.csv"),
  row.names = FALSE
)

write.csv(
  threshold_comparison,
  file.path(task5_folder, "Spline_OOF_Threshold_Comparison.csv"),
  row.names = FALSE
)

saveRDS(
  selected_threshold,
  file.path(task5_folder, "Spline_Training_Selected_Threshold.rds")
)

#We can now freeze the spline logistic model and the training-selected threshold, then evaluate them on the reserved test set.
#The spline model leads on all four development measures: log loss, Brier score, ROC-AUC, and average precision. We should describe it as the best-performing candidate among the models evaluated, not the best possible churn model.
#The threshold decision involves a real trade-off

#Lowering the threshold identifies 112 additional attrited records, while incorrectly flagging 162 additional existing records. The review queue increases by 274 records.
#The F1 improvement is small. Therefore, 0.33921 is an illustrative statistical threshold—not a demonstrated business optimum. Whether the extra workload is worthwhile depends on contact capacity, costs, and intervention effectiveness.

#For the final evaluation:
#- Primary model: spline logistic regression.
#- Primary threshold: the full-precision value stored in selected_threshold.
#- Reference threshold: 0.50.
#- No further tuning based on test results.

#1. Save the decision before evaluating the test set
#Your logistic_flexible model is already fitted to all 8,101 training records. It does not need refitting.

# Task 5: Freeze the selected model and decision rule

final_model <- logistic_flexible
final_threshold <- selected_threshold

selection_record <- data.frame(
  Model = "Logistic regression with three natural spline terms",
  
  Primary_Selection_Criterion =
    "Training cross-validated log loss",
  
  Threshold_Rule =
    "Maximum F1 using training out-of-fold predictions",
  
  Selected_Threshold = final_threshold,
  
  Reference_Threshold = 0.50
)

write.csv(
  selection_record,
  file.path(task5_folder, "Final_Model_Selection_Record.csv"),
  row.names = FALSE
)

saveRDS(
  list(
    model = final_model,
    threshold = final_threshold,
    factor_levels = final_model$xlevels
  ),
  file.path(task5_folder, "Selected_Model_And_Threshold.rds")
)

#2. Prepare test predictors using the training definitions
#We remove the same redundant variable and apply the factor levels learned from training. We do not estimate new transformations from the test data.

test_model <- test_data[
  ,
  names(test_data) != "Avg_Open_To_Buy",
  drop = FALSE
]

# Check and apply training factor levels
for (variable in names(final_model$xlevels)) {
  
  test_values <- as.character(test_model[[variable]])
  training_levels <- final_model$xlevels[[variable]]
  
  unseen_levels <- setdiff(
    unique(test_values),
    training_levels
  )
  
  if (length(unseen_levels) > 0L) {
    stop(
      paste(
        "Unseen test category in",
        variable,
        ":",
        paste(unseen_levels, collapse = ", ")
      )
    )
  }
  
  test_model[[variable]] <- factor(
    test_values,
    levels = training_levels
  )
}

stopifnot(
  nrow(test_model) == 2026L,
  !anyNA(test_model)
)

#Stopping for an unseen category is preferable to silently assigning it an incorrect meaning

#3. Generate test probabilities
test_probability <- predict(
  final_model,
  newdata = test_model,
  type = "response"
)

test_probability <- as.numeric(test_probability)

stopifnot(
  length(test_probability) == nrow(test_model),
  all(is.finite(test_probability)),
  all(test_probability >= 0 & test_probability <= 1)
)

#predict() uses the spline transformations stored in the fitted training model. Do not create separate spline knots from the test data.

#4. Calculate the final probability-performance measures
test_probability_metrics <- probability_metrics(
  test_model$Churn,
  test_probability
)

test_performance <- data.frame(
  Model = "Selected spline logistic",
  
  N = nrow(test_model),
  
  Attrited = sum(test_model$Churn == 1L),
  
  Log_Loss = unname(
    test_probability_metrics["Log_Loss"]
  ),
  
  Brier_Score = unname(
    test_probability_metrics["Brier_Score"]
  ),
  
  ROC_AUC = roc_auc(
    test_model$Churn,
    test_probability
  ),
  
  Average_Precision = average_precision(
    test_model$Churn,
    test_probability
  ),
  
  Mean_Predicted_Probability = mean(test_probability),
  
  Observed_Attrition_Proportion = mean(test_model$Churn)
)

print(
  test_performance,
  row.names = FALSE,
  digits = 6
)

#These results evaluate the fixed model on records excluded from model fitting and cross-validation.
#However, remember the earlier limitation: Task 3 and Task 4 examined the full dataset before the split. Describe this as a held-out modelling evaluation, not an entirely untouched external validation.

#5. Evaluate the two fixed thresholds
test_threshold_comparison <- rbind(
  cbind(
    Rule = "Primary: training-selected threshold",
    
    threshold_metrics(
      test_model$Churn,
      test_probability,
      final_threshold
    )
  ),
  
  cbind(
    Rule = "Reference: threshold 0.50",
    
    threshold_metrics(
      test_model$Churn,
      test_probability,
      0.50
    )
  )
)

print(
  test_threshold_comparison,
  row.names = FALSE,
  digits = 5
)

# Confusion matrix for the primary threshold
test_predicted <- as.integer(
  test_probability >= final_threshold
)

test_confusion_matrix <- table(
  Actual = factor(
    test_model$Churn,
    levels = c(0, 1),
    labels = c("Existing", "Attrited")
  ),
  
  Predicted = factor(
    test_predicted,
    levels = c(0, 1),
    labels = c("Existing", "Attrited")
  )
)

test_confusion_matrix

#Do not choose whichever threshold happens to score better on this test set. The training-selected threshold remains the primary rule; 0.50 is a predefined reference.

#6. Save the evaluation
write.csv(
  test_performance,
  file.path(task5_folder, "Final_Test_Probability_Performance.csv"),
  row.names = FALSE
)

write.csv(
  test_threshold_comparison,
  file.path(task5_folder, "Final_Test_Threshold_Comparison.csv"),
  row.names = FALSE
)

write.csv(
  as.data.frame(test_confusion_matrix),
  file.path(task5_folder, "Final_Test_Confusion_Matrix.csv"),
  row.names = FALSE
)

write.csv(
  data.frame(
    Source_Row = test_rows,
    Actual_Churn = test_model$Churn,
    Predicted_Probability = test_probability,
    Predicted_Churn = test_predicted
  ),
  file.path(task5_folder, "Final_Test_Predictions.csv"),
  row.names = FALSE
)

#Next: finish evaluation with uncertainty and test calibration
#We will assess the fixed model, not change it.

#1. Add confidence intervals for the threshold measures
#These Wilson intervals describe sampling uncertainty conditional on the selected model and threshold. They do not incorporate uncertainty from the entire model-selection process.
# Task 5: Confidence intervals for fixed-threshold metrics

wilson_interval <- function(successes, total, conf.level = 0.95) {
  
  z <- qnorm(1 - (1 - conf.level) / 2)
  p <- successes / total
  
  denominator <- 1 + z^2 / total
  
  centre <- (p + z^2 / (2 * total)) / denominator
  
  half_width <- (
    z * sqrt(
      p * (1 - p) / total +
        z^2 / (4 * total^2)
    )
  ) / denominator
  
  c(
    Estimate = p,
    Lower_95 = centre - half_width,
    Upper_95 = centre + half_width
  )
}

primary_test <- test_threshold_comparison[1, ]

metric_counts <- data.frame(
  Metric = c("Recall", "Precision", "Specificity", "Accuracy"),
  
  Successes = c(
    primary_test$TP,
    primary_test$TP,
    primary_test$TN,
    primary_test$TP + primary_test$TN
  ),
  
  Total = c(
    primary_test$TP + primary_test$FN,
    primary_test$TP + primary_test$FP,
    primary_test$TN + primary_test$FP,
    nrow(test_model)
  )
)

test_metric_intervals <- cbind(
  metric_counts,
  
  as.data.frame(
    t(
      mapply(
        wilson_interval,
        metric_counts$Successes,
        metric_counts$Total
      )
    )
  )
)

print(
  test_metric_intervals,
  row.names = FALSE,
  digits = 4
)

#2. Check test calibration using the training-defined groups
#Reuse the probability boundaries from training. Extend their endpoints to include any test probabilities beyond the observed training range.

# Reuse training calibration boundaries
test_calibration_breaks <- calibration_breaks

test_calibration_breaks[1] <- -Inf

test_calibration_breaks[
  length(test_calibration_breaks)
] <- Inf

test_calibration_group <- cut(
  test_probability,
  breaks = test_calibration_breaks,
  include.lowest = TRUE,
  labels = FALSE
)

test_calibration <- do.call(
  rbind,
  
  lapply(
    sort(unique(test_calibration_group)),
    
    function(group_number) {
      
      rows <- which(
        test_calibration_group == group_number
      )
      
      observed_interval <- wilson_interval(
        sum(test_model$Churn[rows]),
        length(rows)
      )
      
      data.frame(
        Group = group_number,
        N = length(rows),
        
        Mean_Predicted = mean(
          test_probability[rows]
        ),
        
        Observed_Proportion = unname(
          observed_interval["Estimate"]
        ),
        
        Lower_95 = unname(
          observed_interval["Lower_95"]
        ),
        
        Upper_95 = unname(
          observed_interval["Upper_95"]
        )
      )
    }
  )
)

print(
  test_calibration,
  row.names = FALSE,
  digits = 4
)

#These intervals help prevent overinterpreting differences based on small numbers of attrited customers. They are individual intervals, not simultaneous confidence bands.

#3. Save the results
write.csv(
  test_metric_intervals,
  file.path(task5_folder, "Final_Test_Metric_Intervals.csv"),
  row.names = FALSE
)

write.csv(
  test_calibration,
  file.path(task5_folder, "Final_Test_Calibration_Table.csv"),
  row.names = FALSE
)

#Your confidence intervals and calibration calculations are correct. The planned Task 5 evaluation is now complete. You do not need to fit more models or change the threshold based on these test results.
#The unequal group sizes are expected because you reused the training-defined probability boundaries. Do not change them to force equal test-group sizes.

#Save your final R environment details
capture.output(
  sessionInfo(),
  file = file.path(
    task5_folder,
    "Task5_Final_R_Session_Info.txt"
  )
)

