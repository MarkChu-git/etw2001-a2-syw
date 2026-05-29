# =============================================================================
# prepare_superstore.R
# -----------------------------------------------------------------------------
# Transparent, reproducible cleaning of the raw Superstore dataset for the
# ETW2001 A2 dashboard. Replaces the opaque, pre-cleaned `clean_dashboard_data.csv`
# (which shipped a 100%-NA Population column and no build script).
#
# Run from the assignment root:  Rscript scripts/prepare_superstore.R
#
# INPUT : data/superstore.xlsx   (Global Superstore, 51,290 rows x 27 cols)
# OUTPUT: data/superstore_us_clean.csv
#
# Pipeline:
#   1. Read the raw "superstore" sheet.
#   2. Filter to the US market (Country == "United States") — the assignment
#      analyses US states alongside US Census/BLS external data.
#   3. Keep only the columns the dashboard and report use.
#   4. Coerce Order.Date to Date and derive Year from it (not the raw Year
#      column, which is a Tableau export artifact).
#   5. Derive Profit_Margin (= Profit / Sales, guarded against Sales == 0)
#      and Loss_Flag (Profit < 0).
#   6. Write the cleaned CSV and print a reconciliation summary.
# =============================================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
})

raw_path <- "data/superstore.xlsx"
out_path <- "data/superstore_us_clean.csv"
stopifnot(file.exists(raw_path))

raw <- read_excel(raw_path, sheet = "superstore")
message("Raw rows: ", nrow(raw), " | cols: ", ncol(raw))

clean <- raw %>%
  # 2. US market only
  filter(Country == "United States") %>%
  # 3. Keep dashboard/report columns
  transmute(
    Order.Date   = as.Date(Order.Date),
    Region, State, Category, Sub.Category, Segment, Ship.Mode,
    Sales        = as.numeric(Sales),
    Profit       = as.numeric(Profit),
    Discount     = as.numeric(Discount),
    Quantity     = as.integer(Quantity)
  ) %>%
  # 4. Derive Year from the parsed date
  mutate(Year = as.integer(format(Order.Date, "%Y"))) %>%
  # 5. Derived metrics — guard divide-by-zero (1 row has Sales == 0)
  mutate(
    Profit_Margin = if_else(Sales > 0, Profit / Sales, NA_real_),
    Loss_Flag     = if_else(Profit < 0, "Loss", "Profit")
  ) %>%
  arrange(Order.Date) %>%
  select(Order.Date, Year, Region, State, Category, Sub.Category, Segment,
         Ship.Mode, Sales, Profit, Discount, Quantity, Profit_Margin, Loss_Flag)

write.csv(clean, out_path, row.names = FALSE)

# 6. Built-in reconciliation / verification ----------------------------------
cat("\n================ RECONCILIATION ================\n")
cat("Output file      :", out_path, "\n")
cat("Rows             :", nrow(clean), "  (expected 9994)\n")
cat("States           :", length(unique(clean$State)), "\n")
cat("Regions          :", paste(sort(unique(clean$Region)), collapse = ", "), "\n")
cat("Date range       :", as.character(min(clean$Order.Date)), "->",
                          as.character(max(clean$Order.Date)), "\n")
cat("Total Sales      :", round(sum(clean$Sales)),  "  (expected 2297354)\n")
cat("Total Profit     :", round(sum(clean$Profit)), "  (expected 286397)\n")
cat("Profit_Margin NA :", sum(is.na(clean$Profit_Margin)), "  (rows with Sales==0)\n")
cat("Any NA in key col:", any(is.na(clean[, c("Region","State","Category","Sales","Profit")])), "\n")
cat("================================================\n")
