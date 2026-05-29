# =============================================================================
# prepare_external_data.R
# -----------------------------------------------------------------------------
# Reproducibly downloads the THREE external datasets that contextualise the
# Superstore (2011-2014) US analysis, directly from authoritative US sources.
# Every value is fetched live — nothing is hand-entered. All three are
# period-matched to 2011-2014 and keyed on State + Year so they join cleanly
# to data/superstore_us_clean.csv.
#
# Run from the assignment root:  Rscript scripts/prepare_external_data.R
#
# SOURCES (accessed 2026-05-30):
#   1. Population  - US Census Bureau, Population Estimates Program (PEP),
#                    "Annual Estimates of the Resident Population for the
#                    United States, Regions, States... 2010-2019"
#                    (NST-EST2019-alldata). Columns POPESTIMATE2011-2014.
#                    https://www2.census.gov/programs-surveys/popest/datasets/
#                      2010-2019/national/totals/nst-est2019-alldata.csv
#   2. Income      - US Census Bureau, Small Area Income & Poverty Estimates
#                    (SAIPE), state-level "Median Household Income", one file
#                    per year 2011-2014 (est11all.xls ... est14all.xls).
#                    https://www2.census.gov/programs-surveys/saipe/datasets/
#   3. Unemployment- US Bureau of Labor Statistics, Local Area Unemployment
#                    Statistics (LAUS), statewide unemployment-rate series
#                    LASST{fips}0000000000003, 2011-2014. Annual rate computed
#                    as the mean of the 12 monthly values (BLS publishes the
#                    same annual averages). Public API v1 (no key).
#                    https://api.bls.gov/publicAPI/v1/timeseries/data/
#
# OUTPUTS (long format, join key = State + Year):
#   data/population_by_state.csv     State, Year, Population
#   data/income_by_state.csv         State, Year, Median_Household_Income
#   data/unemployment_by_state.csv   State, Year, Unemployment_Rate
#   data/state_econ_by_year.csv      combined master join table
# =============================================================================

suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(tidyr); library(jsonlite); library(httr)
})

ACCESS_DATE <- "2026-05-30"
YEARS <- 2011:2014

# --- States actually present in the Superstore analysis (49: 48 states + DC) -
ss <- read.csv("data/superstore_us_clean.csv", stringsAsFactors = FALSE)
scope_states <- sort(unique(ss$State))
message("Superstore states in scope: ", length(scope_states))

# =============================================================================
# 1. POPULATION  — Census PEP nst-est2019
# =============================================================================
pep_url <- paste0("https://www2.census.gov/programs-surveys/popest/datasets/",
                  "2010-2019/national/totals/nst-est2019-alldata.csv")
pep_tmp <- tempfile(fileext = ".csv")
download.file(pep_url, pep_tmp, quiet = TRUE, mode = "wb")
pep_raw <- read.csv(pep_tmp, stringsAsFactors = FALSE)

pep <- pep_raw %>%
  filter(SUMLEV == 40) %>%                                   # state-level rows
  transmute(
    State = NAME,
    fips  = sprintf("%02d", as.integer(STATE)),
    `2011` = POPESTIMATE2011, `2012` = POPESTIMATE2012,
    `2013` = POPESTIMATE2013, `2014` = POPESTIMATE2014
  )

# state name -> FIPS map (used to build BLS series IDs)
fips_map <- pep %>% select(State, fips)

population <- pep %>%
  select(-fips) %>%
  pivot_longer(-State, names_to = "Year", values_to = "Population") %>%
  mutate(Year = as.integer(Year)) %>%
  filter(State %in% scope_states) %>%
  arrange(State, Year)
message("Population rows: ", nrow(population))

# =============================================================================
# 2. INCOME  — Census SAIPE median household income (one .xls per year)
# =============================================================================
get_saipe <- function(year) {
  yy  <- substr(year, 3, 4)
  url <- sprintf(paste0("https://www2.census.gov/programs-surveys/saipe/",
                        "datasets/%d/%d-state-and-county/est%sall.xls"),
                 year, year, yy)
  tmp <- tempfile(fileext = ".xls")
  download.file(url, tmp, quiet = TRUE, mode = "wb")
  d <- suppressMessages(read_excel(tmp, skip = 2))           # row 3 = header
  names(d)[1:4] <- c("StateFIPS", "CountyFIPS", "Postal", "Name")
  inc_col <- grep("Median Household Income", names(d), ignore.case = TRUE)[1]
  # state totals only; FIPS cols may be character ("000") or numeric (0)
  # depending on the year's file, so compare as integers for robustness.
  # suppressWarnings: a trailing note row coerces to NA and is dropped.
  suppressWarnings(
    d %>%
      filter(as.integer(CountyFIPS) == 0, as.integer(StateFIPS) != 0) %>%
      transmute(State = trimws(Name),
                Year  = as.integer(year),
                Median_Household_Income = as.numeric(.[[inc_col]]))
  )
}
income <- bind_rows(lapply(YEARS, get_saipe)) %>%
  filter(State %in% scope_states) %>%
  arrange(State, Year)
message("Income rows: ", nrow(income))

# =============================================================================
# 3. UNEMPLOYMENT  — BLS LAUS API (monthly -> annual mean)
# =============================================================================
scope_fips <- fips_map %>% filter(State %in% scope_states)
series_ids <- paste0("LASST", scope_fips$fips, strrep("0", 11), "03")
series_to_state <- setNames(scope_fips$State, series_ids)

bls_fetch <- function(series, sy, ey) {
  # toJSON() on a character vector always yields a JSON array (even length 1),
  # which BLS requires for seriesid.
  body <- sprintf('{"seriesid":%s,"startyear":"%d","endyear":"%d"}',
                  toJSON(series), sy, ey)
  resp <- POST("https://api.bls.gov/publicAPI/v1/timeseries/data/",
               content_type_json(), body = body)
  fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE)
}

chunks <- split(series_ids, ceiling(seq_along(series_ids) / 25))  # v1: <=25/req
unemp_rows <- list()
for (ch in chunks) {
  resp <- bls_fetch(ch, min(YEARS), max(YEARS))
  if (!identical(resp$status, "REQUEST_SUCCEEDED"))
    stop("BLS request failed: ", paste(unlist(resp$message), collapse = "; "))
  for (s in resp$Results$series) {
    sid <- s$seriesID
    df  <- bind_rows(lapply(s$data, function(x)
             data.frame(Year = as.integer(x$year),
                        value = as.numeric(x$value), stringsAsFactors = FALSE)))
    ann <- df %>% group_by(Year) %>%
      summarise(Unemployment_Rate = round(mean(value), 1), .groups = "drop") %>%
      mutate(State = series_to_state[[sid]])
    unemp_rows[[length(unemp_rows) + 1]] <- ann
  }
}
unemployment <- bind_rows(unemp_rows) %>%
  filter(State %in% scope_states, Year %in% YEARS) %>%
  select(State, Year, Unemployment_Rate) %>%
  arrange(State, Year)
message("Unemployment rows: ", nrow(unemployment))

# =============================================================================
# 4. WRITE + RECONCILE
# =============================================================================
write.csv(population,   "data/population_by_state.csv",   row.names = FALSE)
write.csv(income,       "data/income_by_state.csv",       row.names = FALSE)
write.csv(unemployment, "data/unemployment_by_state.csv", row.names = FALSE)

state_econ <- population %>%
  left_join(income,       by = c("State", "Year")) %>%
  left_join(unemployment, by = c("State", "Year")) %>%
  arrange(State, Year)
write.csv(state_econ, "data/state_econ_by_year.csv", row.names = FALSE)

cat("\n================ RECONCILIATION ================\n")
cat("Scope states         :", length(scope_states), " (expect 49)\n")
cat("Expected rows each    :", length(scope_states) * length(YEARS), " (49 x 4 = 196)\n")
cat("population rows       :", nrow(population), "\n")
cat("income rows           :", nrow(income), "\n")
cat("unemployment rows     :", nrow(unemployment), "\n")
cat("state_econ rows       :", nrow(state_econ), "\n")
cat("Years covered         :", paste(sort(unique(state_econ$Year)), collapse = ", "), "\n")
cat("NA Population         :", sum(is.na(state_econ$Population)), "\n")
cat("NA Income             :", sum(is.na(state_econ$Median_Household_Income)), "\n")
cat("NA Unemployment       :", sum(is.na(state_econ$Unemployment_Rate)), "\n")
miss <- setdiff(scope_states, unique(state_econ$State))
cat("Scope states unmatched:", if (length(miss)) paste(miss, collapse=", ") else "none", "\n")
cat("\n--- Sample: California & Alabama, all years ---\n")
print(state_econ %>% filter(State %in% c("California","Alabama")) %>% as.data.frame())
cat("================================================\n")
