library(rtables)
library(tern)
library(dplyr)
library(forcats)

# import data
adsl <- ex_adsl
adae <- ex_adae
adrs <- ex_adrs

#Pre-processing

# select variables to include in table
# define the variables to be analyzed
vars <- c("AGE", "SEX")
vars_labels <- c("Age (yr)", "Sex")

# ensure both are character vectors
vars <- as.character(vars)
vars_labels <- as.character(vars_labels)

# reorder the levels in the ARM & SEX variable
adsl$ARM <- factor(adsl$ARM, levels = c("B: Placebo","A: Drug X","C: Combination"))
adsl$SEX <- factor(adsl$SEX, levels = c("M","F","U","UNDIFFERENTIATED"))

# --- end

# Demographic Table

#Run the table creation process
basic_table(show_colcounts = TRUE) %>%
  split_cols_by("ARM") %>%
  add_overall_col("All Patients") %>%
  add_colcounts() %>%
  analyze_vars(
    vars = vars,
    var_labels = vars_labels
    #.stats = c("n","mean_sd","count"),
    #.formats = c(mean_sd = "xx.xx (xx.xx)")
  ) %>%
  build_table(adsl)

# --- end

basic_table(show_colcounts = TRUE) %>%
  split_cols_by("ARM") %>%
  add_overall_col("All Patients") %>%
  add_colcounts() %>%
  analyze_vars(
    vars = vars,
    var_labels = vars_labels
    #.stats = c("n","mean_sd","count"),
    #.formats = c(mean_sd = "xx.xx (xx.xx)")
  ) %>%
  build_table(adsl %>% dplyr::filter(COUNTRY == "BRA")) # country Brazil


basic_table(show_colcounts = TRUE) %>%
  split_cols_by("ARM") %>%
  add_overall_col("All Patients") %>%
  add_colcounts() %>%
  analyze_vars(
    vars = vars,
    var_labels = vars_labels
    #.stats = c("n","mean_sd","count"),
    #.formats = c(mean_sd = "xx.xx (xx.xx)")
  ) %>%
  build_table(adsl %>% dplyr::filter(COUNTRY == "CHN")) # country China

# --- end

# Adverse Event Table
basic_table() %>%
  split_cols_by(var = "ACTARM") %>%
  add_colcounts() %>%
  add_overall_col(label = "All Patients") %>%
  summarize_num_patients(
    var = "USUBJID",
    .stats = c("unique","nonunique"),
    .labels = c(
      unique = "Total number of patients with at least one AE",
      nonunique = "Overall total number of events"
    )
  ) %>%
  build_table(df=adae)







