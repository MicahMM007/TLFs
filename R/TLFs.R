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
    var_labels = vars_labels,
    stats = c("n","mean_sd","count"),
    formats = c(mean_sd = "xx.xx (xx.xx)")
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

# a statistics function calculating th numbers in summarize_num_patients
s_num_patients(x = adae$USUBJID, labelstr = "", .N_col = nrow(adae))

# further analyse the unique adverse events
basic_table() %>%
  split_cols_by(var = "ACTARM") %>%
  add_colcounts() %>%
  add_overall_col(label = "All Patients") %>%
  summarize_num_patients(
    var = "USUBJID",
    .stats = c("unique", "nonunique"),
    .labels = c(
      unique = "Total number of patients with at least one AE",
      nonunique = "Overall total number of events"
    )
  ) %>%
  split_rows_by(
    "AEBODSYS",
    child_labels = "visible",
    nested = FALSE,
    #indent_mod = -1L,
    split_fun = drop_split_levels
  ) %>%
  summarize_num_patients(
    var = "USUBJID",
    .stats = c("unique","nonunique")
  ) %>%
  build_table(df=adae,
              alt_counts_df = adsl)


# further layout using count_occurrences()

basic_table() %>%
  split_cols_by("ACTARM") %>%
  add_colcounts() %>%
  add_overall_col(label = "ALL Patients") %>%
  summarize_num_patients(
    var = "USUBJID",
    .stats = c("unique","nonunique"),
    .labels = c(
      unique = "Total number of patients with at least one AE",
      nonunique = "Overall total number of events"
    )
  ) %>%
  split_rows_by(
    "AEBODSYS"
    #split_fun = drop_split_levels
  ) %>%
  summarize_row_groups() %>% #or use summarize_num_patients(var,.stats,.labels)
  count_occurrences(vars = "AEDECOD") %>%
  build_table(df=adae,
              alt_counts_df = adsl)

# --- end

# Response Table
# __ Proportion of responders in each treatment group

# preprocessing
anl <- adrs %>%
  dplyr::filter(PARAMCD == "BESRSPI") %>%
  dplyr::mutate(is_rsp = AVALC %in% c("CR","PR"))

basic_table() %>%
  split_cols_by(var = "ARM", ref_group = "B: Placebo") %>%
  add_colcounts() %>%
  estimate_proportion(
    vars = "is_rsp",
    table_names = "est_prop",
    conf_level = 0.9
  ) %>%
  build_table(anl)

# --
basic_table() %>%
  split_cols_by(var = "ARM", ref_group = "B: Placebo") %>%
  add_colcounts() %>%
  estimate_proportion_diff( #compare to reference group
    vars = "is_rsp",
    conf_level = 0.9
  ) %>%
  build_table(anl)

# --
basic_table() %>%
  split_cols_by(var = "ARM",ref_group = "B: Placebo") %>%
  add_colcounts() %>%
  test_proportion_diff(vars = "is_rsp") %>% #add method = "schouten"
  build_table(anl)

