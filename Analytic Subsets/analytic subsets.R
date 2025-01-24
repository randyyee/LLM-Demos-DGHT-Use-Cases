# Analytic subsets for AI interpretation ----
# PSNU_IM
library(tidyverse)

df <- read_delim(file.choose(), 
                 "\t", 
                 escape_double = FALSE,
                 trim_ws = TRUE,
                 col_types = cols(.default = col_character(), 
                                  targets = col_double(),
                                  qtr1 = col_double(),
                                  qtr2 = col_double(),
                                  qtr3 = col_double(),
                                  qtr4 = col_double(),
                                  cumulative = col_double()
                 ) 
) %>%
  filter(fiscal_year=="2024")

# write.csv(df, "OUxIM.csv", row.names = F)

# By program area ----
df_prevention <- df %>%
  filter(indicator %in% c("AGYW_PREV",
                          "GEN_GBV",
                          "KP_PREV",
                          "OVC_SERV",
                          "PREP_CT",
                          "PREP_NEW",
                          "TB_PREV",
                          "VMMC_CIRC"))

df_testing <- df %>%
  filter(indicator %in% c("CXCA_SCRN",
                          "HTS_INDEX",
                          "HTS_SELF",
                          "HTS_TST",
                          "HTS_TST_POS",
                          "OVC_HIVSTAT",
                          "PMTCT_EID",
                          "PMTCT_FO",
                          "PMTCT_HEI",
                          "PMTCT_STAT",
                          "TB_STAT"))

df_treatment <- df %>%
  filter(indicator %in% c("CXCA_TX",
                          "PMTCT_ART",
                          "TB_ART",
                          "TX_CURR",
                          "TX_HIV_HTN",
                          "TX_ML",
                          "TX_NEW",
                          "TX_TB",
                          "TX_RTT"))

df_viralsuppression <- df %>%
  filter(indicator == "TX_PVLS")

df_healthsystems <- df %>%
  filter(indicator %in% c("LAB_PTCQI",
                          "SC_ARVDISP"))

## By program area achievements  ----
achievement <- function(df){
  df %>%
    filter(targets != 0 | !is.null(targets)) %>%
    filter(standardizeddisaggregate %in% c("Total Numerator", "Total Denominator")) %>%
    group_by(operatingunit, psnu, funding_agency, prime_partner_name, age_2019, sex, fiscal_year, indicator, standardizeddisaggregate) %>%
    summarize(cumulative = sum(cumulative, na.rm = T),
              targets    = sum(targets, na.rm = T)) %>%
    ungroup() %>%
    bind_rows(
      df %>%
        filter(targets != 0 | !is.null(targets)) %>%
        filter(standardizeddisaggregate %in% c("Total Numerator", "Total Denominator")) %>%
        group_by(operatingunit, fiscal_year, indicator, standardizeddisaggregate) %>%
        summarize(cumulative = sum(cumulative, na.rm = T),
                  targets    = sum(targets, na.rm = T)) %>%
        ungroup()
    ) %>%
    bind_rows(
      df %>%
        filter(targets != 0 | !is.null(targets)) %>%
        filter(standardizeddisaggregate %in% c("Total Numerator", "Total Denominator")) %>%
        group_by(operatingunit, psnu, fiscal_year, indicator, standardizeddisaggregate) %>%
        summarize(cumulative = sum(cumulative, na.rm = T),
                  targets    = sum(targets, na.rm = T)) %>%
        ungroup()
    ) %>%
    bind_rows(
      df %>%
        filter(targets != 0 | !is.null(targets)) %>%
        filter(standardizeddisaggregate %in% c("Total Numerator", "Total Denominator")) %>%
        group_by(operatingunit, funding_agency, fiscal_year, indicator, standardizeddisaggregate) %>%
        summarize(cumulative = sum(cumulative, na.rm = T),
                  targets    = sum(targets, na.rm = T)) %>%
        ungroup()
    ) %>%
    bind_rows(
      df %>%
        filter(targets != 0 | !is.null(targets)) %>%
        filter(standardizeddisaggregate %in% c("Total Numerator", "Total Denominator")) %>%
        group_by(operatingunit, funding_agency, prime_partner_name, fiscal_year, indicator, standardizeddisaggregate) %>%
        summarize(cumulative = sum(cumulative, na.rm = T),
                  targets    = sum(targets, na.rm = T)) %>%
        ungroup()
    )%>%
    bind_rows(
      df %>%
        filter(targets != 0 | !is.null(targets)) %>%
        filter(!standardizeddisaggregate %in% c("Total Numerator", "Total Denominator")) %>%
        group_by(operatingunit, ageasentered, sex, fiscal_year, indicator) %>%
        summarize(cumulative = sum(cumulative, na.rm = T),
                  targets    = sum(targets, na.rm = T)) %>%
        ungroup()
    )
}

df_prevention_achievements <- achievement(df_prevention)
df_testing_achievements <- achievement(df_testing)
df_treatment_achievements <- achievement(df_treatment)
df_viralsuppression_achievements <- achievement(df_viralsuppression)
df_healthsystems_achievements <- achievement(df_healthsystems)

write.csv(df_prevention_achievements, "prevention_achievements.csv", row.names = F)
write.csv(df_testing_achievements, "testing_achievements.csv", row.names = F)
write.csv(df_treatment_achievements, "treatment_achievements.csv", row.names = F)
write.csv(df_viralsuppression_achievements, "viralsuppression_achievements.csv", row.names = F)
## By program area stats  ----

program <- function(){
  df %>%
    pivot_longer(targets:cumulative,
                 names_to = "attribute",
                 values_to = "value") %>%
    unite("period",
          c("fiscal_year", "attribute"),
          sep = "_",
          remove = F) %>%
    filter(value != 0 | !is.null(value)) %>%
    pivot_wider(names_from="indicator", values_from="value", values_fn = ~ sum(.x, na.rm = TRUE))
}