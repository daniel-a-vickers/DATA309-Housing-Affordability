# DATA309 RQ3: urban vs rural housing in Canterbury
# Brief Q3: "Are there notable differences in the influence of these factors
# between urban and rural areas within the Canterbury region?"
#
# Classification scheme:
#   Method A: match LocalityName to the Stats NZ Urban Rural 2025 list
#   Method B: guess from site size and category code (works on every row)
#   Method C: Chch city = urban, rest = rural  (Only used to check previous results)
#   Final: use A where it exists, B where A misses, disagreements become peri-urban


# Load libraries

if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")
if (!requireNamespace("lubridate", quietly = TRUE)) install.packages("lubridate")
if (!requireNamespace("scales", quietly = TRUE)) install.packages("scales")

library(tidyverse, quietly = TRUE)
library(lubridate, quietly = TRUE)
library(ggplot2, quietly = TRUE)


# outputs into outputs_rq3

dir.create("outputs_rq3", showWarnings = FALSE)


# load lookups

locality_lookup <- read_csv("data_urban_rural_locality_lookup.csv", show_col_types = FALSE) %>%
  mutate(
    locality = str_trim(locality),
    main_tla = as.character(dom_tla),
    lookup_class = na_if(broad, ""),
    lookup_method = na_if(method, ""),
    lookup_area = ur_name,
    lookup_code = iur
  )


# load and clean data
# Using ECAN property data

ecan <- read_csv("data/ECAN_Property_Values.csv",
                 show_col_types = FALSE, lazy = FALSE) %>%
  rename_with(~ str_trim(.x)) %>%
  mutate(
    LocalityName = str_trim(LocalityName),
    TLA          = str_trim(TLA),
    LocalCouncil = str_trim(LocalCouncil),
    Category     = str_trim(Category),
    LandUse      = str_trim(LandUse),
    CapitalValue = suppressWarnings(as.numeric(CapitalValue)),
    LandValue = suppressWarnings(as.numeric(LandValue)),
    
    # site size in hectares, use rating hectares first then GIS area
    site_ha = coalesce(suppressWarnings(as.numeric(RatingHectares)),
                       suppressWarnings(as.numeric(AREA_HA)),
                       suppressWarnings(as.numeric(AREA_M2)) / 10000)
  )


# TLA codes in ECAN

tla_names <- c("054" = "Kaikoura", "058" = "Hurunui", "059" = "Waimakariri",
               "060" = "Christchurch City", "062" = "Selwyn", "063" = "Ashburton",
               "064" = "Timaru", "065" = "Mackenzie", "066" = "Waimate",
               "068" = "Waitaki (part)")


# Method A, join locality to the UR2025 list

ecan_lookup <- ecan %>%
  left_join(locality_lookup %>% select(locality, lookup_area, lookup_code,
                                       lookup_class, lookup_method),
            by = c("LocalityName" = "locality"))


# missingness table

missing_table <- ecan_lookup %>%
  filter(is.na(lookup_class)) %>%
  count(TLA, LandUse, sort = TRUE) %>%
  mutate(TLA_name = recode(TLA, !!!tla_names))

write_csv(missing_table, "outputs_rq3/table_A_missingness.csv")


# Method B, guess urban or rural from site size and category code
# trying on every row including blank localities. 
# Code families checked against Improvements text and LandUse plus site size:
#   RF = flats/units/townhouses ("FLAT OI", "OI UNIT"), so Urban
#   RD under 0.5ha = normal residential section, so Urban
#   RD over 1.8ha = large amount of  land, so Rural
#   RD between 0.5 and 1.8ha = grey zone, leave as uncertain 
#   LV/RB/LI = lifestyle blocks (average between 1.4 to 4ha), so Rural
#   RV/OP/blank = vacant or open land or unknown, leave as uncertain
#   anything else goes by site size (under 0.10ha Urban, over 0.5ha Rural)


classify_proxy <- function(site_ha, category) {
  
  category <- str_trim(if_else(is.na(category), "", category))
  
  case_when(
    str_detect(category, "^RF") ~ "Urban",
    str_detect(category, "^(LV|RB)") | str_detect(category, "^LI") ~ "Rural",
    category %in% c("RV", "OP", "") ~ "Uncertain",
    str_detect(category, "^RD") & !is.na(site_ha) & site_ha < 0.50 ~ "Urban",
    str_detect(category, "^RD") & !is.na(site_ha) & site_ha >= 1.8 ~ "Rural",
    str_detect(category, "^RD") ~ "Uncertain",
    !is.na(site_ha) & site_ha < 0.10 ~ "Urban",
    !is.na(site_ha) & site_ha >= 0.50 ~ "Rural",
    TRUE ~ "Uncertain"
  )
  
}

ecan_proxy <- ecan_lookup %>%
  mutate(proxy_class = classify_proxy(site_ha, Category),
         proxy_method = "proxy from site size and category")



# Saftey catch:
# proxy says uncertain but there is a dwelling on site
# and capital value is under 750k, treat as Urban

dwelling_words <- "DWG|BLDG|UNIT|FLAT|HOUSE|BACH|CRIB"

ecan_proxy <- ecan_proxy %>%
  mutate(
    has_dwelling = (coalesce(ImprovementsValue, 0) > 0) |
      str_detect(str_to_upper(Improvements), dwelling_words),
    rescue_urban = proxy_class == "Uncertain" & coalesce(has_dwelling, FALSE) &
      !is.na(CapitalValue) & CapitalValue < 750000,
    proxy_class = if_else(rescue_urban, "Urban", proxy_class),
    proxy_method = if_else(rescue_urban, "proxy rescue: dwelling under 750k",
                           proxy_method)
  )


# Method C, crude check: Chch city = Urban, everything else = Rural
# Only used to check the headline results dont depend on the mapping

ecan_checked <- ecan_proxy %>%
  mutate(check_class = if_else(TLA == "060", "Urban", "Rural"),
         check_method = "TLA060 urban else rural")


# Final class, combine the methods
#   Where A exists it wins (official Stats NZ geography)
#   Where A misses (blank or unmapped locality) fall back to B
#   Where A and B disagree keep "Peri-urban" instead of
#   forcing a binary, the lifestyle blocks on the edge are a real thing

disagree <- !is.na(ecan_checked$lookup_class) &
  ecan_checked$proxy_class %in% c("Urban", "Rural") &
  ecan_checked$lookup_class != ecan_checked$proxy_class

ecan_classified <- ecan_checked %>%
  mutate(
    final_class = if_else(disagree, "Peri-urban",
                          coalesce(lookup_class, proxy_class)),
    class_source = if_else(disagree, "lookup_proxy_disagree",
                           if_else(!is.na(lookup_class),
                                   paste0("lookup:", lookup_method),
                                   "fallback:proxy"))
  )


# final split counts

class_counts <- ecan_classified %>% count(final_class, sort = TRUE) %>%
  mutate(percent = round(100 * n / sum(n), 2))

write_csv(class_counts, "outputs_rq3/table_final_split.csv")


# A vs B agreement table

agreement_table <- ecan_classified %>%
  filter(!is.na(lookup_class), proxy_class %in% c("Urban", "Rural")) %>%
  count(lookup_class, proxy_class) %>%
  group_by(lookup_class) %>% mutate(percent = round(100 * n / sum(n), 1)) %>% ungroup()

write_csv(agreement_table, "outputs_rq3/table_A_vs_B_agreement.csv")


# save classified ECAN

ecan_classified %>%
  select(OBJECTID, TLA, LocalCouncil, StreetAddress, LocalityName, Category,
         LandUse, CapitalValue, LandValue, ImprovementsValue, site_ha,
         lookup_area, lookup_code, lookup_class, proxy_class,
         check_class, final_class, class_source) %>%
  write_csv("outputs_rq3/ecan_classified.csv")


# Valuation roll, Canterbury TAs only
# The CSV is a 6 district extract, not national. Checked with street names:
# Only 60/62 are Canterbury so filter to those. Old 03_00 mapping


valuation <- read_csv("data/nz-properties-national-district-valuation-roll.csv",
                      show_col_types = FALSE, lazy = FALSE) %>%
  mutate(
    district_ta_code = as.character(str_trim(district_ta_code)),
    property_category = str_trim(property_category),
    zoning = str_trim(zoning),
    actual_property_use = str_trim(as.character(actual_property_use)),
    capital_value = suppressWarnings(as.numeric(capital_value)),
    land_value = suppressWarnings(as.numeric(land_value)),
    improvements_value = suppressWarnings(as.numeric(improvements_value)),
    land_area = suppressWarnings(as.numeric(land_area)), # hectares
    building_total_floor_area = suppressWarnings(as.numeric(building_total_floor_area)),
    no_of_bedrooms = suppressWarnings(as.numeric(no_of_bedrooms))
  ) %>%
  filter(district_ta_code %in% c("60", "62")) %>%
  mutate(district = recode(district_ta_code, "60" = "Christchurch City",
                           "62" = "Selwyn"))


# same proxy as ECAN with the valuation roll column names
# (RF = flats so Urban, LV/RB/LI = lifestyle so Rural, RV/OP/blank = uncertain,
#  RD by site size: under 0.50ha Urban, over 1.8ha Rural, else uncertain)

valuation <- valuation %>%
  mutate(
    category_clean = str_trim(if_else(is.na(property_category), "", property_category)),
    final_class = case_when(
      str_detect(category_clean, "^RF") ~ "Urban",
      str_detect(category_clean, "^(LV|RB)") | str_detect(category_clean, "^LI") ~ "Rural",
      category_clean %in% c("RV", "OP", "") ~ "Uncertain",
      str_detect(category_clean, "^RD") & !is.na(land_area) & land_area < 0.40 ~ "Urban",
      str_detect(category_clean, "^RD") & !is.na(land_area) & land_area >= 2.0 ~ "Rural",
      str_detect(category_clean, "^RD") ~ "Uncertain",
      !is.na(land_area) & land_area < 0.10 ~ "Urban",
      !is.na(land_area) & land_area >= 0.40 ~ "Rural",
      TRUE ~ "Uncertain"
    ),
    
    # crude TLA check: Chch = Urban
    check_class = if_else(district == "Christchurch City", "Urban", "Rural")
  )

write_csv(valuation %>% count(district, final_class),
          "outputs_rq3/table_valroll_split.csv")


# EDA:

# median and IQR by group

summarize_by_class <- function(data, column) {
  
  data %>% filter(!is.na(.data[[column]]), !is.na(final_class)) %>%
    group_by(final_class) %>%
    summarise(n = n(),
              median = median(.data[[column]], na.rm = TRUE),
              lower_quartile = quantile(.data[[column]], 0.25, na.rm = TRUE),
              upper_quartile = quantile(.data[[column]], 0.75, na.rm = TRUE),
              mean = mean(.data[[column]], na.rm = TRUE),
              .groups = "drop")
  
}


# capital value (ECAN, drop the $0 and blank ones)

value_summary <- summarize_by_class(ecan_classified %>% filter(CapitalValue > 1000), "CapitalValue")

write_csv(value_summary, "outputs_rq3/eda_capital_value.csv")


# land share of value (ECAN)

ecan_classified <- ecan_classified %>%
  mutate(land_share = if_else(CapitalValue > 0, LandValue / CapitalValue, NA_real_))

land_share_summary <- summarize_by_class(ecan_classified %>% filter(!is.na(land_share), land_share >= 0,
                                                                    land_share <= 1), "land_share")

write_csv(land_share_summary, "outputs_rq3/eda_land_share.csv")


# site size (ECAN)

site_summary <- summarize_by_class(ecan_classified %>% filter(!is.na(site_ha), site_ha > 0,
                                                              site_ha < 100), "site_ha")

write_csv(site_summary, "outputs_rq3/eda_site_ha.csv")


# valuation roll: values, floor area, bedrooms by class (Chch and Selwyn)

for (column in c("capital_value", "land_value", "building_total_floor_area",
                 "no_of_bedrooms", "land_area")) {
  
  result <- valuation %>% filter(!is.na(.data[[column]]), .data[[column]] > 0) %>%
    group_by(district, final_class) %>%
    summarise(n = n(), median = median(.data[[column]], na.rm = TRUE),
              mean = mean(.data[[column]], na.rm = TRUE), .groups = "drop")
  
  write_csv(result, paste0("outputs_rq3/eda_valroll_", column, ".csv"))
  
}


# tests, Urban vs Rural only (peri-urban and uncertain left out)

urban_rural_only <- ecan_classified %>% filter(final_class %in% c("Urban", "Rural"),
                                               CapitalValue > 1000)

test_value <- wilcox.test(CapitalValue ~ final_class, data = urban_rural_only)

test_land_share <- wilcox.test(land_share ~ final_class,
                               data = urban_rural_only %>% filter(!is.na(land_share)))

test_category <- chisq.test(table(urban_rural_only$final_class,
                                  str_sub(urban_rural_only$Category, 1, 2)))

sink("outputs_rq3/tests.txt")

cat("Capital value (Wilcoxon): W =", round(test_value$statistic), " p =", format.pval(test_value$p.value), "\n")
cat("Land share (Wilcoxon): W =", round(test_land_share$statistic), " p =", format.pval(test_land_share$p.value), "\n")
cat("Category prefix (Chi-sq): X2 =", round(test_category$statistic, 1), " p =", format.pval(test_category$p.value), "\n")

sink()


# interaction check, does the urban premium change by district
# log(CV) on class * TLA, the interaction is the RQ3 answer in one model

model_data <- ecan_classified %>%
  filter(final_class %in% c("Urban", "Rural"),
         CapitalValue > 1000, TLA %in% c("060", "062", "059", "063", "064")) %>%
  mutate(log_cv = log(CapitalValue),
         final_class = factor(final_class, levels = c("Rural", "Urban")),
         TLA = factor(TLA))

value_model <- lm(log_cv ~ final_class * TLA, data = model_data)

capture.output(summary(value_model), file = "outputs_rq3/model_urban_x_tla.txt")


# final split bar

ggplot(as.data.frame(class_counts),
       aes(x = reorder(final_class, n), y = n, fill = final_class)) +
  geom_col(width = 0.8, show.legend = FALSE) +
  coord_flip() +
  geom_text(aes(label = paste0(format(n, big.mark = ","), " (", percent, "%)")),
            hjust = -0.12, size = 3.5) +
  scale_y_continuous(
    labels = scales::comma,
    expand = expansion(mult = c(0, 0.3))) +
  labs(
    title = "Final classification (ECAN, n=329k)",
    x = "Class",
    y = "# of Properties",
    fill = "Class"
  ) +
  theme_bw()

ggsave("outputs_rq3/plot_final_split.png", width = 8, height = 4, dpi = 150)


# capital value distributions

value_plot_data <- ecan_classified %>% filter(CapitalValue > 1000, CapitalValue < 5e6)

ggplot(value_plot_data, aes(x = final_class, y = CapitalValue, fill = final_class)) +
  geom_boxplot(outlier.size = 0.4) +
  scale_y_continuous(labels = scales::dollar_format(prefix = "$")) +
  labs(
    title = "Capital value by urban/rural class",
    x = "Class",
    y = "Capital value",
    fill = "Class"
  ) +
  theme_bw()

ggsave("outputs_rq3/plot_capital_value.png", width = 8, height = 5, dpi = 150)


# land share density

share_plot_data <- ecan_classified %>% filter(!is.na(land_share), land_share >= 0, land_share <= 1) %>%
  filter(final_class %in% c("Urban", "Rural", "Peri-urban"))

ggplot(share_plot_data, aes(x = land_share, fill = final_class)) +
  geom_density(alpha = 0.35) +
  labs(
    title = "Land share of capital value",
    x = "Land value / Capital value",
    y = "Density",
    fill = "Class"
  ) +
  theme_bw()

ggsave("outputs_rq3/plot_land_share.png", width = 8, height = 4.5, dpi = 150)


# median CV by TLA and class

tla_plot_data <- ecan_classified %>% filter(CapitalValue > 1000,
                                            final_class %in% c("Urban", "Rural")) %>%
  group_by(TLA, final_class) %>%
  summarise(median_value = median(CapitalValue), .groups = "drop") %>%
  mutate(tla_label = paste0(recode(TLA, !!!tla_names), " (", TLA, ")"))

ggplot(tla_plot_data, aes(x = reorder(tla_label, median_value), y = median_value, fill = final_class)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(
    title = "Median capital value, urban vs rural in each TLA",
    x = "TLA",
    y = "Median capital value",
    fill = "Class"
  ) +
  theme_bw()

ggsave("outputs_rq3/plot_median_by_TLA.png", width = 8, height = 5, dpi = 150)
