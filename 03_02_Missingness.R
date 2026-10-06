#   1. missingness by column (bar)
#   2. missingness table (csv)
#   3. joint missing patterns (upset style table + plot)
#   4. missingness by group (TLA for ECAN, suburb size for prices)



# setup

if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")

library(tidyverse)
library(ggplot2, quietly = TRUE)

dir.create("outputs_missingness", showWarnings = FALSE)


# blank strings are missing too

as_missing <- function(x) {

  is.na(x) | (is.character(x) & str_trim(x) == "")

}


# ECAN, load a slim set of columns so it stays fast

ecan <- read_csv("data/ECAN_Property_Values.csv",
                 show_col_types = FALSE, lazy = FALSE) %>%
  select(TLA, LocalityName, StreetAddress, Category, LandUse,
         CapitalValue, LandValue, ImprovementsValue, Improvements,
         RatingHectares)


# standardize blanks to NA first

ecan <- ecan %>%
  mutate(across(where(is.character), ~ na_if(str_trim(.x), "")))


# missingness by column

ecan_col_miss <- ecan %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "field", values_to = "missing_n") %>%
  mutate(missing_pct = round(100 * missing_n / nrow(ecan), 1)) %>%
  arrange(desc(missing_n))

write_csv(ecan_col_miss, "outputs_missingness/ecan_missing_by_column.csv")

ggplot(ecan_col_miss, aes(x = fct_reorder(field, missing_n), y = missing_n)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = paste0(format(missing_n, big.mark = ","),
                               " (", missing_pct, "%)")),
            hjust = -0.1, size = 3.5) +
  coord_flip() +
  scale_y_continuous(
    labels = scales::comma,
    expand = expansion(mult = c(0, 0.35))) +
  labs(
    title = "Missingness by column (ECAN, n=329k)",
    x = "Field",
    y = "Missing rows"
  ) +
  theme_bw()

ggsave("outputs_missingness/ecan_missing_by_column.png",
       width = 8, height = 5, dpi = 150)


# joint missing patterns
# which blanks arrive together to tell MNAR - MCAR

ecan_patterns <- ecan %>%
  transmute(
    no_value = is.na(CapitalValue),
    no_category = is.na(Category),
    no_landuse = is.na(LandUse),
    no_locality = is.na(LocalityName),
    no_improvements = is.na(Improvements)
  ) %>%
  count(no_value, no_category, no_landuse, no_locality, no_improvements,
        sort = TRUE) %>%
  mutate(pct = round(100 * n / sum(n), 2))

write_csv(ecan_patterns, "outputs_missingness/ecan_joint_patterns.csv")


# top 10 patterns plot

ggplot(head(ecan_patterns, 10),
       aes(x = reorder(paste(no_value, no_category, no_landuse,
                             no_locality, no_improvements, sep = "-"), n),
           y = n)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = paste0(format(n, big.mark = ","),
                               " (", pct, "%)")),
            hjust = -0.1, size = 3.5) +
  coord_flip() +
  scale_y_continuous(
    labels = scales::comma,
    expand = expansion(mult = c(0, 0.35))) +
  labs(
    title = "Top 10 joint missing patterns (ECAN)",
    subtitle = "order: no_value, no_category, no_landuse, no_locality, no_improvements",
    x = "Pattern (TRUE = missing)",
    y = "Rows"
  ) +
  theme_bw()

ggsave("outputs_missingness/ecan_joint_patterns.png",
       width = 8, height = 5, dpi = 150)


# missingness by TLA


ecan_by_tla <- ecan %>%
  group_by(TLA) %>%
  summarise(
    n = n(),
    no_locality = sum(is.na(LocalityName)),
    no_category = sum(is.na(Category)),
    no_value = sum(is.na(CapitalValue)),
    .groups = "drop"
  ) %>%
  mutate(
    pct_locality = round(100 * no_locality / n, 1),
    pct_category = round(100 * no_category / n, 1),
    pct_value = round(100 * no_value / n, 1)
  )

write_csv(ecan_by_tla, "outputs_missingness/ecan_missing_by_tla.csv")

ecan_by_tla_long <- ecan_by_tla %>%
  select(TLA, no_locality, no_category, no_value) %>%
  pivot_longer(-TLA, names_to = "field", values_to = "missing_n")

ggplot(ecan_by_tla_long, aes(x = TLA, y = missing_n, fill = field)) +
  geom_col(position = "dodge", width = 0.7) +
  scale_y_continuous(
    labels = scales::comma,
    expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Missingness by TLA (ECAN)",
    x = "TLA",
    y = "Missing rows",
    fill = "Field"
  ) +
  theme_bw()

ggsave("outputs_missingness/ecan_missing_by_tla.png",
       width = 8, height = 5, dpi = 150)


# DVR valuation roll

dvr <- read_csv("nz-properties-national-district-valuation-roll.csv",
                show_col_types = FALSE, lazy = FALSE) %>%
  select(district_ta_code, capital_value, land_value, improvements_value,
         no_of_bedrooms, building_total_floor_area, land_area,
         property_category, zoning, improvements_description,
         annual_value, gross_rental, trees, production)

dvr_col_miss <- dvr %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "field", values_to = "missing_n") %>%
  mutate(missing_pct = round(100 * missing_n / nrow(dvr), 1)) %>%
  arrange(desc(missing_n))

write_csv(dvr_col_miss, "outputs_missingness/dvr_missing_by_column.csv")

ggplot(dvr_col_miss, aes(x = fct_reorder(field, missing_n), y = missing_n)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = paste0(missing_pct, "%")),
            hjust = -0.1, size = 3.5) +
  coord_flip() +
  scale_y_continuous(
    labels = scales::comma,
    expand = expansion(mult = c(0, 0.35))) +
  labs(
    title = "Missingness by column (valuation roll, n=272k)",
    x = "Field",
    y = "Missing rows"
  ) +
  theme_bw()

ggsave("outputs_missingness/dvr_missing_by_column.png",
       width = 8, height = 5, dpi = 150)


# suburb prices, missingness by suburb (thin sales gaps)

# clear any stale suburb object from .RData before reading fresh

rm(suburb, suburb_miss)

suburb <- read_csv("chch_suburb_prices.csv",
                   show_col_types = FALSE, lazy = FALSE)

n_suburb <- nrow(suburb)

# same across pattern as the ECAN and DVR blocks above,
# row count kept outside mutate so nothing resolves oddly

suburb_miss <- suburb %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "suburb", values_to = "missing_n") %>%
  filter(suburb != "Date") %>%
  mutate(missing_pct = round(100 * missing_n / n_suburb, 1)) %>%
  arrange(desc(missing_n))

write_csv(suburb_miss, "outputs_missingness/suburb_missing_by_suburb.csv")

ggplot(head(suburb_miss, 20),
       aes(x = fct_reorder(suburb, missing_n), y = missing_n)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = paste0(missing_n, " (", missing_pct, "%)")),
            hjust = -0.1, size = 3.5) +
  coord_flip() +
  scale_y_continuous(
    labels = scales::comma,
    expand = expansion(mult = c(0, 0.35))) +
  labs(
    title = "Top 20 suburbs by missing months (Chch prices)",
    x = "Suburb",
    y = "Missing months"
  ) +
  theme_bw()

ggsave("outputs_missingness/suburb_missing_by_suburb.png",
       width = 8, height = 5, dpi = 150)
