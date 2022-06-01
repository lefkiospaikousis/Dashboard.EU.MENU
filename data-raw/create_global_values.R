## code to prepare `global_values` dataset goes here

library(dplyr)
library(stringr)



# Datasets ----------------------------------------------------------------

path <- "C:/Users/pcuser/OneDrive/IMPROVAST/SGL/Data/"

mtx_parent             <- readRDS(paste0(path, "mtx_parent.rds"))
fdx2_chain_name        <- readRDS(paste0(path, "fdx2_chain_name.rds"))
fdx2_chain_code        <- readRDS(paste0(path, "fdx2_chain_code.rds"))
fdx2_chain_hierararchy <- readRDS(paste0(path, "fdx2_chain_hierarchy.rds"))
mtx_levels             <- fdx2_chain_code[c("termCode", "termExtendedName" ,"depth")]
fdx2_list_simple       <- readRDS(paste0(path, "fdx2_list_simple.rds"))
fdx2_list_explicit     <- readRDS(paste0(path, "fdx2_list_explicit.rds"))
#fdx2_tree              <- readRDS(paste0(path, "fdx2_tree.rds")) 


# FACET
cooking_facets <- 
  readxl::read_xlsx(paste0(path, "COOKING FACETS.xlsx")) %>%
  dplyr::select(termExtendedName = PROCESS, termCode=FOODEXCODE, depth = LEVEL)

process_facets <- readRDS(paste0(path, "F28_facets.rds"))


# Vectors of values  ------------------------------------------------------

efsa_pop_class    <- c("Infants", "Toddlers","Other children", "Adolescents", "Adults", "Elderly","Pregnant Women")

scenarios         <- c("LB", "MB",  "UB")

fdx2_levels_names <- paste0("Level ", 1:7)

sub_categories    <- c("Additive", "Pesticide", "Veterinary Drug Residue", "Contaminant", "Genotoxic-Carcinogen")
sub_ref_type      <- c("Acceptable Intake", "Tolerable Intake", "Provisional Maximum Tolerable Intake", "Benchmark Dose Level (BMDL)")
sub_frequency     <- c("DAILY"  =  1, "WEEKLY"  = 7)

impro_colours <- c(
  "#a6bddb",
  "#756bb1",
  "#2ca25f"
  
)


# For database checking
vars_needed_occurrenceFdx2 <- c("termCode","N", "LB_mean", "MB_mean", "UB_mean")
vars_needed_consumptionFdx2 <- tolower(c("SERIAL", "SUBJECTID", "DAY", "AMOUNTFOOD", 
                                         "AMOUNTFCOOKED", "FOODEXCODE", "GENDER", 
                                         "AGE", "WEIGHT", "AREA", "POP_CLASS", 
                                         "WCOEFF", 
                                         
                                         "ORFOODNAME", "ENFOODNAME", "ENRECIPEDESC"
                                         )
                                       )
vars_valid_consumption_foodex1 <- tolower(c("SERIAL", "SUBJECTID", "DAY", "FOODEX1", "FOODEX1_name", "GENDER", "AGE", "WEIGHT", "AREA", "POP_CLASS", "WCOEFF"))
vars_needed_RawOccurrence <- c("foodex2", "res_num", "lod", "t_uom")


# SAMPLE DATASETS. READY FOR SHINY APP ------------------------------------

consumption_sample <- readRDS(paste0(path, "Consumption_EUMENU-FDX2-LOT1_sample.rds")) 

# consumption_sample <- readxl::read_xlsx(paste0(path, "Consumption_EUMENU-FDX2-LOT1_sample.xlsx"))
# 
# saveRDS(consumption_sample, paste0(path, "Consumption_EUMENU-FDX2-LOT1_sample.rds"))

consumption_sample <- 
  consumption_sample %>% 
  rename_all(tolower) %>% 
  select(all_of(vars_needed_consumptionFdx2), any_of(c("FOODEX1_name", "foodex1_name"))) %>% 
  mutate(
    across(c(day, amountfood, amountfcooked, age, weight, wcoeff), as.numeric),
    across(c(serial, subjectid, foodexcode, gender, area, pop_class), as.character)
  ) %>% 
  filter(across(c(amountfood, area, serial, subjectid, foodexcode, age, pop_class, weight, wcoeff), ~!is.na(.))) %>% 
  # add the  foodname. I did not inlude it in the vars_needed. Maybe they have a wrong name there
  mutate(termCode = stringr::str_extract(foodexcode,"^.{5}")) %>% 
  left_join(
    mtx_levels %>% select(termCode, foodname = termExtendedName)
    , by = "termCode"
  ) %>% 
  select(-termCode) %>% 
  relocate(foodname, .after =  foodexcode)


aggregate_summary <- list(
  #n      = ~dplyr::n(),
  min    = ~min(., na.rm = TRUE),
  mean   = ~mean(., na.rm = TRUE),
  sd     = ~sd(., na.rm = TRUE),
  median = ~median(., na.rm = TRUE),
  p95    = ~quantile(., 0.95),
  max    = ~max(., na.rm = TRUE)
  
)

labels_list <- list(
  
  "Gender" = "gender", 
  "Population Class" = "pop_class", 
  "Area" = "area",
  "Weight" = "weight",
  "Age" = "age",
  "Weight coefficient" ="wcoeff",
  "Subject ID" = "subjectid",
  
  
  "Serial" = "serial",
  "Day" = "day",
  "Amount of food (cooked)" = "amountfcooked",
  "Amount of food (raw)" = "amountfood",
  "Food name (fdx2)" = "foodname",
  "FoodEx code" = "foodexcode",
  "Original Food Name" = "orfoodname",
  "Recipe description" = "enrecipedesc",
  "English food name" = "enfoodname",
  
  "n_days" = "Number of days",
  "Total" = "total",
  
  
  "grams/day" = "gr_day", 
  "grams/day/Kg bw"= "gr_day_kg_bw",
  
  "FoodEx1 Name" = "foodex1_name", 
  "FoodEx1" = "foodex1",
  
  
  "Participants" = "population",
  "Consumers" = "consumers",
  "% of consumers" = "pct_cons",
  "Mean" = "mean",
  "Median" = "median",
  "SD" = "sd",
  "95th percentile" = "p95",
  "Max"  = "max",
  "Min" = "min"
)


usethis::use_data(
  mtx_parent
  , fdx2_chain_name
  , fdx2_chain_code
  , fdx2_chain_hierararchy
  , mtx_levels
  , fdx2_list_simple
  , fdx2_list_explicit
  , cooking_facets
  , process_facets
  , scenarios
  , sub_categories
  , sub_ref_type
  , sub_frequency 
  , fdx2_levels_names
  , vars_needed_consumptionFdx2
  , vars_valid_consumption_foodex1
  , vars_needed_RawOccurrence
  , vars_needed_occurrenceFdx2
  #, fdx2_tree
  , efsa_pop_class
  , aggregate_summary
  , impro_colours
  , internal = TRUE
  , consumption_sample
  , labels_list
  #, internal = FALSE
  , overwrite = TRUE)


