# Functions for  facilitating the import_consumption module

check_consumption_dataset <- function(dataset){
  
  n_row <- nrow(dataset)
  
  missing <- 
    dataset %>%
    purrr::map_df(~ sum(is.na(.))) %>%
    tidyr::gather("Column","missing") %>%
    mutate(pct_missing = percent(accuracy= 0.001, missing/n_row))
  
  specs <- 
    tibble::tribble(
      ~"Column"      , ~"Valid type", ~"Valid content",
      "SERIAL"        , "character",  "Unique values",
      "SUBJECTID"     , "character",  "Free Text/ Numbers",
      "DAY"           , "numeric",    "Integer & Positive value <=5",
      "AMOUNTFOOD"    , "numeric",    "Positive value",
      "AMOUNTFCOOKED" , "numeric",    "Positive value",
      "FOODEXCODE"    , "character",  "termCode found in'MTX' database",
      "GENDER"        , "character",  "One of (MALE,FEMALE, Other)",
      "AGE"           , "numeric",    "Positive value",
      "WEIGHT"        , "numeric",    "Positive value",
      "AREA"          , "character",  "Free text",
      "POP_CLASS"     , "character",  "EFSA population class [Infants, Toddlers, Other children, Adolescents, Adults, Elderly, Pregnant Women]",
      "WCOEFF"        , "numeric",    "Positive value",
    )
  
  specs$Column <- tolower(specs$Column)
  
  rule_type_needed <- specs[c("Column", "Valid type")]
  
  
  col_type <-
    dataset %>%
    # No check are made on the subjectid or area or serial.
    purrr::map_df(typeof) %>%
    tidyr::gather("Column","type_read") %>%
    mutate(type_read = if_else(type_read == "double", "numeric", type_read)) %>% 
    left_join(rule_type_needed, by = "Column") %>%
    mutate(
      is_type_ok = type_read == .data[['Valid type']]
    )
  
  # after the check with types
  # eg. "fff" >0 is TRUE
  
  #sample_string <- "A00DK#F19.A07PR$F22.A07SH$F28.A07MS"
  #regex_valiidte <- "^.{5}"
  
  rule_valid_content <- specs[c("Column", "Valid content")]
  
  dups_serial <- 
    dataset %>% 
    tibble::rowid_to_column() %>% 
    group_by(serial) %>% filter(n()>1) %>% 
    pull(rowid)
  
  
  # TODO need to check the F28 .i.e. FOOL  foodex code ith F28
  
  # tbl indicating whether there is an error (TRUE) or not (FALSE)
  ind_dta <- 
    dataset %>%
    tibble::rowid_to_column() %>% 
    mutate(
      #serial       = n_distinct(serial) !=  nrow(dataset),
      subjectid     = FALSE,
      day           = !(day == round(day) & day>0 & day<6) & !is.na(amountfood),
      amountfood    = amountfood<0 & !is.na(amountfood),
      amountfcooked = amountfcooked<0 & !is.na(amountfcooked), 
      foodexcode    = !stringr::str_extract(foodexcode, "^.{5}") %in% mtx_levels$termCode & !is.na(foodexcode),
      gender        = !gender %in% c("MALE", "FEMALE", "Other") & !is.na(gender),
      age           = age<0 & !is.na(age),
      weight        = weight<0 & !is.na(weight),
      area          = FALSE,
      pop_class     = !pop_class %in% efsa_pop_class & !is.na(pop_class),
      wcoeff        = wcoeff<0 & !is.na(wcoeff),
    ) %>% 
    mutate(
      serial = if_else(rowid %in% dups_serial, TRUE, FALSE)
    )
  
  
  col_content <- 
    ind_dta %>% 
    select(-rowid) %>% 
    summarise_all(sum) %>% 
    tidyr::gather("Column",'Invalid Cases') %>%
    left_join(rule_valid_content, by = "Column") %>%
    relocate('Invalid Cases', .after= last_col()) %>%
    mutate(is_content_ok = .data[['Invalid Cases']] == 0 )
  
  
  problem_cols <-
    col_content %>%
    filter(!is_content_ok) %>%
    pull(Column)
  
  
  
  tbl_problem_ids <- 
    tibble( col = names(ind_dta %>% select(-rowid))) %>% 
    mutate(
      indx = purrr::map(col, function(x) {
        
        ind_dta[ind_dta[[x]],] %>% pull(rowid)
      }
      )
    )
  
  
  problem_ids <- tbl_problem_ids$indx %>% Reduce(c, .) %>% unique()
  
  return(
    dplyr::lst(
      n_row,
      missing,
      col_type,
      col_content,
      problem_cols,
      tbl_problem_ids,
      problem_ids
    )
  )
  
}
