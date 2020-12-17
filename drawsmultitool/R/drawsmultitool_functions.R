# publish_column_cell, draws_percent_change, and draws_mean functions

#' Merges Value and Uncertainty values into one single cell
#'
#' @param col_name Common column header for all three columns. Format 'col_name_val'.
#' @param dt A data table with the three columns you want to merge.
#' @param decimals Decimal places.
#'
#' @return A data table with the merged columns specified.
#' @export
#'
#' @examples
#' publish_column_cell(col_name = "pct", dt = dt, decimals = 0 )
publish_column_cell<-function(col_name,dt,decimals){
  # Breakpoints
  if(!paste0(col_name,"_val") %in% colnames(dt)){
    stop("Error: In your dataset make sure that the following column format is included, 'pct_value', pct will be specified by the col_name argument - col_name can be any label, as long as its included in your data.")
  } else if(!paste0(col_name, "_lower") %in% colnames(dt)){
    stop("Error: In your dataset make sure that the following column format is included, 'pct_lower', pct will be specified by the col_name argument - col_name can be any label, as long as its included in your data.")
  } else if(!paste0(col_name, "_upper") %in% colnames(dt)){
    stop("Error: In your dataset make sure that the following column format is included, 'pct_upper', pct will be specified by the col_name argument - col_name can be any label, as long as its included in your data.")
  }

  # Checking/Coercing to data table
  dt <- as.data.table(dt)

  # Cell Merging --------------------
  dt[,(col_name):= paste0(trimws(format(round(get(paste0(col_name,"_val")), decimals), big.mark = "")), "\n(",
                          trimws(format(round(get(paste0(col_name,"_lower")), decimals), big.mark = "")), " to ",
                          trimws(format(round(get(paste0(col_name,"_upper")), decimals), big.mark = "")), ")")]

  #Clean up
  delete_cols<-paste0(col_name,c("_val","_lower","_upper"))
  dt[,(delete_cols):=NULL]
  return(dt)
}

#' Calculates percent change of get_draws for specified years
#'
#' @param draws A get_draws data set.
#' @param pops A get_population data set.
#' @param locs A get_location_metadata data set.
#' @param year_start The desired year_start for percent change.
#' @param year_end The desired year_end for percent change.
#'
#' @return A data table with percent change for draws, for the specified years.
#' @export
#'
#' @examples
#' draws_percent_change(draws = get_draws, pops = get_population, locs = get_location_metadata, year_start = "1980", year_end = "2015")
draws_percent_change <- function(draws, pops, locs, year_start, year_end){
  # Breakpoints -------------------------------
  if(!is.character(year_start)){
    stop("Error: Please use a character string for year_start and year_end")
  } else if(!is.character(year_end)){
    stop("Error: Please use a character string for year_start and year_end")
  } else if(!all(c("age_group_id", "year_id", "sex_id", "population") %in% colnames(pops))){
    stop("NOTE - Please make sure that your pops dataset contains: 'age_group_id', 'year_id', 'sex_id', and 'population' ~ Make sure your original call contains these metrics.")
  } else {
    warning("NOTE - Please make sure that all your get_draws and get_population datasets contain the same metrics. Including, but not limited to the desired 'year_id'")
  }

  # Upload Data -------------------------------
  locs <- select(locs, location_id)
  pops <- select(pops, age_group_id, location_id, year_id, sex_id, population)
  pct_change <- "pct_change"

  # Melting & Casting -------------------------
  final_draws <- draws %>% melt(id.vars = c("age_group_id", "location_id", "sex_id", "year_id", "measure_id", "cause_id", "metric_id"))
  # Merging Modifications
  final_merge <- merge(final_draws, pops, by = c("age_group_id", 'year_id', 'location_id', 'sex_id'), all.x = TRUE)
  final_merge <- mutate(final_merge, value = value*population)
  final_merge <- final_merge[, 'population' := NULL]
  # Casting Calculations
  final_cast <- dcast(final_merge, formula = measure_id + cause_id + metric_id + age_group_id + location_id + sex_id + variable ~ year_id, value.var = 'value') %>% as.data.table()
  # Percent Change
  final_cast[, paste0(pct_change) := 100*(get(year_end) - get(year_start))/get(year_start)]
  final_cast <- final_cast[, c(year_start, year_end) := NULL]
  final <- dcast(final_cast, formula = age_group_id + location_id + sex_id + cause_id + measure_id + metric_id ~ variable, value.var = 'pct_change') %>%
    as.data.frame()

  # Uncertainty Intervals ----------------------
  final <- final[c(
    names(final)[grepl("draw", names(final))],
    names(final)[grepl("draw", names(final))==FALSE])]
  # Lower and Upper UI's, using the apply function instead of a loop.
  final <- mutate(final, pct_val = rowMeans(final[,1:1000], na.rm = T))
  final <- mutate(final, pct_lower = apply(final[,1:1000], 1, quantile, probs=0.025, na.rm = T))
  final <- mutate(final, pct_upper = apply(final[,1:1000], 1, quantile, probs=0.975, na.rm = T))

  # Final Conversion ----------------------------
  final <- final %>% select(age_group_id, sex_id, location_id, cause_id, measure_id, metric_id, pct_val, pct_lower, pct_upper) %>%
    as.data.table()
}

#' Calculates mean of get_draws for specified years
#'
#' @param draws A get_draws data set.
#' @param pops A get population data set.
#' @param locs A get_location_metadata data set.
#' @param year_id The specified single year_id.
#'
#' @return A data table with mean for draws, for the specified year.
#' @export
#'
#' @examples
#' draws_mean(draws = get_draws, pops = get_population, locs = get_location_metadata, year_id = "2015")
draws_mean <- function(draws, pops, locs, year_id){
  # Breakpoints --------------------------
  if(!is.character(year_id)){
    stop("Error: Please use a character string for year_start and year_end")
  } else if(!all(c("age_group_id", "year_id", "sex_id", "population") %in% colnames(pops))){
    stop("NOTE - Please make sure that your pops dataset contains: 'age_group_id', 'year_id', 'sex_id', and 'population' ~ Make sure your original call contains these metrics.")
  } else {
    warning("NOTE - Please make sure that all three data inputs contain the same metrics as called. Including, but not limited to the desired 'year_id'")
  }
  # Selection
  locs <- select(locs, location_id)
  pops <- select(pops, age_group_id, location_id, year_id, sex_id, population)
  #pct_change <- "mean"

  # Melting & Casting -------------------------
  final_draws <- draws %>% melt(id.vars = c("age_group_id", "location_id", "sex_id", "year_id", "measure_id", "cause_id", "metric_id"))
  # Merging Modifications
  final_merge <- merge(final_draws, pops, by = c("age_group_id", 'year_id', 'location_id', 'sex_id'), all.x = TRUE)
  final_merge <- mutate(final_merge, value = value*population)
  final_merge <- final_merge[, 'population' := NULL]
  # Casting Calculations
  final_cast <- dcast(final_merge, formula = measure_id + cause_id + metric_id + age_group_id + location_id + sex_id + variable ~ year_id, value.var = 'value')
  final <- dcast(final_cast, formula = age_group_id + location_id + sex_id + cause_id + measure_id + metric_id ~ variable, value.var = paste0(year_id)) %>%
    as.data.frame()

  # Uncertainty Intervals ----------------------
  final <- final[c(
    names(final)[grepl("draw", names(final))],
    names(final)[grepl("draw", names(final))==FALSE])]
  #Lower and Upper UI's, using the apply function instead of a loop.
  final <- mutate(final, mean_val = rowMeans(final[,1:1000], na.rm = T))
  final <- mutate(final, mean_lower = apply(final[,1:1000], 1, quantile, probs=0.025, na.rm = T))
  final <- mutate(final, mean_upper = apply(final[,1:1000], 1, quantile, probs=0.975, na.rm = T))

  # Final Conversion ----------------------------
  final <- final %>% select(age_group_id, sex_id, location_id, cause_id, measure_id, metric_id, year_id, mean_val, mean_lower, mean_upper) %>%
    as.data.table()
}

