extract_units <- function(vec){
  vec |> 
  str_trim() |> 
  str_extract("[a-zA-Z]+$")
}

remove_units <- function(vec){
  vec |> 
    str_trim() |> 
    str_remove("[a-zA-Z]+$") |> 
    str_trim() 
}

format_area_units <- function(vec){
  paste0(vec,"^2")
}

convert_numeric_value_to_char <- function(df){
  df$value <- as.character(df$value)
  
  df
}

split_on_x_num_vals <- function(vec, index){
  vec |> str_split("x") |> 
  map(possibly(
    function(x) x[[index]], otherwise = NA_character_
    )
    ) |> 
  list_c() |> 
  str_trim() |> 
    str_extract("\\d+\\.?\\d*")
}
