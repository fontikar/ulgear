extract_units <- function(vec){
  vec |> 
  str_trim() |> 
  str_extract("[a-zA-Z]+")
}

format_area_units <- function(vec){
  paste0(vec,"^2")
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
