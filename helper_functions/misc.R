save_model <- function(type){ 
  obj <- paste(d, type, sep = "_")
  assign(obj, mdl)
  save(list = obj,
       file = glue::glue("data/{obj}.rda"),
       compress = "xz")
}