save_model <- function(obj, type){ 
  nam <- paste(d, type, sep = "_")
  assign(nam, obj)
  save(list = nam,
       file = glue::glue("data/{nam}.rda"),
       compress = "xz")
}