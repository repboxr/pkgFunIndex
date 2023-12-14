example = function() {
  file = "C:/libraries/pkgFunIndex/arrow/NAMESPACE"
  get_namespace_fun_exports(file=file)
}

parse_namespace_exports = function(file=NULL, text=NULL) {
  restore.point("parse_namespace_exports")
  ns_df = parse_namespace(file, text)
  keep = tolower(ns_df$fun) %in% c("export","exportpattern","s3method")
  ns_df = ns_df[keep,]
  ns_df$code = gsub('"',"",ns_df$code, fixed=TRUE)
  ns_df$code = gsub("'","",ns_df$code, fixed=TRUE)
  ns_df$code = gsub(' ',"",ns_df$code, fixed=TRUE)

  txt = trimws(ns_df$code[ns_df$fun=="export"])
  export_fun = substring(txt, 8, nchar(txt)-1)

  txt = trimws(ns_df$code[ns_df$fun=="exportPattern"])
  export_pattern = substring(txt, 15, nchar(txt)-1)

  txt = trimws(ns_df$code[tolower(ns_df$fun)=="s3method"])
  if (length(txt)>0) {
    txt = substring(txt, 10, nchar(txt)-1)
    s3_method = str.left.of(txt, ",")
    s3_class = str.right.of(txt,",")
    fun = paste0(s3_method, ".", s3_class)
    s3 = data.frame(method=s3_method, class=s3_class, fun=fun)
  } else {
    s3 = data.frame(method=character(0), class=character(0), fun = character(0))
  }


  list(funs=export_fun, patterns = export_pattern, s3_df=s3)
}

parse_namespace = function(file=NULL, text=NULL) {
  restore.point("parse_namespace")
  if (is.null(text) & !is.null(file)) {
    text = readLines(file)
  }
  somo = somo_init(code = paste0(text, collapse="\n"))
  call_df = somo$call_pd %>%
    select(fun, code=text)
  call_df
}
