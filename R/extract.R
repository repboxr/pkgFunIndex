example = function(pkg_file) {
  pkg_file = "C:/libraries/pkgFunIndex/arrow_14.0.0.2.tar.gz"
}

extract_function_info_from_source_pkg = function(pkg_file, keep_code=FALSE) {
  restore.point("extract_function_info_from_source_pkg")
  base = basename(pkg_file)
  pkg = str.left.of(base, "_")
  version = str.between(base, "_", ".tar.gz")

  cat("\n  Extract R function code from", base,"\n")
  fun_df = extract_r_functions_from_archive(pkg_file)

  cat("\n  Map NAMESPACE exports")
  fun_df = add_namespace_info_to_fun_df(pkg_file, fun_df)

  cat("\n Parse all function arguments")
  fun_df = add_fun_df_args_str(fun_df)


  fun_df$pkg = rep(pkg, NROW(fun_df))
  fun_df$version = rep(version, NROW(fun_df))

  cols = c("pkg","version","fun", "exported","is_s3","args_str")
  if (keep_code) cols = c(cols, "code")

  fun_df[,cols]

}

add_fun_df_args_str = function(fun_df) {
  restore.point("add_fun_df_args_str")
  fun_df$args_str = args_str = rep("", NROW(fun_df))
  rows = seq_len(NROW(fun_df))
  if (length(rows)==0) return(fun_df)

  row = 1
  for (row in rows) {
    code = fun_df$code[row]
    f = try(eval(parse(text=code)))
    if (is(f, "try-error")) {
      args_str[row] = NA_character_
    } else {
      res = names(base::formals(f))
      if (!is.null(res)) {
        args_str[row] = paste0(res, collapse=",")
      }
    }
  }
  fun_df$args_str = args_str
  fun_df
}

add_namespace_info_to_fun_df = function(pkg_file, fun_df) {
  restore.point("add_namespace_info_to_fun_df")

  base = basename(pkg_file)
  pkg = str.left.of(base, "_")


  # Get NAMESPACE
  ns_text = read_text_from_archive_files(pkg_file, file.path(pkg,"NAMESPACE"))
  ns_exp = parse_namespace_exports(text=ns_text)

  all_funs = c(ns_exp$export_fun, ns_exp$s3_df$fun)
  fun_df$exported = fun_df$fun %in% all_funs
  fun_df$is_s3 = fun_df$fun %in% ns_exp$s3_df$fun

  # Mark exports based on patterns
  patterns = ns_exp$patterns
  if (length(patterns)>0) {
    for (pat in patterns) {
      fun_df$exported = fun_df$exported | grepl(pat,fun_df$fun,perl = TRUE)
    }
  }
  fun_df
}

extract_r_functions_from_archive = function(pkg_file) {
  restore.point("extract_r_functions_from_archive")

  base = basename(pkg_file)
  pkg = str.left.of(base, "_")

  # Get R code
  a = archive::archive(pkg_file)

  lfiles = tolower(a$path)
  rows = which(startsWith(lfiles, paste0(paste0(tolower(pkg),"/r/"))) & endsWith(lfiles, ".r"))

  r_files = a$path[rows]
  r_code = read_text_from_archive_files(pkg_file, r_files)

  r_code = iconv(r_code, to="UTF-8")
  # Change modern pipes. So that parsing also works
  # if run in older R versions
  r_code = gsub("|>","%>%", r_code, fixed=TRUE)
  # Parse R code
  somo = sourcemodify::somo_init(code = r_code,add_funid = FALSE)
  as_df = somo$as_df
  pd_df = somo$pd
  rows = as_df$is_fun & as_df$lhs_parent_expr == ""
  fun_df = as_df[rows,]
  fun_df$parent = pd_df$parent[fun_df$id]
  fun_df = fun_df[fun_df$parent <= 0,]

  ignore_funs = c(".onLoad",".onAttach",".onDetach",".onUnload")
  fun_df = fun_df[!fun_df$var %in% ignore_funs,]

  if (NROW(fun_df)==0) return("")

  fun_df$code = sourcemodify::somo_make_pd_code(somo, pd_rows = fun_df$id)
  fun_df = fun_df[, c("id","var","code")]
  names(fun_df)[2] = "fun"
  fun_df
}


read_text_from_archive_files = function(pkg_file, files, as_list=FALSE) {
  temp_dir=tempdir()
  untar(pkg_file,files = files,exdir = temp_dir)

  long_files = file.path(temp_dir, files)
  res = lapply(long_files, function(file) {
    paste0(readLines(file,encoding="UTF-8",warn = FALSE), collapse="\n")
  })
  file.remove(long_files)
  names(res) = files
  if (!as_list) return(unlist(res) %>% enc2utf8())

  res
}
