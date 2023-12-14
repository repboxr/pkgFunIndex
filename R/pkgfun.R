example = function() {
  pkg = "ggplot2"
  df = extract_pkg_funs_info(pkg)
}

#' Extract information about all exported functions of an installed package
#'
#' @param pkg Name the package like "stats"
#' @returns a data frame
#' @export
extract_installed_pkg_funs_info = function(pkg) {
  library(pkg,character.only = TRUE)
  version = as.character(packageVersion(pkg))
  funs = ls(paste0("package:", pkg))
  res_li = lapply(funs,function(fun) {
    internal.extract.installed.pkg.fun.info(pkg, fun, version)
  })
  res_df = as.data.frame(do.call(rbind, res_li))
  res_df
}

internal.extract.installed.pkg.fun.info = function(pkg, fun, version=as.character(packageVersion(pkg))) {
  #restore.point("extract_function_info")
  f = try(eval(parse(text=paste0(pkg,"::`", fun,"`"))), silent=TRUE)
  if (is(f, "try-error")) return(NULL)
  if (!is.function(f)) return(NULL)
  res = names(base::formals(f))
  if (is.null(res)) res = ""
  c(pkg=pkg,version=version, fun=fun, args = paste0(res, collapse=","))
}
