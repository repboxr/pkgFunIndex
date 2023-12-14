example = function() {
  df = readRDS("C:/libraries/pkgFunIndex/pkgFunIndex/inst/cran.toc.rds")
  row = 1
  pkg = df$Package[row]
  version = df$Version[row]
  dir = parent_dir =  "C:/libraries/pkgFunIndex"

  pkg_file = download_pkg_source(pkg, version, dir)
  extract_slim_pkg(pkg, pkg_file, parent_dir)

  df = extract
  getNamespaceExports(ns)
}

download_pkg_source = function(pkg, version, dir, overwrite=FALSE) {
  short_file = paste0(pkg,"_",version,".tar.gz")
  dest_file = file.path(dir,short_file)
  if (!overwrite) {
    if (file.exists(dest_file)) return(dest_file)
  }
  url = paste0("https://cran.r-project.org/src/contrib/Archive/",pkg,"/",short_file)
  res = try(download.file(url, dest_file))

  if (is(res,"try-error")) {
    url = paste0("https://cran.r-project.org/src/contrib/", short_file)
    res = try(download.file(url, dest_file))
    if (is(res,"try-error")) {
      cat("\nCould not find file ", short_file)
      return(NULL)
    }
  }
  return(dest_file)
}
