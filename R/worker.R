# This code tries to extract the function information for all CRAN packages
example = function() {
  work_dir = "C:/libraries/pkgFunIndex/work"
  runs = 2
  analyse_some_cran_pkg_funs(work_dir, runs=10)
  from=1
  to = 4

}


analyse_some_cran_pkg_funs = function(work_dir, runs = 2, clear.sources = TRUE, wait.sec = 2) {
  restore.point("analyse_some_cran_pkg_funs")
  cran_toc_file = file.path(work_dir, "cran.toc.rds")
  if (!file.exists(cran_toc_file)) update_cran_toc(work_dir)
  cran_df = readRDS(cran_toc_file)
  cran_df = cran_df[cran_df$Package != "R",]
  cran_df = arrange(cran_df, Package, Published)
  done_file = file.path(work_dir, "done.Rds")


  if (file.exists(done_file)) {
    done_df = readRDS(done_file)
    cran_df = anti_join(cran_df, done_df, by=c("Package","Version"))
  } else {
    done_df = NULL
  }

  fun_li = vector("list", runs)
  is_err = rep(FALSE, runs)
  row = 1
  source_dir = file.path(work_dir,"sources")
  if (!dir.exists(source_dir)) dir.create(source_dir)
  for (row in 1:runs) {
    entry = cran_df[row,]
    cat(paste0("\n",row," of ", runs, " ", entry$Package, " ", entry$Version,"\n"))
    pkg_file = download_pkg_source(pkg = entry$Package,version = entry$Version,dir = source_dir)
    if (is.null(pkg_file)) {
      is_err[row] = TRUE
      next
    }
    fun_df = try(extract_function_info_from_source_pkg(pkg_file))
    if (is(fun_df, "try-error")) {
      is_err[row] = TRUE
      next
    }
    fun_li[[row]] = fun_df
    #file.remove(pkg_file)
    Sys.sleep(wait.sec)
  }

  fun_dir = file.path(work_dir, "fun")
  if (!file.exists(fun_dir)) dir.create(fun_dir)
  fun_files = list.files(fun_dir, glob2rx("*.Rds"),full.names = FALSE)
  inds = str.between(fun_files,"funs_", ".") %>% as.integer()
  batch = max(max(inds)+1,1)

  fun_df = bind_rows(fun_li)
  saveRDS(fun_df, paste0(fun_dir, "/funs_", batch, ".Rds"))
  new_done_df = cran_df[1:runs, c("Package","Version")]
  new_done_df$is_err = is_err
  new_done_df$batch = batch

  saveRDS(bind_rows(done_df, new_done_df), paste0(work_dir,"/done.Rds"))

  if (clear.sources) {
    sources_dir = file.path(work_dir, "sources")
    source.files = list.files(sources_dir, glob2rx("*.tar.gz"), full.names = TRUE)
    file.remove(source.files)
  }

  invisible(fun_df)
}

update_cran_toc = function(work_dir) {
  cat("\nDownload groundhog CRAN data set...\n")
  cran_toc_file = file.path(work_dir, "cran.toc.rds")
  download.file("http://groundhogr.com/cran.toc.rds",cran_toc_file)

}

remove_pkg_source_archives = function(work_dir) {
  source_dir = file.path("work_dir")
  files = list.files(source_dir)
}

get_fun_batch_files = function(work_dir, from=NULL, to = NULL) {
  fun_dir = file.path(work_dir, "fun")
  fun_files = list.files(fun_dir, glob2rx("*.Rds"),full.names = FALSE)
  batches = str.between(fun_files,"funs_", ".") %>% as.integer()
  use = rep(TRUE, NROW(batches))
  if (!is.null(from)) {
    use[batches < from] = FALSE
  }
  if (!is.null(to)) {
    use[batches > to] = FALSE
  }
  paste0(fun_dir, "/",fun_files[use])
}

read_fun_batches = function(work_dir, from=NULL, to = NULL) {
  files = get_fun_batch_files(work_dir, from, to)
  res_li = lapply(files, function(file) {
    readRDS(file)
  })
  bind_rows(res_li)
}

remove.faulty.batch = function(work_dir, faulty_batch) {
  done_file = file.path(work_dir, "done.Rds")
  done_df = readRDS(done_file)
  done_df = filter(done_df, !batch %in% faulty_batch)
  saveRDS(done_df, done_file)
}


merge_batches = function(work_dir, from, to, new_batch = from) {
  batch_files = get_fun_batch_files(work_dir, from, to)
  fun_df = read_fun_batches(work_dir, from, to)
  done_df = readRDS(file.path(work_dir,"done.Rds"))
  done_df$batch[done_df$batch >= from | done_df$batch <=to] = new_batch

  saveRDS(done_df,file.path(work_dir,"done.Rds"))
  file.remove(batch_files)
  new_batch_file = file.path(work_dir,"fun", paste0("funs_",new_batch,".Rds"))
  saveRDS(fun_df, new_batch_file)
}
