# Create history of package functions across versions
example = function() {
  work_dir = "C:/libraries/pkgFunIndex/work"
  cran_df = readRDS(file.path(work_dir, "cran.toc.rds"))
  fun_df = read_fun_batches(work_dir)
  hist_df = create_fun_history(work_dir, fun_df, cran_df)
}


create_fun_history = function(work_dir, fun_df, cran_df, save=TRUE) {
  restore.point("create_fun_history")

  # Only study exported functions
  fun_df = filter(fun_df, exported==TRUE)

  fun_df = add_fun_df_date(fun_df, cran_df)

  fun_df = fun_df %>%
    arrange(pkg, fun, date) %>%
    group_by(pkg, fun, is_s3, exported) %>%
    mutate(
      fun_version = 1:n(),
      prev_args_str = ifelse(fun_version==1, args_str,lag(args_str)),
      args_changed = args_str != prev_args_str,
      args_just_added = args_changed &
        stringi::stri_sub(args_str, 1, nchar(prev_args_str)) == prev_args_str,
      breaking_change = args_changed & !args_just_added,
      args_version = compute_change_blocks(args_changed)
    ) %>%
    ungroup() %>%
    mutate(
      args_with_dots = grepl("(^|[,])...($|[,])",args_str)
    )

  # Candidates for breaking changes
  rows = which(fun_df$args_version > 1 & fun_df$args_changed & !fun_df$args_just_added)

  if (length(rows)>0) {
    fun_df$breaking_change[rows] = internal.is.breaking.change(fun_df$args_str[rows],fun_df$args_with_dots[rows],fun_df$args_str[rows-1],fun_df$args_with_dots[rows-1])
  }
  table(fun_df$breaking_change)

  if (save) {
    saveRDS(fun_df, file.path(work_dir, "exp_fun_hist.Rds"))
  }


  # Aggregate function versions over arg_versions
  agg_df = fun_df %>%
    group_by(pkg, fun,is_s3, exported, args_str, args_version, args_just_added, breaking_change) %>%
    summarize(
      num_pkg_versions = n(),
      date_first = min(date),
      date_last = max(date)
    ) %>%
    ungroup()

  if (save) {
    saveRDS(filter(agg_df, exported==TRUE), file.path(work_dir, "exp_fun_short_hist.Rds"))
  }

  agg_df
}

# Write an example for function internal.is.breaking.change
examples.internal.is.breaking.change = function() {
  # Not breaking
  internal.is.breaking.change(args="a,b,...",has_dots = TRUE, prev_args="a,b,c", prev_has_dots = FALSE)
  internal.is.breaking.change(args="a,b,...",has_dots = TRUE, prev_args="a,b,c,...", prev_has_dots = TRUE)
  internal.is.breaking.change(args="a,b,c,...",has_dots = TRUE, prev_args="a,b,c", prev_has_dots = TRUE)
  internal.is.breaking.change(args="a,b,...,c",has_dots = TRUE, prev_args="a,b,...", prev_has_dots = TRUE)

  # Breaking
  internal.is.breaking.change(args="a,b,c,...",has_dots = TRUE, prev_args="a,b", prev_has_dots = FALSE)
  internal.is.breaking.change(args="a,b,...",has_dots = TRUE, prev_args="a,c,...", prev_has_dots = FALSE)



}
# Given that not only an argument is added
internal.is.breaking.change = function(args,has_dots, prev_args, prev_has_dots) {
  restore.point("internal.is.breaking.change")

  # If args has no ... the change is always breaking
  is_breaking = !has_dots

  # Is breaking if left of the ... we have a breaking change
  dotStart = startsWith(args,"...") | startsWith(prev_args,"...")

  lhs = str.left.of(args, ",...",not.found = rep("...", length(args)))
  lhs_prev = str.left.of(prev_args, ",...")
  is_breaking = is_breaking | (!startsWith(lhs_prev,lhs) & !startsWith(lhs,lhs_prev) &!dotStart)

  is_breaking
}

add_fun_df_date = function(fun_df, cran_df) {
  if ("date" %in% colnames(fun_df)) return(fun_df)

  fun_df = left_join(fun_df, select(cran_df, pkg=Package, version=Version, date=Published), by=c("pkg","version"))

  fun_df
}

# Adds more columns to hist_df that describe function changes
add_hist_fun_change_extra_cols = function(hist_df, cran_df) {

  if (!all(c("pkg","version","date") %in% names(cran_df))) {
    cran_df = cran_df %>%
      rename(pkg = Package, version = Version, date=Published)
  }

  cran_df = cran_df %>%
    semi_join(hist_df, by=c("pkg"))

  all_vers = cran_df %>%
    select(pkg, version, date) %>%
    unique() %>%
    arrange(pkg, date, version) %>%
    group_by(pkg) %>%
    mutate(
      version_num = 1:n(),
      prev_version = lag(version),
      prev_date = lag(date)
    )

  hist_df = hist_df %>%
    left_join(all_vers, by=c("pkg", "version", "date"))

  hist_df = hist_df %>%
    arrange(pkg, fun, date, version) %>%
    group_by(pkg) %>%
    mutate(max_version_num = max(version_num)) %>%
    group_by(pkg, fun) %>%
    mutate(
      fun_is_added = version_num > 1 & is.true(lag(version_num)!=version_num-1),
      fun_will_be_removed =
        (version_num == max(version_num) &
         version_num < max_version_num),
      fun_will_be_temporarily_removed =
        is.true(lead(version_num) != version_num+1),
      fun_is_fine = !fun_will_be_removed & !fun_will_be_temporarily_removed & !breaking_change
    ) %>%
    ungroup()

  hist_df
}

