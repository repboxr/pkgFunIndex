
is.true = function(x) {
  x[is.na(x)] = FALSE
  x
}

compute_change_blocks = function(changed) {
  cummax(rle_block(changed, ignore_val = FALSE))+1
}

rle_block = function(x, ignore_val=NULL) {
  rle = rle(x)
  block_vals = seq_along(rle$values)
  if (length(ignore_val)>0) {
    zero_rows = rle$values %in% ignore_val
    block_vals[zero_rows] = 0
    uni = unique(block_vals[!zero_rows])
    block_vals[!zero_rows] = match(block_vals[!zero_rows],uni)
  }
  rep(block_vals, rle$lengths)
}
