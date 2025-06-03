
calc_node_tl_std <- function(fw) {
  TL <- NetIndices::TrophInd(as.matrix(fw))[, 1]
  names(TL) <- rownames(fw)

  return(TL)
}

calc_node_tl_all <- function(fw, method = NULL) {
  if (is.null(method)) {
    all_tls <- cbind(
      `standard` = calc_node_tl_std(fw),
      `network` = calc_node_tl_nw(fw),
    
      `short-weighted` = calc_node_tl_sw(fw)
    )
  } else {
    if (method == "standard") {
      all_tls <- calc_node_tl_std(fw)
    } else if (method == "short-weighted") {
      all_tls <- calc_node_tl_sw(fw)

    
    } else if (method == "network") {
      all_tls <- calc_node_tl_nw(fw)
    } else {

    }
  }

  return(all_tls)
}