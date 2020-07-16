print_matrix <- function(mat, rownm = NULL, colnm = NULL, colwidth = 10,
  between_cols = 2, ndigits = 2, shift = 0, isint = FALSE) {
  nr <- nrow(mat)
  nc <- ncol(mat)
  if (is.null(rownm)) {
    rownm <- paste0("Row ", 1:nr)
  }
  if (is.null(colnm)) {
    colnm <- paste0("Col ", 1:nc)
  }
  stopifnot(length(rownm) == nr, length(colnm) == nc)
  maxwidth <- max(nchar(format(round(mat, digits = ndigits), nsmall = ifelse(isint, 0, 2))))
  if (colwidth < maxwidth) colwidth <- maxwidth
  mat_str <- format(round(mat, digits = ndigits), nsmall = ifelse(isint, 0, 2), width = colwidth)
  if (any(is.na(mat))) mat_str <- gsub("NA", " -", mat_str)

  # if (any(nchar(rownm) > colwidth)) {
  #   rownm <- abbreviate(rownm, minlength = colwidth, strict = TRUE, named = FALSE)
  # }
  if (any(nchar(colnm) > colwidth)) {
    colnm <- abbreviate(colnm, minlength = colwidth, strict = TRUE, named = FALSE)
  }
  firstcolwidth <- max(nchar(rownm))
  empty_string_shift <- strrep(" ", shift)
  empty_string_firstcol <- strrep(" ", firstcolwidth)
  empty_string_between_cols <- strrep(" ", between_cols)
  empty_string_rows <- empty_string_cols <- character(nc)

  cat(empty_string_shift, sep = "")
  cat(empty_string_firstcol, sep = "")
  cat(empty_string_between_cols, sep = "")
  for (j in 1:nc) {
    empty_string_cols[j] <- strrep(" ", colwidth - nchar(colnm[j]))
    cat(empty_string_cols[j], colnm[j], sep = "")
    cat(empty_string_between_cols, sep = "")
  }
  cat("\n")
  for (i in 1:nr) {
    empty_string_rows[i] <- strrep(" ", firstcolwidth - nchar(rownm[i]))
    cat(empty_string_shift, sep = "")
    cat(rownm[i], empty_string_rows[i], sep = "")
    cat(empty_string_between_cols, sep = "")
    for (j in 1:nc) {
      cat(mat_str[i, j], sep = "")
      cat(empty_string_between_cols, sep = "")
      if (j == nc) cat("\n")
    }
  }
}

mediate <- function(frmlist, data, indep, med, dep, B = 50, bca = FALSE,
  level = 0.95, fit_plssem) {
  indeff_boot <- function(frm, dat, indices) {
    d <- dat[indices, ]
    fit_tmp <- systemfit::systemfit(frm, method = "SUR", data = d)
    b_tmp <- coef(fit_tmp)
    moi <- b_tmp[paste0(med, "_", indep)]
    dom <- b_tmp[paste0(dep, "_", med)]
    return(dom*moi)
  }
  indeff <- function(x, frm, dat) {
    d <- dat[x, ]
    fit_tmp <- systemfit::systemfit(frm, method = "SUR", data = d)
    b_tmp <- coef(fit_tmp)
    moi <- b_tmp[paste0(med, "_", indep)]
    dom <- b_tmp[paste0(dep, "_", med)]
    return(dom*moi)
  }
  
  cat("Bootstrapping mediation effect...\n\n")

  lvl <- level*100
  low <- (100 - lvl)/2
  upp <- 100 - low
  low <- low/100
  upp <- upp/100
  # res <- bootstrap::bootstrap(1:B, B, indeff, frmlist, data)
  # res_perc <- quantile(res$thetastar, probs = c(c(low, upp)))
  res_boot <- boot::boot(data = data, statistic = indeff_boot, R = B, frm = frmlist)
  res_perc <- boot::boot.ci(res_boot, type = "perc", conf = level)$percent[4:5]
  if (bca) {
    res_bca <- bootstrap::bcanon(1:B, B, indeff, frmlist, data)
    idx <- match(c(low, upp), as.numeric(res_bca$confpoints[, 1]))
    res_bca <- res_bca$confpoints[idx, 2]
  }
  
  fit <- systemfit::systemfit(frmlist, method = "SUR", data = data)
  b <- coef(fit)
  V <- vcov(fit)
  moi_coef <- b[paste0(med, "_", indep)]
  dom_coef <- b[paste0(dep, "_", med)]
  doi_coef <- b[paste0(dep, "_", indep)]

  moi_var <- V[paste0(med, "_", indep), paste0(med, "_", indep)]
  dom_var <- V[paste0(dep, "_", med), paste0(dep, "_", med)]

  prodterm <- dom_coef*moi_coef
 
  indmeas <- matrix(NA, nrow = 6, ncol = 3)
  rownames(indmeas) <- c("Indirect effect", "Standard error",
    "Z statistic", "P-value", "Lower CI", "Upper CI")
  colnames(indmeas) <- c("Sobel", "Delta", "Bootstrap")
  
  # Sobel's method
  sobel_se <- sqrt((dom_coef^2)*moi_var + (moi_coef^2)*dom_var)
  sobel_z <- prodterm/sobel_se
  sobel_pv <- 2*pnorm(abs(sobel_z), lower.tail = FALSE)
  normperc <- qnorm(1 - (1 - level)/2)
  sobel_lci <- prodterm - normperc*sobel_se
  sobel_uci <- prodterm + normperc*sobel_se
  indmeas[1, 1] <- prodterm
  indmeas[2, 1] <- sobel_se
  indmeas[3, 1] <- sobel_z
  indmeas[4, 1] <- sobel_pv
  indmeas[5, 1] <- sobel_lci
  indmeas[6, 1] <- sobel_uci

  # Delta method
  g <- function(cf) {
    moi_i <- which(names(cf) == paste0(med, "_", indep))
  dom_i <- which(names(cf) == paste0(dep, "_", med))
    return(cf[dom_i]*cf[moi_i])
  }
  grad_g <- numDeriv::jacobian(g, b)
  delta_se <- sqrt(as.numeric(grad_g %*% V %*% t(grad_g)))
  delta_z <- prodterm/delta_se
  delta_pv <- 2*pnorm(abs(delta_z), lower.tail = FALSE)
  delta_lci <- prodterm - normperc*delta_se
  delta_uci <- prodterm + normperc*delta_se
  indmeas[1, 2] <- prodterm
  indmeas[2, 2] <- delta_se
  indmeas[3, 2] <- delta_z
  indmeas[4, 2] <- delta_pv
  indmeas[5, 2] <- delta_lci
  indmeas[6, 2] <- delta_uci

  # Bootstrap method
  boot_se <- sd(res_boot$t)
  boot_z <- prodterm/boot_se
  boot_pv <- 2*pnorm(abs(boot_z), lower.tail = FALSE)
  boot_lci <- ifelse(bca, res_bca[1], res_perc[1])
  boot_uci <- ifelse(bca, res_bca[2], res_perc[2])
  indmeas[1, 3] <- prodterm
  indmeas[2, 3] <- boot_se
  indmeas[3, 3] <- boot_z
  indmeas[4, 3] <- boot_pv
  indmeas[5, 3] <- boot_lci
  indmeas[6, 3] <- boot_uci
  
  # baron & kenny
  struct_b <- t(fit_plssem$Estimates$Path_estimates)
  struct_b <- struct_b[which(rowSums(struct_b) != 0), which(colSums(struct_b) != 0)]
  coef <- struct_b
  fit_plssem_sum <- cSEM::summarize(fit_plssem)
  pval_tmp <- fit_plssem_sum$Estimates$Path_estimates[, "p_value"]
  stderr_tmp <- fit_plssem_sum$Estimates$Path_estimates[, "Std_err"]
  pval <- stderr <- coef
  count <- 1
  for (j in 1:ncol(pval)) {
    for (i in 1:nrow(pval)) {
      if (coef[i, j] != 0) {
        pval[i, j] <- pval_tmp[count]
        stderr[i, j] <- stderr_tmp[count]
        count <- count + 1
      }
    }
  }

  # X -> M
  coef_moi <- coef[indep, med]
  moi_pval <- pval[indep, med]
  
  # M -> Y
  coef_dom <- coef[med, dep]
  dom_pval <- pval[med, dep]
  
  # X -> Y
  coef_doi <- coef[indep, dep]
  doi_pval <- pval[indep, dep]

  mat_bk <- matrix(NA, nrow = 7, ncol = 1)
  mat_bk[1, 1] <- coef_doi
  mat_bk[2, 1] <- coef_moi
  mat_bk[3, 1] <- coef_dom
  mat_bk[4, 1] <- doi_pval
  mat_bk[5, 1] <- moi_pval
  mat_bk[6, 1] <- dom_pval
  mat_bk[7, 1] <- sobel_pv

  # zhao, lynch & chen
  axbxc <- coef_moi*coef_dom*coef_doi

  mat_zlc <- matrix(NA, nrow = 4, ncol = 1)
  mat_zlc[1, 1] <- boot_pv
  mat_zlc[2, 1] <- doi_pval
  mat_zlc[3, 1] <- coef_doi
  mat_zlc[4, 1] <- axbxc

  # rit/rid
  ind_eff <- prodterm
  dir_eff <- coef_doi
  tot_eff <- dir_eff + ind_eff

  res_mediate <- list(indmeas = indmeas, bk = mat_bk, zlc = mat_zlc,
    B = B, indep = indep, med = med, dep = dep, level = level, bca = bca,
    ind_eff = ind_eff, dir_eff = dir_eff, tot_eff = tot_eff)

  return(res_mediate)
}

mediate_print <- function(res_mediate, zlc = TRUE, rit = TRUE, rid = TRUE, digits = 3) {
  indmeas <- res_mediate$indmeas
  mat_bk <- res_mediate$bk
  indep <- res_mediate$indep
  med <- res_mediate$med
  dep <- res_mediate$dep
  ind_eff <- abs(res_mediate$ind_eff)
  dir_eff <- abs(res_mediate$dir_eff)
  tot_eff <- abs(res_mediate$tot_eff)
  B <- res_mediate$B
  level <- res_mediate$level
  mat_zlc <- res_mediate$zlc
  rit_val <- ind_eff/tot_eff
  rid_val <- ind_eff/dir_eff

  cat("Significance testing of (standardized) indirect effect\n\n")
  print_matrix(indmeas, rownm = rownames(indmeas), colnm = colnames(indmeas),
    ndigits = digits)
  cat("---\n")
  cat(paste0("confidence level: ", level*100, "%\n"))
  cat(paste0("bootstrap replications: ", B, "\n"))

  cat("\n")
  cat("Baron & Kenny approach to testing mediation\n")
  if (mat_bk[5, 1] > 0.05 | mat_bk[6, 1] > 0.05) {
    cat(paste0("STEP 1 - ", med, ":", indep, " (X -> M) with b = ",
      format(mat_bk[2, 1], digits = digits), " and p = ", format(mat_bk[5, 1], digits = digits), "\n"))
    cat(paste0("STEP 2 - ", dep, ":", med, " (M -> Y) with b = ", format(mat_bk[3, 1], digits = digits), " and p = ", format(mat_bk[6, 1], digits = digits), "\n"))
    cat("         As either STEP 1 or STEP 2 (or both) are not significant,\n")
    cat("         there is no mediation\n")
  }
  else {
    if (mat_bk[5, 1] < 0.05 & mat_bk[6, 1] < 0.05 & mat_bk[7, 1] < 0.05 & mat_bk[4, 1] > 0.05) {
      cat(paste0("STEP 1 - ", med, ":", indep, " (X -> M) with b = ", format(mat_bk[2, 1], digits = digits), " and p = ", format(mat_bk[5, 1], digits = digits), "\n"))
      cat(paste0("STEP 2 - ", dep, ":", med, " (M -> Y) with b = ", format(mat_bk[3, 1], digits = digits), " and p = ", format(mat_bk[6, 1], digits = digits), "\n"))
      cat(paste0("STEP 3 - ", dep, ":", indep, " (X -> Y) with b = ", format(mat_bk[1, 1], digits = digits), " and p = ", format(mat_bk[4, 1], digits = digits), "\n"))
      cat("         As STEP 1, STEP 2 and the Sobel's test above are significant \n")
      cat("         and STEP 3 is not significant the mediation is complete\n")
    }
    else {
      if (mat_bk[5, 1] < 0.05 & mat_bk[6, 1] < 0.05 & mat_bk[7, 1] < 0.05 & mat_bk[4, 1] < 0.05) {
        cat(paste0("STEP 1 - ", med, ":", indep, " (X -> M) with b = ", format(mat_bk[2, 1], digits = digits), " and p = ", format(mat_bk[5, 1], digits = digits), "\n"))
        cat(paste0("STEP 2 - ", dep, ":", med, " (M -> Y) with b = ", format(mat_bk[3, 1], digits = digits), " and p = ", format(mat_bk[6, 1], digits = digits), "\n"))
        cat(paste0("STEP 3 - ", dep, ":", indep, " (X -> Y) with b = ", format(mat_bk[1, 1], digits = digits), " and p = ", format(mat_bk[4, 1], digits = digits), "\n"))
        cat("         As STEP 1, STEP 2 and STEP 3 as well as the Sobel's test above\n")
        cat("         are significant the mediation is partial\n")
      }
      else {
        if (mat_bk[5, 1] < 0.05 & mat_bk[6, 1] < 0.05 & mat_bk[7, 1] > 0.05 & mat_bk[4, 1] < 0.05) {
          cat(paste0("STEP 1 - ", med, ":", indep, " (X -> M) with b = ", format(mat_bk[2, 1], digits = digits), " and p = ", format(mat_bk[5, 1], digits = digits), "\n"))
          cat(paste0("STEP 2 - ", dep, ":", med, " (M -> Y) with b = ", format(mat_bk[3, 1], digits = digits), " and p = ", format(mat_bk[6, 1], digits = digits), "\n"))
          cat(paste0("STEP 3 - ", dep, ":", indep, " (X -> Y) with b = ", format(mat_bk[1, 1], digits = digits), " and p = ", format(mat_bk[4, 1], digits = digits), "\n"))
          cat("         As STEP 1, STEP 2 and STEP 3 are all significant and the\n")
          cat("         Sobel's test above is not significant the mediation is partial\n")
        }
        else {
          if (mat_bk[5, 1] < 0.05 & mat_bk[6, 1] < 0.05 & mat_bk[7, 1] > 0.05 & mat_bk[4, 1] > 0.05) {
            cat(paste0("STEP 1 - ", med, ":", indep, " (X -> M) with b = ", format(mat_bk[2, 1], digits = digits), " and p = ", format(mat_bk[5, 1], digits = digits), "\n"))
            cat(paste0("STEP 2 - ", dep, ":", med, " (M -> Y) with b = ", format(mat_bk[3, 1], digits = digits), " and p = ", format(mat_bk[6, 1], digits = digits), "\n"))
            cat(paste0("STEP 3 - ", dep, ":", indep, " (X -> Y) with b = ", format(mat_bk[1, 1], digits = digits), " and p = ", format(mat_bk[4, 1], digits = digits), "\n"))
            cat("         As STEP 1 and STEP 2 are significant and neither STEP 3 nor\n")
            cat("         the Sobel's test above is significant the mediation is partial\n")
          }
        }
      }
    }
  }

  if (zlc) {
    cat("\n")
    cat("Zhao, Lynch & Chen's approach to testing mediation\n")
    if (mat_zlc[1, 1] < 0.05 & mat_zlc[2, 1] > 0.05) {
      cat(paste0("STEP 1 - ", dep, ":", indep, " (X -> Y) with b = ", format(mat_zlc[3, 1], digits = digits), " and p = ", format(mat_zlc[2, 1], digits = digits), "\n"))
      cat(paste0("         As the bootstrap test above is significant and STEP 1 is not\n"))
      cat(paste0("         significant you have indirect-only mediation (full mediation)\n"))
    } else if (mat_zlc[1, 1] > 0.05 & mat_zlc[2, 1] < 0.05) {
      cat(paste0("STEP 1 - ", dep, ":", indep, " (X -> Y) with b = ", format(mat_zlc[3, 1], digits = digits), " and p = ", format(mat_zlc[2, 1], digits = digits), "\n"))
      cat(paste0("         As the bootstrap test above is not significant and STEP 1 is\n"))
      cat(paste0("         significant you have direct-only nonmediation (no mediation)\n"))
    } else if (mat_zlc[1, 1] > 0.05 & mat_zlc[2, 1] > 0.05) {
      cat(paste0("STEP 1 - ", dep, ":", indep, " (X -> Y) with b = ", format(mat_zlc[3, 1], digits = digits), " and p = ", format(mat_zlc[2, 1], digits = digits), "\n"))
      cat(paste0("         As the bootstrap test above is not significant and STEP 1 is\n"))
      cat(paste0("         not significant you have no effect nonmediation (no mediation)\n"))
    } else if (mat_zlc[1, 1] < 0.05 & mat_zlc[2, 1] < 0.05 & mat_zlc[4, 1] > 0) {
      cat(paste0("STEP 1 - ", dep, ":", indep, " (X -> Y) with b = ", format(mat_zlc[3, 1], digits = digits), " and p = ", format(mat_zlc[2, 1], digits = digits), "\n"))
      cat(paste0("         As the bootstrap test above is significant, STEP 1 is\n"))
      cat(paste0("         significant and their coefficients point in same direction,\n"))
      cat(paste0("         you have complementary mediation (partial mediation)\n"))
    } else if (mat_zlc[1, 1] < 0.05 & mat_zlc[2, 1] < 0.05 & mat_zlc[4, 1] < 0) {
      cat(paste0("STEP 1 - ", dep, ":", indep, " (X -> Y) with b = ", format(mat_zlc[3, 1], digits = digits), " and p = ", format(mat_zlc[2, 1], digits = digits), "\n"))
      cat(paste0("         As the bootstrap test above is significant, STEP 1 is\n"))
      cat(paste0("         significant and their coefficients point in opposite\n"))
      cat(paste0("         direction, you have competitive mediation (partial mediation)\n"))
    }
  }

  if (rit) {
    cat("\n")
    cat("RIT  =   (Indirect effect / Total effect)\n")
    cat(paste0("         (", round(ind_eff, digits = digits),
      " / ", round(tot_eff, digits = digits), ") = ",
      round(rit_val, digits = digits), "\n"))
    cat(paste0("         Meaning that about ", round(rit_val*100, digits = 1),
      "% of the effect of ", indep, "\n"))
    cat(paste0("         on ", dep, " is mediated by ", med, "\n"))
  }

  if (rid) {
    cat("\n")
    cat("RID  =   (Indirect effect / Direct effect)\n")
    cat(paste0("         (", round(ind_eff, digits = digits),
      " / ", round(dir_eff, digits = digits), ") = ",
      round(rid_val, digits = digits), "\n"))
    cat(paste0("         That is, the mediated effect is about ",
    round(rid_val, digits = digits), " times as\n"))
    cat(paste0("         large as the direct effect of ", indep, " on ",
      dep, "\n"))
  }
}
