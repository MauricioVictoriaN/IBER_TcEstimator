# ==============================================================================
# IBER_TcEstimator_v1.0.0.R
# ==============================================================================
# ------------------------------------------------------------------------------
# Author:   Mauricio Javier Victoria Niño
#           Independent researcher · Cali, Colombia
#           hidratecsa@gmail.com
#           ORCID: 0009-0003-4328-5691
# ------------------------------------------------------------------------------
# Description:
#   Estimates the Time of Concentration (Tc) from IBER hydraulic-hydrological
#   model outputs. Implements state-of-the-art operational hydrology according
#   to WMO, ASCE, NRCS and ISO protocols.
#
#   INCLUDES HYDROLOGICAL EVENT SIGNATURE ANALYSIS (FDC)
#   The Flow Duration Curve is interpreted as the temporal distribution of flows
#   during the design flood, NOT as a long-term hydrological balance. It enables
#   diagnosis of the system's attenuation capacity and hydrograph shape.
# ------------------------------------------------------------------------------
# Included modules:
#   A1. GLUE - Full posterior distribution of Tc
#   B1. Three baseflow separation filters (Eckhardt, Chapman, Lyne-Hollick)
#   B2. Automatic alpha estimation by recession curve regression
#   C1. Explicit effective precipitation via CN-NRCS
#   C2. IBER vs CN volume consistency check
#   D1. Geomorphologic Unit Hydrograph (GIUH)
#   D2. Clark HU (translation time + storage)
#   D3. Inverse deconvolution (Tikhonov) for empirical HU
#   E1. Phase and amplitude errors (Peak Time Error, Peak Flow Bias)
#   E2. Residual autocorrelation tests (Durbin-Watson, Ljung-Box)
#   E3. Peak-specific metrics (PBPF)
#   E4. Tc->Qp elasticity
#   E5. Hydrological Event Signature (FDC) - Percentiles Q5, Q10, Q50, Q90, Q95
#       - Richards-Baker Flashiness Index
#       - Recession Slope (Q10-Q90)
#       - Dual-panel FDC + Derivative plot
#
# ------------------------------------------------------------------------------
# License: MIT
# Version: 1.0.0 | 2026
# ==============================================================================

# --- 1. CENTRALISED CONFIGURATION ---
CONFIG <- list(
  version           = "1.0.0",
  ruta_excel        = "D:/R/Hyetograph_HydrographIBER_EN.xlsx",
  output_dir        = "D:/R",
  dt_interp_h       = 0.005,
  n_boot            = 2000,
  boot_seed         = 42,
  boot_conf         = 0.95,
  glue_N            = 5000,
  glue_seed         = 123,
  glue_kge_threshold = 0.50,
  glue_Tc_range     = c(0.05, 24),
  lag_to_tc_coef    = 0.6,
  BFI_max           = 0.80,
  alpha_baseflow    = NULL,
  baseflow_methods  = c("eckhardt", "chapman", "lyne_hollick"),
  CN_forced         = NULL,
  Ia_lambda         = 0.2,
  use_SCS_UH        = TRUE,
  use_Clark_UH      = TRUE,
  use_GIUH          = TRUE,
  use_deconv        = TRUE,
  lambda_tikhonov   = 0.01,
  bca_conv_tol      = 0.01,
  min_records       = 10,
  outlier_threshold = 3.0,
  qbase_threshold_pct = 5.0,
  use_empirical_formulas = TRUE,
  dpi_plots         = 300,
  fig_width         = 14,
  fig_height        = 7,
  export_csv        = TRUE,
  export_config_csv = TRUE,
  warn_duration_factor = 2.5
)

# --- 2. PACKAGES ---
required_packages <- c("zoo", "dplyr", "hydroGOF", "boot", "lmtest",
                       "ggplot2", "patchwork", "scales", "readxl", "writexl")
cat("Checking required packages...\n")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat("  Installing:", pkg, "\n")
    install.packages(pkg, quiet = TRUE)
  }
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}
cat("  All packages loaded.\n\n")

# --- 3. DIRECTORY STRUCTURE AND LOG ---
TIMESTAMP      <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
MAIN_DIR       <- file.path(CONFIG$output_dir,
                            paste0("IBERv", CONFIG$version, "_Results_", TIMESTAMP))
DATA_DIR       <- file.path(MAIN_DIR, "01_Input_Data")
RESULTS_DIR    <- file.path(MAIN_DIR, "02_Numerical_Results")
REPORT_DIR     <- file.path(MAIN_DIR, "03_Text_Report")
PLOTS_DIR      <- file.path(MAIN_DIR, "04_Plots_PNG")
LOG_DIR        <- file.path(MAIN_DIR, "05_Execution_Log")

for (d in c(MAIN_DIR, DATA_DIR, RESULTS_DIR, REPORT_DIR, PLOTS_DIR, LOG_DIR)) {
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
}

EXCEL_OUT_PATH <- file.path(RESULTS_DIR, "IBERv_Results.xlsx")
REPORT_PATH    <- file.path(REPORT_DIR,    "IBERv_Report.txt")
LOG_PATH       <- file.path(LOG_DIR,       "IBERv_Log.txt")

log_con <- file(LOG_PATH, open = "wt")
sink(log_con, split = TRUE)
on.exit({ sink(); close(log_con) }, add = TRUE)

cat("EXECUTION LOG -", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")
cat("IBER_TcEstimator v", CONFIG$version, " — Proof-of-Concept\n\n", sep = "")

warnings_list <- character(0)
accumulate_warning <- function(msg) {
  warnings_list <<- c(warnings_list, paste0("[", length(warnings_list) + 1, "] ", msg))
  cat("  ⚠ ", msg, "\n")
}

# ==============================================================================
# 4. GENERAL AUXILIARY FUNCTIONS
# ==============================================================================

detect_outliers_iqr <- function(x, name = "variable", iqr_factor = 3.0) {
  q25  <- stats::quantile(x, 0.25, na.rm = TRUE)
  q75  <- stats::quantile(x, 0.75, na.rm = TRUE)
  iqr  <- q75 - q25
  lim_low <- q25 - iqr_factor * iqr
  lim_high <- q75 + iqr_factor * iqr
  idx   <- which(x < lim_low | x > lim_high)
  if (length(idx) > 0)
    cat("  ALERT IQR outliers in", name, ":", length(idx),
        "point(s) outside [", round(lim_low, 4), ",", round(lim_high, 4), "]\n")
  return(idx)
}

check_time_series <- function(t, name = "series") {
  dt   <- diff(t)
  if (any(dt <= 0))
    warning("Non-strictly increasing times in ", name)
  cv   <- stats::sd(dt) / mean(dt)
  if (cv > 0.05)
    cat("  NOTE:", name, "has irregular dt (CV=", round(cv*100,1), "%).\n")
  list(dt_mean = mean(dt), dt_cv = cv)
}

# ==============================================================================
# 5. MODULE B: BASEFLOW SEPARATION
# ==============================================================================

eckhardt_filter <- function(Q, alpha, BFI_max) {
  n  <- length(Q); Qb <- numeric(n); Qb[1] <- Q[1]
  d  <- 1 - BFI_max * alpha
  for (i in 2:n) {
    Qb[i] <- ((1 - BFI_max) * alpha * Qb[i-1] + (1 - alpha) * BFI_max * Q[i]) / d
    Qb[i] <- min(Qb[i], Q[i]); Qb[i] <- max(Qb[i], 0)
  }
  list(baseflow = Qb, direct_runoff = Q - Qb)
}

chapman_filter <- function(Q, alpha) {
  n  <- length(Q); Qb <- numeric(n); Qb[1] <- Q[1]
  for (i in 2:n) {
    Qb[i] <- (3*alpha - 1)/(3 - alpha) * Qb[i-1] + (1 - alpha)/(3 - alpha) * (Q[i] + Q[i-1])
    Qb[i] <- min(Qb[i], Q[i]); Qb[i] <- max(Qb[i], 0)
  }
  list(baseflow = Qb, direct_runoff = Q - Qb)
}

lyne_hollick_filter <- function(Q, alpha) {
  n   <- length(Q); Qd <- numeric(n); Qd[1] <- 0
  for (i in 2:n) {
    Qd[i] <- alpha * Qd[i-1] + (1 + alpha)/2 * (Q[i] - Q[i-1])
    Qd[i] <- max(Qd[i], 0); Qd[i] <- min(Qd[i], Q[i])
  }
  Qb <- Q - Qd
  list(baseflow = Qb, direct_runoff = Qd)
}

estimate_alpha_recession <- function(Q, dt_h, min_segment = 5) {
  n   <- length(Q)
  dec <- c(FALSE, diff(Q) < 0)
  groups <- cumsum(!dec)
  segments <- split(seq_len(n), groups)
  rec_segments <- Filter(function(idx) length(idx) >= min_segment && all(dec[idx]), segments)
  
  if (length(rec_segments) == 0) {
    cat("  NOTE: No recession segments found. Using alpha = 0.925.\n")
    return(0.925)
  }
  
  alphas <- numeric(0)
  for (idx in rec_segments) {
    q_seg <- Q[idx]
    if (any(q_seg <= 0)) next
    t_seg <- seq(0, (length(idx)-1) * dt_h, by = dt_h)
    lm_fit <- tryCatch(stats::lm(log(q_seg) ~ t_seg), error = function(e) NULL)
    if (is.null(lm_fit)) next
    k_est <- coef(lm_fit)[2]
    a_est <- exp(k_est * dt_h)
    if (a_est > 0.5 && a_est < 0.999) alphas <- c(alphas, a_est)
  }
  
  if (length(alphas) == 0) {
    cat("  NOTE: Recession regression yielded no valid results. Using alpha = 0.925.\n")
    return(0.925)
  }
  
  alpha_est <- stats::median(alphas)
  cat("  Estimated alpha by recession:", round(alpha_est, 4),
      "(median of", length(alphas), "segments)\n")
  return(alpha_est)
}

compare_baseflow_separation <- function(Q, dt_h, BFI_max, alpha_cfg,
                                        methods = CONFIG$baseflow_methods) {
  cat("  Estimating alpha via recession curve...\n")
  alpha <- if (is.null(alpha_cfg)) estimate_alpha_recession(Q, dt_h) else alpha_cfg
  
  results <- list()
  if ("eckhardt" %in% methods) {
    r <- eckhardt_filter(Q, alpha, BFI_max)
    results$eckhardt <- list(
      baseflow = r$baseflow, direct_runoff = r$direct_runoff,
      BFI = sum(r$baseflow) / sum(Q), alpha = alpha, method = "Eckhardt (2005)"
    )
  }
  if ("chapman" %in% methods) {
    r <- chapman_filter(Q, alpha)
    results$chapman <- list(
      baseflow = r$baseflow, direct_runoff = r$direct_runoff,
      BFI = sum(r$baseflow) / sum(Q), alpha = alpha, method = "Chapman (1999)"
    )
  }
  if ("lyne_hollick" %in% methods) {
    r <- lyne_hollick_filter(Q, alpha)
    results$lyne_hollick <- list(
      baseflow = r$baseflow, direct_runoff = r$direct_runoff,
      BFI = sum(r$baseflow) / sum(Q), alpha = alpha, method = "Lyne-Hollick (1979)"
    )
  }
  
  cat("  Filter comparison (alpha =", round(alpha, 4), "):\n")
  for (nm in names(results)) {
    cat("   ", results[[nm]]$method, "-> BFI =", round(results[[nm]]$BFI, 4), "\n")
  }
  
  primary_method <- if ("eckhardt" %in% names(results)) "eckhardt" else names(results)[1]
  cat("  Primary method for analysis:", results[[primary_method]]$method, "\n")
  
  return(list(primary = results[[primary_method]], all = results, alpha = alpha))
}

# ==============================================================================
# 6. MODULE C: EFFECTIVE PRECIPITATION (CN-NRCS)
# ==============================================================================

effective_precipitation_CN <- function(P_cum, CN, lambda = 0.2) {
  if (is.na(CN) || CN <= 0 || CN >= 100) {
    cat("  WARNING: Invalid or unspecified CN. Using total P.\n")
    return(NULL)
  }
  S   <- 25.4 * (1000 / CN - 10)
  Ia  <- lambda * S
  
  Qe_cum <- ifelse(P_cum > Ia, (P_cum - Ia)^2 / (P_cum - Ia + S), 0)
  Pe_rate <- c(0, diff(Qe_cum))
  Pe_rate[Pe_rate < 0] <- 0
  
  cat("  CN =", CN, "| S =", round(S, 2), "mm | Ia =", round(Ia, 2),
      "mm | Pe_total =", round(max(Qe_cum), 2), "mm\n")
  
  return(list(Qe_cum = Qe_cum, Pe_rate = Pe_rate, S = S, Ia = Ia, CN = CN))
}

check_CN_consistency <- function(Vol_Q_m3, area_km2, Pe_mm) {
  Vol_CN_m3 <- Pe_mm / 1000 * area_km2 * 1e6
  diff_pct   <- 100 * (Vol_CN_m3 - Vol_Q_m3) / Vol_CN_m3
  cat("  CN-NRCS runoff volume:", format(round(Vol_CN_m3, 0), big.mark = ","), "m3\n")
  cat("  IBER runoff volume   :", format(round(Vol_Q_m3, 0), big.mark = ","), "m3\n")
  cat("  CN vs IBER difference:", round(diff_pct, 2), "%\n")
  if (abs(diff_pct) > 25)
    cat("  ALERT: Difference >25%. Check CN consistency in IBER.\n")
  return(diff_pct)
}

# ==============================================================================
# 7. MODULE D: UNIT HYDROGRAPHS
# ==============================================================================

generate_SCS_UH <- function(t, Tc, D, A = 1) {
  Tp <- D / 2 + 0.6 * Tc
  Tb <- 2.67 * Tp
  qp <- (0.208 * A) / Tp
  UH <- ifelse(t <= 0, 0,
               ifelse(t <= Tp, qp * (t / Tp),
                      ifelse(t <= Tb, qp * (1 - (t - Tp) / (Tb - Tp)), 0)))
  return(UH)
}

generate_Clark_UH <- function(t, Tc, R = NULL, A = 1) {
  if (is.null(R) || is.na(R)) R <- Tc
  dt  <- mean(diff(t))
  if (dt <= 0) return(rep(0, length(t)))
  
  n_steps <- ceiling(Tc / dt)
  if (n_steps < 2) n_steps <- 2
  t_tia   <- seq(0, Tc, length.out = n_steps + 1)
  frac_ac <- ifelse(t_tia / Tc <= 0.5,
                    1.414 * (t_tia / Tc)^1.5,
                    1 - 1.414 * (1 - t_tia / Tc)^1.5)
  frac_ac[frac_ac > 1] <- 1; frac_ac[frac_ac < 0] <- 0
  IA      <- diff(frac_ac) * A
  
  C1 <- dt / (R + dt / 2)
  C2 <- 1 - C1
  n_t <- length(t)
  Q_out <- numeric(n_t)
  for (i in seq_along(IA)) {
    if (i > n_t) break
    for (j in i:n_t) {
      Q_out[j] <- C1 * IA[i] / dt + (if (j > i) C2 * Q_out[j-1] else 0)
    }
  }
  vol_tot <- sum(Q_out) * dt * 3600
  if (vol_tot > 0) Q_out <- Q_out / vol_tot * A
  return(Q_out)
}

generate_GIUH <- function(t, L_km, A_km2, v_ms = 1.0, Rb = 4.5, Rl = 2.0) {
  if (is.na(L_km) || is.na(A_km2) || L_km <= 0 || A_km2 <= 0) {
    cat("  WARNING GIUH: L or A not specified. GIUH omitted.\n")
    return(NULL)
  }
  
  L_m   <- L_km * 1000
  t_r   <- L_m / v_ms / 3600
  k_g   <- 0.44 * t_r * (Rb / Rl)^0.55
  m_g   <- 3.29 * (Rb / Rl)^0.78 * (L_m / (A_km2 * 1e6)^0.5)^0.07
  
  if (k_g <= 0 || m_g <= 0) {
    cat("  WARNING GIUH: invalid parameters. GIUH omitted.\n")
    return(NULL)
  }
  
  dt_h   <- mean(diff(t))
  t_rel  <- t - min(t)
  t_rel[t_rel <= 0] <- 1e-9
  giuh   <- stats::dgamma(t_rel, shape = m_g, scale = k_g)
  
  vol    <- sum(giuh) * dt_h
  if (vol > 0) giuh <- giuh / vol * (A_km2 * 1e6) / (1000 * 3600)
  
  cat("  GIUH: k =", round(k_g, 4), "h | m =", round(m_g, 4),
      "| Qp_GIUH =", round(max(giuh), 4), "m3/s/mm\n")
  return(giuh)
}

tikhonov_deconvolution <- function(P_vec, Q_vec, dt_h, lambda = 0.01) {
  n <- length(Q_vec)
  m <- length(P_vec)
  
  P_mat <- matrix(0, nrow = n, ncol = n)
  for (i in seq_len(n)) {
    for (j in seq_len(i)) {
      if ((i - j + 1) <= m) P_mat[i, j] <- P_vec[i - j + 1] * dt_h
    }
  }
  
  A_mat <- t(P_mat) %*% P_mat + lambda * diag(n)
  b_vec <- t(P_mat) %*% Q_vec
  h_est <- tryCatch(
    as.vector(solve(A_mat, b_vec)),
    error = function(e) {
      cat("  WARNING Deconvolution: singular system.", e$message, "\n")
      return(NULL)
    }
  )
  if (!is.null(h_est)) h_est[h_est < 0] <- 0
  return(h_est)
}

# ==============================================================================
# 8. MODULE A: GLUE
# ==============================================================================

analyse_GLUE <- function(t_grid, Pe, Qd_obs, D, A,
                         N = CONFIG$glue_N, kge_thresh = CONFIG$glue_kge_threshold,
                         Tc_range = CONFIG$glue_Tc_range, seed = CONFIG$glue_seed) {
  cat("\n[GLUE] Monte Carlo sampling: N =", N, "realisations...\n")
  cat("[GLUE] Parallelisation: DISABLED (sequential default).\n")
  cat("[GLUE] For multi-event analysis, enable future.apply in code.\n")
  
  set.seed(seed)
  Tc_samples <- stats::runif(N, Tc_range[1], Tc_range[2])
  
  dt_h  <- mean(diff(t_grid))
  t_rel <- t_grid - min(t_grid)
  Pe_dt <- Pe * dt_h
  n_t   <- length(t_grid)
  
  evaluate_one_realisation <- function(Tc_i) {
    UH_i <- generate_SCS_UH(t_rel, Tc_i, D, A)
    Qs_i  <- pmax(0, stats::convolve(Pe_dt, rev(UH_i), type = "open")[seq_len(n_t)])
    kge_val <- calculate_KGE(Qs_i, Qd_obs)
    if (is.na(kge_val)) -Inf else kge_val
  }
  
  KGE_samples <- unlist(lapply(Tc_samples, evaluate_one_realisation))
  
  idx_beh  <- which(KGE_samples >= kge_thresh)
  n_beh    <- length(idx_beh)
  cat("  Behavioural realisations (KGE >=", kge_thresh, "):", n_beh,
      "(", round(100 * n_beh / N, 1), "% of total)\n")
  
  if (n_beh < 10) {
    cat("  WARNING: <10 behavioural realisations.\n")
    return(NULL)
  }
  
  Tc_beh   <- Tc_samples[idx_beh]
  KGE_beh  <- KGE_samples[idx_beh]
  
  weights    <- KGE_beh - min(KGE_beh) + 1e-9
  weights    <- weights / sum(weights)
  
  Tc_mean  <- sum(Tc_beh * weights)
  Tc_med   <- stats::median(Tc_beh)
  Tc_sd    <- sqrt(sum(weights * (Tc_beh - Tc_mean)^2))
  Tc_q025  <- stats::quantile(Tc_beh, 0.025)
  Tc_q975  <- stats::quantile(Tc_beh, 0.975)
  
  cat("  Posterior Tc — median:", round(Tc_med, 4),
      "h | 95% CI [", round(Tc_q025, 4), ",", round(Tc_q975, 4), "] h\n")
  
  return(list(
    Tc_samples = Tc_samples, KGE_samples = KGE_samples,
    beh_idx = idx_beh, Tc_beh = Tc_beh, KGE_beh = KGE_beh,
    weights = weights, Tc_mean = Tc_mean, Tc_median = Tc_med,
    Tc_sd = Tc_sd, Tc_CI95 = c(lo = unname(Tc_q025), hi = unname(Tc_q975)),
    n_beh = n_beh, N = N
  ))
}

# ==============================================================================
# 9. MODULE E: ADVANCED METRICS
# ==============================================================================

calculate_KGE <- function(sim, obs) {
  ok <- stats::complete.cases(sim, obs) & is.finite(sim) & is.finite(obs)
  s  <- sim[ok]; o <- obs[ok]
  if (length(s) < CONFIG$min_records) return(NA_real_)
  if (stats::sd(s) == 0 || stats::sd(o) == 0) return(NA_real_)
  r  <- stats::cor(s, o)
  1 - sqrt((r - 1)^2 + (stats::sd(s)/stats::sd(o) - 1)^2 + (mean(s)/mean(o) - 1)^2)
}

calculate_KGEpp <- function(sim, obs) {
  ok <- stats::complete.cases(sim, obs) & is.finite(sim) & is.finite(obs)
  s  <- sim[ok]; o <- obs[ok]
  if (length(s) < CONFIG$min_records) return(NA_real_)
  if (mean(o) == 0 || mean(s) == 0) return(NA_real_)
  r     <- stats::cor(s, o)
  gamma <- (stats::sd(s) / mean(s)) / (stats::sd(o) / mean(o))
  beta  <- mean(s) / mean(o)
  1 - sqrt((r - 1)^2 + (gamma - 1)^2 + (beta - 1)^2)
}

calculate_NSE <- function(sim, obs) {
  ok <- stats::complete.cases(sim, obs) & is.finite(sim) & is.finite(obs)
  s  <- sim[ok]; o <- obs[ok]
  if (length(s) < CONFIG$min_records) return(NA_real_)
  d  <- sum((o - mean(o))^2); if (d == 0) return(NA_real_)
  1 - sum((o - s)^2) / d
}

calculate_RMSE <- function(sim, obs) {
  ok   <- stats::complete.cases(sim, obs) & is.finite(sim) & is.finite(obs)
  s    <- sim[ok]; o <- obs[ok]
  if (length(s) < CONFIG$min_records) return(list(RMSE = NA, NRMSE = NA))
  rmse <- sqrt(mean((o - s)^2))
  rng  <- diff(range(o)); nrmse <- if (rng > 0) rmse / rng else NA_real_
  list(RMSE = rmse, NRMSE = nrmse)
}

peak_metrics <- function(sim, obs, t_vec) {
  ok  <- is.finite(sim) & is.finite(obs)
  s   <- sim[ok]; o <- obs[ok]; t <- t_vec[ok]
  Qp_obs <- max(o, na.rm = TRUE)
  Qp_sim <- max(s, na.rm = TRUE)
  tp_obs <- t[which.max(o)]
  tp_sim <- t[which.max(s)]
  list(
    Peak_Flow_Bias_pct = 100 * (Qp_sim - Qp_obs) / Qp_obs,
    Peak_Time_Error_h  = tp_sim - tp_obs,
    Amplitude_Ratio    = Qp_sim / Qp_obs,
    Qp_obs = Qp_obs, Qp_sim = Qp_sim,
    tp_obs = tp_obs, tp_sim = tp_sim
  )
}

autocorrelation_test <- function(sim, obs) {
  resid <- as.numeric(sim - obs)
  resid <- resid[is.finite(resid)]
  if (length(resid) < 15) {
    cat("  NOTE: Series too short for autocorrelation tests.\n")
    return(list(DW_stat = NA, DW_pval = NA, LB_stat = NA, LB_pval = NA,
                conclusion = "Insufficient series length"))
  }
  
  dw_res <- tryCatch({
    df_tmp <- data.frame(r = resid, x = seq_along(resid))
    lm_tmp <- stats::lm(r ~ x, data = df_tmp)
    lmtest::dwtest(lm_tmp)
  }, error = function(e) NULL)
  
  DW_stat <- if (!is.null(dw_res)) unname(dw_res$statistic) else NA
  DW_pval <- if (!is.null(dw_res)) dw_res$p.value else NA
  
  lag_lb  <- min(10, floor(length(resid) / 5))
  lb_res  <- tryCatch(
    stats::Box.test(resid, lag = lag_lb, type = "Ljung-Box"),
    error = function(e) NULL
  )
  LB_stat <- if (!is.null(lb_res)) unname(lb_res$statistic) else NA
  LB_pval <- if (!is.null(lb_res)) lb_res$p.value else NA
  
  conclusion <- if (is.na(DW_pval) && is.na(LB_pval)) {
    "Not calculated"
  } else if ((!is.na(DW_pval) && DW_pval < 0.05) || (!is.na(LB_pval) && LB_pval < 0.05)) {
    "Significant autocorrelation: structural model error"
  } else {
    "No significant autocorrelation: residuals acceptably random"
  }
  
  cat("  Durbin-Watson: stat =", round(DW_stat, 3),
      "| p-value =", round(DW_pval, 4), "\n")
  cat("  Ljung-Box    : stat =", round(LB_stat, 3),
      "| p-value =", round(LB_pval, 4), "\n")
  cat("  Conclusion   :", conclusion, "\n")
  
  list(DW_stat = DW_stat, DW_pval = DW_pval, LB_stat = LB_stat, LB_pval = LB_pval,
       conclusion = conclusion)
}

calculate_Tc_Qp_elasticity <- function(t_rel, Pe, Tc_nom, D, A, dt_h) {
  delta  <- 0.05
  Tc_up  <- Tc_nom * (1 + delta)
  Tc_dn  <- Tc_nom * (1 - delta)
  
  UH_up <- generate_SCS_UH(t_rel, Tc_up, D, A)
  UH_dn <- generate_SCS_UH(t_rel, Tc_dn, D, A)
  UH_0  <- generate_SCS_UH(t_rel, Tc_nom, D, A)
  
  conv_func <- function(UH) pmax(0,
                                 stats::convolve(Pe * dt_h, rev(UH), type = "open")[seq_along(t_rel)])
  
  Qp_up  <- max(conv_func(UH_up))
  Qp_dn  <- max(conv_func(UH_dn))
  Qp_0   <- max(conv_func(UH_0))
  
  if (Qp_0 == 0) return(NA_real_)
  
  eps <- ((Qp_up - Qp_dn) / (2 * Qp_0)) / delta
  cat("  Elasticity epsilon (Tc->Qp):", round(eps, 4),
      "  [DeltaQp/Qp per DeltaTc/Tc]\n")
  return(eps)
}

check_bca_convergence <- function(boot_dist, tol = CONFIG$bca_conv_tol) {
  n <- length(boot_dist)
  if (n < 100) {
    cat("  [BCa Conv] Series too short (n=", n, ") for convergence test.\n", sep = "")
    return(list(converged = NA, relative_difference = NA))
  }
  half <- floor(n / 2)
  ci_full <- stats::quantile(boot_dist, c(0.025, 0.5, 0.975), na.rm = TRUE)
  ci_half <- stats::quantile(boot_dist[1:half], c(0.025, 0.5, 0.975), na.rm = TRUE)
  diff_width <- abs(diff(ci_full[c(1, 3)]) - diff(ci_half[c(1, 3)]))
  rel_diff <- diff_width / diff(ci_full[c(1, 3)])
  
  result <- list(
    converged = if (!is.na(rel_diff)) rel_diff < tol else NA,
    relative_difference = rel_diff,
    ci_full = ci_full[c(1, 3)],
    ci_half = ci_half[c(1, 3)]
  )
  
  cat("  [BCa Conv] Relative CI width difference (50% vs 100% replicates):",
      if (!is.na(rel_diff)) round(rel_diff * 100, 4) else NA, "%\n")
  cat("  [BCa Conv] Status:",
      if (isTRUE(result$converged)) "CONVERGED (OK)" else "NOT CONVERGED — increase R\n", sep = "")
  
  return(result)
}

# ==============================================================================
# 9b. MODULE E5: HYDROLOGICAL EVENT SIGNATURE (FDC)
# ==============================================================================

calculate_event_signature <- function(Q, dt_h) {
  
  cat("\n  --- HYDROLOGICAL EVENT SIGNATURE (FDC) ---\n")
  cat("  METHODOLOGICAL NOTE: The computed FDC corresponds to the synthetic\n")
  cat("  design event and does NOT represent the annual hydrological regime.\n")
  cat("  It is interpreted as the temporal flow distribution during the flood.\n")
  
  RB <- sum(abs(diff(Q))) / sum(Q)
  cat("  Richards-Baker Flashiness Index:", round(RB, 4))
  if (RB < 0.05) {
    cat(" -> Highly ATTENUATED hydrograph\n")
  } else if (RB < 0.2) {
    cat(" -> Moderately variable hydrograph\n")
  } else {
    cat(" -> TORRENTIAL hydrograph (high variability)\n")
  }
  
  Q_sort   <- sort(Q, decreasing = TRUE)
  n        <- length(Q_sort)
  prob_exc <- (1:n) / (n + 1) * 100
  
  fdc_df  <- data.frame(
    Exceedance_pct = round(prob_exc, 2),
    Discharge_m3s  = round(Q_sort, 5)
  )
  
  Q5  <- stats::quantile(Q, 0.95, na.rm = TRUE)
  Q10 <- stats::quantile(Q, 0.90, na.rm = TRUE)
  Q50 <- stats::quantile(Q, 0.50, na.rm = TRUE)
  Q90 <- stats::quantile(Q, 0.10, na.rm = TRUE)
  Q95 <- stats::quantile(Q, 0.05, na.rm = TRUE)
  
  if (Q10 > 0 && Q90 > 0 && Q10 != Q90) {
    recession_slope <- (log10(Q10) - log10(Q90)) / (90 - 10)
  } else {
    recession_slope <- NA_real_
  }
  
  cat("  --- Event Percentiles (Flow Duration) ---\n")
  cat("    Q5  (exc. 5%)  :", round(Q5, 4), "m3/s  [Flood peak]\n")
  cat("    Q10 (exc. 10%) :", round(Q10, 4), "m3/s  [High flows]\n")
  cat("    Q50 (exc. 50%) :", round(Q50, 4), "m3/s  [Event median flow]\n")
  cat("    Q90 (exc. 90%) :", round(Q90, 4), "m3/s  [Recession / Low flow]\n")
  cat("    Q95 (exc. 95%) :", round(Q95, 4), "m3/s  [Baseflow / End]\n")
  cat("  Recession Slope (Q10-Q90):", round(recession_slope, 6))
  if (!is.na(recession_slope)) {
    if (recession_slope > -0.01) {
      cat(" -> VERY SLOW recession (High attenuation)\n")
    } else if (recession_slope > -0.03) {
      cat(" -> SLOW recession (Moderate attenuation)\n")
    } else {
      cat(" -> FAST recession (Low attenuation, flashy response)\n")
    }
  } else {
    cat("\n")
  }
  
  return(list(
    RB = RB, fdc_df = fdc_df,
    Q5 = Q5, Q10 = Q10, Q50 = Q50, Q90 = Q90, Q95 = Q95,
    recession_slope = recession_slope
  ))
}

interpret_metrics <- function(KGE, NSE, PBIAS) {
  rating_KGE <- if (is.na(KGE)) "Not calculated"
  else if (KGE > 0.75) "Very Good"
  else if (KGE > 0.50) "Good"
  else if (KGE > 0.00) "Satisfactory"
  else if (KGE > -0.41) "Unsatisfactory (>mean obs.)"
  else "Unsatisfactory (<mean obs.)"
  
  rating_NSE <- if (is.na(NSE)) "Not calculated"
  else if (NSE > 0.75) "Very Good"
  else if (NSE > 0.65) "Good"
  else if (NSE > 0.50) "Satisfactory"
  else "Unsatisfactory"
  
  rating_PB  <- if (is.na(PBIAS)) "Not calculated"
  else if (abs(PBIAS) < 10) "Very Good"
  else if (abs(PBIAS) < 15) "Good"
  else if (abs(PBIAS) < 25) "Satisfactory"
  else "Unsatisfactory"
  
  data.frame(
    Metric = c("KGE", "NSE", "PBIAS (%)"),
    Value = round(c(KGE, NSE, PBIAS), 3),
    Rating = c(rating_KGE, rating_NSE, rating_PB),
    stringsAsFactors = FALSE
  )
}

empirical_Tc_formulas <- function(L, S, A, H = NA) {
  res <- data.frame(Formula = character(), Tc_hours = numeric(),
                    Applicability = character(), stringsAsFactors = FALSE)
  if (!is.na(L) && !is.na(S) && S > 0) {
    L_m  <- L * 1000
    Tc_k <- 0.0663 * L_m^0.77 / S^0.385 / 60
    res  <- rbind(res, data.frame(Formula = "Kirpich (1940)",
                                  Tc_hours = round(Tc_k, 4),
                                  Applicability = "Small agricultural watersheds (<0.45 km2)"))
    Tc_t <- 0.3 * (L / S^0.25)^0.76
    res  <- rbind(res, data.frame(Formula = "Temez (1978)",
                                  Tc_hours = round(Tc_t, 4),
                                  Applicability = "Mediterranean/semi-arid watersheds (MOPU)"))
  }
  if (!is.na(A) && !is.na(L) && !is.na(H) && H > 0) {
    Tc_g <- (4 * sqrt(A) + 1.5 * L) / (0.8 * sqrt(H))
    res  <- rbind(res, data.frame(Formula = "Giandotti (1934)",
                                  Tc_hours = round(Tc_g, 4),
                                  Applicability = "Medium-large watersheds, Southern Europe"))
  }
  return(res)
}

validate_inputs <- function(data) {
  cat("\n  --- Input data integrity validation ---\n")
  checks <- list(
    has_rain       = "rain" %in% names(data),
    has_discharge  = "discharge" %in% names(data),
    has_area       = "watershed_area" %in% names(data) && is.numeric(data$watershed_area),
    area_positive  = data$watershed_area > 0,
    rain_no_na     = sum(is.na(data$rain$Precip)) == 0,
    discharge_no_na = sum(is.na(data$discharge$Discharge)) == 0,
    precip_nonneg  = all(data$rain$Precip >= 0, na.rm = TRUE),
    discharge_nonneg = all(data$discharge$Discharge >= -0.001, na.rm = TRUE),
    time_increasing_rain = all(diff(data$rain$Time) > 0),
    time_increasing_discharge = all(diff(data$discharge$Time) > 0),
    min_records    = nrow(data$rain) >= 5 && nrow(data$discharge) >= 5
  )
  
  failed <- names(checks)[!unlist(checks)]
  if (length(failed) > 0) {
    cat("  ⚠ Validation failures:\n")
    for (f in failed) cat("    -", f, "\n")
    stop("INPUT VALIDATION FAILED: ", paste(failed, collapse = ", "))
  }
  cat("  Input validation: PASSED (", length(checks), " checks)\n\n", sep = "")
  return(invisible(TRUE))
}

# ==============================================================================
# 10. DATA READING AND VALIDATION
# ==============================================================================

detect_excel_language <- function(meta_df) {
  spanish_indicators <- c("area de la cuenca", "duracion", "tormenta",
                          "coeficiente de escorrentia", "precipitacion")
  col_name <- tolower(names(meta_df)[1])
  first_col <- tolower(as.character(meta_df[[1]]))
  spanish_hits <- sum(sapply(spanish_indicators, function(p) any(grepl(p, first_col, perl = TRUE))))
  if (spanish_hits >= 2) {
    return("es")
  }
  return("en")
}

read_metadata_parameter <- function(meta, patterns_es, patterns_en, lang) {
  patterns <- if (lang == "es") patterns_es else patterns_en
  col_name <- tolower(names(meta)[1])
  first_col <- tolower(as.character(meta[[1]]))
  for (pattern in patterns) {
    idx <- grep(pattern, first_col, ignore.case = TRUE, perl = TRUE)[1]
    if (!is.na(idx)) {
      valor <- suppressWarnings(as.numeric(meta[[2]][idx]))
      if (!is.na(valor)) return(valor)
    }
  }
  return(NA_real_)
}

read_and_validate_data <- function(excel_path, data_dir) {
  cat("\n", paste(rep("-", 80), collapse = ""), "\n")
  cat("   MODULE 1: DATA READING AND VALIDATION (v", CONFIG$version, ")\n", sep = "")
  cat(paste(rep("-", 80), collapse = ""), "\n\n")
  
  if (!file.exists(excel_path)) stop("ERROR: File not found: ", excel_path)
  file.copy(excel_path,
            file.path(data_dir, "Original_Input_Data.xlsx"),
            overwrite = TRUE)
  cat("File:", excel_path, "\n")
  
  sheets <- readxl::excel_sheets(excel_path)
  cat("Sheets:", paste(sheets, collapse = ", "), "\n")
  
  expected_sheets_es <- c("Metadatos", "Hietograma", "Hidrograma")
  expected_sheets_en <- c("Metadata", "Hyetograph", "Hydrograph")
  
  lang <- if (all(expected_sheets_es %in% sheets)) "es" else "en"
  cat("Detected Excel language:", if (lang == "es") "Spanish" else "English", "\n")
  
  if (lang == "es") {
    required_sheets <- expected_sheets_es
    hyetograph_sheet <- "Hietograma"
    hydrograph_sheet <- "Hidrograma"
  } else {
    required_sheets <- expected_sheets_en
    hyetograph_sheet <- "Hyetograph"
    hydrograph_sheet <- "Hydrograph"
  }
  
  for (s in required_sheets) {
    if (!s %in% sheets) stop("ERROR: Sheet '", s, "' not found")
  }
  
  metadata_sheet <- if (lang == "es") "Metadatos" else "Metadata"
  meta_raw <- readxl::read_excel(excel_path, sheet = metadata_sheet)
  meta <- meta_raw
  
  if (!(lang %in% c("es", "en"))) {
    lang <- detect_excel_language(meta)
    cat("Detected content language:", if (lang == "es") "Spanish" else "English", "\n")
  }
  
  watershed_area <- read_metadata_parameter(meta,
                                            patterns_es = c("Area de la cuenca", "area.*cuenca", "area.*km"),
                                            patterns_en = c("Watershed area", "watershed.*area", "area.*km", "catchment.*area"),
                                            lang = lang)
  
  storm_duration <- read_metadata_parameter(meta,
                                            patterns_es = c("Duracion de tormenta", "duracion", "tormenta"),
                                            patterns_en = c("Storm duration", "duration", "storm"),
                                            lang = lang)
  
  channel_length_km <- read_metadata_parameter(meta,
                                               patterns_es = c("L_cauce_km", "L_cauce", "L_km", "longitud.*cauce"),
                                               patterns_en = c("Channel length", "L_km", "channel.*length", "L_cauce"),
                                               lang = lang)
  
  slope_m_per_m <- read_metadata_parameter(meta,
                                           patterns_es = c("S_pendiente_m_m", "S_pendiente", "pendiente", "S_m"),
                                           patterns_en = c("Slope.*m/m", "S_pendiente", "slope", "S_m"),
                                           lang = lang)
  
  elevation_drop_m <- read_metadata_parameter(meta,
                                              patterns_es = c("H_desnivel_m", "H_desnivel", "desnivel", "H_m"),
                                              patterns_en = c("Elevation drop", "H_desnivel", "elevation", "H_m"),
                                              lang = lang)
  
  CN_meta <- read_metadata_parameter(meta,
                                     patterns_es = c("CN (Numero de Curva NRCS)", "CN", "Numero de Curva", "curve.*number"),
                                     patterns_en = c("CN.*Curve Number", "CN", "Curve Number", "curve.*number"),
                                     lang = lang)
  
  R_clark <- read_metadata_parameter(meta,
                                     patterns_es = c("R_clark_h", "R_clark", "coeficiente.*almacen"),
                                     patterns_en = c("R_clark", "R_clark_h", "storage.*coefficient"),
                                     lang = lang)
  
  Rb_strahler <- read_metadata_parameter(meta,
                                         patterns_es = c("Rb_Strahler", "Rb", "bifurcacion"),
                                         patterns_en = c("Rb_Strahler", "Rb", "bifurcation"),
                                         lang = lang)
  
  Rl_strahler <- read_metadata_parameter(meta,
                                         patterns_es = c("Rl_Strahler", "Rl", "longitud.*horton"),
                                         patterns_en = c("Rl_Strahler", "Rl", "length.*horton"),
                                         lang = lang)
  
  v_giuh <- read_metadata_parameter(meta,
                                    patterns_es = c("v_GIUH_m_s", "v_GIUH", "velocidad"),
                                    patterns_en = c("v_GIUH", "v_GIUH_m_s", "velocity"),
                                    lang = lang)
  
  CN_use <- if (!is.null(CONFIG$CN_forced)) CONFIG$CN_forced else CN_meta
  
  if (is.na(watershed_area) || watershed_area <= 0)
    stop("ERROR: Watershed area not found or invalid.")
  
  cat("\nMetadata loaded:\n")
  cat("  Area (km2)       :", watershed_area, "\n")
  cat("  Duration (h)     :", ifelse(is.na(storm_duration), "Not specified", storm_duration), "\n")
  cat("  Channel L (km)   :", ifelse(is.na(channel_length_km), "N/A", channel_length_km), "\n")
  cat("  Slope (m/m)      :", ifelse(is.na(slope_m_per_m), "N/A", slope_m_per_m), "\n")
  cat("  Elevation (m)    :", ifelse(is.na(elevation_drop_m), "N/A", elevation_drop_m), "\n")
  cat("  CN               :", ifelse(is.na(CN_use), "N/A", CN_use), "\n")
  cat("  R Clark (h)      :", ifelse(is.na(R_clark), "N/A (using R=Tc)", R_clark), "\n")
  cat("  Rb Strahler      :", ifelse(is.na(Rb_strahler), "N/A (default 4.5)", Rb_strahler), "\n")
  cat("  Rl Strahler      :", ifelse(is.na(Rl_strahler), "N/A (default 2.0)", Rl_strahler), "\n")
  cat("  v GIUH (m/s)     :", ifelse(is.na(v_giuh), "N/A (default 1.0)", v_giuh), "\n")
  
  rain_raw <- readxl::read_excel(excel_path, sheet = hyetograph_sheet, col_names = FALSE)
  rain <- data.frame(Time = as.numeric(rain_raw[[1]]),
                     Precip = as.numeric(rain_raw[[2]]))
  rain <- rain[stats::complete.cases(rain) & rain$Precip >= 0, ]
  rain <- rain[order(rain$Time), ]
  
  disc_raw <- readxl::read_excel(excel_path, sheet = hydrograph_sheet, col_names = FALSE)
  discharge <- data.frame(Time = as.numeric(disc_raw[[1]]),
                          Discharge = as.numeric(disc_raw[[2]]))
  discharge <- discharge[stats::complete.cases(discharge) & discharge$Discharge >= 0, ]
  discharge <- discharge[order(discharge$Time), ]
  
  cat("\nRecords: Hyetograph =", nrow(rain), "| Hydrograph =", nrow(discharge), "\n")
  
  cat("\nQuality checks:\n")
  check_time_series(rain$Time, "Hyetograph")
  check_time_series(discharge$Time, "Hydrograph")
  detect_outliers_iqr(rain$Precip, "Precipitation", CONFIG$outlier_threshold)
  detect_outliers_iqr(discharge$Discharge, "Discharge", CONFIG$outlier_threshold)
  
  dt_P   <- mean(diff(rain$Time))
  P_total <- sum(rain$Precip) * dt_P
  Qpeak  <- max(discharge$Discharge, na.rm = TRUE)
  Q_ini  <- mean(utils::head(discharge$Discharge, 5))
  pct_b  <- 100 * Q_ini / Qpeak
  
  cat("\nPreliminary statistics:\n")
  cat("  Total P (sum*dt):", round(P_total, 2), "mm\n")
  cat("  Peak discharge  :", round(Qpeak, 4), "m3/s\n")
  cat("  Initial Q       :", round(Q_ini, 4), "m3/s (", round(pct_b, 1), "% of peak)\n")
  if (pct_b > CONFIG$qbase_threshold_pct)
    cat("  ALERT: Significant baseflow detected. Separation will be applied.\n")
  
  assign("excel_lang", lang, envir = parent.frame())
  
  result <- list(
    rain = rain, discharge = discharge,
    watershed_area = watershed_area, storm_duration = storm_duration,
    channel_length_km = channel_length_km, slope_m_per_m = slope_m_per_m,
    elevation_drop_m = elevation_drop_m, CN = CN_use,
    R_clark = R_clark,
    Rb_strahler = ifelse(is.na(Rb_strahler), 4.5, Rb_strahler),
    Rl_strahler = ifelse(is.na(Rl_strahler), 2.0, Rl_strahler),
    v_giuh = ifelse(is.na(v_giuh), 1.0, v_giuh),
    P_total = P_total, Qpeak = Qpeak, Q_initial = Q_ini
  )
  
  validate_inputs(result)
  return(result)
}

# ==============================================================================
# 11. MAIN HYDROLOGICAL ANALYSIS
# ==============================================================================

hydrological_analysis <- function(data) {
  profile_log <- data.frame(module = character(), time_seconds = numeric(),
                            stringsAsFactors = FALSE)
  
  cat("\n", paste(rep("=", 80), collapse = ""), "\n")
  cat("   MODULE 2: COMPLETE HYDROLOGICAL ANALYSIS (v", CONFIG$version, ")\n", sep = "")
  cat(paste(rep("=", 80), collapse = ""), "\n\n")
  
  rain <- data$rain; discharge <- data$discharge
  A    <- data$watershed_area; D <- data$storm_duration
  
  t_min  <- max(min(rain$Time), min(discharge$Time))
  t_max  <- min(max(rain$Time), max(discharge$Time))
  t_grid <- seq(t_min, t_max, by = CONFIG$dt_interp_h)
  dt_h   <- CONFIG$dt_interp_h
  t_rel  <- t_grid - min(t_grid)
  
  P_grid <- pmax(0, stats::approx(rain$Time, rain$Precip, xout = t_grid, rule = 2)$y)
  Q_grid <- pmax(0, stats::approx(discharge$Time, discharge$Discharge, xout = t_grid, rule = 2)$y)
  cat("[1/9] Interpolation: dt =", dt_h, "h |", length(t_grid), "points\n")
  
  Vol_P_m3 <- sum(P_grid) * dt_h / 1000 * (A * 1e6)
  Vol_Q_m3 <- sum(Q_grid) * dt_h * 3600
  PBIAS    <- 100 * (Vol_P_m3 - Vol_Q_m3) / Vol_P_m3
  cat("\n[2/9] Mass balance:\n")
  cat("  Rainfall volume:", format(round(Vol_P_m3,0), big.mark=","), "m3\n")
  cat("  Discharge volume:", format(round(Vol_Q_m3,0), big.mark=","), "m3\n")
  cat("  PBIAS:", round(PBIAS, 2), "%\n")
  
  t0_bf <- Sys.time()
  cat("\n[3/9] Baseflow separation (Modules B1, B2):\n")
  bf_sep <- compare_baseflow_separation(Q_grid, dt_h, CONFIG$BFI_max,
                                        CONFIG$alpha_baseflow, CONFIG$baseflow_methods)
  alpha_est <- bf_sep$alpha
  Qb_grid   <- bf_sep$primary$baseflow
  Qd_grid   <- bf_sep$primary$direct_runoff
  profile_log <- rbind(profile_log,
                       data.frame(module = "Baseflow separation (3 filters)",
                                  time_seconds = as.numeric(difftime(Sys.time(), t0_bf, units = "secs"))))
  
  t0_cn <- Sys.time()
  cat("\n[4/9] Effective precipitation CN-NRCS (Module C1):\n")
  P_cum_grid <- cumsum(P_grid) * dt_h
  cn_res <- effective_precipitation_CN(P_cum_grid, data$CN, CONFIG$Ia_lambda)
  
  if (!is.null(cn_res)) {
    Pe_grid <- cn_res$Pe_rate
    cat("\n  CN vs IBER consistency check (Module C2):\n")
    dif_CN_IBER <- check_CN_consistency(Vol_Q_m3, A, max(cn_res$Qe_cum))
  } else {
    Pe_grid <- P_grid
    dif_CN_IBER <- NA
  }
  profile_log <- rbind(profile_log,
                       data.frame(module = "Effective precipitation (CN-NRCS)",
                                  time_seconds = as.numeric(difftime(Sys.time(), t0_cn, units = "secs"))))
  
  t0_tc <- Sys.time()
  cat("\n[5/9] Tc estimation — SCS Lag method:\n")
  P_cum_c <- cumsum(P_grid); sum_P <- sum(P_grid)
  if (sum_P <= 0) stop("Cumulative precipitation = 0.")
  idx_T50  <- which(P_cum_c >= 0.5 * sum_P)[1]
  T50      <- t_grid[idx_T50]
  idx_Qp   <- which.max(Qd_grid)[1]
  Qp_time  <- t_grid[idx_Qp]
  Lag_nom  <- abs(Qp_time - T50)
  if (Lag_nom == 0) stop("Lag = 0: Qp_time == T50.")
  Tc_nom   <- Lag_nom / CONFIG$lag_to_tc_coef
  
  cat("  T50:", round(T50, 4), "h | Qp_time:", round(Qp_time, 4), "h\n")
  cat("  Lag:", round(Lag_nom, 4), "h | Tc:", round(Tc_nom, 4), "h")
  cat("  (Lag->Tc coef =", CONFIG$lag_to_tc_coef, ")\n")
  
  stat_lag <- function(data, i, t_v, p_v, q_v) {
    pb <- p_v[i]; qb <- q_v[i]
    sp <- sum(pb); if (sp <= 0) return(Lag_nom)
    i50 <- which(cumsum(pb) >= 0.5 * sp)[1]
    if (is.na(i50)) return(Lag_nom)
    abs(t_v[which.max(qb)[1]] - t_v[i50])
  }
  set.seed(CONFIG$boot_seed)
  boot_obj <- tryCatch(
    boot::boot(seq_along(t_grid), stat_lag, R = CONFIG$n_boot,
               sim = "ordinary", t_v = t_grid, p_v = P_grid, q_v = Qd_grid),
    error = function(e) {
      cat("  ⚠ WARNING: Bootstrap failed:", e$message, "\n")
      return(NULL)
    }
  )
  if (!is.null(boot_obj)) {
    boot_samples <- as.vector(boot_obj$t[, 1])
    boot_samples <- boot_samples[is.finite(boot_samples) & boot_samples > 0]
    
    ci_obj <- tryCatch(
      boot::boot.ci(boot_obj, conf = CONFIG$boot_conf, type = "bca"),
      error = function(e) boot::boot.ci(boot_obj, conf = CONFIG$boot_conf, type = "perc")
    )
    ci_m   <- if (!is.null(ci_obj$bca)) ci_obj$bca else ci_obj$percent
    Lag_lo <- if (!is.null(ci_m)) ci_m[4] else Lag_nom * 0.85
    Lag_hi <- if (!is.null(ci_m)) ci_m[5] else Lag_nom * 1.15
    
    cat("  Bootstrap: median Lag =", round(stats::median(boot_samples), 4),
        "h | mean =", round(mean(boot_samples), 4),
        "h | sd =", round(stats::sd(boot_samples), 4), "h\n")
    
    if (length(boot_samples) > 50) {
      conv_result <- check_bca_convergence(boot_samples)
      assign("bca_conv_result", conv_result, envir = parent.frame())
    }
  } else {
    Lag_lo <- Lag_nom * 0.85; Lag_hi <- Lag_nom * 1.15
    cat("  ⚠ Using heuristic CI (±15%) as fallback.\n")
  }
  
  Tc_lo <- Lag_lo / CONFIG$lag_to_tc_coef
  Tc_hi <- Lag_hi / CONFIG$lag_to_tc_coef
  cat("  Tc", round(CONFIG$boot_conf*100), "% CI (Bootstrap BCa): [",
      round(Tc_lo, 4), ",", round(Tc_hi, 4), "] h\n")
  profile_log <- rbind(profile_log,
                       data.frame(module = "Bootstrap BCa (Tc estimation)",
                                  time_seconds = as.numeric(difftime(Sys.time(), t0_tc, units = "secs"))))
  
  t0_glue <- Sys.time()
  glue_res <- NULL
  if (!is.na(D) && D > 0) {
    cat("\n[6/9] GLUE — Tc posterior distribution (Module A1):\n")
    glue_res <- analyse_GLUE(t_grid, Pe_grid, Qd_grid, D, A)
  }
  profile_log <- rbind(profile_log,
                       data.frame(module = "GLUE (N=5000)",
                                  time_seconds = as.numeric(difftime(Sys.time(), t0_glue, units = "secs"))))
  
  t0_uh <- Sys.time()
  KGE_SCS <- NA; KGEpp_SCS <- NA; NSE_SCS <- NA
  RMSE_SCS <- NA; NRMSE_SCS <- NA
  Qsim_SCS <- NULL; Qsim_Clark <- NULL; Qsim_GIUH <- NULL; UH_emp <- NULL
  peak_met <- NULL; acorr_test <- NULL; elasticity <- NA
  
  if (!is.na(D) && D > 0) {
    cat("\n[7/9] Unit hydrographs (Modules D1, D2, D3):\n")
    
    if (CONFIG$use_SCS_UH) {
      UH_scs  <- generate_SCS_UH(t_rel, Tc_nom, D, A)
      Qsim_SCS <- pmax(0, stats::convolve(Pe_grid * dt_h, rev(UH_scs), type="open")[seq_along(t_grid)])
      KGE_SCS   <- calculate_KGE(Qsim_SCS, Qd_grid)
      KGEpp_SCS <- calculate_KGEpp(Qsim_SCS, Qd_grid)
      NSE_SCS   <- calculate_NSE(Qsim_SCS, Qd_grid)
      rm_scs    <- calculate_RMSE(Qsim_SCS, Qd_grid)
      RMSE_SCS  <- rm_scs$RMSE; NRMSE_SCS <- rm_scs$NRMSE
      cat("  SCS UH   | KGE =", round(KGE_SCS, 3), "| NSE =", round(NSE_SCS, 3), "\n")
    }
    
    if (CONFIG$use_Clark_UH) {
      cat("  Generating Clark UH...\n")
      UH_clark  <- generate_Clark_UH(t_rel, Tc_nom, data$R_clark, A)
      Qsim_Clark <- pmax(0, stats::convolve(Pe_grid * dt_h, rev(UH_clark), type="open")[seq_along(t_grid)])
      KGE_cl <- calculate_KGE(Qsim_Clark, Qd_grid)
      NSE_cl <- calculate_NSE(Qsim_Clark, Qd_grid)
      cat("  Clark UH | KGE =", round(KGE_cl, 3), "| NSE =", round(NSE_cl, 3), "\n")
    }
    
    if (CONFIG$use_GIUH) {
      cat("  Generating GIUH...\n")
      GIUH_vec <- generate_GIUH(t_rel, data$channel_length_km, A,
                                data$v_giuh, data$Rb_strahler, data$Rl_strahler)
      if (!is.null(GIUH_vec)) {
        Qsim_GIUH <- pmax(0, stats::convolve(Pe_grid * dt_h, rev(GIUH_vec), type="open")[seq_along(t_grid)])
        KGE_gi <- calculate_KGE(Qsim_GIUH, Qd_grid)
        NSE_gi <- calculate_NSE(Qsim_GIUH, Qd_grid)
        cat("  GIUH     | KGE =", round(KGE_gi, 3), "| NSE =", round(NSE_gi, 3), "\n")
      }
    }
    
    if (CONFIG$use_deconv && sum(Pe_grid) > 0) {
      cat("  Tikhonov deconvolution (empirical UH)...\n")
      UH_emp <- tikhonov_deconvolution(Pe_grid, Qd_grid, dt_h, CONFIG$lambda_tikhonov)
      if (!is.null(UH_emp))
        cat("  Empirical UH: Qp =", round(max(UH_emp), 5), "m3/s/mm\n")
    }
    
    cat("\n[8/9] Advanced metrics (Modules E1-E4):\n")
    if (!is.null(Qsim_SCS)) {
      cat("  Peak metrics (E1, E3):\n")
      peak_met <- peak_metrics(Qsim_SCS, Qd_grid, t_grid)
      cat("    Peak Flow Bias (PBPF):", round(peak_met$Peak_Flow_Bias_pct, 2), "%\n")
      cat("    Peak Time Error      :", round(peak_met$Peak_Time_Error_h, 4), "h\n")
      cat("    Amplitude Ratio      :", round(peak_met$Amplitude_Ratio, 4), "\n")
      
      cat("  Residual autocorrelation tests (E2):\n")
      acorr_test <- autocorrelation_test(Qsim_SCS, Qd_grid)
      
      cat("  Tc -> Qp elasticity (E4):\n")
      elasticity <- calculate_Tc_Qp_elasticity(t_rel, Pe_grid, Tc_nom, D, A, dt_h)
    }
  }
  profile_log <- rbind(profile_log,
                       data.frame(module = "Unit hydrographs & metrics",
                                  time_seconds = as.numeric(difftime(Sys.time(), t0_uh, units = "secs"))))
  
  cat("\n[9/9] Hydrological Event Signature — FDC (Module E5):\n")
  signature_res <- calculate_event_signature(Q_grid, dt_h)
  
  cat("\n--- Empirical Tc Formulas (Reference) ---\n")
  df_emp <- empirical_Tc_formulas(data$channel_length_km, data$slope_m_per_m, A, data$elevation_drop_m)
  if (nrow(df_emp) > 0) {
    for (i in seq_len(nrow(df_emp)))
      cat("  ", df_emp$Formula[i], ":", df_emp$Tc_hours[i], "h\n")
  } else {
    cat("  Could not calculate (L, S or H missing).\n")
  }
  
  record_duration <- max(t_grid) - min(t_grid)
  if (record_duration < CONFIG$warn_duration_factor * Tc_nom) {
    accumulate_warning(paste0(
      "Record duration (", round(record_duration, 2), " h) < ",
      CONFIG$warn_duration_factor, " x Tc (", round(Tc_nom, 2), " h). ",
      "Baseflow separation may be unreliable. ",
      "Recommended duration >= ", round(CONFIG$warn_duration_factor * Tc_nom, 1), " h."
    ))
  }
  
  Pe_total_approx <- if (!is.null(cn_res)) max(cn_res$Qe_cum) else data$P_total
  Qp_expected <- 0.208 * A / Tc_nom * Pe_total_approx
  Qp_iber <- max(Q_grid, na.rm = TRUE)
  if (Qp_expected > 0 && Qp_iber > 0) {
    ratio_qp <- Qp_iber / Qp_expected
    if (ratio_qp < 0.1 || ratio_qp > 10) {
      accumulate_warning(paste0(
        "IBER peak discharge (", round(Qp_iber, 2), " m3/s) differs significantly ",
        "from SCS UH expectation (", round(Qp_expected, 2), " m3/s). ",
        "Ratio = ", round(ratio_qp, 3), ". Check IBER hydrograph consistency."
      ))
    }
  }
  
  write.csv(profile_log,
            file.path(RESULTS_DIR, "performance_log.csv"), row.names = FALSE)
  cat("\n--- Execution times by module ---\n")
  for (i in seq_len(nrow(profile_log))) {
    cat(sprintf("  %-40s %8.2f s\n", profile_log$module[i], profile_log$time_seconds[i]))
  }
  cat(sprintf("  %-40s %8.2f s (TOTAL)\n", "TOTAL", sum(profile_log$time_seconds)))
  
  return(list(
    t_grid = t_grid, dt_h = dt_h, t_rel = t_rel,
    P_grid = P_grid, Pe_grid = Pe_grid,
    Q_grid = Q_grid, Qd_grid = Qd_grid, Qb_grid = Qb_grid,
    Qsim_SCS = Qsim_SCS, Qsim_Clark = Qsim_Clark, Qsim_GIUH = Qsim_GIUH,
    UH_emp = UH_emp,
    T50 = T50, Qp_time = Qp_time, Lag = Lag_nom, Tc_nom = Tc_nom,
    Tc_CI_boot = c(lo = Tc_lo, hi = Tc_hi),
    glue = glue_res,
    PBIAS = PBIAS, Vol_P_m3 = Vol_P_m3, Vol_Q_m3 = Vol_Q_m3,
    bf_sep = bf_sep, alpha = alpha_est,
    cn_res = cn_res, dif_CN_IBER = dif_CN_IBER,
    KGE_SCS = KGE_SCS, KGEpp_SCS = KGEpp_SCS, NSE_SCS = NSE_SCS,
    RMSE_SCS = RMSE_SCS, NRMSE_SCS = NRMSE_SCS,
    peak_met = peak_met, acorr_test = acorr_test,
    elasticity = elasticity,
    signature = signature_res,
    df_emp = df_emp,
    profile_log = profile_log
  ))
}

# ==============================================================================
# 12. PLOT GENERATION (10 FIGURES)
# ==============================================================================

generate_plots <- function(data, res, plot_dir) {
  cat("\nGenerating plots...\n")
  
  th <- ggplot2::theme_bw(base_size = 11) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", hjust = 0.5, size = 11),
      axis.title = ggplot2::element_text(size = 10),
      legend.position = "bottom",
      legend.key.width = ggplot2::unit(1.2, "cm"),
      panel.grid.minor = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(8, 8, 8, 8)
    )
  
  t   <- res$t_grid; P <- res$P_grid; Q <- res$Q_grid
  Qd  <- res$Qd_grid; Qb <- res$Qb_grid
  
  maxQ <- max(Q, na.rm=TRUE); if (maxQ <= 0) maxQ <- 1
  maxP <- max(P, na.rm=TRUE); if (maxP <= 0) maxP <- 1
  scale_factor <- maxQ / maxP * 0.65
  
  df <- data.frame(t=t, P=P, Q=Q, Qd=Qd, Qb=Qb)
  
  save_plot <- function(filename, p, h = CONFIG$fig_height, w = NULL) {
    tryCatch(
      ggplot2::ggsave(file.path(plot_dir, filename), p,
                      width = if (is.null(w)) CONFIG$fig_width else w, 
                      height = h,
                      dpi = CONFIG$dpi_plots, units = "in"),
      error = function(e) cat("  ERROR saving", filename, ":", e$message, "\n")
    )
  }
  
  p1 <- ggplot2::ggplot(df, ggplot2::aes(x = t)) +
    ggplot2::geom_col(ggplot2::aes(y = P * scale_factor), fill = "#4a90d9", alpha = 0.30, width = res$dt_h * 0.9) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = Qb, ymax = Q), fill = "#2ca02c", alpha = 0.20) +
    ggplot2::geom_line(ggplot2::aes(y = Qb, color = "Baseflow"), linewidth = 0.7, linetype = "dotted") +
    ggplot2::geom_line(ggplot2::aes(y = Q, color = "Total discharge"), linewidth = 1.2) +
    ggplot2::geom_line(ggplot2::aes(y = Qd, color = "Direct runoff"), linewidth = 0.9, linetype = "dashed") +
    ggplot2::geom_vline(xintercept = res$T50, color="#1a9850", linetype=2, linewidth=0.7) +
    ggplot2::geom_vline(xintercept = res$Qp_time, color="#d73027", linetype=2, linewidth=0.7) +
    ggplot2::annotate("label", x = res$T50, y = maxQ * 0.92,
                      label = paste0("T50=", round(res$T50, 3), "h"),
                      color = "#1a9850", size = 3, fill = "white",
                      label.padding = ggplot2::unit(0.15, "lines"),
                      label.r = ggplot2::unit(0.1, "lines")) +
    ggplot2::annotate("label", x = res$Qp_time, y = maxQ * 0.78,
                      label = paste0("Qp_time=", round(res$Qp_time, 3), "h"),
                      color = "#d73027", size = 3, fill = "white",
                      label.padding = ggplot2::unit(0.15, "lines"),
                      label.r = ggplot2::unit(0.1, "lines")) +
    ggplot2::scale_y_continuous(
      name = expression(paste("Discharge (m"^3, "/s)")),
      sec.axis = ggplot2::sec_axis(~ . / scale_factor, name = "Precipitation (mm/h)")) +
    ggplot2::scale_color_manual(values = c(
      "Total discharge" = "#1f77b4", "Direct runoff" = "#d62728", "Baseflow" = "#2ca02c")) +
    ggplot2::labs(
      title = paste0("Hyetograph, Hydrograph and Baseflow Separation\n",
                     "Tc = ", round(res$Tc_nom, 3), " h | alpha = ", round(res$alpha, 4)),
      x = "Time (h)", color = NULL) + th
  save_plot("Fig01_Hyetograph_Hydrograph_Baseflow.png", p1)
  
  has_scs   <- !is.null(res$Qsim_SCS)
  has_clark <- !is.null(res$Qsim_Clark)
  has_giuh  <- !is.null(res$Qsim_GIUH)
  
  if (any(has_scs, has_clark, has_giuh)) {
    df2 <- df
    colores2 <- c("Observed runoff" = "#1f77b4")
    if (has_scs)   { df2$SCS   <- res$Qsim_SCS;   colores2 <- c(colores2, "SCS UH" = "#d62728") }
    if (has_clark) { df2$Clark <- res$Qsim_Clark; colores2 <- c(colores2, "Clark UH" = "#ff7f0e") }
    if (has_giuh)  { df2$GIUH  <- res$Qsim_GIUH;  colores2 <- c(colores2, "GIUH" = "#9467bd") }
    
    lab_m <- paste0("KGE=", round(res$KGE_SCS, 3), "  NSE=", round(res$NSE_SCS, 3))
    p2 <- ggplot2::ggplot(df2, ggplot2::aes(x = t)) +
      ggplot2::geom_line(ggplot2::aes(y = Qd, color = "Observed runoff"), linewidth = 1.3) +
      { if (has_scs)   ggplot2::geom_line(ggplot2::aes(y = SCS,   color = "SCS UH"),  linewidth=1.0, linetype="dashed") } +
      { if (has_clark) ggplot2::geom_line(ggplot2::aes(y = Clark, color = "Clark UH"), linewidth=1.0, linetype="dotdash") } +
      { if (has_giuh)  ggplot2::geom_line(ggplot2::aes(y = GIUH,  color = "GIUH"),     linewidth=1.0, linetype="dotted") } +
      ggplot2::scale_color_manual(values = colores2) +
      ggplot2::labs(title = paste0("Unit Hydrograph Comparison\n", lab_m),
                    x = "Time (h)", y = expression(paste("Discharge (m"^3, "/s)")), color = NULL) + th
    save_plot("Fig02_UH_Comparison.png", p2)
  }
  
  if (!is.null(res$Qsim_SCS)) {
    df3 <- data.frame(obs = Qd, sim = res$Qsim_SCS)
    df3 <- df3[is.finite(df3$obs) & is.finite(df3$sim), ]
    lim  <- max(c(df3$obs, df3$sim)) * 1.05
    p3  <- ggplot2::ggplot(df3, ggplot2::aes(x = obs, y = sim)) +
      ggplot2::geom_point(alpha = 0.35, color = "#2c7bb6", size = 1.2) +
      ggplot2::geom_abline(intercept=0, slope=1, linetype="dashed", color="red", linewidth=0.8) +
      ggplot2::geom_smooth(method="lm", se=TRUE, color="#1a9850", fill="#b8e186", alpha=0.3, linewidth=0.8) +
      ggplot2::coord_fixed(xlim=c(0,lim), ylim=c(0,lim)) +
      ggplot2::labs(title = "Scatter: Observed vs SCS UH Runoff",
                    x = expression(paste("Observed Q (m"^3, "/s)")),
                    y = expression(paste("Simulated Q (m"^3, "/s)"))) + th
    save_plot("Fig03_Obs_vs_Sim_Scatter.png", p3, h = 7, w = 7)
  }
  
  if (!is.null(res$glue)) {
    gl   <- res$glue
    df_g <- data.frame(Tc = gl$Tc_samples, KGE = gl$KGE_samples,
                       beh = gl$KGE_samples >= CONFIG$glue_kge_threshold)
    p4 <- ggplot2::ggplot(df_g, ggplot2::aes(x = Tc, y = KGE, color = beh, alpha = beh)) +
      ggplot2::geom_point(size = 0.6) +
      ggplot2::geom_hline(yintercept = CONFIG$glue_kge_threshold, linetype = "dashed", color = "red", linewidth = 0.7) +
      ggplot2::geom_vline(xintercept = gl$Tc_median, linetype = "dashed", color = "#1a9850", linewidth = 0.7) +
      ggplot2::scale_color_manual(values = c("FALSE" = "gray70", "TRUE" = "#2c7bb6"),
                                  labels = c("Non-behavioural", "Behavioural")) +
      ggplot2::scale_alpha_manual(values = c("FALSE" = 0.2, "TRUE" = 0.7), guide = "none") +
      ggplot2::labs(title = paste0("GLUE — Parameter Space Tc vs KGE\n",
                                   "N behavioural = ", gl$n_beh,
                                   " | Tc median = ", round(gl$Tc_median, 3), " h"),
                    x = "Tc (h)", y = "KGE", color = NULL) + th
    save_plot("Fig04_GLUE_Posterior.png", p4)
    
    df_beh_h <- data.frame(Tc = gl$Tc_beh, weight = gl$weights)
    p4b <- ggplot2::ggplot(df_beh_h, ggplot2::aes(x = Tc, weight = weight)) +
      ggplot2::geom_histogram(bins = 40, fill = "#2c7bb6", color = "white", alpha = 0.8) +
      ggplot2::geom_vline(xintercept = gl$Tc_median, color = "#d73027", linewidth = 1) +
      ggplot2::geom_vline(xintercept = gl$Tc_CI95["lo"], color = "#d73027", linewidth = 0.7, linetype = "dashed") +
      ggplot2::geom_vline(xintercept = gl$Tc_CI95["hi"], color = "#d73027", linewidth = 0.7, linetype = "dashed") +
      ggplot2::labs(title = paste0("Tc Posterior Distribution (GLUE)\n",
                                   "95% CI [", round(gl$Tc_CI95["lo"], 3), " — ",
                                   round(gl$Tc_CI95["hi"], 3), "] h"),
                    x = "Tc (h)", y = "Weighted density") + th
    save_plot("Fig04b_GLUE_Tc_Histogram.png", p4b)
  }
  
  df_m <- data.frame(
    t = t,
    P_pct  = cumsum(P) / max(sum(P), 1e-9) * 100,
    Qd_pct = cumsum(Qd) / max(sum(Qd), 1e-9) * 100
  )
  p5 <- ggplot2::ggplot(df_m, ggplot2::aes(x = t)) +
    ggplot2::geom_line(ggplot2::aes(y = P_pct,  color = "Precipitation"), linewidth = 1.2) +
    ggplot2::geom_line(ggplot2::aes(y = Qd_pct, color = "Direct runoff"), linewidth = 1.2) +
    ggplot2::geom_hline(yintercept = 50, linetype = "dotted", color = "gray40") +
    ggplot2::scale_color_manual(values = c("Precipitation" = "#4a90d9", "Direct runoff" = "#d62728")) +
    ggplot2::labs(title = "Cumulative Mass Curve", x = "Time (h)",
                  y = "Cumulative percentage (%)", color = NULL) + th
  save_plot("Fig05_Cumulative_Mass.png", p5)
  
  fdc_plot_df <- res$signature$fdc_df
  fdc_plot_df <- fdc_plot_df[fdc_plot_df$Discharge_m3s > 0, ]
  Q_min_pos <- min(fdc_plot_df$Discharge_m3s, na.rm = TRUE)
  Q_max_pos <- max(fdc_plot_df$Discharge_m3s, na.rm = TRUE)
  lim_inf_p6 <- 10^(floor(log10(Q_min_pos)))
  lim_sup_p6 <- 10^(ceiling(log10(Q_max_pos)))
  Q10_p6 <- if (!is.na(res$signature$Q10) && res$signature$Q10 > 0) res$signature$Q10 else NA
  Q90_p6 <- if (!is.na(res$signature$Q90) && res$signature$Q90 > 0) res$signature$Q90 else NA
  
  p6 <- ggplot2::ggplot(fdc_plot_df, ggplot2::aes(x = Exceedance_pct, y = Discharge_m3s)) +
    ggplot2::geom_line(color = "#1f77b4", linewidth = 1.2) +
    { if (!is.na(Q10_p6)) ggplot2::geom_hline(yintercept = Q10_p6, linetype = "dashed", color = "#d73027", linewidth = 0.6) } +
    { if (!is.na(Q90_p6)) ggplot2::geom_hline(yintercept = Q90_p6, linetype = "dashed", color = "#2ca02c", linewidth = 0.6) } +
    ggplot2::scale_y_log10(
      limits = c(lim_inf_p6, lim_sup_p6),
      labels = scales::comma,
      breaks = scales::log_breaks(n = 6)
    ) +
    ggplot2::labs(title = paste0("Hydrological Event Signature (FDC)\n",
                                 "R-B Flashiness Index = ", round(res$signature$RB, 4)),
                  x = "Exceedance (%)",
                  y = expression(paste("Discharge (m"^3, "/s) — log scale"))) + th
  save_plot("Fig06_Event_FDC.png", p6)
  
  fdc_plot_df <- res$signature$fdc_df
  fdc_plot_df <- fdc_plot_df[fdc_plot_df$Discharge_m3s > 0, ]
  
  p6b_top <- ggplot2::ggplot(fdc_plot_df, ggplot2::aes(x = Exceedance_pct, y = Discharge_m3s)) +
    ggplot2::geom_line(color = "#1f77b4", linewidth = 1.2) +
    ggplot2::geom_point(data = data.frame(x = c(10, 50, 90), 
                                          y = c(res$signature$Q10, res$signature$Q50, res$signature$Q90)),
                        ggplot2::aes(x = x, y = y), color = "#d73027", size = 3, shape = 18) +
    ggplot2::annotate("segment", 
                      x = 10, y = res$signature$Q10, 
                      xend = 90, yend = res$signature$Q90,
                      linetype = "dashed", color = "#2ca02c", linewidth = 0.8) +
    ggplot2::annotate("text", x = 50, y = sqrt(res$signature$Q10 * res$signature$Q90),
                      label = paste0("Slope = ", round(res$signature$recession_slope, 5)),
                      hjust = 0.5, vjust = -1, color = "#2ca02c", fontface = "bold") +
    ggplot2::scale_y_log10(
      labels = scales::comma,
      breaks = scales::log_breaks(n = 6)
    ) +
    ggplot2::labs(title = "Flow Duration Curve (FDC) - Log Scale",
                  x = NULL, y = expression(paste("Discharge (m"^3, "/s)"))) + th +
    ggplot2::theme(plot.margin = ggplot2::margin(5, 5, 0, 5))
  
  df_diff <- data.frame(
    Exceedance = fdc_plot_df$Exceedance_pct[-1],
    Delta_Q    = -diff(fdc_plot_df$Discharge_m3s) / diff(fdc_plot_df$Exceedance_pct)
  )
  df_diff$Delta_Q_smooth <- stats::lowess(df_diff$Exceedance, df_diff$Delta_Q, f = 0.1)$y
  
  p6b_bottom <- ggplot2::ggplot(df_diff, ggplot2::aes(x = Exceedance, y = Delta_Q)) +
    ggplot2::geom_line(color = "gray70", alpha = 0.7, linewidth = 0.5) +
    ggplot2::geom_line(ggplot2::aes(y = Delta_Q_smooth), color = "#ff7f0e", linewidth = 1.2) +
    ggplot2::scale_y_log10() +
    ggplot2::labs(title = "FDC Derivative (Rate of Change of Discharge)",
                  x = "Exceedance Percentage (%)",
                  y = expression(paste("-ΔQ/ΔExc (m"^3, "/s / %)"))) + th +
    ggplot2::theme(plot.margin = ggplot2::margin(0, 5, 5, 5))
  
  p6b <- patchwork::wrap_plots(p6b_top, p6b_bottom, ncol = 1, heights = c(2, 1))
  save_plot("Fig06b_FDC_Advanced_Analysis.png", p6b, h = 9)
  
  if (!is.null(res$UH_emp)) {
    df_uh <- data.frame(t = res$t_rel, UH_emp = res$UH_emp)
    p7 <- ggplot2::ggplot(df_uh, ggplot2::aes(x = t, y = UH_emp)) +
      ggplot2::geom_line(color = "#9467bd", linewidth = 1.2) +
      ggplot2::labs(title = paste0("Empirical UH by Tikhonov Deconvolution\n",
                                   "(lambda = ", CONFIG$lambda_tikhonov, ")"),
                    x = "Time (h)", y = "UH (m3/s/mm)") + th
    save_plot("Fig07_Empirical_UH_Tikhonov.png", p7)
  }
  
  if (length(res$bf_sep$all) > 1) {
    df_bf <- data.frame(t = t, Q_total = Q)
    bf_colors <- c("Q total" = "#1f77b4")
    for (nm in names(res$bf_sep$all)) {
      col_nm <- paste0("Qb_", nm)
      df_bf[[col_nm]] <- res$bf_sep$all[[nm]]$baseflow
      bf_colors[res$bf_sep$all[[nm]]$method] <- switch(nm,
                                                       eckhardt = "#d62728", chapman = "#ff7f0e", lyne_hollick = "#9467bd")
    }
    p8 <- ggplot2::ggplot(df_bf, ggplot2::aes(x = t)) +
      ggplot2::geom_line(ggplot2::aes(y = Q_total, color = "Q total"), linewidth = 1.2) +
      { if ("Qb_eckhardt" %in% names(df_bf))
        ggplot2::geom_line(ggplot2::aes(y = Qb_eckhardt, color = "Eckhardt (2005)"), linewidth = 0.9, linetype="dashed") } +
      { if ("Qb_chapman" %in% names(df_bf))
        ggplot2::geom_line(ggplot2::aes(y = Qb_chapman, color = "Chapman (1999)"), linewidth = 0.9, linetype="dotdash") } +
      { if ("Qb_lyne_hollick" %in% names(df_bf))
        ggplot2::geom_line(ggplot2::aes(y = Qb_lyne_hollick, color = "Lyne-Hollick (1979)"), linewidth = 0.9, linetype="dotted") } +
      ggplot2::scale_color_manual(values = c(
        "Q total" = "#1f77b4", "Eckhardt (2005)" = "#d62728",
        "Chapman (1999)" = "#ff7f0e", "Lyne-Hollick (1979)" = "#9467bd")) +
      ggplot2::labs(title = "Comparison of Baseflow Separation Filters\n(ISO 748 — sensitivity analysis)",
                    x = "Time (h)", y = expression(paste("Discharge (m"^3, "/s)")), color = NULL) + th
    save_plot("Fig08_Baseflow_Filter_Comparison.png", p8)
  }
  
  if (!is.null(res$glue)) {
    boot_samples <- res$glue$Tc_beh
    if (length(boot_samples) > 10) {
      df_qq <- data.frame(
        theoretical = stats::qnorm(stats::ppoints(length(boot_samples))),
        observed = sort(boot_samples)
      )
      sw_test <- tryCatch(
        stats::shapiro.test(boot_samples),
        error = function(e) list(statistic = NA, p.value = NA)
      )
      p9 <- ggplot2::ggplot(df_qq, ggplot2::aes(x = theoretical, y = observed)) +
        ggplot2::geom_point(color = "steelblue", alpha = 0.6, size = 1.5) +
        ggplot2::geom_abline(
          intercept = mean(boot_samples, na.rm = TRUE),
          slope = stats::sd(boot_samples, na.rm = TRUE),
          color = "red", linetype = "dashed", linewidth = 1
        ) +
        ggplot2::labs(
          title = "Q-Q Plot: Bootstrap BCa Distribution of Tc",
          subtitle = paste0("Shapiro-Wilk W = ", round(sw_test$statistic, 3),
                            ", p = ", round(sw_test$p.value, 4)),
          x = "Theoretical quantiles (normal)", y = "Observed quantiles (Tc, h)"
        ) + th
      save_plot("Fig09_QQ_Bootstrap_Tc.png", p9, h = 6)
    }
  }
  
  cat("  Plots saved to:", plot_dir, "\n")
}

# ==============================================================================
# 13. RESULTS EXPORT (EXCEL + CSV)
# ==============================================================================

export_results_excel <- function(data, res, out_path) {
  cat("\nExporting results to Excel...\n")
  
  gl <- res$glue
  
  df_summary <- data.frame(
    Parameter = c(
      "Watershed area (km2)", "Storm duration (h)", "CN (NRCS)",
      "Recession alpha (estimated)", "BFI (Eckhardt primary)",
      "Total precipitation (mm)", "Peak discharge total (m3/s)",
      "T50 rainfall centroid (h)", "Qp_time peak Qd (h)",
      "Lag (h)", "Tc_SCS (h)", "Tc CI95 lower Bootstrap (h)", "Tc CI95 upper Bootstrap (h)",
      "Tc GLUE median (h)", "Tc GLUE CI95 lower (h)", "Tc GLUE CI95 upper (h)",
      "Rainfall volume (m3)", "Runoff volume (m3)", "PBIAS (%)", "CN vs IBER diff (%)",
      "KGE_SCS", "KGEpp_SCS", "NSE_SCS", "RMSE_SCS (m3/s)", "NRMSE_SCS",
      "Peak Flow Bias PBPF (%)", "Peak Time Error (h)", "Amplitude Ratio",
      "Tc->Qp Elasticity", "R-B Flashiness Index"
    ),
    Value = c(
      data$watershed_area,
      ifelse(is.na(data$storm_duration), NA, data$storm_duration),
      ifelse(is.na(data$CN), NA, data$CN),
      round(res$alpha, 4), round(res$bf_sep$primary$BFI, 4),
      round(data$P_total, 3), round(data$Qpeak, 4),
      round(res$T50, 4), round(res$Qp_time, 4),
      round(res$Lag, 4), round(res$Tc_nom, 4),
      round(res$Tc_CI_boot["lo"], 4), round(res$Tc_CI_boot["hi"], 4),
      ifelse(is.null(gl), NA, round(gl$Tc_median, 4)),
      ifelse(is.null(gl), NA, round(gl$Tc_CI95["lo"], 4)),
      ifelse(is.null(gl), NA, round(gl$Tc_CI95["hi"], 4)),
      round(res$Vol_P_m3, 0), round(res$Vol_Q_m3, 0), round(res$PBIAS, 3),
      ifelse(is.na(res$dif_CN_IBER), NA, round(res$dif_CN_IBER, 2)),
      round(res$KGE_SCS, 4), round(res$KGEpp_SCS, 4), round(res$NSE_SCS, 4),
      round(res$RMSE_SCS, 5), round(res$NRMSE_SCS, 5),
      ifelse(is.null(res$peak_met), NA, round(res$peak_met$Peak_Flow_Bias_pct, 2)),
      ifelse(is.null(res$peak_met), NA, round(res$peak_met$Peak_Time_Error_h, 4)),
      ifelse(is.null(res$peak_met), NA, round(res$peak_met$Amplitude_Ratio, 4)),
      ifelse(is.na(res$elasticity), NA, round(res$elasticity, 4)),
      round(res$signature$RB, 4)
    ), stringsAsFactors = FALSE
  )
  
  df_int <- interpret_metrics(res$KGE_SCS, res$NSE_SCS, res$PBIAS)
  
  df_hydro <- data.frame(
    Time_h = round(res$t_grid, 5), P_mm_h = round(res$P_grid, 5),
    Pe_mm_h = round(res$Pe_grid, 5), Q_total_m3s = round(res$Q_grid, 5),
    Q_base_m3s = round(res$Qb_grid, 5), Q_direct_m3s = round(res$Qd_grid, 5)
  )
  if (!is.null(res$Qsim_SCS)) df_hydro$Q_SCS_UH_m3s <- round(res$Qsim_SCS, 5)
  if (!is.null(res$Qsim_Clark)) df_hydro$Q_Clark_UH_m3s <- round(res$Qsim_Clark, 5)
  if (!is.null(res$Qsim_GIUH)) df_hydro$Q_GIUH_m3s <- round(res$Qsim_GIUH, 5)
  
  df_filters <- data.frame(Time_h = round(res$t_grid, 5), Q_total = round(res$Q_grid, 5))
  for (nm in names(res$bf_sep$all)) {
    df_filters[[paste0("Qb_", nm)]] <- round(res$bf_sep$all[[nm]]$baseflow, 5)
    df_filters[[paste0("Qd_", nm)]] <- round(res$bf_sep$all[[nm]]$direct_runoff, 5)
  }
  
  df_signature <- data.frame(
    Indicator = c("Total Runoff Volume (m3)", "Maximum Discharge (m3/s)", "Mean Discharge (m3/s)",
                  "Q5 (Flood peak)", "Q10 (High flows)", "Q50 (Event median)",
                  "Q90 (Recession/Low)", "Q95 (Baseflow/End)",
                  "R-B Flashiness Index", "Recession Slope (Q10-Q90)"),
    Value = c(
      round(res$Vol_Q_m3, 0),
      round(max(res$Q_grid), 4),
      round(mean(res$Q_grid), 4),
      round(res$signature$Q5, 4),
      round(res$signature$Q10, 4),
      round(res$signature$Q50, 4),
      round(res$signature$Q90, 4),
      round(res$signature$Q95, 4),
      round(res$signature$RB, 6),
      round(res$signature$recession_slope, 6)
    ),
    Unit = c("m3", "m3/s", "m3/s", "m3/s", "m3/s", "m3/s", "m3/s", "m3/s", "-", "-"),
    Interpretation = c(
      "Total event volume", "Maximum recorded discharge", "Average event discharge",
      "Exceeded only 5% of the time", "Exceeded 10% of the time", "Median discharge",
      "Exceeded 90% of the time", "Exceeded 95% of the time",
      ifelse(res$signature$RB < 0.05, "ATTENUATED hydrograph", "Variable hydrograph"),
      ifelse(res$signature$recession_slope > -0.01, "VERY SLOW recession", "Moderate/fast recession")
    )
  )
  
  sheet_list <- list(
    Summary = df_summary, Interpretation = df_int, Hydrographs = df_hydro,
    Baseflow_Filters = df_filters, Flow_Duration_Curve = res$signature$fdc_df,
    Event_Signature = df_signature
  )
  if (nrow(res$df_emp) > 0) sheet_list$Empirical_Tc <- res$df_emp
  if (!is.null(res$glue)) {
    idx_sub <- sample(seq_along(res$glue$Tc_samples), min(500, length(res$glue$Tc_samples)))
    sheet_list$GLUE_Samples <- data.frame(
      Tc_h = round(res$glue$Tc_samples[idx_sub], 4),
      KGE  = round(res$glue$KGE_samples[idx_sub], 4),
      Behavioural = res$glue$KGE_samples[idx_sub] >= CONFIG$glue_kge_threshold
    )
  }
  
  writexl::write_xlsx(sheet_list, out_path)
  cat("  Excel:", out_path, "\n")
  
  if (CONFIG$export_csv) {
    csv_dir <- RESULTS_DIR
    cat("  Exporting additional CSVs...\n")
    
    write.csv(df_summary, file.path(csv_dir, "summary_results.csv"), row.names = FALSE)
    write.csv(df_hydro, file.path(csv_dir, "hydrographs_comparison.csv"), row.names = FALSE)
    write.csv(df_signature, file.path(csv_dir, "hydrological_signature.csv"), row.names = FALSE)
    
    if (!is.null(res$glue)) {
      write.csv(data.frame(
        Tc_h = res$glue$Tc_samples,
        KGE = res$glue$KGE_samples,
        behavioural = res$glue$KGE_samples >= CONFIG$glue_kge_threshold
      ), file.path(csv_dir, "GLUE_samples.csv"), row.names = FALSE)
    }
    
    if (!is.null(res$profile_log)) {
      write.csv(res$profile_log, file.path(csv_dir, "performance_log.csv"), row.names = FALSE)
    }
    
    cat("  CSVs saved to:", csv_dir, "\n")
  }
}

# ==============================================================================
# 14. TEXT REPORT
# ==============================================================================

generate_text_report <- function(data, res, report_path) {
  cat("Generating text report...\n")
  lines <- character(0)
  L  <- function(...) lines <<- c(lines, paste0(...))
  S  <- paste(rep("=", 80), collapse = "")
  s  <- paste(rep("-", 80), collapse = "")
  gl <- res$glue
  ac <- res$acorr_test
  
  L(S)
  L("        HYDROLOGICAL REPORT — IBER_TcEstimator v", CONFIG$version, sep = "")
  L("        Hydrological Event Signature")
  L(S)
  L("Date    : ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  L("Folder  : ", MAIN_DIR)
  L(s)
  L("")
  L("1. WATERSHED DATA")
  L("   Area (km2)       : ", data$watershed_area)
  L("   Duration (h)     : ", ifelse(is.na(data$storm_duration), "Not specified", data$storm_duration))
  L("   CN               : ", ifelse(is.na(data$CN), "Not specified", data$CN))
  L("   Channel L (km)   : ", ifelse(is.na(data$channel_length_km), "N/A", data$channel_length_km))
  L("   Slope (m/m)      : ", ifelse(is.na(data$slope_m_per_m), "N/A", data$slope_m_per_m))
  L("")
  L("2. BASEFLOW SEPARATION (ISO 748 — three filters)")
  L("   Alpha (recession): ", round(res$alpha, 4))
  for (nm in names(res$bf_sep$all)) {
    r_nm <- res$bf_sep$all[[nm]]
    L("   ", r_nm$method, "-> BFI = ", round(r_nm$BFI, 4))
  }
  L("")
  L("3. EFFECTIVE PRECIPITATION CN-NRCS")
  if (!is.null(res$cn_res)) {
    L("   CN      : ", res$cn_res$CN, " | S = ", round(res$cn_res$S, 2),
      " mm | Ia = ", round(res$cn_res$Ia, 2), " mm")
    L("   Pe total: ", round(max(res$cn_res$Qe_cum), 2), " mm")
    L("   CN vs IBER diff: ", round(res$dif_CN_IBER, 2), " %")
  } else {
    L("   Not calculated (CN not specified).")
  }
  L("")
  L("4. Tc ESTIMATION — SCS LAG METHOD")
  L("   T50 (h) : ", round(res$T50, 4))
  L("   Qp_time (h) : ", round(res$Qp_time, 4))
  L("   Lag (h) : ", round(res$Lag, 4))
  L("   Lag->Tc coef : ", CONFIG$lag_to_tc_coef)
  L("   Tc  (h) : ", round(res$Tc_nom, 4), " = ", round(res$Tc_nom * 60, 2), " min")
  L("   95% CI Bootstrap BCa: [", round(res$Tc_CI_boot["lo"], 4),
    ", ", round(res$Tc_CI_boot["hi"], 4), "] h")
  L("")
  L("5. GLUE — POSTERIOR DISTRIBUTION OF Tc")
  if (!is.null(gl)) {
    L("   N samples     : ", gl$N, " | N behavioural : ", gl$n_beh)
    L("   KGE threshold : ", CONFIG$glue_kge_threshold)
    L("   Tc median (h) : ", round(gl$Tc_median, 4))
    L("   Tc mean   (h) : ", round(gl$Tc_mean, 4))
    L("   Tc SD     (h) : ", round(gl$Tc_sd, 4))
    L("   95% CI GLUE   : [", round(gl$Tc_CI95["lo"], 4),
      ", ", round(gl$Tc_CI95["hi"], 4), "] h")
  } else {
    L("   Not calculated.")
  }
  L("")
  L("6. GOODNESS-OF-FIT METRICS (Qd obs vs SCS UH)")
  if (!is.na(res$KGE_SCS)) {
    L("   KGE   (Gupta 2009) : ", round(res$KGE_SCS, 4))
    L("   KGE'' (Kling 2012) : ", round(res$KGEpp_SCS, 4))
    L("   NSE   (Nash 1970)  : ", round(res$NSE_SCS, 4))
    L("   RMSE  (m3/s)       : ", round(res$RMSE_SCS, 5))
    L("   NRMSE              : ", round(res$NRMSE_SCS, 5))
    df_int <- interpret_metrics(res$KGE_SCS, res$NSE_SCS, res$PBIAS)
    for (i in seq_len(nrow(df_int)))
      L("   ", df_int$Metric[i], " -> ", df_int$Rating[i])
  }
  L("")
  L("7. PHASE AND AMPLITUDE METRICS (Modules E1-E3)")
  if (!is.null(res$peak_met)) {
    mp <- res$peak_met
    L("   Peak Flow Bias (PBPF) : ", round(mp$Peak_Flow_Bias_pct, 2), " %")
    L("   Peak Time Error       : ", round(mp$Peak_Time_Error_h, 4), " h")
    L("   Amplitude Ratio       : ", round(mp$Amplitude_Ratio, 4))
  }
  L("")
  L("8. RESIDUAL DIAGNOSTICS (Module E2)")
  if (!is.null(ac)) {
    L("   Durbin-Watson stat : ", round(ac$DW_stat, 3), " | p-value : ", round(ac$DW_pval, 4))
    L("   Ljung-Box stat     : ", round(ac$LB_stat, 3), " | p-value : ", round(ac$LB_pval, 4))
    L("   Conclusion         : ", ac$conclusion)
  }
  L("")
  L("9. SENSITIVITY AND ELASTICITY (Module E4)")
  L("   Tc->Qp Elasticity : ", ifelse(is.na(res$elasticity), "NA", round(res$elasticity, 4)))
  L("")
  L("10. HYDROLOGICAL EVENT SIGNATURE — FDC (Module E5)")
  L("    METHODOLOGICAL NOTE: The computed FDC corresponds to the synthetic")
  L("    design event and does NOT represent the annual hydrological regime.")
  L("    It is interpreted as the temporal flow distribution during the flood.")
  L("")
  L("    --- Hydrograph Shape Metrics ---")
  L("    R-B Flashiness Index : ", round(res$signature$RB, 6))
  if (!is.na(res$signature$RB) && res$signature$RB < 0.05) {
    L("    >> Interpretation: Highly ATTENUATED hydrograph.")
  } else if (!is.na(res$signature$RB) && res$signature$RB < 0.2) {
    L("    >> Interpretation: Moderately variable hydrograph.")
  } else {
    L("    >> Interpretation: TORRENTIAL hydrograph (high intrinsic variability).")
  }
  L("")
  L("    --- Event Percentiles (High Flow Duration) ---")
  L("    Q5  (Flood peak, exc. 5%)   : ", round(res$signature$Q5, 4), " m3/s")
  L("    Q10 (High flows, exc. 10%)  : ", round(res$signature$Q10, 4), " m3/s")
  L("    Q50 (Event median)          : ", round(res$signature$Q50, 4), " m3/s")
  L("    Q90 (Recession, exc. 90%)   : ", round(res$signature$Q90, 4), " m3/s")
  L("    Q95 (Baseflow/End, exc. 95%): ", round(res$signature$Q95, 4), " m3/s")
  L("")
  L("    Recession Slope (Q10-Q90) : ", round(res$signature$recession_slope, 6))
  if (!is.na(res$signature$recession_slope)) {
    if (res$signature$recession_slope > -0.01) {
      L("    >> Interpretation: VERY SLOW recession.")
    } else if (res$signature$recession_slope > -0.03) {
      L("    >> Interpretation: SLOW recession.")
    } else {
      L("    >> Interpretation: FAST recession.")
    }
  }
  L("")
  if (nrow(res$df_emp) > 0) {
    L("11. EMPIRICAL Tc FORMULAS (reference only)")
    for (i in seq_len(nrow(res$df_emp)))
      L("    ", res$df_emp$Formula[i], ": ", res$df_emp$Tc_hours[i], " h")
    L("")
  }
  L(S)
  L("REFERENCES (selected)")
  L("  Baker et al. (2004). JAWRA 40(2), 503-522.")
  L("  Beven & Binley (1992). Hydrol. Process. 6(3), 279-298.")
  L("  Eckhardt (2005). Hydrol. Process. 19(2), 507-515.")
  L("  Gupta et al. (2009). J. Hydrol. 377, 80-91.")
  L("  Moriasi et al. (2007). Trans. ASABE 50(3), 885-900.")
  L("  Nash & Sutcliffe (1970). J. Hydrol. 10(3), 282-290.")
  L("  NRCS (2004, 2010). NEH Part 630, Chapters 10, 15, 16. USDA.")
  L("  Rodriguez-Iturbe & Valdes (1979). WRR 15(6), 1409-1420.")
  L("  Searcy (1959). Flow-duration curves. USGS Water Supply Paper 1542-A.")
  L("  WMO (2008). Guide to Hydrological Practices, WMO-No. 168.")
  L(S)
  
  writeLines(lines, report_path)
  cat("  Report TXT:", report_path, "\n")
}

# ==============================================================================
# 15. MAIN EXECUTION
# ==============================================================================

cat(paste(rep("=", 80), collapse = ""), "\n")
cat("   STARTING ANALYSIS — IBER_TcEstimator v", CONFIG$version, " - Hydrological Event Signature\n", sep = "")
cat(paste(rep("=", 80), collapse = ""), "\n")

if (!file.exists(CONFIG$ruta_excel))
  stop("CRITICAL ERROR: File not found:\n  ", CONFIG$ruta_excel)

if (CONFIG$export_config_csv) {
  tryCatch({
    config_df <- data.frame(
      parameter = names(CONFIG),
      value = as.character(sapply(CONFIG, function(x) {
        if (length(x) > 1) paste(x, collapse = ", ") else as.character(x)
      })),
      stringsAsFactors = FALSE
    )
    write.csv(config_df, file.path(LOG_DIR, "config_used.csv"), row.names = FALSE)
    cat("Configuration saved to: ", file.path(LOG_DIR, "config_used.csv"), "\n")
  }, error = function(e) {
    cat("  NOTE: Could not export configuration:", e$message, "\n")
  })
}

t_global <- Sys.time()

input_data <- read_and_validate_data(CONFIG$ruta_excel, DATA_DIR)
results    <- hydrological_analysis(input_data)
generate_plots(input_data, results, PLOTS_DIR)
export_results_excel(input_data, results, EXCEL_OUT_PATH)
generate_text_report(input_data, results, REPORT_PATH)

total_time <- as.numeric(difftime(Sys.time(), t_global, units = "secs"))

cat("\n", paste(rep("=", 80), collapse = ""), "\n")
cat("   ANALYSIS COMPLETED — IBER_TcEstimator v", CONFIG$version, "\n", sep = "")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

cat("MAIN RESULTS:\n")
cat("  Tc SCS (Bootstrap) =", round(results$Tc_nom, 4), "h")
cat("  (", round(results$Tc_nom * 60, 2), "min)\n")
cat("  95% CI Bootstrap BCa: [",
    round(results$Tc_CI_boot["lo"], 4), ",",
    round(results$Tc_CI_boot["hi"], 4), "] h\n")
if (!is.null(results$glue)) {
  cat("  Tc GLUE median     =", round(results$glue$Tc_median, 4), "h\n")
  cat("  95% CI GLUE        : [",
      round(results$glue$Tc_CI95["lo"], 4), ",",
      round(results$glue$Tc_CI95["hi"], 4), "] h\n")
}

cat("\n--- HYDROLOGICAL EVENT SIGNATURE (FDC) ---\n")
cat("  NOTE: The FDC describes the temporal flow distribution during the flood.\n")
cat("  R-B Flashiness Index:", round(results$signature$RB, 6))
if (results$signature$RB < 0.05) {
  cat(" -> ATTENUATED hydrograph\n")
} else if (results$signature$RB < 0.2) {
  cat(" -> Moderately variable hydrograph\n")
} else {
  cat(" -> TORRENTIAL hydrograph\n")
}
cat("  Recession Slope:", round(results$signature$recession_slope, 6))
if (!is.na(results$signature$recession_slope)) {
  if (results$signature$recession_slope > -0.01) {
    cat(" -> VERY SLOW recession (High attenuation)\n")
  } else if (results$signature$recession_slope > -0.03) {
    cat(" -> SLOW recession (Moderate attenuation)\n")
  } else {
    cat(" -> FAST recession (Low attenuation)\n")
  }
}
cat("\n")
cat("  Q10 (High flow):", round(results$signature$Q10, 4), "m3/s\n")
cat("  Q50 (Median):   ", round(results$signature$Q50, 4), "m3/s\n")
cat("  Q90 (Recession):", round(results$signature$Q90, 4), "m3/s\n")

cat("\nTotal execution time:", round(total_time, 2), "s\n")

if (length(warnings_list) > 0) {
  cat("\n", paste(rep("=", 80), collapse = ""), "\n")
  cat("   ⚠ WARNINGS SUMMARY (", length(warnings_list), ")\n", sep = "")
  cat(paste(rep("=", 80), collapse = ""), "\n")
  for (w in warnings_list) {
    cat("  ", w, "\n")
  }
  cat("\n")
}

cat("\nRESULTS FOLDER:\n  ", MAIN_DIR, "\n\n")
cat("STRUCTURE:\n")
cat("  +-- 01_Input_Data/\n")
cat("  +-- 02_Numerical_Results/    <- Excel + CSV\n")
cat("  +-- 03_Text_Report/\n")
cat("  +-- 04_Plots_PNG/            <- 10 figures\n")
cat("  +-- 05_Execution_Log/        <- Config + session_info\n\n")

session_file <- file.path(LOG_DIR, "session_info.txt")
tryCatch({
  si <- utils::capture.output(utils::sessionInfo())
  writeLines(c(paste("Session IBER_TcEstimator v", CONFIG$version, " — ",
                     format(Sys.time(), "%Y-%m-%d %H:%M:%S"), sep = ""),
               paste(rep("-", 60), collapse=""), si), session_file)
  cat("  session_info saved to:", session_file, "\n")
}, error = function(e) invisible(NULL))

cat(paste(rep("=", 80), collapse = ""), "\n")
cat("   END OF PROGRAM\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
