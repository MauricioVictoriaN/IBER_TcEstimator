# ==============================================================================
# IBER_TcEstimator_v1.0_ES.R
# ==============================================================================
# Autor:       [Mauricio Javier Victoria Niño]
# Institucion: [Nombre de la Institucion]
# Fecha:       2026-04-21
# Version:     1.0.0 — Version Final (Espanol)
#
# Descripcion:
#   Estima el Tiempo de Concentracion (Tc) a partir de datos de salida del
#   modelo hidraulico-hidrologico IBER. Implementa el estado del arte en
#   hidrologia operativa segun protocolos WMO, ASCE, NRCS e ISO.
#
#   INCLUYE ANALISIS DE FIRMA HIDROLOGICA DEL EVENTO EXTREMO (FDC)
#   La Curva de Duracion de Caudales se interpreta como la distribucion
#   temporal de caudales durante la avenida de diseño, NO como un balance
#   hidrologico de largo plazo. Permite diagnosticar la capacidad de
#   laminacion del sistema y la forma del hidrograma.
#
# Modulos incluidos:
#   A1. GLUE - distribucion posterior completa de Tc
#   B1. Tres filtros de separacion de caudal base (Eckhardt, Chapman, Lyne-Hollick)
#   B2. Estimacion automatica de alpha por regresion de la curva de recesion
#   C1. Precipitacion efectiva explicita via CN-NRCS
#   C2. Verificacion de consistencia volumen IBER vs CN
#   D1. Hidrograma Unitario Geomorfologico (GIUH)
#   D2. HU Clark (tiempo de traslacion + almacenamiento)
#   D3. Deconvolucion inversa (Tikhonov) para HU empirico
#   E1. Error de fase y amplitud (Peak Time Error, Peak Flow Bias)
#   E2. Test de autocorrelacion de residuales (Durbin-Watson, Ljung-Box)
#   E3. Metricas especificas de pico (PBPF)
#   E4. Elasticidad Tc->Qp
#   E5. Firma Hidrologica del Evento (FDC) - Percentiles Q5, Q10, Q50, Q90, Q95
#       - Indice de Flashiness R-B
#       - Pendiente de Recesion (Slope Q10-Q90)
#       - Grafico de doble panel (FDC + Derivada)
#
#
# Licencia: MIT
# ==============================================================================

cat("\n")
cat("  _                                 _   _              _____           _ \n")
cat(" | |                               | | (_)            |_   _|         | |\n")
cat(" | |     __ _  __ _ _ __ ___   ___ | |_ _  ___  _ __    | |    ___ ___| |\n")
cat(" | |    / _` |/ _` | '_ ` _ \\ / _ \\| __| |/ _ \\| '_ \\   | |   / __/ __| |\n")
cat(" | |___| (_| | (_| | | | | | | (_) | |_| | (_) | | | | _| |_ | (__\\__ \\_|\n")
cat(" |______\\__,_|\\__, |_| |_| |_|\\___/ \\__|_|\\___/|_| |_| \\___/ \\___|___(_)\n")
cat("               __/ |                                                      \n")
cat("              |___/                                                       \n")
cat("\n")
cat("  =======================================================================\n")
cat("     LagTime & Tc Estimation - IBER & R Coupling\n")
cat("     IBER_TcEstimator v5.0 | Estimacion Tc & Firma Hidrologica del Evento\n")
cat("  =======================================================================\n\n")

# --- 1. CONFIGURACION CENTRALIZADA ---
CONFIG <- list(
  ruta_excel      = "D:/R/Hietograma_HidrogramaIBER.xlsx",
  ruta_salida     = "D:/R",
  dt_interp_h     = 0.005,
  n_boot          = 2000,
  boot_seed       = 42,
  boot_conf       = 0.95,
  glue_N          = 5000,
  glue_seed       = 123,
  glue_kge_umbral = 0.50,
  glue_Tc_rango   = c(0.05, 24),
  BFI_max         = 0.80,
  alpha_baseflow  = NULL,
  metodos_base    = c("eckhardt", "chapman", "lyne_hollick"),
  CN_forzado      = NULL,
  Ia_lambda       = 0.2,
  usar_HUS_SCS    = TRUE,
  usar_HU_Clark   = TRUE,
  usar_GIUH       = TRUE,
  usar_deconv     = TRUE,
  lambda_tikhonov = 0.01,
  min_registros   = 10,
  umbral_outlier  = 3.0,
  umbral_qbase_pct = 5.0,
  usar_formulas_empiricas = TRUE,
  dpi_graficos    = 300,
  ancho_fig       = 14,
  alto_fig        = 7
)

# --- 2. PAQUETES ---
required_packages <- c("zoo", "dplyr", "hydroGOF", "boot", "lmtest",
                       "ggplot2", "patchwork", "scales", "readxl", "writexl")
cat("Verificando paquetes requeridos...\n")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat("  Instalando:", pkg, "\n")
    install.packages(pkg, quiet = TRUE)
  }
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}
cat("  Todos los paquetes cargados.\n\n")

# --- 3. ESTRUCTURA DE DIRECTORIOS Y LOG ---
TIMESTAMP      <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
DIR_PRINCIPAL  <- file.path(CONFIG$ruta_salida, paste0("Resultados_IBERv5_", TIMESTAMP))
DIR_DATOS      <- file.path(DIR_PRINCIPAL, "01_Datos_Entrada")
DIR_RESULTADOS <- file.path(DIR_PRINCIPAL, "02_Resultados_Numericos")
DIR_INFORME    <- file.path(DIR_PRINCIPAL, "03_Informe_TXT")
DIR_GRAFICOS   <- file.path(DIR_PRINCIPAL, "04_Graficos_PNG")
DIR_LOG        <- file.path(DIR_PRINCIPAL, "05_Log_Ejecucion")

for (d in c(DIR_PRINCIPAL, DIR_DATOS, DIR_RESULTADOS,
            DIR_INFORME, DIR_GRAFICOS, DIR_LOG)) {
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
}

RUTA_EXCEL_OUT <- file.path(DIR_RESULTADOS, "Resultados_IBERv5.xlsx")
RUTA_INFORME   <- file.path(DIR_INFORME,    "Informe_IBERv5.txt")
RUTA_LOG       <- file.path(DIR_LOG,        "Log_IBERv5.txt")

log_con <- file(RUTA_LOG, open = "wt")
sink(log_con, split = TRUE)
on.exit({ sink(); close(log_con) }, add = TRUE)

cat("LOG DE EJECUCION -", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

# ==============================================================================
# 4. FUNCIONES AUXILIARES GENERALES
# ==============================================================================

detectar_outliers_iqr <- function(x, nombre = "variable", factor_iqr = 3.0) {
  q25  <- stats::quantile(x, 0.25, na.rm = TRUE)
  q75  <- stats::quantile(x, 0.75, na.rm = TRUE)
  iqr  <- q75 - q25
  lim_i <- q25 - factor_iqr * iqr
  lim_s <- q75 + factor_iqr * iqr
  idx   <- which(x < lim_i | x > lim_s)
  if (length(idx) > 0)
    cat("  ALERTA outliers IQR en", nombre, ":", length(idx),
        "punto(s) fuera de [", round(lim_i, 4), ",", round(lim_s, 4), "]\n")
  return(idx)
}

verificar_serie_temporal <- function(t, nombre = "serie") {
  dt   <- diff(t)
  if (any(dt <= 0))
    warning("Tiempos no estrictamente crecientes en ", nombre)
  cv   <- stats::sd(dt) / mean(dt)
  if (cv > 0.05)
    cat("  NOTA:", nombre, "tiene dt irregular (CV=", round(cv*100,1), "%).\n")
  list(dt_mean = mean(dt), dt_cv = cv)
}

# ==============================================================================
# 5. MODULO B: SEPARACION DE CAUDAL BASE
# ==============================================================================

filtro_eckhardt <- function(Q, alpha, BFI_max) {
  n  <- length(Q); Qb <- numeric(n); Qb[1] <- Q[1]
  d  <- 1 - BFI_max * alpha
  for (i in 2:n) {
    Qb[i] <- ((1 - BFI_max) * alpha * Qb[i-1] + (1 - alpha) * BFI_max * Q[i]) / d
    Qb[i] <- min(Qb[i], Q[i]); Qb[i] <- max(Qb[i], 0)
  }
  list(Q_base = Qb, Q_directa = Q - Qb)
}

filtro_chapman <- function(Q, alpha) {
  n  <- length(Q); Qb <- numeric(n); Qb[1] <- Q[1]
  for (i in 2:n) {
    Qb[i] <- (3*alpha - 1)/(3 - alpha) * Qb[i-1] + (1 - alpha)/(3 - alpha) * (Q[i] + Q[i-1])
    Qb[i] <- min(Qb[i], Q[i]); Qb[i] <- max(Qb[i], 0)
  }
  list(Q_base = Qb, Q_directa = Q - Qb)
}

filtro_lyne_hollick <- function(Q, alpha) {
  n   <- length(Q); Qd <- numeric(n); Qd[1] <- 0
  for (i in 2:n) {
    Qd[i] <- alpha * Qd[i-1] + (1 + alpha)/2 * (Q[i] - Q[i-1])
    Qd[i] <- max(Qd[i], 0); Qd[i] <- min(Qd[i], Q[i])
  }
  Qb <- Q - Qd
  list(Q_base = Qb, Q_directa = Qd)
}

estimar_alpha_recesion <- function(Q, dt_h, min_tramo = 5) {
  n   <- length(Q)
  dec <- c(FALSE, diff(Q) < 0)
  grupos <- cumsum(!dec)
  tramos <- split(seq_len(n), grupos)
  tramos_rec <- Filter(function(idx) length(idx) >= min_tramo && all(dec[idx]), tramos)
  
  if (length(tramos_rec) == 0) {
    cat("  NOTA: No se encontraron tramos de recesion. Se usara alpha = 0.925.\n")
    return(0.925)
  }
  
  alphas <- numeric(0)
  for (idx in tramos_rec) {
    q_tr <- Q[idx]
    if (any(q_tr <= 0)) next
    t_tr <- seq(0, (length(idx)-1) * dt_h, by = dt_h)
    lm_fit <- tryCatch(stats::lm(log(q_tr) ~ t_tr), error = function(e) NULL)
    if (is.null(lm_fit)) next
    k_est <- coef(lm_fit)[2]
    a_est <- exp(k_est * dt_h)
    if (a_est > 0.5 && a_est < 0.999) alphas <- c(alphas, a_est)
  }
  
  if (length(alphas) == 0) {
    cat("  NOTA: Regresion de recesion sin resultados validos. Se usa alpha = 0.925.\n")
    return(0.925)
  }
  
  alpha_est <- stats::median(alphas)
  cat("  Alpha estimado por recesion:", round(alpha_est, 4),
      "(mediana de", length(alphas), "tramos)\n")
  return(alpha_est)
}

separar_caudal_base_comparado <- function(Q, dt_h, BFI_max, alpha_cfg,
                                          metodos = CONFIG$metodos_base) {
  cat("  Estimando alpha por curva de recesion...\n")
  alpha <- if (is.null(alpha_cfg)) estimar_alpha_recesion(Q, dt_h) else alpha_cfg
  
  resultados <- list()
  if ("eckhardt" %in% metodos) {
    r <- filtro_eckhardt(Q, alpha, BFI_max)
    resultados$eckhardt <- list(
      Q_base = r$Q_base, Q_directa = r$Q_directa,
      BFI = sum(r$Q_base) / sum(Q), alpha = alpha, metodo = "Eckhardt (2005)"
    )
  }
  if ("chapman" %in% metodos) {
    r <- filtro_chapman(Q, alpha)
    resultados$chapman <- list(
      Q_base = r$Q_base, Q_directa = r$Q_directa,
      BFI = sum(r$Q_base) / sum(Q), alpha = alpha, metodo = "Chapman (1999)"
    )
  }
  if ("lyne_hollick" %in% metodos) {
    r <- filtro_lyne_hollick(Q, alpha)
    resultados$lyne_hollick <- list(
      Q_base = r$Q_base, Q_directa = r$Q_directa,
      BFI = sum(r$Q_base) / sum(Q), alpha = alpha, metodo = "Lyne-Hollick (1979)"
    )
  }
  
  cat("  Comparacion de filtros (alpha =", round(alpha, 4), "):\n")
  for (nm in names(resultados)) {
    cat("    ", resultados[[nm]]$metodo, "-> BFI =", round(resultados[[nm]]$BFI, 4), "\n")
  }
  
  metodo_ppal <- if ("eckhardt" %in% names(resultados)) "eckhardt" else names(resultados)[1]
  cat("  Metodo principal para analisis:", resultados[[metodo_ppal]]$metodo, "\n")
  
  return(list(primario = resultados[[metodo_ppal]], todos = resultados, alpha = alpha))
}

# ==============================================================================
# 6. MODULO C: PRECIPITACION EFECTIVA CN-NRCS
# ==============================================================================

precipitacion_efectiva_CN <- function(P_acum, CN, lambda = 0.2) {
  if (is.na(CN) || CN <= 0 || CN >= 100) {
    cat("  ADVERTENCIA: CN invalido o no especificado. Se usa P total.\n")
    return(NULL)
  }
  S   <- 25.4 * (1000 / CN - 10)
  Ia  <- lambda * S
  
  Q_e_acum <- ifelse(P_acum > Ia, (P_acum - Ia)^2 / (P_acum - Ia + S), 0)
  Pe_tasa <- c(0, diff(Q_e_acum))
  Pe_tasa[Pe_tasa < 0] <- 0
  
  cat("  CN =", CN, "| S =", round(S, 2), "mm | Ia =", round(Ia, 2),
      "mm | Pe_total =", round(max(Q_e_acum), 2), "mm\n")
  
  return(list(Q_e_acum = Q_e_acum, Pe_tasa = Pe_tasa, S = S, Ia = Ia, CN = CN))
}

verificar_consistencia_CN <- function(Vol_Q_m3, area_km2, Pe_mm) {
  Vol_CN_m3 <- Pe_mm / 1000 * area_km2 * 1e6
  dif_pct   <- 100 * (Vol_CN_m3 - Vol_Q_m3) / Vol_CN_m3
  cat("  Vol. escorrentia CN-NRCS:", format(round(Vol_CN_m3, 0), big.mark = ","), "m3\n")
  cat("  Vol. escorrentia IBER   :", format(round(Vol_Q_m3, 0), big.mark = ","), "m3\n")
  cat("  Diferencia CN vs IBER   :", round(dif_pct, 2), "%\n")
  if (abs(dif_pct) > 25)
    cat("  ALERTA: Diferencia >25%. Verificar consistencia del CN en IBER.\n")
  return(dif_pct)
}

# ==============================================================================
# 7. MODULO D: HIDROGRAMAS UNITARIOS
# ==============================================================================

generar_HUS_SCS <- function(t, Tc, D, A = 1) {
  Tp <- D / 2 + 0.6 * Tc
  Tb <- 2.67 * Tp
  qp <- (0.208 * A) / Tp
  HUS <- ifelse(t <= 0, 0,
         ifelse(t <= Tp, qp * (t / Tp),
         ifelse(t <= Tb, qp * (1 - (t - Tp) / (Tb - Tp)), 0)))
  return(HUS)
}

generar_HU_Clark <- function(t, Tc, R = NULL, A = 1) {
  if (is.null(R) || is.na(R)) R <- Tc
  dt  <- mean(diff(t))
  if (dt <= 0) return(rep(0, length(t)))
  
  n_pasos <- ceiling(Tc / dt)
  if (n_pasos < 2) n_pasos <- 2
  t_tia   <- seq(0, Tc, length.out = n_pasos + 1)
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

generar_GIUH <- function(t, L_km, A_km2, v_ms = 1.0, Rb = 4.5, Rl = 2.0) {
  if (is.na(L_km) || is.na(A_km2) || L_km <= 0 || A_km2 <= 0) {
    cat("  ADVERTENCIA GIUH: L o A no especificados. GIUH omitido.\n")
    return(NULL)
  }
  
  L_m   <- L_km * 1000
  t_r   <- L_m / v_ms / 3600
  k_g   <- 0.44 * t_r * (Rb / Rl)^0.55
  m_g   <- 3.29 * (Rb / Rl)^0.78 * (L_m / (A_km2 * 1e6)^0.5)^0.07
  
  if (k_g <= 0 || m_g <= 0) {
    cat("  ADVERTENCIA GIUH: parametros invalidos. GIUH omitido.\n")
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

deconvolucion_tikhonov <- function(P_vec, Q_vec, dt_h, lambda = 0.01) {
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
      cat("  ADVERTENCIA Deconvolucion: sistema singular.", e$message, "\n")
      return(NULL)
    }
  )
  if (!is.null(h_est)) h_est[h_est < 0] <- 0
  return(h_est)
}

# ==============================================================================
# 8. MODULO A: GLUE
# ==============================================================================

analisis_GLUE <- function(t_grid, P_efectiva, Qd_obs, D, A,
                           N = CONFIG$glue_N, kge_umb = CONFIG$glue_kge_umbral,
                           Tc_rango = CONFIG$glue_Tc_rango, semilla = CONFIG$glue_seed) {
  cat("\n[GLUE] Muestreo Monte Carlo: N =", N, "realizaciones...\n")
  set.seed(semilla)
  Tc_samples <- stats::runif(N, Tc_rango[1], Tc_rango[2])
  
  dt_h  <- mean(diff(t_grid))
  t_rel <- t_grid - min(t_grid)
  Pe_dt <- P_efectiva * dt_h
  n_t   <- length(t_grid)
  
  evaluar_una_realizacion <- function(Tc_i) {
    HUS_i <- generar_HUS_SCS(t_rel, Tc_i, D, A)
    Qs_i  <- pmax(0, stats::convolve(Pe_dt, rev(HUS_i), type = "open")[seq_len(n_t)])
    kge_val <- calcular_KGE(Qs_i, Qd_obs)
    if (is.na(kge_val)) -Inf else kge_val
  }
  
  KGE_samples <- unlist(lapply(Tc_samples, evaluar_una_realizacion))
  
  idx_beh  <- which(KGE_samples >= kge_umb)
  n_beh    <- length(idx_beh)
  cat("  Realizaciones behavioral (KGE >=", kge_umb, "):", n_beh,
      "(", round(100 * n_beh / N, 1), "% del total)\n")
  
  if (n_beh < 10) {
    cat("  ADVERTENCIA: <10 realizaciones behavioral.\n")
    return(NULL)
  }
  
  Tc_beh   <- Tc_samples[idx_beh]
  KGE_beh  <- KGE_samples[idx_beh]
  
  pesos    <- KGE_beh - min(KGE_beh) + 1e-9
  pesos    <- pesos / sum(pesos)
  
  Tc_media <- sum(Tc_beh * pesos)
  Tc_med   <- stats::median(Tc_beh)
  Tc_sd    <- sqrt(sum(pesos * (Tc_beh - Tc_media)^2))
  Tc_q025  <- stats::quantile(Tc_beh, 0.025)
  Tc_q975  <- stats::quantile(Tc_beh, 0.975)
  
  cat("  Posterior Tc — mediana:", round(Tc_med, 4),
      "h | IC 95% [", round(Tc_q025, 4), ",", round(Tc_q975, 4), "] h\n")
  
  return(list(
    Tc_samples = Tc_samples, KGE_samples = KGE_samples,
    idx_beh = idx_beh, Tc_beh = Tc_beh, KGE_beh = KGE_beh,
    pesos = pesos, Tc_media = Tc_media, Tc_mediana = Tc_med,
    Tc_sd = Tc_sd, Tc_IC95 = c(lo = unname(Tc_q025), hi = unname(Tc_q975)),
    n_beh = n_beh, N = N
  ))
}

# ==============================================================================
# 9. MODULO E: METRICAS AVANZADAS
# ==============================================================================

calcular_KGE <- function(sim, obs) {
  ok <- stats::complete.cases(sim, obs) & is.finite(sim) & is.finite(obs)
  s  <- sim[ok]; o <- obs[ok]
  if (length(s) < CONFIG$min_registros) return(NA_real_)
  if (stats::sd(s) == 0 || stats::sd(o) == 0) return(NA_real_)
  r  <- stats::cor(s, o)
  1 - sqrt((r - 1)^2 + (stats::sd(s)/stats::sd(o) - 1)^2 + (mean(s)/mean(o) - 1)^2)
}

calcular_KGEpp <- function(sim, obs) {
  ok <- stats::complete.cases(sim, obs) & is.finite(sim) & is.finite(obs)
  s  <- sim[ok]; o <- obs[ok]
  if (length(s) < CONFIG$min_registros) return(NA_real_)
  if (mean(o) == 0 || mean(s) == 0) return(NA_real_)
  r     <- stats::cor(s, o)
  gamma <- (stats::sd(s) / mean(s)) / (stats::sd(o) / mean(o))
  beta  <- mean(s) / mean(o)
  1 - sqrt((r - 1)^2 + (gamma - 1)^2 + (beta - 1)^2)
}

calcular_NSE <- function(sim, obs) {
  ok <- stats::complete.cases(sim, obs) & is.finite(sim) & is.finite(obs)
  s  <- sim[ok]; o <- obs[ok]
  if (length(s) < CONFIG$min_registros) return(NA_real_)
  d  <- sum((o - mean(o))^2); if (d == 0) return(NA_real_)
  1 - sum((o - s)^2) / d
}

calcular_RMSE <- function(sim, obs) {
  ok   <- stats::complete.cases(sim, obs) & is.finite(sim) & is.finite(obs)
  s    <- sim[ok]; o <- obs[ok]
  if (length(s) < CONFIG$min_registros) return(list(RMSE = NA, NRMSE = NA))
  rmse <- sqrt(mean((o - s)^2))
  rng  <- diff(range(o)); nrmse <- if (rng > 0) rmse / rng else NA_real_
  list(RMSE = rmse, NRMSE = nrmse)
}

metricas_pico <- function(sim, obs, t_vec) {
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

test_autocorrelacion <- function(sim, obs) {
  resid <- as.numeric(sim - obs)
  resid <- resid[is.finite(resid)]
  if (length(resid) < 15) {
    cat("  NOTA: Serie demasiado corta para tests de autocorrelacion.\n")
    return(list(DW_stat = NA, DW_pval = NA, LB_stat = NA, LB_pval = NA,
                conclusion = "Serie insuficiente"))
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
  
  conclu <- if (is.na(DW_pval) && is.na(LB_pval)) {
    "No calculado"
  } else if ((!is.na(DW_pval) && DW_pval < 0.05) || (!is.na(LB_pval) && LB_pval < 0.05)) {
    "Autocorrelacion significativa: error estructural en el modelo"
  } else {
    "Sin autocorrelacion significativa: residuales aceptablemente aleatorios"
  }
  
  cat("  Durbin-Watson: stat =", round(DW_stat, 3),
      "| p-valor =", round(DW_pval, 4), "\n")
  cat("  Ljung-Box    : stat =", round(LB_stat, 3),
      "| p-valor =", round(LB_pval, 4), "\n")
  cat("  Conclusion   :", conclu, "\n")
  
  list(DW_stat = DW_stat, DW_pval = DW_pval, LB_stat = LB_stat, LB_pval = LB_pval,
       conclusion = conclu)
}

calcular_elasticidad_Tc_Qp <- function(t_rel, P_efectiva, Tc_nom, D, A, dt_h) {
  delta  <- 0.05
  Tc_up  <- Tc_nom * (1 + delta)
  Tc_dn  <- Tc_nom * (1 - delta)
  
  HUS_up <- generar_HUS_SCS(t_rel, Tc_up, D, A)
  HUS_dn <- generar_HUS_SCS(t_rel, Tc_dn, D, A)
  HUS_0  <- generar_HUS_SCS(t_rel, Tc_nom, D, A)
  
  conv_func <- function(HUS) pmax(0,
    stats::convolve(P_efectiva * dt_h, rev(HUS), type = "open")[seq_along(t_rel)])
  
  Qp_up  <- max(conv_func(HUS_up))
  Qp_dn  <- max(conv_func(HUS_dn))
  Qp_0   <- max(conv_func(HUS_0))
  
  if (Qp_0 == 0) return(NA_real_)
  
  eps <- ((Qp_up - Qp_dn) / (2 * Qp_0)) / delta
  cat("  Elasticidad epsilon (Tc->Qp):", round(eps, 4),
      "  [DeltaQp/Qp por DeltaTc/Tc]\n")
  return(eps)
}

# ==============================================================================
# 9. MODULO E5: FIRMA HIDROLOGICA DEL EVENTO (FDC)
# ==============================================================================

calcular_firma_hidrologica_evento <- function(Q, dt_h) {
  
  cat("\n  --- FIRMA HIDROLOGICA DEL EVENTO EXTREMO (FDC) ---\n")
  cat("  NOTA METODOLOGICA: La FDC calculada corresponde al evento sintetico\n")
  cat("  de diseno y NO representa el regimen hidrologico anual. Se interpreta\n")
  cat("  como la distribucion temporal de caudales durante la avenida.\n")
  
  # 1. Indice de Flashiness Richards-Baker (R-B Index)
  RB <- sum(abs(diff(Q))) / sum(Q)
  cat("  Indice Flashiness R-B:", round(RB, 4))
  if (RB < 0.05) {
    cat(" -> Hidrograma altamente LAMINADO\n")
  } else if (RB < 0.2) {
    cat(" -> Hidrograma moderadamente variable\n")
  } else {
    cat(" -> Hidrograma TORRENCIAL (alta variabilidad)\n")
  }

  # 2. Construccion de la Curva de Permanencia del Evento
  Q_sort   <- sort(Q, decreasing = TRUE)
  n        <- length(Q_sort)
  prob_exc <- (1:n) / (n + 1) * 100
  
  df_perm  <- data.frame(
    Excedencia_pct = round(prob_exc, 2),
    Caudal_m3s     = round(Q_sort, 5)
  )
  
  # 3. Percentiles del Evento
  Q5  <- stats::quantile(Q, 0.95, na.rm = TRUE)
  Q10 <- stats::quantile(Q, 0.90, na.rm = TRUE)
  Q50 <- stats::quantile(Q, 0.50, na.rm = TRUE)
  Q90 <- stats::quantile(Q, 0.10, na.rm = TRUE)
  Q95 <- stats::quantile(Q, 0.05, na.rm = TRUE)
  
  # 4. Pendiente de Recesion (Slope Q10-Q90)
  if (Q10 > 0 && Q90 > 0 && Q10 != Q90) {
      slope_recesion <- (log10(Q10) - log10(Q90)) / (90 - 10)
  } else {
      slope_recesion <- NA_real_
  }
  
  cat("  --- Percentiles del Evento (Duracion de Caudales) ---\n")
  cat("    Q5  (exc. 5%)  :", round(Q5, 4), "m3/s  [Pico de la avenida]\n")
  cat("    Q10 (exc. 10%) :", round(Q10, 4), "m3/s  [Caudales altos]\n")
  cat("    Q50 (exc. 50%) :", round(Q50, 4), "m3/s  [Caudal mediano del evento]\n")
  cat("    Q90 (exc. 90%) :", round(Q90, 4), "m3/s  [Recesion / Agotamiento]\n")
  cat("    Q95 (exc. 95%) :", round(Q95, 4), "m3/s  [Caudal base / Final]\n")
  cat("  Pendiente de Recesion (Slope Q10-Q90):", round(slope_recesion, 6))
  if (!is.na(slope_recesion)) {
    if (slope_recesion > -0.01) {
      cat(" -> Recesion MUY LENTA (Alta laminacion)\n")
    } else if (slope_recesion > -0.03) {
      cat(" -> Recesion LENTA (Laminacion moderada)\n")
    } else {
      cat(" -> Recesion RAPIDA (Poca laminacion)\n")
    }
  } else {
    cat("\n")
  }

  return(list(
    RB             = RB,
    df_perm        = df_perm,
    Q5             = Q5,
    Q10            = Q10,
    Q50            = Q50,
    Q90            = Q90,
    Q95            = Q95,
    slope_recesion = slope_recesion
  ))
}

interpretar_metricas <- function(KGE, NSE, PBIAS) {
  rating_KGE <- if (is.na(KGE)) "No calculado"
  else if (KGE > 0.75) "Muy Bueno"
  else if (KGE > 0.50) "Bueno"
  else if (KGE > 0.00) "Satisfactorio"
  else if (KGE > -0.41) "Insatisfactorio (>media obs.)"
  else "Insatisfactorio (<media obs.)"
  
  rating_NSE <- if (is.na(NSE)) "No calculado"
  else if (NSE > 0.75) "Muy Bueno"
  else if (NSE > 0.65) "Bueno"
  else if (NSE > 0.50) "Satisfactorio"
  else "Insatisfactorio"
  
  rating_PB  <- if (is.na(PBIAS)) "No calculado"
  else if (abs(PBIAS) < 10) "Muy Bueno"
  else if (abs(PBIAS) < 15) "Bueno"
  else if (abs(PBIAS) < 25) "Satisfactorio"
  else "Insatisfactorio"
  
  data.frame(
    Metrica       = c("KGE", "NSE", "PBIAS (%)"),
    Valor         = round(c(KGE, NSE, PBIAS), 3),
    Interpretacion = c(rating_KGE, rating_NSE, rating_PB),
    stringsAsFactors = FALSE
  )
}

formulas_empiricas_Tc <- function(L, S, A, H = NA) {
  res <- data.frame(Formula = character(), Tc_horas = numeric(),
                    Aplicacion = character(), stringsAsFactors = FALSE)
  if (!is.na(L) && !is.na(S) && S > 0) {
    L_m  <- L * 1000
    Tc_k <- 0.0663 * L_m^0.77 / S^0.385 / 60
    res  <- rbind(res, data.frame(Formula = "Kirpich (1940)",
              Tc_horas = round(Tc_k, 4),
              Aplicacion = "Cuencas agricolas pequenas (<0.45 km2)"))
    Tc_t <- 0.3 * (L / S^0.25)^0.76
    res  <- rbind(res, data.frame(Formula = "Temez (1978)",
              Tc_horas = round(Tc_t, 4),
              Aplicacion = "Cuencas mediterraneas/semiaridas (MOPU)"))
  }
  if (!is.na(A) && !is.na(L) && !is.na(H) && H > 0) {
    Tc_g <- (4 * sqrt(A) + 1.5 * L) / (0.8 * sqrt(H))
    res  <- rbind(res, data.frame(Formula = "Giandotti (1934)",
              Tc_horas = round(Tc_g, 4),
              Aplicacion = "Cuencas medianas-grandes, Europa meridional"))
  }
  return(res)
}

# ==============================================================================
# 10. LECTURA Y VALIDACION DE DATOS
# ==============================================================================

leer_y_validar_datos <- function(ruta_excel, dir_datos) {
  cat("\n", paste(rep("-", 80), collapse = ""), "\n")
  cat("   MODULO 1: LECTURA Y VALIDACION DE DATOS\n")
  cat(paste(rep("-", 80), collapse = ""), "\n\n")
  
  if (!file.exists(ruta_excel)) stop("ERROR: Archivo no encontrado: ", ruta_excel)
  file.copy(ruta_excel,
            file.path(dir_datos, "Datos_Entrada_Original.xlsx"),
            overwrite = TRUE)
  cat("Archivo:", ruta_excel, "\n")
  
  hojas <- readxl::excel_sheets(ruta_excel)
  cat("Hojas:", paste(hojas, collapse = ", "), "\n")
  
  for (h in c("Metadatos", "Hietograma", "Hidrograma")) {
    if (!h %in% hojas) stop("ERROR: Falta hoja '", h, "'")
  }
  
  meta_raw <- readxl::read_excel(ruta_excel, sheet = "Metadatos")
  names(meta_raw) <- tolower(trimws(names(meta_raw)))
  meta <- meta_raw
  
  leer_meta_robusto <- function(meta, patrones) {
    for (patron in patrones) {
      idx <- grep(patron, meta$parametro, ignore.case = TRUE, perl = TRUE)[1]
      if (!is.na(idx)) {
        valor <- suppressWarnings(as.numeric(meta$valor[idx]))
        if (!is.na(valor)) return(valor)
      }
    }
    return(NA_real_)
  }
  
  area_cuenca <- leer_meta_robusto(meta, c(
    "Area de la cuenca", "area.*cuenca", "area.*km"
  ))
  
  duracion_tormenta <- leer_meta_robusto(meta, c(
    "Duracion de tormenta", "duracion", "tormenta"
  ))
  
  L_cauce_km <- leer_meta_robusto(meta, c(
    "L_cauce_km", "L_cauce", "L_km", "longitud.*cauce"
  ))
  
  S_pendiente <- leer_meta_robusto(meta, c(
    "S_pendiente_m_m", "S_pendiente", "pendiente", "S_m"
  ))
  
  H_desnivel <- leer_meta_robusto(meta, c(
    "H_desnivel_m", "H_desnivel", "desnivel", "H_m"
  ))
  
  CN_meta <- leer_meta_robusto(meta, c(
    "CN (Numero de Curva NRCS)", "CN", "Numero de Curva", "curve.*number"
  ))
  
  R_clark <- leer_meta_robusto(meta, c(
    "R_clark_h", "R_clark", "coeficiente.*almacen"
  ))
  
  Rb_strahler <- leer_meta_robusto(meta, c(
    "Rb_Strahler", "Rb", "bifurcacion"
  ))
  
  Rl_strahler <- leer_meta_robusto(meta, c(
    "Rl_Strahler", "Rl", "longitud.*horton"
  ))
  
  v_giuh <- leer_meta_robusto(meta, c(
    "v_GIUH_m_s", "v_GIUH", "velocidad"
  ))
  
  CN_usar <- if (!is.null(CONFIG$CN_forzado)) CONFIG$CN_forzado else CN_meta
  
  if (is.na(area_cuenca) || area_cuenca <= 0)
    stop("ERROR: Area de cuenca no encontrada o invalida.")
  
  cat("\nMetadatos cargados:\n")
  cat("  Area (km2)       :", area_cuenca, "\n")
  cat("  Duracion (h)     :", ifelse(is.na(duracion_tormenta), "No especificada", duracion_tormenta), "\n")
  cat("  L cauce (km)     :", ifelse(is.na(L_cauce_km), "No esp.", L_cauce_km), "\n")
  cat("  S media (m/m)    :", ifelse(is.na(S_pendiente), "No esp.", S_pendiente), "\n")
  cat("  H desnivel (m)   :", ifelse(is.na(H_desnivel), "No esp.", H_desnivel), "\n")
  cat("  CN               :", ifelse(is.na(CN_usar), "No esp.", CN_usar), "\n")
  cat("  R Clark (h)      :", ifelse(is.na(R_clark), "No esp. (se usa R=Tc)", R_clark), "\n")
  cat("  Rb Strahler      :", ifelse(is.na(Rb_strahler), "No esp. (defecto 4.5)", Rb_strahler), "\n")
  cat("  Rl Strahler      :", ifelse(is.na(Rl_strahler), "No esp. (defecto 2.0)", Rl_strahler), "\n")
  cat("  v GIUH (m/s)     :", ifelse(is.na(v_giuh), "No esp. (defecto 1.0)", v_giuh), "\n")
  
  ll_raw <- readxl::read_excel(ruta_excel, sheet = "Hietograma")
  lluvia <- data.frame(Tiempo = as.numeric(ll_raw[[1]]),
                       Precip = as.numeric(ll_raw[[2]]))
  lluvia <- lluvia[stats::complete.cases(lluvia) & lluvia$Precip >= 0, ]
  lluvia <- lluvia[order(lluvia$Tiempo), ]
  
  ca_raw <- readxl::read_excel(ruta_excel, sheet = "Hidrograma")
  caudal <- data.frame(Tiempo = as.numeric(ca_raw[[1]]),
                       Caudal = as.numeric(ca_raw[[2]]))
  caudal <- caudal[stats::complete.cases(caudal) & caudal$Caudal >= 0, ]
  caudal <- caudal[order(caudal$Tiempo), ]
  
  cat("\nRegistros: Hietograma =", nrow(lluvia), "| Hidrograma =", nrow(caudal), "\n")
  
  cat("\nVerificacion de calidad:\n")
  verificar_serie_temporal(lluvia$Tiempo, "Hietograma")
  verificar_serie_temporal(caudal$Tiempo, "Hidrograma")
  detectar_outliers_iqr(lluvia$Precip, "Precipitacion", CONFIG$umbral_outlier)
  detectar_outliers_iqr(caudal$Caudal, "Caudal", CONFIG$umbral_outlier)
  
  dt_P   <- mean(diff(lluvia$Tiempo))
  P_tot  <- sum(lluvia$Precip) * dt_P
  Qpico  <- max(caudal$Caudal, na.rm = TRUE)
  Q_ini  <- mean(utils::head(caudal$Caudal, 5))
  pct_b  <- 100 * Q_ini / Qpico
  
  cat("\nEstadisticas previas:\n")
  cat("  P total (suma*dt):", round(P_tot, 2), "mm\n")
  cat("  Caudal pico      :", round(Qpico, 4), "m3/s\n")
  cat("  Caudal inicial   :", round(Q_ini, 4), "m3/s (", round(pct_b, 1), "% del pico)\n")
  if (pct_b > CONFIG$umbral_qbase_pct)
    cat("  ALERTA: Caudal base significativo detectado. Se aplicara separacion.\n")
  
  return(list(
    lluvia            = lluvia,
    caudal            = caudal,
    area_cuenca       = area_cuenca,
    duracion_tormenta = duracion_tormenta,
    L_cauce_km        = L_cauce_km,
    S_pendiente       = S_pendiente,
    H_desnivel        = H_desnivel,
    CN                = CN_usar,
    R_clark           = R_clark,
    Rb_strahler       = ifelse(is.na(Rb_strahler), 4.5, Rb_strahler),
    Rl_strahler       = ifelse(is.na(Rl_strahler), 2.0, Rl_strahler),
    v_giuh            = ifelse(is.na(v_giuh), 1.0, v_giuh),
    P_total           = P_tot,
    Qpico             = Qpico,
    Q_ini             = Q_ini
  ))
}

# ==============================================================================
# 11. ANALISIS HIDROLOGICO PRINCIPAL
# ==============================================================================

analisis_hidrologico <- function(datos) {
  cat("\n", paste(rep("=", 80), collapse = ""), "\n")
  cat("   MODULO 2: ANALISIS HIDROLOGICO COMPLETO (v4.0.1)\n")
  cat(paste(rep("=", 80), collapse = ""), "\n\n")
  
  lluvia <- datos$lluvia; caudal <- datos$caudal
  A      <- datos$area_cuenca; D  <- datos$duracion_tormenta
  
  t_min  <- max(min(lluvia$Tiempo), min(caudal$Tiempo))
  t_max  <- min(max(lluvia$Tiempo), max(caudal$Tiempo))
  t_grid <- seq(t_min, t_max, by = CONFIG$dt_interp_h)
  dt_h   <- CONFIG$dt_interp_h
  t_rel  <- t_grid - min(t_grid)
  
  P_grid <- pmax(0, stats::approx(lluvia$Tiempo, lluvia$Precip, xout = t_grid, rule = 2)$y)
  Q_grid <- pmax(0, stats::approx(caudal$Tiempo, caudal$Caudal, xout = t_grid, rule = 2)$y)
  cat("[1/9] Interpolacion: dt =", dt_h, "h |", length(t_grid), "puntos\n")
  
  Vol_P_m3 <- sum(P_grid) * dt_h / 1000 * (A * 1e6)
  Vol_Q_m3 <- sum(Q_grid) * dt_h * 3600
  PBIAS    <- 100 * (Vol_P_m3 - Vol_Q_m3) / Vol_P_m3
  cat("\n[2/9] Balance de masa:\n")
  cat("  Vol. lluvia:", format(round(Vol_P_m3,0), big.mark=","), "m3\n")
  cat("  Vol. caudal:", format(round(Vol_Q_m3,0), big.mark=","), "m3\n")
  cat("  PBIAS:", round(PBIAS, 2), "%\n")
  
  cat("\n[3/9] Separacion de caudal base (Modulos B1, B2):\n")
  sep_base <- separar_caudal_base_comparado(Q_grid, dt_h, CONFIG$BFI_max,
                                             CONFIG$alpha_baseflow, CONFIG$metodos_base)
  alpha_est <- sep_base$alpha
  Qb_grid   <- sep_base$primario$Q_base
  Qd_grid   <- sep_base$primario$Q_directa
  
  cat("\n[4/9] Precipitacion efectiva CN-NRCS (Modulo C1):\n")
  P_acum_grid <- cumsum(P_grid) * dt_h
  cn_res <- precipitacion_efectiva_CN(P_acum_grid, datos$CN, CONFIG$Ia_lambda)
  
  if (!is.null(cn_res)) {
    Pe_grid <- cn_res$Pe_tasa
    cat("\n  Verificacion consistencia CN vs IBER (Modulo C2):\n")
    dif_CN_IBER <- verificar_consistencia_CN(Vol_Q_m3, A, max(cn_res$Q_e_acum))
  } else {
    Pe_grid <- P_grid
    dif_CN_IBER <- NA
  }
  
  cat("\n[5/9] Estimacion Tc — metodo Lag SCS:\n")
  P_acum_c <- cumsum(P_grid); sum_P <- sum(P_grid)
  if (sum_P <= 0) stop("Precipitacion acumulada = 0.")
  idx_T50  <- which(P_acum_c >= 0.5 * sum_P)[1]
  T50      <- t_grid[idx_T50]
  idx_Tp   <- which.max(Qd_grid)[1]
  Tp       <- t_grid[idx_Tp]
  Lag_nom  <- abs(Tp - T50)
  if (Lag_nom == 0) stop("Lag = 0: Tp == T50.")
  Tc_nom   <- Lag_nom / 0.6
  
  cat("  T50:", round(T50, 4), "h | Tp:", round(Tp, 4), "h\n")
  cat("  Lag:", round(Lag_nom, 4), "h | Tc:", round(Tc_nom, 4), "h\n")
  
  stat_lag <- function(idx, t_v, p_v, q_v) {
    pb <- p_v[idx]; qb <- q_v[idx]
    sp <- sum(pb); if (sp <= 0) return(Lag_nom)
    i50 <- which(cumsum(pb) >= 0.5 * sp)[1]
    if (is.na(i50)) return(Lag_nom)
    abs(t_v[which.max(qb)[1]] - t_v[i50])
  }
  set.seed(CONFIG$boot_seed)
  boot_obj <- tryCatch(
    boot::boot(seq_along(t_grid), stat_lag, R = CONFIG$n_boot,
               sim = "ordinary", t_v = t_grid, p_v = P_grid, q_v = Qd_grid),
    error = function(e) NULL
  )
  if (!is.null(boot_obj)) {
    ci_obj <- tryCatch(
      boot::boot.ci(boot_obj, conf = CONFIG$boot_conf, type = "bca"),
      error = function(e) boot::boot.ci(boot_obj, conf = CONFIG$boot_conf, type = "perc")
    )
    ci_m   <- if (!is.null(ci_obj$bca)) ci_obj$bca else ci_obj$percent
    Lag_lo <- if (!is.null(ci_m)) ci_m[4] else Lag_nom * 0.85
    Lag_hi <- if (!is.null(ci_m)) ci_m[5] else Lag_nom * 1.15
  } else {
    Lag_lo <- Lag_nom * 0.85; Lag_hi <- Lag_nom * 1.15
  }
  Tc_lo <- Lag_lo / 0.6; Tc_hi <- Lag_hi / 0.6
  cat("  Tc IC", round(CONFIG$boot_conf*100), "% (Bootstrap BCa): [",
      round(Tc_lo, 4), ",", round(Tc_hi, 4), "] h\n")
  
  glue_res <- NULL
  if (!is.na(D) && D > 0) {
    cat("\n[6/9] GLUE — distribucion posterior de Tc (Modulo A1):\n")
    glue_res <- analisis_GLUE(t_grid, Pe_grid, Qd_grid, D, A)
  }
  
  KGE_SCS <- NA; KGEpp_SCS <- NA; NSE_SCS <- NA
  RMSE_SCS <- NA; NRMSE_SCS <- NA
  Qsint_SCS <- NULL; Qsint_Clark <- NULL; Qsint_GIUH <- NULL; HU_emp <- NULL
  mets_pico <- NULL; test_acorr <- NULL; elasticidad <- NA
  
  if (!is.na(D) && D > 0) {
    cat("\n[7/9] Hidrogramas unitarios (Modulos D1, D2, D3):\n")
    
    if (CONFIG$usar_HUS_SCS) {
      HUS_scs  <- generar_HUS_SCS(t_rel, Tc_nom, D, A)
      Qsint_SCS <- pmax(0, stats::convolve(Pe_grid * dt_h, rev(HUS_scs), type="open")[seq_along(t_grid)])
      KGE_SCS   <- calcular_KGE(Qsint_SCS, Qd_grid)
      KGEpp_SCS <- calcular_KGEpp(Qsint_SCS, Qd_grid)
      NSE_SCS   <- calcular_NSE(Qsint_SCS, Qd_grid)
      rm_scs    <- calcular_RMSE(Qsint_SCS, Qd_grid)
      RMSE_SCS  <- rm_scs$RMSE; NRMSE_SCS <- rm_scs$NRMSE
      cat("  HUS SCS   | KGE =", round(KGE_SCS, 3), "| NSE =", round(NSE_SCS, 3), "\n")
    }
    
    if (CONFIG$usar_HU_Clark) {
      cat("  Generando HU Clark...\n")
      HU_clark  <- generar_HU_Clark(t_rel, Tc_nom, datos$R_clark, A)
      Qsint_Clark <- pmax(0, stats::convolve(Pe_grid * dt_h, rev(HU_clark), type="open")[seq_along(t_grid)])
      KGE_cl <- calcular_KGE(Qsint_Clark, Qd_grid)
      NSE_cl <- calcular_NSE(Qsint_Clark, Qd_grid)
      cat("  HU Clark  | KGE =", round(KGE_cl, 3), "| NSE =", round(NSE_cl, 3), "\n")
    }
    
    if (CONFIG$usar_GIUH) {
      cat("  Generando GIUH...\n")
      GIUH_vec <- generar_GIUH(t_rel, datos$L_cauce_km, A,
                               datos$v_giuh, datos$Rb_strahler, datos$Rl_strahler)
      if (!is.null(GIUH_vec)) {
        Qsint_GIUH <- pmax(0, stats::convolve(Pe_grid * dt_h, rev(GIUH_vec), type="open")[seq_along(t_grid)])
        KGE_gi <- calcular_KGE(Qsint_GIUH, Qd_grid)
        NSE_gi <- calcular_NSE(Qsint_GIUH, Qd_grid)
        cat("  GIUH      | KGE =", round(KGE_gi, 3), "| NSE =", round(NSE_gi, 3), "\n")
      }
    }
    
    if (CONFIG$usar_deconv && sum(Pe_grid) > 0) {
      cat("  Deconvolucion Tikhonov (HU empirico)...\n")
      HU_emp <- deconvolucion_tikhonov(Pe_grid, Qd_grid, dt_h, CONFIG$lambda_tikhonov)
      if (!is.null(HU_emp))
        cat("  HU empirico: Qp =", round(max(HU_emp), 5), "m3/s/mm\n")
    }
    
    cat("\n[8/9] Metricas avanzadas (Modulos E1-E4):\n")
    if (!is.null(Qsint_SCS)) {
      cat("  Metricas de pico (E1, E3):\n")
      mets_pico <- metricas_pico(Qsint_SCS, Qd_grid, t_grid)
      cat("    Peak Flow Bias (PBPF):", round(mets_pico$Peak_Flow_Bias_pct, 2), "%\n")
      cat("    Peak Time Error      :", round(mets_pico$Peak_Time_Error_h, 4), "h\n")
      cat("    Amplitude Ratio      :", round(mets_pico$Amplitude_Ratio, 4), "\n")
      
      cat("  Test autocorrelacion residuales (E2):\n")
      test_acorr <- test_autocorrelacion(Qsint_SCS, Qd_grid)
      
      cat("  Elasticidad Tc -> Qp (E4):\n")
      elasticidad <- calcular_elasticidad_Tc_Qp(t_rel, Pe_grid, Tc_nom, D, A, dt_h)
    }
  }
  
  cat("\n[9/9] Firma Hidrologica del Evento Extremo - FDC (Modulo E5):\n")
  flash_res <- calcular_firma_hidrologica_evento(Q_grid, dt_h)
  
  cat("\n--- Formulas empiricas de Tc (Referencia) ---\n")
  df_emp <- formulas_empiricas_Tc(datos$L_cauce_km, datos$S_pendiente, A, datos$H_desnivel)
  if (nrow(df_emp) > 0) {
    for (i in seq_len(nrow(df_emp)))
      cat("  ", df_emp$Formula[i], ":", df_emp$Tc_horas[i], "h\n")
  } else {
    cat("  No se pudieron calcular (faltan L, S o H).\n")
  }
  
  return(list(
    t_grid = t_grid, dt_h = dt_h, t_rel = t_rel,
    P_grid = P_grid, Pe_grid = Pe_grid,
    Q_grid = Q_grid, Qd_grid = Qd_grid, Qb_grid = Qb_grid,
    Qsint_SCS = Qsint_SCS, Qsint_Clark = Qsint_Clark, Qsint_GIUH = Qsint_GIUH,
    HU_emp = HU_emp,
    T50 = T50, Tp = Tp, Lag = Lag_nom, Tc_nom = Tc_nom,
    Tc_IC_boot = c(lo = Tc_lo, hi = Tc_hi),
    glue = glue_res,
    PBIAS = PBIAS, Vol_P_m3 = Vol_P_m3, Vol_Q_m3 = Vol_Q_m3,
    sep_base = sep_base, alpha = alpha_est,
    cn_res = cn_res, dif_CN_IBER = dif_CN_IBER,
    KGE_SCS = KGE_SCS, KGEpp_SCS = KGEpp_SCS, NSE_SCS = NSE_SCS,
    RMSE_SCS = RMSE_SCS, NRMSE_SCS = NRMSE_SCS,
    mets_pico = mets_pico, test_acorr = test_acorr,
    elasticidad = elasticidad,
    flash_res = flash_res,
    df_emp = df_emp
  ))
}

# ==============================================================================
# 12. GENERACION DE GRAFICOS (9 FIGURAS - CORREGIDO)
# ==============================================================================

generar_graficos <- function(datos, res, dir_out) {
  cat("\nGenerando graficos...\n")
  
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
  esc  <- maxQ / maxP * 0.65
  
  df <- data.frame(t=t, P=P, Q=Q, Qd=Qd, Qb=Qb)
  
  salvar <- function(nombre, p, h = CONFIG$alto_fig) {
    tryCatch(
      ggplot2::ggsave(file.path(dir_out, nombre), p,
                      width = CONFIG$ancho_fig, height = h,
                      dpi = CONFIG$dpi_graficos, units = "in"),
      error = function(e) cat("  ERROR guardando", nombre, ":", e$message, "\n")
    )
  }
  
  # Fig 1: Hietograma + Hidrograma + separacion caudal base
  p1 <- ggplot2::ggplot(df, ggplot2::aes(x = t)) +
    ggplot2::geom_col(ggplot2::aes(y = P * esc), fill = "#4a90d9", alpha = 0.30, width = res$dt_h * 0.9) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = Qb, ymax = Q), fill = "#2ca02c", alpha = 0.20) +
    ggplot2::geom_line(ggplot2::aes(y = Qb, color = "Caudal base"), linewidth = 0.7, linetype = "dotted") +
    ggplot2::geom_line(ggplot2::aes(y = Q, color = "Caudal total"), linewidth = 1.2) +
    ggplot2::geom_line(ggplot2::aes(y = Qd, color = "Escorrentia directa"), linewidth = 0.9, linetype = "dashed") +
    ggplot2::geom_vline(xintercept = res$T50, color="#1a9850", linetype=2, linewidth=0.7) +
    ggplot2::geom_vline(xintercept = res$Tp, color="#d73027", linetype=2, linewidth=0.7) +
    ggplot2::annotate("label", x = res$T50, y = maxQ * 0.92,
                      label = paste0("T50=", round(res$T50, 3), "h"),
                      color = "#1a9850", size = 3, fill = "white",
                      label.padding = ggplot2::unit(0.15, "lines"),
                      label.r = ggplot2::unit(0.1, "lines")) +
    ggplot2::annotate("label", x = res$Tp, y = maxQ * 0.78,
                      label = paste0("Tp=", round(res$Tp, 3), "h"),
                      color = "#d73027", size = 3, fill = "white",
                      label.padding = ggplot2::unit(0.15, "lines"),
                      label.r = ggplot2::unit(0.1, "lines")) +
    ggplot2::scale_y_continuous(
      name = expression(paste("Caudal (m"^3, "/s)")),
      sec.axis = ggplot2::sec_axis(~ . / esc, name = "Precipitacion (mm/h)")) +
    ggplot2::scale_color_manual(values = c(
      "Caudal total" = "#1f77b4", "Escorrentia directa" = "#d62728", "Caudal base" = "#2ca02c")) +
    ggplot2::labs(
      title = paste0("Hietograma, Hidrograma y Separacion de Caudal Base\n",
                     "Tc = ", round(res$Tc_nom, 3), " h | alpha = ", round(res$alpha, 4)),
      x = "Tiempo (h)", color = NULL) + th
  salvar("Fig01_Hietograma_Hidrograma_Baseflow.png", p1)
  
  # Fig 2: Comparacion de HU sinteticos
  has_scs   <- !is.null(res$Qsint_SCS)
  has_clark <- !is.null(res$Qsint_Clark)
  has_giuh  <- !is.null(res$Qsint_GIUH)
  
  if (any(has_scs, has_clark, has_giuh)) {
    df2 <- df
    colores2 <- c("Escorrentia obs." = "#1f77b4")
    if (has_scs)   { df2$SCS   <- res$Qsint_SCS;   colores2 <- c(colores2, "HUS SCS" = "#d62728") }
    if (has_clark) { df2$Clark <- res$Qsint_Clark; colores2 <- c(colores2, "HU Clark" = "#ff7f0e") }
    if (has_giuh)  { df2$GIUH  <- res$Qsint_GIUH;  colores2 <- c(colores2, "GIUH" = "#9467bd") }
    
    lab_m <- paste0("KGE=", round(res$KGE_SCS, 3), "  NSE=", round(res$NSE_SCS, 3))
    p2 <- ggplot2::ggplot(df2, ggplot2::aes(x = t)) +
      ggplot2::geom_line(ggplot2::aes(y = Qd, color = "Escorrentia obs."), linewidth = 1.3) +
      { if (has_scs)   ggplot2::geom_line(ggplot2::aes(y = SCS,   color = "HUS SCS"),  linewidth=1.0, linetype="dashed") } +
      { if (has_clark) ggplot2::geom_line(ggplot2::aes(y = Clark, color = "HU Clark"), linewidth=1.0, linetype="dotdash") } +
      { if (has_giuh)  ggplot2::geom_line(ggplot2::aes(y = GIUH,  color = "GIUH"),     linewidth=1.0, linetype="dotted") } +
      ggplot2::scale_color_manual(values = colores2) +
      ggplot2::labs(title = paste0("Comparacion de Hidrogramas Unitarios\n", lab_m),
                    x = "Tiempo (h)", y = expression(paste("Caudal (m"^3, "/s)")), color = NULL) + th
    salvar("Fig02_Comparacion_HU.png", p2)
  }
  
  # Fig 3: Dispersion obs vs SCS
  if (!is.null(res$Qsint_SCS)) {
    df3 <- data.frame(obs = Qd, sim = res$Qsint_SCS)
    df3 <- df3[is.finite(df3$obs) & is.finite(df3$sim), ]
    lim  <- max(c(df3$obs, df3$sim)) * 1.05
    p3  <- ggplot2::ggplot(df3, ggplot2::aes(x = obs, y = sim)) +
      ggplot2::geom_point(alpha = 0.35, color = "#2c7bb6", size = 1.2) +
      ggplot2::geom_abline(intercept=0, slope=1, linetype="dashed", color="red", linewidth=0.8) +
      ggplot2::geom_smooth(method="lm", se=TRUE, color="#1a9850", fill="#b8e186", alpha=0.3, linewidth=0.8) +
      ggplot2::coord_fixed(xlim=c(0,lim), ylim=c(0,lim)) +
      ggplot2::labs(title = "Dispersion: Escorrentia obs vs HUS SCS",
                    x = expression(paste("Q observado (m"^3, "/s)")),
                    y = expression(paste("Q simulado (m"^3, "/s)"))) + th
    salvar("Fig03_Dispersion_obs_sim.png", p3, h = 6)
  }
  
  # Fig 4: Posterior GLUE de Tc
  if (!is.null(res$glue)) {
    gl   <- res$glue
    df_g <- data.frame(Tc = gl$Tc_samples, KGE = gl$KGE_samples,
                       beh = gl$KGE_samples >= CONFIG$glue_kge_umbral)
    p4 <- ggplot2::ggplot(df_g, ggplot2::aes(x = Tc, y = KGE, color = beh, alpha = beh)) +
      ggplot2::geom_point(size = 0.6) +
      ggplot2::geom_hline(yintercept = CONFIG$glue_kge_umbral, linetype = "dashed", color = "red", linewidth = 0.7) +
      ggplot2::geom_vline(xintercept = gl$Tc_mediana, linetype = "dashed", color = "#1a9850", linewidth = 0.7) +
      ggplot2::scale_color_manual(values = c("FALSE" = "gray70", "TRUE" = "#2c7bb6"),
                                  labels = c("No behavioral", "Behavioral")) +
      ggplot2::scale_alpha_manual(values = c("FALSE" = 0.2, "TRUE" = 0.7), guide = "none") +
      ggplot2::labs(title = paste0("GLUE — Espacio parametrico Tc vs KGE\n",
                                   "N behavioral = ", gl$n_beh,
                                   " | Tc mediana = ", round(gl$Tc_mediana, 3), " h"),
                    x = "Tc (h)", y = "KGE", color = NULL) + th
    salvar("Fig04_GLUE_posterior.png", p4)
    
    df_beh_h <- data.frame(Tc = gl$Tc_beh, peso = gl$pesos)
    p4b <- ggplot2::ggplot(df_beh_h, ggplot2::aes(x = Tc, weight = peso)) +
      ggplot2::geom_histogram(bins = 40, fill = "#2c7bb6", color = "white", alpha = 0.8) +
      ggplot2::geom_vline(xintercept = gl$Tc_mediana, color = "#d73027", linewidth = 1) +
      ggplot2::geom_vline(xintercept = gl$Tc_IC95["lo"], color = "#d73027", linewidth = 0.7, linetype = "dashed") +
      ggplot2::geom_vline(xintercept = gl$Tc_IC95["hi"], color = "#d73027", linewidth = 0.7, linetype = "dashed") +
      ggplot2::labs(title = paste0("Distribucion Posterior de Tc (GLUE)\n",
                                   "IC 95% [", round(gl$Tc_IC95["lo"], 3), " — ",
                                   round(gl$Tc_IC95["hi"], 3), "] h"),
                    x = "Tc (h)", y = "Densidad ponderada") + th
    salvar("Fig04b_GLUE_histograma_Tc.png", p4b)
  }
  
  # Fig 5: Curva de masa acumulada
  df_m <- data.frame(
    t = t,
    P_pct  = cumsum(P) / max(sum(P), 1e-9) * 100,
    Qd_pct = cumsum(Qd) / max(sum(Qd), 1e-9) * 100
  )
  p5 <- ggplot2::ggplot(df_m, ggplot2::aes(x = t)) +
    ggplot2::geom_line(ggplot2::aes(y = P_pct,  color = "Precipitacion"), linewidth = 1.2) +
    ggplot2::geom_line(ggplot2::aes(y = Qd_pct, color = "Escorrentia directa"), linewidth = 1.2) +
    ggplot2::geom_hline(yintercept = 50, linetype = "dotted", color = "gray40") +
    ggplot2::scale_color_manual(values = c("Precipitacion" = "#4a90d9", "Escorrentia directa" = "#d62728")) +
    ggplot2::labs(title = "Curva de Masa Acumulada", x = "Tiempo (h)",
                  y = "Porcentaje acumulado (%)", color = NULL) + th
  salvar("Fig05_Curva_Masa.png", p5)
  
  # Fig 6: Firma Hidrologica del Evento (FDC) Basica
  df_perm_plot <- res$flash_res$df_perm
  df_perm_plot <- df_perm_plot[df_perm_plot$Caudal_m3s > 0, ]
  Q_min_pos <- min(df_perm_plot$Caudal_m3s, na.rm = TRUE)
  Q_max_pos <- max(df_perm_plot$Caudal_m3s, na.rm = TRUE)
  lim_inf_p6 <- 10^(floor(log10(Q_min_pos)))
  lim_sup_p6 <- 10^(ceiling(log10(Q_max_pos)))
  Q10_p6 <- if (!is.na(res$flash_res$Q10) && res$flash_res$Q10 > 0) res$flash_res$Q10 else NA
  Q90_p6 <- if (!is.na(res$flash_res$Q90) && res$flash_res$Q90 > 0) res$flash_res$Q90 else NA
  
  p6 <- ggplot2::ggplot(df_perm_plot, ggplot2::aes(x = Excedencia_pct, y = Caudal_m3s)) +
    ggplot2::geom_line(color = "#1f77b4", linewidth = 1.2) +
    { if (!is.na(Q10_p6)) ggplot2::geom_hline(yintercept = Q10_p6, linetype = "dashed", color = "#d73027", linewidth = 0.6) } +
    { if (!is.na(Q90_p6)) ggplot2::geom_hline(yintercept = Q90_p6, linetype = "dashed", color = "#2ca02c", linewidth = 0.6) } +
    ggplot2::scale_y_log10(
      limits = c(lim_inf_p6, lim_sup_p6),
      labels = scales::comma,
      breaks = scales::log_breaks(n = 6)
    ) +
    ggplot2::labs(title = paste0("Firma Hidrologica del Evento Extremo (FDC)\n",
                                 "Indice R-B = ", round(res$flash_res$RB, 4)),
                  x = "Excedencia (%)",
                  y = expression(paste("Caudal (m"^3, "/s) — escala log"))) + th
  salvar("Fig06_FDC_Evento.png", p6)
  
  # Fig 6b: Analisis Avanzado de FDC (Panel Doble) - CORREGIDO
  p6b_top <- ggplot2::ggplot(df_perm_plot, ggplot2::aes(x = Excedencia_pct, y = Caudal_m3s)) +
    ggplot2::geom_line(color = "#1f77b4", linewidth = 1.2) +
    ggplot2::geom_point(data = data.frame(x = c(10, 50, 90), 
                                          y = c(res$flash_res$Q10, res$flash_res$Q50, res$flash_res$Q90)),
                        ggplot2::aes(x = x, y = y), color = "#d73027", size = 3, shape = 18) +
    ggplot2::annotate("segment", 
                      x = 10, y = res$flash_res$Q10, 
                      xend = 90, yend = res$flash_res$Q90,
                      linetype = "dashed", color = "#2ca02c", linewidth = 0.8) +
    ggplot2::annotate("text", x = 50, y = sqrt(res$flash_res$Q10 * res$flash_res$Q90),
                      label = paste0("Pendiente = ", round(res$flash_res$slope_recesion, 5)),
                      hjust = 0.5, vjust = -1, color = "#2ca02c", fontface = "bold") +
    ggplot2::scale_y_log10(
      labels = scales::comma,
      breaks = scales::log_breaks(n = 6)
    ) +
    ggplot2::labs(title = "Curva de Duracion de Caudales (FDC) - Escala Logaritmica",
                  x = NULL, y = expression(paste("Caudal (m"^3, "/s)"))) + th +
    ggplot2::theme(plot.margin = ggplot2::margin(5, 5, 0, 5))

  df_diff <- data.frame(
    Excedencia = df_perm_plot$Excedencia_pct[-1],
    Delta_Q    = -diff(df_perm_plot$Caudal_m3s) / diff(df_perm_plot$Excedencia_pct)
  )
  df_diff$Delta_Q_smooth <- stats::lowess(df_diff$Excedencia, df_diff$Delta_Q, f = 0.1)$y
  
  p6b_bottom <- ggplot2::ggplot(df_diff, ggplot2::aes(x = Excedencia, y = Delta_Q)) +
    ggplot2::geom_line(color = "gray70", alpha = 0.7, linewidth = 0.5) +
    ggplot2::geom_line(ggplot2::aes(y = Delta_Q_smooth), color = "#ff7f0e", linewidth = 1.2) +
    ggplot2::scale_y_log10() +
    ggplot2::labs(title = "Derivada de la FDC (Tasa de Cambio del Caudal)",
                  x = "Porcentaje de Excedencia (%)",
                  y = expression(paste("-ΔQ / ΔExc (m"^3, "/s / %)"))) + th +
    ggplot2::theme(plot.margin = ggplot2::margin(0, 5, 5, 5))

  p6b <- patchwork::wrap_plots(p6b_top, p6b_bottom, ncol = 1, heights = c(2, 1))
  salvar("Fig06b_FDC_Analisis_Avanzado.png", p6b, h = 9)
  
  # Fig 7: HU empirico (deconvolucion Tikhonov)
  if (!is.null(res$HU_emp)) {
    df_hu <- data.frame(t = res$t_rel, HU_emp = res$HU_emp)
    p7 <- ggplot2::ggplot(df_hu, ggplot2::aes(x = t, y = HU_emp)) +
      ggplot2::geom_line(color = "#9467bd", linewidth = 1.2) +
      ggplot2::labs(title = paste0("HU Empirico por Deconvolucion Tikhonov\n",
                                   "(lambda = ", CONFIG$lambda_tikhonov, ")"),
                    x = "Tiempo (h)", y = "HU (m3/s/mm)") + th
    salvar("Fig07_HU_Empirico_Tikhonov.png", p7)
  }
  
  # Fig 8: Comparacion filtros caudal base
  if (length(res$sep_base$todos) > 1) {
    df_base <- data.frame(t = t, Q_total = Q)
    colores_base <- c("Q total" = "#1f77b4")
    for (nm in names(res$sep_base$todos)) {
      col_nm <- paste0("Qb_", nm)
      df_base[[col_nm]] <- res$sep_base$todos[[nm]]$Q_base
      colores_base[res$sep_base$todos[[nm]]$metodo] <- switch(nm,
        eckhardt = "#d62728", chapman = "#ff7f0e", lyne_hollick = "#9467bd")
    }
    p8 <- ggplot2::ggplot(df_base, ggplot2::aes(x = t)) +
      ggplot2::geom_line(ggplot2::aes(y = Q_total, color = "Q total"), linewidth = 1.2) +
      { if ("Qb_eckhardt" %in% names(df_base))
          ggplot2::geom_line(ggplot2::aes(y = Qb_eckhardt, color = "Eckhardt (2005)"), linewidth = 0.9, linetype="dashed") } +
      { if ("Qb_chapman" %in% names(df_base))
          ggplot2::geom_line(ggplot2::aes(y = Qb_chapman, color = "Chapman (1999)"), linewidth = 0.9, linetype="dotdash") } +
      { if ("Qb_lyne_hollick" %in% names(df_base))
          ggplot2::geom_line(ggplot2::aes(y = Qb_lyne_hollick, color = "Lyne-Hollick (1979)"), linewidth = 0.9, linetype="dotted") } +
      ggplot2::scale_color_manual(values = c(
        "Q total" = "#1f77b4", "Eckhardt (2005)" = "#d62728",
        "Chapman (1999)" = "#ff7f0e", "Lyne-Hollick (1979)" = "#9467bd")) +
      ggplot2::labs(title = "Comparacion de Filtros de Separacion de Caudal Base\n(ISO 748 — analisis de sensibilidad)",
                    x = "Tiempo (h)", y = expression(paste("Caudal (m"^3, "/s)")), color = NULL) + th
    salvar("Fig08_Comparacion_Filtros_Base.png", p8)
  }
  
  cat("  Graficos guardados en:", dir_out, "\n")
}

# ==============================================================================
# 13. EXPORTACION EXCEL MULTI-HOJA
# ==============================================================================

exportar_resultados_excel <- function(datos, res, ruta_sal) {
  cat("\nExportando resultados a Excel...\n")
  
  gl <- res$glue
  
  df_res <- data.frame(
    Parametro = c(
      "Area cuenca (km2)", "Duracion tormenta (h)", "CN (NRCS)",
      "Alpha recesion (estimado)", "BFI (Eckhardt primario)",
      "Precipitacion total (mm)", "Caudal pico total (m3/s)",
      "T50 centroide lluvia (h)", "Tp pico Qd (h)",
      "Lag (h)", "Tc_SCS (h)", "Tc IC95 inf Bootstrap (h)", "Tc IC95 sup Bootstrap (h)",
      "Tc GLUE mediana (h)", "Tc GLUE IC95 inf (h)", "Tc GLUE IC95 sup (h)",
      "Vol. lluvia (m3)", "Vol. escorrentia (m3)", "PBIAS (%)", "Dif. CN vs IBER (%)",
      "KGE_SCS", "KGEpp_SCS", "NSE_SCS", "RMSE_SCS (m3/s)", "NRMSE_SCS",
      "Peak Flow Bias PBPF (%)", "Peak Time Error (h)", "Amplitude Ratio",
      "Elasticidad Tc->Qp", "Indice Flashiness R-B"
    ),
    Valor = c(
      datos$area_cuenca,
      ifelse(is.na(datos$duracion_tormenta), NA, datos$duracion_tormenta),
      ifelse(is.na(datos$CN), NA, datos$CN),
      round(res$alpha, 4), round(res$sep_base$primario$BFI, 4),
      round(datos$P_total, 3), round(datos$Qpico, 4),
      round(res$T50, 4), round(res$Tp, 4),
      round(res$Lag, 4), round(res$Tc_nom, 4),
      round(res$Tc_IC_boot["lo"], 4), round(res$Tc_IC_boot["hi"], 4),
      ifelse(is.null(gl), NA, round(gl$Tc_mediana, 4)),
      ifelse(is.null(gl), NA, round(gl$Tc_IC95["lo"], 4)),
      ifelse(is.null(gl), NA, round(gl$Tc_IC95["hi"], 4)),
      round(res$Vol_P_m3, 0), round(res$Vol_Q_m3, 0), round(res$PBIAS, 3),
      ifelse(is.na(res$dif_CN_IBER), NA, round(res$dif_CN_IBER, 2)),
      round(res$KGE_SCS, 4), round(res$KGEpp_SCS, 4), round(res$NSE_SCS, 4),
      round(res$RMSE_SCS, 5), round(res$NRMSE_SCS, 5),
      ifelse(is.null(res$mets_pico), NA, round(res$mets_pico$Peak_Flow_Bias_pct, 2)),
      ifelse(is.null(res$mets_pico), NA, round(res$mets_pico$Peak_Time_Error_h, 4)),
      ifelse(is.null(res$mets_pico), NA, round(res$mets_pico$Amplitude_Ratio, 4)),
      ifelse(is.na(res$elasticidad), NA, round(res$elasticidad, 4)),
      round(res$flash_res$RB, 4)
    ), stringsAsFactors = FALSE
  )
  
  df_int <- interpretar_metricas(res$KGE_SCS, res$NSE_SCS, res$PBIAS)
  
  df_hid <- data.frame(
    Tiempo_h = round(res$t_grid, 5), P_mm_h = round(res$P_grid, 5),
    Pe_mm_h = round(res$Pe_grid, 5), Q_total_m3s = round(res$Q_grid, 5),
    Q_base_m3s = round(res$Qb_grid, 5), Q_directa_m3s = round(res$Qd_grid, 5)
  )
  if (!is.null(res$Qsint_SCS)) df_hid$Q_SCS_m3s <- round(res$Qsint_SCS, 5)
  if (!is.null(res$Qsint_Clark)) df_hid$Q_Clark_m3s <- round(res$Qsint_Clark, 5)
  if (!is.null(res$Qsint_GIUH)) df_hid$Q_GIUH_m3s <- round(res$Qsint_GIUH, 5)
  
  df_filt <- data.frame(Tiempo_h = round(res$t_grid, 5), Q_total = round(res$Q_grid, 5))
  for (nm in names(res$sep_base$todos)) {
    df_filt[[paste0("Qb_", nm)]] <- round(res$sep_base$todos[[nm]]$Q_base, 5)
    df_filt[[paste0("Qd_", nm)]] <- round(res$sep_base$todos[[nm]]$Q_directa, 5)
  }
  
  df_firma <- data.frame(
    Indicador = c("Volumen Total Escurrido (m3)", "Caudal Maximo (m3/s)", "Caudal Medio (m3/s)",
                  "Q5 (Pico de la avenida)", "Q10 (Caudales altos)", "Q50 (Mediana del evento)",
                  "Q90 (Recesion/Agotamiento)", "Q95 (Caudal base/Final)",
                  "Indice Flashiness R-B", "Pendiente de Recesion (Slope Q10-Q90)"),
    Valor = c(
      round(res$Vol_Q_m3, 0),
      round(max(res$Q_grid), 4),
      round(mean(res$Q_grid), 4),
      round(res$flash_res$Q5, 4),
      round(res$flash_res$Q10, 4),
      round(res$flash_res$Q50, 4),
      round(res$flash_res$Q90, 4),
      round(res$flash_res$Q95, 4),
      round(res$flash_res$RB, 6),
      round(res$flash_res$slope_recesion, 6)
    ),
    Unidad = c("m3", "m3/s", "m3/s", "m3/s", "m3/s", "m3/s", "m3/s", "m3/s", "-", "-"),
    Interpretacion = c(
      "Volumen total del evento", "Caudal maximo registrado", "Caudal promedio del evento",
      "Superado solo el 5% del tiempo", "Superado el 10% del tiempo", "Caudal mediano",
      "Superado el 90% del tiempo", "Superado el 95% del tiempo",
      ifelse(res$flash_res$RB < 0.05, "Hidrograma LAMINADO", "Hidrograma variable"),
      ifelse(res$flash_res$slope_recesion > -0.01, "Recesion MUY LENTA", "Recesion moderada/rapida")
    )
  )
  
  lista <- list(
    Resumen = df_res, Interpretacion = df_int, Hidrogramas = df_hid,
    Filtros_Base = df_filt, Curva_Permanencia = res$flash_res$df_perm,
    Firma_Hidrologica = df_firma
  )
  if (nrow(res$df_emp) > 0) lista$Tc_Empiricas <- res$df_emp
  if (!is.null(res$glue)) {
    idx_sub <- sample(seq_along(res$glue$Tc_samples), min(500, length(res$glue$Tc_samples)))
    lista$GLUE_Muestras <- data.frame(
      Tc_h = round(res$glue$Tc_samples[idx_sub], 4),
      KGE  = round(res$glue$KGE_samples[idx_sub], 4),
      Behavioral = res$glue$KGE_samples[idx_sub] >= CONFIG$glue_kge_umbral
    )
  }
  
  writexl::write_xlsx(lista, ruta_sal)
  cat("  Excel:", ruta_sal, "\n")
}

# ==============================================================================
# 14. INFORME TXT COMPLETO
# ==============================================================================

generar_informe_txt <- function(datos, res, ruta_inf) {
  cat("Generando informe TXT...\n")
  lns <- character(0)
  ln  <- function(...) lns <<- c(lns, paste0(...))
  S   <- paste(rep("=", 80), collapse = "")
  s   <- paste(rep("-", 80), collapse = "")
  gl  <- res$glue
  ac  <- res$test_acorr
  
  ln(S)
  ln("        INFORME HIDROLOGICO — IBER_TcEstimator v4.0.1")
  ln("        Firma Hidrologica del Evento Extremo")
  ln(S)
  ln("Fecha   : ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  ln("Carpeta : ", DIR_PRINCIPAL)
  ln(s)
  ln("")
  ln("1. DATOS DE LA CUENCA")
  ln("   Area (km2)       : ", datos$area_cuenca)
  ln("   Duracion (h)     : ", ifelse(is.na(datos$duracion_tormenta), "No especificada", datos$duracion_tormenta))
  ln("   CN               : ", ifelse(is.na(datos$CN), "No especificado", datos$CN))
  ln("   L cauce (km)     : ", ifelse(is.na(datos$L_cauce_km), "No esp.", datos$L_cauce_km))
  ln("   S media (m/m)    : ", ifelse(is.na(datos$S_pendiente), "No esp.", datos$S_pendiente))
  ln("")
  ln("2. SEPARACION CAUDAL BASE (ISO 748 — tres filtros)")
  ln("   Alpha (recesion) : ", round(res$alpha, 4))
  for (nm in names(res$sep_base$todos)) {
    r_nm <- res$sep_base$todos[[nm]]
    ln("   ", r_nm$metodo, "-> BFI = ", round(r_nm$BFI, 4))
  }
  ln("")
  ln("3. PRECIPITACION EFECTIVA CN-NRCS")
  if (!is.null(res$cn_res)) {
    ln("   CN      : ", res$cn_res$CN, " | S = ", round(res$cn_res$S, 2),
       " mm | Ia = ", round(res$cn_res$Ia, 2), " mm")
    ln("   Pe total: ", round(max(res$cn_res$Q_e_acum), 2), " mm")
    ln("   Dif. CN vs IBER: ", round(res$dif_CN_IBER, 2), " %")
  } else {
    ln("   No calculada (CN no especificado).")
  }
  ln("")
  ln("4. ESTIMACION DE Tc — METODO SCS LAG")
  ln("   T50 (h) : ", round(res$T50, 4))
  ln("   Tp  (h) : ", round(res$Tp, 4))
  ln("   Lag (h) : ", round(res$Lag, 4))
  ln("   Tc  (h) : ", round(res$Tc_nom, 4), " = ", round(res$Tc_nom * 60, 2), " min")
  ln("   IC 95% Bootstrap BCa: [", round(res$Tc_IC_boot["lo"], 4),
     ", ", round(res$Tc_IC_boot["hi"], 4), "] h")
  ln("")
  ln("5. GLUE — DISTRIBUCION POSTERIOR DE Tc")
  if (!is.null(gl)) {
    ln("   N muestras     : ", gl$N, " | N behavioral : ", gl$n_beh)
    ln("   Umbral KGE     : ", CONFIG$glue_kge_umbral)
    ln("   Tc mediana (h) : ", round(gl$Tc_mediana, 4))
    ln("   Tc media   (h) : ", round(gl$Tc_media, 4))
    ln("   Tc SD      (h) : ", round(gl$Tc_sd, 4))
    ln("   IC 95% GLUE    : [", round(gl$Tc_IC95["lo"], 4),
       ", ", round(gl$Tc_IC95["hi"], 4), "] h")
  } else {
    ln("   No calculado.")
  }
  ln("")
  ln("6. METRICAS DE BONDAD DE AJUSTE (Qd obs vs HUS SCS)")
  if (!is.na(res$KGE_SCS)) {
    ln("   KGE   (Gupta 2009) : ", round(res$KGE_SCS, 4))
    ln("   KGE'' (Kling 2012) : ", round(res$KGEpp_SCS, 4))
    ln("   NSE   (Nash 1970)  : ", round(res$NSE_SCS, 4))
    ln("   RMSE  (m3/s)       : ", round(res$RMSE_SCS, 5))
    ln("   NRMSE              : ", round(res$NRMSE_SCS, 5))
    df_int <- interpretar_metricas(res$KGE_SCS, res$NSE_SCS, res$PBIAS)
    for (i in seq_len(nrow(df_int)))
      ln("   ", df_int$Metrica[i], " -> ", df_int$Interpretacion[i])
  }
  ln("")
  ln("7. METRICAS DE FASE Y AMPLITUD (Modulo E1-E3)")
  if (!is.null(res$mets_pico)) {
    mp <- res$mets_pico
    ln("   Peak Flow Bias (PBPF) : ", round(mp$Peak_Flow_Bias_pct, 2), " %")
    ln("   Peak Time Error       : ", round(mp$Peak_Time_Error_h, 4), " h")
    ln("   Amplitude Ratio       : ", round(mp$Amplitude_Ratio, 4))
  }
  ln("")
  ln("8. DIAGNOSTICO DE RESIDUALES (Modulo E2)")
  if (!is.null(ac)) {
    ln("   Durbin-Watson stat : ", round(ac$DW_stat, 3), " | p-valor : ", round(ac$DW_pval, 4))
    ln("   Ljung-Box stat     : ", round(ac$LB_stat, 3), " | p-valor : ", round(ac$LB_pval, 4))
    ln("   Conclusion         : ", ac$conclusion)
  }
  ln("")
  ln("9. SENSIBILIDAD Y ELASTICIDAD (Modulo E4)")
  ln("   Elasticidad Tc->Qp : ", ifelse(is.na(res$elasticidad), "NA", round(res$elasticidad, 4)))
  ln("   [Delta Qp / Qp por Delta Tc / Tc]")
  ln("")
  ln("10. FIRMA HIDROLOGICA DEL EVENTO EXTREMO - FDC (Modulo E5)")
  ln("    NOTA METODOLOGICA: La FDC calculada corresponde al evento sintetico")
  ln("    de diseno (Tr) y NO representa el regimen hidrologico anual.")
  ln("    Se interpreta como la distribucion temporal de caudales durante la avenida.")
  ln("")
  ln("    --- Metricas de Forma del Hidrograma ---")
  ln("    Indice Flashiness R-B : ", round(res$flash_res$RB, 6))
  if (!is.na(res$flash_res$RB) && res$flash_res$RB < 0.05) {
      ln("    >> Interpretacion: Hidrograma altamente LAMINADO. El sistema tiene")
      ln("       gran capacidad de almacenamiento o el transito en el cauce es lento.")
  } else if (!is.na(res$flash_res$RB) && res$flash_res$RB < 0.2) {
      ln("    >> Interpretacion: Hidrograma moderadamente variable.")
  } else {
      ln("    >> Interpretacion: Hidrograma TORRENCIAL (alta variabilidad intrinseca).")
  }
  ln("")
  ln("    --- Percentiles del Evento (Duracion de Caudales Altos) ---")
  ln("    Q5  (Pico de la avenida, exc. 5%)  : ", round(res$flash_res$Q5, 4), " m3/s")
  ln("    Q10 (Caudales altos, exc. 10%)     : ", round(res$flash_res$Q10, 4), " m3/s")
  ln("    Q50 (Caudal mediano del evento)    : ", round(res$flash_res$Q50, 4), " m3/s")
  ln("    Q90 (Recesion/Agotamiento, exc. 90%): ", round(res$flash_res$Q90, 4), " m3/s")
  ln("    Q95 (Caudal base/Final, exc. 95%)  : ", round(res$flash_res$Q95, 4), " m3/s")
  ln("")
  ln("    Pendiente de Recesion (Slope Q10-Q90) : ", round(res$flash_res$slope_recesion, 6))
  if (!is.na(res$flash_res$slope_recesion)) {
    if (res$flash_res$slope_recesion > -0.01) {
      ln("    >> Interpretacion: Recesion MUY LENTA. Indica alta laminacion")
      ln("       y una liberacion prolongada del agua almacenada.")
    } else if (res$flash_res$slope_recesion > -0.03) {
      ln("    >> Interpretacion: Recesion LENTA. Laminacion moderada del hidrograma.")
    } else {
      ln("    >> Interpretacion: Recesion RAPIDA. Poca capacidad de laminacion")
      ln("       del sistema, respuesta hidrologica tipo flashy.")
    }
  }
  ln("")
  if (nrow(res$df_emp) > 0) {
    ln("11. FORMULAS EMPIRICAS DE Tc (solo referencia)")
    for (i in seq_len(nrow(res$df_emp)))
      ln("    ", res$df_emp$Formula[i], ": ", res$df_emp$Tc_horas[i], " h")
    ln("")
  }
  ln(S)
  ln("REFERENCIAS (seleccion)")
  ln("  Baker et al. (2004). JAWRA 40(2), 503-522.")
  ln("  Beven & Binley (1992). Hydrol. Process. 6(3), 279-298.")
  ln("  Eckhardt (2005). Hydrol. Process. 19(2), 507-515.")
  ln("  Gupta et al. (2009). J. Hydrol. 377, 80-91.")
  ln("  Knoben et al. (2019). HESS 23, 4323-4331.")
  ln("  Moriasi et al. (2007). Trans. ASABE 50(3), 885-900.")
  ln("  Nash & Sutcliffe (1970). J. Hydrol. 10(3), 282-290.")
  ln("  NRCS (2004, 2010). NEH Part 630, Chapters 10, 15, 16. USDA.")
  ln("  Rodriguez-Iturbe & Valdes (1979). WRR 15(6), 1409-1420.")
  ln("  Searcy (1959). Flow-duration curves. USGS Water Supply Paper 1542-A.")
  ln("  WMO (2008). Guide to Hydrological Practices, WMO-No. 168.")
  ln(S)
  
  writeLines(lns, ruta_inf)
  cat("  Informe TXT:", ruta_inf, "\n")
}

# ==============================================================================
# 15. EJECUCION PRINCIPAL
# ==============================================================================

cat(paste(rep("=", 80), collapse = ""), "\n")
cat("   INICIANDO ANALISIS v4.0.1 - Firma Hidrologica del Evento Extremo\n")
cat(paste(rep("=", 80), collapse = ""), "\n")

if (!file.exists(CONFIG$ruta_excel))
  stop("ERROR CRITICO: Archivo no encontrado:\n  ", CONFIG$ruta_excel)

datos_entrada <- leer_y_validar_datos(CONFIG$ruta_excel, DIR_DATOS)
resultados    <- analisis_hidrologico(datos_entrada)
generar_graficos(datos_entrada, resultados, DIR_GRAFICOS)
exportar_resultados_excel(datos_entrada, resultados, RUTA_EXCEL_OUT)
generar_informe_txt(datos_entrada, resultados, RUTA_INFORME)

cat("\n", paste(rep("=", 80), collapse = ""), "\n")
cat("   ANALISIS COMPLETADO — IBER_TcEstimator v5.0 (ES)\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

cat("RESULTADO PRINCIPAL:\n")
cat("  Tc SCS (Bootstrap) =", round(resultados$Tc_nom, 4), "h")
cat("  (", round(resultados$Tc_nom * 60, 2), "min)\n")
cat("  IC 95% Bootstrap BCa: [",
    round(resultados$Tc_IC_boot["lo"], 4), ",",
    round(resultados$Tc_IC_boot["hi"], 4), "] h\n")
if (!is.null(resultados$glue)) {
  cat("  Tc GLUE mediana     =", round(resultados$glue$Tc_mediana, 4), "h\n")
  cat("  IC 95% GLUE         : [",
      round(resultados$glue$Tc_IC95["lo"], 4), ",",
      round(resultados$glue$Tc_IC95["hi"], 4), "] h\n")
}

cat("\n--- FIRMA HIDROLOGICA DEL EVENTO (FDC) ---\n")
cat("  NOTA: La FDC describe la distribucion temporal de caudales durante la avenida.\n")
cat("  Indice Flashiness R-B:", round(resultados$flash_res$RB, 6))
if (resultados$flash_res$RB < 0.05) {
  cat(" -> Hidrograma LAMINADO\n")
} else if (resultados$flash_res$RB < 0.2) {
  cat(" -> Hidrograma moderadamente variable\n")
} else {
  cat(" -> Hidrograma TORRENCIAL\n")
}
cat("  Pendiente de Recesion:", round(resultados$flash_res$slope_recesion, 6))
if (!is.na(resultados$flash_res$slope_recesion)) {
  if (resultados$flash_res$slope_recesion > -0.01) {
    cat(" -> Recesion MUY LENTA (Alta laminacion)\n")
  } else if (resultados$flash_res$slope_recesion > -0.03) {
    cat(" -> Recesion LENTA (Laminacion moderada)\n")
  } else {
    cat(" -> Recesion RAPIDA (Poca laminacion)\n")
  }
}
cat("  Q10 (Caudal alto):", round(resultados$flash_res$Q10, 4), "m3/s\n")
cat("  Q50 (Mediana):     ", round(resultados$flash_res$Q50, 4), "m3/s\n")
cat("  Q90 (Recesion):    ", round(resultados$flash_res$Q90, 4), "m3/s\n")

cat("\nCARPETA DE RESULTADOS:\n  ", DIR_PRINCIPAL, "\n\n")
cat("ESTRUCTURA:\n")
cat("  ├── 01_Datos_Entrada/\n")
cat("  ├── 02_Resultados_Numericos/    <- Excel 7 hojas (inc. Firma_Hidrologica)\n")
cat("  ├── 03_Informe_TXT/\n")
cat("  ├── 04_Graficos_PNG/            <- 9 figuras (inc. FDC Avanzada)\n")
cat("  └── 05_Log_Ejecucion/\n\n")
# Exportar session_info para reproducibilidad (WMO-168 §1.4)
session_file <- file.path(DIR_LOG, "session_info.txt")
tryCatch({
  si <- utils::capture.output(utils::sessionInfo())
  writeLines(c(paste("Sesion IBER_TcEstimator v5.0 —", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
               paste(rep("-", 60), collapse=""), si), session_file)
  cat("  session_info guardado en:", session_file, "\n")
}, error = function(e) invisible(NULL))

cat(paste(rep("=", 80), collapse = ""), "\n")
cat("   FIN DEL PROGRAMA\n")
cat(paste(rep("=", 80), collapse = ""), "\n")