# IBER_TcEstimator v1.0

**R Framework for Event-Specific Time of Concentration Estimation with Uncertainty Quantification**

[![License: MIT](https://img.shields.io/badge/Code-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![License: CC BY 4.0](https://img.shields.io/badge/Docs-CC%20BY%204.0-lightgrey.svg)](https://creativecommons.org/licenses/by/4.0/)
[![Preprint: EngrXiv](https://img.shields.io/badge/Preprint-EngrXiv-blue.svg)](https://engrxiv.org)
[![R: ≥4.2.0](https://img.shields.io/badge/R-%E2%89%A54.2.0-276DC3.svg)](https://www.r-project.org/)
[![ORCID](https://img.shields.io/badge/ORCID-0009--0003--4328--5691-A6CE39.svg)](https://orcid.org/0009-0003-4328-5691)

**Author:** Mauricio Javier Victoria Niño  
**Affiliation:** Independent researcher, Cali, Colombia  
**Contact:** hidratecsa@gmail.com  
**ORCID:** [0009-0003-4328-5691](https://orcid.org/0009-0003-4328-5691)

---

## 📋 Overview

**IBER_TcEstimator v1.0** is an open-source R framework that extracts event-specific Time of Concentration (*T*<sub>c</sub>) estimates — with formal uncertainty quantification — from hydrographs produced by the **IBER** hydraulic-hydrological model, and characterises the flood event through a hydrological signature analysis.

Unlike conventional empirical formulas (Kirpich, Temez, Bransby-Williams, SCS Lag), which treat *T*<sub>c</sub> as a static, event-independent watershed property, this framework captures the full physics of flow routing, storage, attenuation, and nonlinearity explicitly modelled by IBER — including outputs from the **distributed hydrological module of IBER v3** (Sanz-Ramos et al., 2022).

---

## 📄 Preprint

This work is deposited as a preprint on **EngrXiv** (Engineering Archive) and the source code is released openly to facilitate transparency, reproducibility, and community discussion prior to formal journal publication.

> Victoria Niño, M. J. (2026). *Event-Specific Time of Concentration and Hydrological Signature from IBER Hydrographs: A Proof-of-Concept Framework with Uncertainty Quantification*. EngrXiv. https://doi.org/[pending assignment]

The community is invited to review, use, and comment on the framework and results.

> ⚠️ **Proof-of-concept scope:** Results presented in the manuscript are based on a synthetic 150.5 km² watershed with a 6 h SCS type-II design storm. Validation against real gauged catchments is the primary objective of the next development phase.

---

## 🧠 Integrated Methodology

The framework integrates five analytical modules aligned with WMO, ASCE, ISO, and NRCS protocols:

| Module | Description | Methods |
|--------|-------------|---------|
| **A — Uncertainty** | Formal *T*<sub>c</sub> uncertainty quantification | GLUE (N = 5 000) + Bootstrap BCa (R = 2 000) |
| **B — Baseflow** | Automatic baseflow separation with recession parameter estimation | Eckhardt (2005), Chapman (1999), Lyne-Hollick (1979) |
| **C — Effective precipitation** | Net rainfall from total precipitation | CN-NRCS method (λ = 0.20) |
| **D — Unit hydrographs** | Generation and comparison of synthetic and empirical UH | SCS/NRCS, Clark, GIUH, Tikhonov deconvolution |
| **E — Advanced diagnostics** | Multi-criterion performance evaluation and event characterisation | KGE, NSE, PBIAS, Durbin-Watson, Ljung-Box, *T*<sub>c</sub>→*Q*<sub>p</sub> elasticity, FDC, Richards-Baker flashiness index |

---

## 📊 Proof-of-Concept Results

On a synthetic 150.5 km² watershed (CN = 82, *L*<sub>c</sub> = 28.36 km, *S* = 0.0073):

| Result | Value |
|--------|-------|
| *T*<sub>c</sub> SCS Lag (point estimate) | 17.85 h |
| *T*<sub>c</sub> 95 % BCa Bootstrap CI | [13.02, 22.59] h |
| *T*<sub>c</sub> GLUE posterior median | 16.20 h |
| *T*<sub>c</sub> GLUE 95 % CI | [12.71, 19.75] h |
| KGE (SCS UH) | 0.667 |
| NSE (SCS UH) | 0.808 |
| PBIAS | 26.64 % |
| R-B Flashiness index | 0.0009 (highly attenuated) |
| Recession slope (Q₁₀–Q₉₀) | 0.0179 decades/% |
| BFI range (3 filters) | 0.82–0.85 (CV < 2 %) |

The IBER-derived *T*<sub>c</sub> is 2.1–7.1 times larger than six empirical formulas (range: 2.50–9.47 h), reflecting the storage and attenuation effects of a 150.5 km² low-slope catchment that kinematic-wave formulas cannot capture.

---

## 🗂️ Repository Structure

```
IBER_TcEstimator/
│
├── IBER_TcEstimator_v1.0.0.R          # Main R script (~2 200 lines, 15 sections)
├── Hyetograph_HydrographIBER_EN.xlsx  # Example input file (English, 3 sheets)
├── README.md                           # This file
└── LICENSE                             # MIT licence (code)
```

### Input Excel file structure

The script reads a bilingual Excel workbook (Spanish or English sheets auto-detected):

| Sheet (English) | Sheet (Spanish) | Contents |
|-----------------|-----------------|----------|
| `Metadata`      | `Metadatos`     | Watershed parameters (area, CN, *L*<sub>c</sub>, *S*, Δ*H*, GIUH params) |
| `Hyetograph`    | `Hietograma`    | Time (h) · Precipitation (mm/h) |
| `Hydrograph`    | `Hidrograma`    | Time (h) · Discharge (m³/s) |

### Output folder structure

Each execution creates a time-stamped folder `IBERv1.0.0_Results_YYYY-MM-DD_HH-MM-SS/`:

```
IBERv1.0.0_Results_.../
├── 01_Input_Data/          # Exact copy of the input Excel (traceability)
├── 02_Numerical_Results/   # Excel workbook (7 sheets) + supplementary CSVs
├── 03_Text_Report/         # Plain-text reproducible report (all metrics)
├── 04_Plots_PNG/           # 10 publication-quality figures at 300 dpi
└── 05_Execution_Log/       # Console log + sessionInfo() + config_used.csv
```

---

## ⚙️ Installation and Usage

### Requirements

- **R** ≥ 4.2.0
- The following packages are installed automatically on first run if missing:

| Package | Min. version | Purpose |
|---------|-------------|---------|
| `zoo` | ≥ 1.8 | Time series handling |
| `dplyr` | ≥ 1.0 | Data manipulation |
| `hydroGOF` | ≥ 0.5 | Hydrological performance metrics |
| `boot` | ≥ 1.3 | Bootstrap BCa confidence intervals |
| `lmtest` | ≥ 0.9 | Durbin-Watson test |
| `ggplot2` | ≥ 3.4 | Figures |
| `patchwork` | ≥ 1.1 | Multi-panel figures |
| `scales` | ≥ 1.2 | Axis formatting |
| `readxl` | ≥ 1.4 | Excel reading |
| `writexl` | ≥ 1.4 | Excel writing |

### Quick start

1. Clone or download this repository.
2. Open `IBER_TcEstimator_v1.0.0.R` in RStudio or any R environment.
3. Edit the `CONFIG` block at the top of the script (lines 50–85):

```r
CONFIG <- list(
  ruta_excel  = "path/to/Hyetograph_HydrographIBER_EN.xlsx",
  output_dir  = "path/to/output/folder",
  n_boot      = 2000,      # Bootstrap BCa resamples
  glue_N      = 5000,      # GLUE Monte Carlo samples
  lag_to_tc_coef = 0.6,    # SCS Lag-to-Tc coefficient
  ...
)
```

4. Source the script (`Ctrl+Shift+S` in RStudio) or run:

```r
source("IBER_TcEstimator_v1.0.0.R")
```

5. All outputs appear in the time-stamped folder under `output_dir`.

### Reproducibility

Random seeds are fixed (`boot_seed = 42`, `glue_seed = 123`). The full `CONFIG` used in each run is saved as `config_used.csv` in `05_Execution_Log/`. The input Excel file is preserved in `01_Input_Data/` alongside all outputs.

---

## 📐 Key Configuration Parameters

| Parameter | Default | Description |
|-----------|---------|-------------|
| `n_boot` | 2000 | Bootstrap BCa resamples (convergence verified at R = 1000 vs 2000: 0.76 % CI width difference) |
| `glue_N` | 5000 | GLUE Monte Carlo realisations |
| `glue_kge_threshold` | 0.50 | Behavioural threshold (KGE ≥ 0.50) |
| `glue_Tc_range` | [0.05, 24] h | Prior range for *T*<sub>c</sub> |
| `lag_to_tc_coef` | 0.6 | SCS Lag-to-*T*<sub>c</sub> conversion coefficient (NRCS, rural catchments) |
| `BFI_max` | 0.80 | Eckhardt filter BFI<sub>max</sub> (porous aquifer default) |
| `Ia_lambda` | 0.20 | CN-NRCS initial abstraction ratio |
| `lambda_tikhonov` | 0.01 | Tikhonov regularisation parameter |
| `dt_interp_h` | 0.005 h | Interpolation time step (= 0.3 min) |

---

## 📈 Generated Figures (10 PNG at 300 dpi)

| File | Content |
|------|---------|
| `Fig01_Hyetograph_Hydrograph_Baseflow.png` | Hyetograph, total hydrograph, baseflow separation, T₅₀ and *T*<sub>p</sub> markers |
| `Fig02_UH_Comparison.png` | Observed vs. SCS, Clark, and GIUH synthetic unit hydrographs |
| `Fig03_Obs_vs_Sim_Scatter.png` | Observed vs. SCS UH scatter plot with regression |
| `Fig04_GLUE_Posterior.png` | GLUE parametric space: KGE vs. *T*<sub>c</sub> |
| `Fig04b_GLUE_Tc_Histogram.png` | Weighted posterior distribution of *T*<sub>c</sub> (GLUE) |
| `Fig05_Cumulative_Mass.png` | Cumulative mass curves (precipitation and direct runoff) |
| `Fig06_Event_FDC.png` | Event Flow Duration Curve (log scale) |
| `Fig06b_FDC_Advanced_Analysis.png` | Dual-panel FDC + derivative with LOESS smoothing |
| `Fig07_Empirical_UH_Tikhonov.png` | Empirical unit hydrograph via Tikhonov deconvolution |
| `Fig08_Baseflow_Filter_Comparison.png` | Three baseflow filters comparison (ISO 748 sensitivity) |
| `Fig09_QQ_Bootstrap_Tc.png` | Q-Q plot: bootstrap *T*<sub>c</sub> distribution (Shapiro-Wilk) |

---

## ⚠️ Known Limitations

1. **Synthetic watershed only.** Validated on a controlled synthetic case; real-catchment performance is yet to be demonstrated.
2. **SCS Lag coefficient.** The Lag = 0.6 *T*<sub>c</sub> relation was calibrated on small rural catchments (< 25 km²); site-specific calibration is recommended for large or urban basins.
3. **Volumetric bias.** PBIAS = 26.64 % (unsatisfactory per Moriasi et al., 2007) due to the triangular SCS UH approximation and potential floodplain storage effects.
4. **Residual autocorrelation.** DW ≈ 0 and LB = 27 553 indicate structural model error; BCa intervals should be treated as lower bounds on uncertainty.
5. **Single-event analysis.** Event-to-event *T*<sub>c</sub> variability cannot be characterised with the current implementation.
6. **GLUE prior range.** Uniform prior [0.05, 24] h is adequate for *T*<sub>c</sub> up to ~20 h; adjust `glue_Tc_range` for other catchment types.

---

## 🗺️ Roadmap

- [ ] Real-catchment validation on gauged watersheds with calibrated IBER models
- [ ] Multi-event analysis: *T*<sub>c</sub> = *f*(*i*) curves across return periods
- [ ] Global sensitivity analysis (Sobol', Morris methods)
- [ ] Block-bootstrap for autocorrelated residuals
- [ ] Automatic calibration (genetic algorithms, particle swarm)
- [ ] Shiny graphical interface
- [ ] GIS integration (GeoJSON/shapefile export)

---

## 📚 How to Cite

If you use this framework in your research, please cite the preprint:

> Victoria Niño, M. J. (2026). Event-Specific Time of Concentration and Hydrological Signature from IBER Hydrographs: A Proof-of-Concept Framework with Uncertainty Quantification. *EngrXiv*. https://doi.org/[pending assignment]

BibTeX:
```bibtex
@article{victorianino2026,
  author    = {Victoria Ni{\~n}o, Mauricio Javier},
  title     = {Event-Specific Time of Concentration and Hydrological
               Signature from {IBER} Hydrographs: A Proof-of-Concept
               Framework with Uncertainty Quantification},
  journal   = {EngrXiv},
  year      = {2026},
  doi       = {pending},
  note      = {Preprint. Source code: \url{https://github.com/MauricioVictoriaN/IBER_TcEstimator}}
}
```

---

## 📜 Licence

| Component | Licence |
|-----------|---------|
| Source code (`IBER_TcEstimator_v1.0.0.R`) | [MIT](https://opensource.org/licenses/MIT) |
| Documentation and manuscript | [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/) |

© 2026 Mauricio Javier Victoria Niño. Use, modification, and redistribution are permitted with appropriate attribution.

---

## 📬 Contact

For questions, comments, or collaboration:

- **Author:** Mauricio Javier Victoria Niño
- **Email:** hidratecsa@gmail.com
- **ORCID:** [0009-0003-4328-5691](https://orcid.org/0009-0003-4328-5691)
