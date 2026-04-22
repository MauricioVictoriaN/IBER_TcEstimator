# IBER_TcEstimator v1.0: Marco en R para la estimación del Tiempo de Concentración específico del evento con cuantificación de incertidumbre

**Autor:** Mauricio Javier Victoria Niño  
**ORCID:** [0009-0003-4328-5691](https://orcid.org/0009-0003-4328-5691)  
**Estatus:** Investigador Independiente

---

## Descripción

**IBER_TcEstimator** es un marco computacional de código abierto implementado en **R** que extrae estimaciones del **Tiempo de Concentración ($T_c$)** específicas del evento —con cuantificación formal de incertidumbre— a partir de hidrogramas producidos por el modelo hidráulico-hidrológico **IBER**.

A diferencia de las fórmulas empíricas tradicionales (Kirpich, Temez, Bransby-Williams), que tratan el $T_c$ como una propiedad estática de la cuenca, este marco captura la **física completa del flujo** (almacenamiento, laminación, no-linealidad) simulada por IBER para un evento de diseño específico.

---

## Estado del Proyecto

🔒 **EN REVISIÓN POR PARES (PEER REVIEW)**  
Este repositorio es **PRIVADO** y se encuentra actualmente bajo evaluación para su publicación en una revista científica de hidrología y modelación ambiental.

---

## Metodología Integrada

El marco integra **cinco módulos** alineados con los protocolos WMO, ASCE, ISO y NRCS:

| Módulo | Descripción | Métodos implementados |
|--------|-------------|----------------------|
| **A. Incertidumbre** | Cuantificación formal de la incertidumbre en la estimación del $T_c$ | GLUE + Bootstrap BCa |
| **B. Caudal base** | Separación automática del caudal base | Eckhardt, Chapman, Lyne-Hollick |
| **C. Precipitación efectiva** | Cálculo de lluvia neta a partir de precipitación total | Método CN-NRCS |
| **D. Hidrogramas unitarios** | Generación y comparación de hidrogramas sintéticos | SCS/NRCS, Clark, GIUH + Tikhonov |
| **E. Diagnóstico avanzado** | Evaluación del desempeño del modelo | KGE, NSE, PBIAS, autocorrelación, firma hidrológica |

---


