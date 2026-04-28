# IBER_TcEstimator v1.0

**Marco en R para la estimación del Tiempo de Concentración específico del evento con cuantificación de incertidumbre**

**Autor:** Mauricio Javier Victoria Niño  
**ORCID:** 0009-0003-4328-5691  
**Estado:** Investigador Independiente

---

## 📘 Descripción

**IBER_TcEstimator** es un marco computacional de código abierto implementado en R que extrae estimaciones del Tiempo de Concentración (T<sub>c</sub>) específicas del evento —con cuantificación formal de incertidumbre— a partir de hidrogramas producidos por el modelo hidráulico-hidrológico IBER.

A diferencia de las fórmulas empíricas tradicionales (Kirpich, Temez, Bransby-Williams), que tratan el T<sub>c</sub> como una propiedad estática de la cuenca, este marco captura la física completa del flujo (almacenamiento, laminación, no linealidad) simulada por IBER para un evento de diseño específico.

---

## 📄 Estado del Proyecto

✅ **PREPRINT DISPONIBLE Y CÓDIGO PÚBLICO**

Este trabajo ha sido depositado como preprint en **SciELO Preprints** y el código se libera de forma abierta para facilitar la transparencia, reproducibilidad y discusión abierta previa a la publicación formal en revista científica.

- **Preprint:** [Enlace al preprint en SciELO – pendiente de asignación]
- **Código fuente:** Disponible en este repositorio bajo licencia abierta

Se invita a la comunidad hidrológica a revisar, utilizar y comentar el marco y los resultados presentados.

---

## 🧠 Metodología Integrada

El marco integra cinco módulos alineados con los protocolos WMO, ASCE, ISO y NRCS:

| Módulo | Descripción | Métodos implementados |
|--------|-------------|----------------------|
| **A. Incertidumbre** | Cuantificación formal de la incertidumbre en la estimación del T<sub>c</sub> | GLUE + Bootstrap BCa |
| **B. Flujo base** | Separación automática del flujo base | Eckhardt, Chapman, Lyne-Hollick |
| **C. Precipitación efectiva** | Cálculo de lluvia neta a partir de precipitación total | Método CN-NRCS |
| **D. Hidrogramas unitarios** | Generación y comparación de hidrogramas sintéticos | SCS/NRCS, Clark, GIUH + Tikhonov |
| **E. Diagnóstico avanzado** | Evaluación del desempeño del modelo | KGE, NSE, PBIAS, autocorrelación, firma hidrológica |

---

## 📖 Citación

Si utiliza este marco en su investigación, por favor cite el preprint:

> Victoria Niño, M. J. (2026). *IBER_TcEstimator v1.0: Marco en R para la estimación del Tiempo de Concentración específico del evento con cuantificación de incertidumbre*. SciELO Preprints. https://doi.org/XXXXX

---

## 📜 Licencia

© 2026 Mauricio Javier Victoria Niño.

Este código se distribuye bajo la licencia **MIT** (código) y **CC BY 4.0** (documentación), permitiendo su uso, modificación y redistribución con la debida atribución al autor.

---

## 📬 Contacto

Para consultas, comentarios o colaboraciones:

- **Autor:** Mauricio Javier Victoria Niño
- **Correo:** hidratecsa@gmail.com

