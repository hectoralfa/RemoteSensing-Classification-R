# Análisis Espacial del Crimen en la CDMX

Análisis geoespacial de delitos en la Ciudad de México utilizando KDE sobre red vial y Getis-Ord Gi*.  
El proyecto genera mapas de **hotspots** y **coldspots** de crimen a partir de puntos de robo, empleando una metodología de análisis espacial en redes urbanas.

---

## 🚨 Tablero interactivo

> 📊 **Tablero interactivo disponible aquí:**  
> 👉 [🔗 HAZ CLIC PARA VER EL TABLERO](https://hectoralfa.github.io/Spatial-Crime-Analysis-CDMX/)

o utiliza este botón:

[![Ver tablero interactivo](https://img.shields.io/badge/Ver%20tablero-Interactivo-blue?style=for-the-badge&logo=Tableau)](https://hectoralfa.github.io/Spatial-Crime-Analysis-CDMX/)

---

## 🧩 Descripción general

Este proyecto analiza la **distribución espacial de delitos de robo en la Ciudad de México** utilizando la **red vial** como soporte principal. A través de técnicas de:

- Análisis de red vial  
- Segmentación en *lixels*  
- Representación como grafo  
- **Kernel Density Estimation (KDE)** sobre red  
- Estadístico **Getis-Ord Gi\*** para identificación de *hotspots*  

se construye un **tablero interactivo** que permite explorar zonas con mayor riesgo en los robos más comunes:  
**robo a transporte/pasajero, robo en vía pública y robo a negocio** a lo largo de las vialidades de la CDMX.

El objetivo es aportar una herramienta útil para:

- **Diseño y evaluación de políticas públicas**
- **Áreas de seguridad, análisis de riesgo y planeación urbana**

---

## 🎯 Objetivo del proyecto

Desarrollar una **herramienta digital geoespacial** que:

- Recopile, procese y analice información sobre **redes viales y delitos de robo**.  
- Estime la **concentración de eventos delictivos** sobre la red vial, no solo sobre el espacio continuo.  
- Identifique **tramos de calle** con riesgo elevado mediante técnicas estadísticas (Getis-Ord Gi\*).  
- Permita **visualizar los resultados** de forma clara y accesible mediante un **tablero interactivo**.

> 🔗 Acceso rápido: [Ver tablero interactivo](https://hectoralfa.github.io/Spatial-Crime-Analysis-CDMX/)

---

## 🧠 Contexto y motivación

El estudio del crimen y la violencia requiere una mirada **integral** que combine:

- Factores **sociales y económicos**
- Características del **entorno urbano**
- Estructura y conectividad de la **red vial**

En lugar de analizar solo puntos en un mapa, este proyecto considera **por dónde se mueve la vida cotidiana: las calles y carreteras**. Esto permite:

- Estimar mejor la **exposición al riesgo**  
- Detectar **corredores de violencia**  
- Apoyar decisiones de **seguridad y planeación urbana** con evidencia espacial.

---

## 🗺️ Análisis de la red vial

La base del proyecto es la **red vial de la CDMX**, construida a partir de:

- Información del **INEGI**  

Se realiza un preprocesamiento para:

- Corregir segmentos desconectados  
- Homogeneizar la red  
- Preparar los datos para el análisis sobre *lixels* y grafos

---

## 📏 Lixels: segmentando la red

Una vez limpia la red, se divide en **segmentos pequeños y uniformes** llamados *lixels* (line + pixel).

¿Por qué es útil?

- Permite analizar la red a una escala más **fina y homogénea**.  
- Facilita calcular **densidades de robos por tramo**.  
- Ayuda a localizar con mayor precisión **en qué parte de una calle se concentran los eventos**.

Cada lixel se convierte en una unidad de análisis sobre la cual se calculan indicadores de riesgo.

---

## 🔗 Representación como grafo

La red vial también se representa como un **grafo**:

- **Nodos** → cruces o intersecciones  
- **Aristas** → tramos de calle entre intersecciones  

Esta representación permite:

- Modelar la **conectividad** de la ciudad  
- Entender cómo se pueden **propagar fenómenos** a lo largo de la red  
- Integrar métricas de red (distancias, accesibilidad, rutas mínimas, etc.)

---

## 📈 Kernel Density Estimation (KDE) sobre la red vial

Para analizar si los robos se concentran en ciertas zonas, se utiliza **Kernel Density Estimation (KDE)**, pero en lugar de aplicarlo al espacio continuo, se aplica:

> ✅ **Directamente sobre la red vial**, utilizando los *lixels* como soporte.

### ¿Qué hace KDE en este proyecto?

- Cada evento de robo genera una “mancha” de influencia sobre la red.  
- Si varias manchas se superponen en un mismo tramo, se estima una **mayor densidad de robos**.  
- El resultado es un mapa de **intensidad de robos por tramo de calle**.

Se consideran:

- **Ancho de banda (*bandwidth*)**: controla el tamaño de la mancha.  
- **Tipo de kernel**: en este caso, se utiliza el **kernel de Epanechnikov**, que:
  - Minimiza el **error cuadrático medio integrado (MISE)**  
  - Tiene **soporte compacto**, lo que ayuda a reducir el costo computacional.

---

## 🔥 Identificación de hotspots: Getis-Ord Gi\*

KDE muestra dónde hay **concentraciones altas**, pero no dice si estas son **estadísticamente significativas**.

Para eso se utiliza el estadístico **Getis-Ord Gi\*** sobre los lixels, con el fin de:

- Distinguir entre zonas que solo “parecen” densas  
- Y aquellas que, **estadísticamente**, tienen **muchos más robos de lo esperado** en comparación con sus vecinos.

El resultado se visualiza en forma de:

- 🔴 **Zonas calientes (hot spots)**: tramos con concentración significativamente alta  
- 🔵 **Zonas frías (cold spots)**: tramos con concentración significativamente baja  

Estos resultados alimentan el **tablero interactivo** para una interpretación más intuitiva.

---

## 🧪 Datos utilizados

La base de delitos se construyó a partir de:

- Registros de las carpetas de investigación de la **Fiscalía de la Ciudad de México**  
- Incluye **tres tipos de robo:** robo a transeúnte, robo a pasajero y robo a negocio  
- Cobertura: **Ciudad de México**  
- Periodo: **enero–diciembre de 2023**  

La red vial proviene de:

- **INEGI** – [https://www.inegi.org.mx](https://www.inegi.org.mx)

---

## 🧮 Herramientas y tecnologías

Algunas de las herramientas utilizadas:

- 📦 **R**  
  - Paquete **`spNetwork`** para KDE sobre red vial  
- 🧪 Librerías para análisis estadístico y espacial  
- 📊 **d3.js** para visualización interactiva  
- 🌐 **HTML/CSS/JavaScript** para la construcción de la interfaz del tablero

---

## 📂 Estructura del repositorio

```text
.
├── estilos/
│   ├── general.css
│   └── mapa.css
├── imagenes/
├── scripts/
│   ├── capa.js
│   ├── card-capas.js
│   ├── eventos_selector.js
│   ├── footer.js
│   ├── geocoder.js
│   ├── header.js
│   ├── main.js
│   ├── mapa_config.js
│   └── restablecer.js
├── scripts_r/
│   ├── crime_analysis.R
│   └── funciones.R
├── docs/
│   └── metodologia.md
├── index.html
└── README.md
