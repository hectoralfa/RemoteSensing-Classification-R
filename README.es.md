🌎 Language: [English](README.md) | [Español](README.es.md)

# Métodos estadísticos de aprendizaje supervisado para la detección de cobertura del suelo a partir de imágenes satelitales

## 📌 Resumen 

Este repositorio contiene la implementación en **R** de un proyecto de **tesis de licenciatura en Actuaría (UNAM)**, cuyo objetivo es **clasificar cobertura del suelo (agricultura vs. no agricultura)** a partir de imágenes satelitales **Landsat 5**, utilizando **métodos estadísticos de aprendizaje supervisado**.

El trabajo adopta un enfoque **reproducible y comparativo**, integrando:

- Preprocesamiento de imágenes satelitales  
- Generación de variables espectrales y de textura  
- Entrenamiento de modelos de clasificación  
- Evaluación rigurosa del desempeño predictivo  

Se pone especial énfasis en **limitaciones prácticas**, **costos computacionales** y **generalización temporal**, aspectos relevantes tanto en contextos **académicos** como **industriales**.

---

## 🎓 Contexto académico

Este repositorio corresponde a la tesis:

> **“Métodos estadísticos de aprendizaje supervisado para la detección de cobertura del suelo a través de imágenes satelitales”**  
> **Autor:** Héctor Miguel Olivares García  
> **Licenciatura en Actuaría – Facultad de Ciencias, UNAM (2023)**

📄 El documento completo de la tesis se encuentra en la carpeta `doc/`.

---

## 🌍 Motivación y problema abordado

La clasificación de cobertura del suelo es una herramienta clave para:

- Monitoreo ambiental  
- Planeación territorial  
- Evaluación de políticas públicas  
- Estudios de cambio climático y sostenibilidad  

Los métodos tradicionales de levantamiento de información suelen ser **costosos**, **lentos** y **difíciles de actualizar**.  
Este proyecto explora el uso de **imágenes satelitales de acceso gratuito**, combinadas con **métodos estadísticos clásicos y de machine learning**, como una alternativa **escalable y reproducible**.

---

## 🎯 Objetivo del proyecto

Clasificar píxeles de imágenes satelitales en dos categorías:

- **Agricultura**
- **No agricultura**

utilizando información **multiespectral**, **índices derivados** e imágenes correspondientes a **diferentes estaciones del mismo año**, con el fin de analizar el desempeño y la estabilidad de los modelos entrenados.

---

## 🧠 Metodología general

### 1️⃣ Obtención de imágenes satelitales
- Landsat 5  
- Datos etiquetados disponibles públicamente  

### 2️⃣ Preprocesamiento
- Corrección radiométrica  
- Reproyección geográfica  
- Índices espectrales: **NDVI, SAVI, MSAVI**  
- Índices de textura mediante **GLCM**

### 3️⃣ Construcción del conjunto de datos
- Cada píxel se trata como una observación  
- Variables espectrales + textura + etiqueta de cobertura  

### 4️⃣ Modelos supervisados
- Regresión logística, probit y LASSO  
- K vecinos más cercanos (KNN)  
- Máquinas de soporte vectorial (SVM)  
- Bosques aleatorios (Random Forest)

### 5️⃣ Evaluación
- Repeated Hold-Out  
- Validación cruzada K-fold (K = 10)  
- Métricas: exactitud, sensibilidad y especificidad  

### 6️⃣ Generalización temporal
- Entrenamiento en una estación  
- Evaluación en otra estación del mismo año  

---

## 📊 Resultados principales

- **Random Forest** obtuvo el mejor desempeño:
  - Exactitud aproximada del **87%**
  - Buen desempeño en la clase minoritaria (agricultura)
- Diferencias importantes en **tiempos de ejecución**
- Deterioro del desempeño al cambiar de estación, lo que resalta:
  - Necesidad de monitoreo continuo
  - Reentrenamiento ante cambios estacionales

---

## 💻 Tecnologías y herramientas

- **Lenguaje:** R  
- **IDE:** RStudio  
- **Imágenes satelitales:** `raster`, `terra`, `RStoolbox`  
- **Machine Learning:** `caret`, `randomForest`, `e1071`  
- **Textura:** `glcm`  
- **Computación:** programación en paralelo  

> ℹ️ Parte del código fue actualizado respecto a la tesis original debido a cambios o deprecación de librerías.

---

## 📁 Estructura del repositorio

```text
├── doc/
│   └── Tesis_Olivares_García_Héctor.pdf
│
├── scripts/
│   ├── preprocesamiento.R
│   ├── construccion_dataset.R
│   ├── modelos_clasificacion.R
│   └── evaluacion_modelos.R
│
└── README.md
```
---
## 📚 Referencias

Este proyecto se basa, entre otros, en la metodología descrita en:

Kamusoko, C. (2013, 2019). Remote Sensing Image Classification in R

Dicho trabajo fue una referencia clave para:

Preprocesamiento de imágenes

Construcción de variables

Estrategias de clasificación y evaluación

## ⚠️ Limitaciones y trabajo futuro

Limitaciones:

Región geográfica específica (Harare, Zimbabue)

No se exploran modelos de deep learning

La generalización temporal presenta retos importantes

Trabajo futuro:

Incorporar modelos espaciales o temporales

Evaluar imágenes de mayor resolución

Automatizar procesos de reentrenamiento

##📎 Cómo citar este trabajo

Si utilizas este repositorio o la tesis como referencia académica:

Olivares García, H. M. (2023). Métodos estadísticos de aprendizaje supervisado para la detección de cobertura del suelo a través de imágenes satelitales. Facultad de Ciencias, UNAM.

## 👤 Autor

Héctor Miguel Olivares García
Actuario – UNAM

Intereses: Machine Learning, Estadística Aplicada, Imágenes Satelitales, Ciencia de Datos
