rm(list = ls(all.names = TRUE))
gc()

source("00_funciones.R")

# Librerías
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")

pacman::p_load(
  sp, terra, raster, RStoolbox, ggplot2, gridExtra, dplyr, RColorBrewer,
  glcm, reshape, grid, rasterVis, corrplot, doParallel, parallelly,
  glmnet, caret, pROC, rsample, purrr, furrr, MASS, ranger, e1071, beepr
)

set.seed(1234)

#Lectura de datos desde el metado
metaDato<- RStoolbox::readMeta("../input/conjunto/LT51700721984174XXX08_MTL.txt")


# Combinar la informacion de las imagenes que se descargan en el documento
MT_6_84 <- stackMeta(metaDato)

# # checa lo que trae el objeto
MT_6_84

# Verificando posición geográfica (CRS)
crs(MT_6_84)
#UTM zone 36N

## Se remueve la banda 6 debido a la resolución que tiene
TM5_6_84 <- MT_6_84[[-6]]
# Imagen Satelital RGB (combinacionde bandas)
plotRGB(TM5_6_84, r=5, g=4, b=3, stretch="lin")




# Verificación de la distribución de los pixeles
p <- plot_pixel_distributions(
  TM5_6_84,
  bands = c("B1_dn","B2_dn","B3_dn","B4_dn","B5_dn","B7_dn"),
  title = "Distribución de pixeles por banda",
  ncol = 2
)
p


# VAlores de GAIN y Offset
# Ranciancia=Gain*ND+Ooffset
TM5_radParameters <- metaDato$CALRAD
TM5_radParameters

## Corrección Radiometrica

# APREF

apref_6_84 <- radCor(TM5_6_84, metaData = metaDato, method = "apref")

# Imagen Satelital aplicando correción APREF
plotRGB(apref_6_84, r=5, g=4, b=3, stretch="lin")

# Distrubución de los pixeles aplicando APREF
p <- plot_pixel_distributions(
  apref_6_84,
  bands = c("B1_tre","B2_tre","B3_tre","B4_tre"),
  title = "Distribución de pixeles por banda con APREF",
  ncol = 2
)
p

#SDOS

# Estamación Haze
hazeDN <- estimateHaze(TM5_6_84, hazeBands = 1:4, darkProp = 0.01, plot = TRUE)

# Aplicación SDOS.
sdos_6_84 <- radCor(TM5_6_84, metaData = metaDato, method = "sdos", 
                    hazeValues = hazeDN, hazeBands = 1:4)

# Imagen Satelital aplicando correción SDOS
plotRGB(sdos_6_84, r=5, g=4, b=3, stretch="lin")


# Distribución de los pixeles aplicando SDOS
p <- plot_pixel_distributions(
  sdos_6_84,
  bands = c("B1_sre","B2_sre","B3_sre","B4_sre"),
  title = "Distribución de pixeles por banda con SDOS",
  ncol = 2
)
p


# Aplicación DOS
dos_6_84 <- radCor(TM5_6_84, metaData = metaDato, method = "dos")

# Imagen Satelital aplicando correción DOS
plotRGB(dos_6_84, r=5, g=4, b=3, stretch="lin")

# Distribución de los pixeles aplicando DOS
p <- plot_pixel_distributions(
  dos_6_84,
  bands = c("B1_sre","B2_sre","B3_sre","B4_sre"),
  title = "Distribución de pixeles por banda con DOS",
  ncol = 2
)
p

# COSTZ

# Estamacion Haze
hazeDN <- estimateHaze(TM5_6_84, hazeBands = 1:4, darkProp = 0.01, plot = F)

# Aplicación CostZ
costz_6_84 <- radCor(TM5_6_84, metaData = metaDato, method = "costz", 
                     hazeValues = hazeDN, hazeBands = 1:4)


plotRGB(costz_6_84, r=5, g=4, b=3, stretch="lin")

p <- plot_pixel_distributions(
  costz_6_84,
  bands = c("B1_sre","B2_sre","B3_sre","B4_sre"),
  title = "Distribución de pixeles por banda con COSTZ",
  ncol = 2
)
p

# REPROYECCION

# Georeferenciar la zona al hemisferío sur
newproj <- "+proj=utm +zone=36 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"

# Georeferenciar la zona de las bandas corregidas al hemisferío sur
# Ver la resolucion del pixel en res, aplicadas en la reproyeccion
costz_6_84_proy <- terra::project(TM5_6_84, newproj, res=30)

original_mean  <- terra::global(TM5_6_84, mean, na.rm = TRUE)
projected_mean <- terra::global(costz_6_84_proy, mean, na.rm = TRUE)

# Lectura de archivo shapefile, forma poligonal 
# para delimitar zona
clip_boundary_sf <- sf::st_read("../input/conjunto/Hre_Boundary.shp")

# 2) Pasar a terra (SpatVector)
clip_boundary <- vect(clip_boundary_sf)

# 3) Asegura mismo CRS
clip_boundary <- project(clip_boundary, crs(costz_6_84_proy))

# 4) Función clip (crop + mask)
clip_terra <- function(r, shape) {
  r_crop <- crop(r, shape)
  mask(r_crop, shape)
}

# 5) Recorte + máscara
costz_6_84R <- clip_terra(costz_6_84_proy, clip_boundary)

# 6) RGB (ojo: r/g/b son índices de capa en terra)
plotRGB(costz_6_84R, r = 5, g = 4, b = 3, stretch = "lin")

# Guardar información de la correción radiometrica

out_img <- "../output/datos/CostzPr_6_84.img"

sidecars <- c(out_img,
              sub("\\.img$", ".hdr", out_img, ignore.case = TRUE),
              paste0(out_img, ".aux.xml"),
              paste0(out_img, ".ovr"))

# Borra si existen
invisible(lapply(sidecars, function(f) if (file.exists(f)) file.remove(f)))

# Escribe de nuevo
raster::writeRaster(costz_6_84R,
                    filename = out_img,
                    datatype = "FLT4S",
                    overwrite = TRUE)

####     Inf_conCorRad6y4IT    Tratamiento de los indices espectrales

#Lista de archivos que contienen imagenes
ListaImg<-list.files("../output/datos",pattern="img$",
                     full.names=TRUE)

# extracción de información
costz_6_84R <- stack(ListaImg)

#Checar Atributos
summary(costz_6_84R)

#Cálculo de indices espectrales
# Dependiendo de los indices que se quieran
# calcular se colocan las bandas o layer denomindas
IE_6_84 <- spectralIndices(costz_6_84R,
                           red = "B3_dn",
                           nir = "B4_dn",
                           indices = c("NDVI", "SAVI", "MSAVI"))



# Guardar información espectral
writeRaster(IE_6_84$NDVI,"../output/datos/NDVI_22_06_84.img", datatype='FLT4S', overwrite = TRUE)
writeRaster(IE_6_84$SAVI,"../output/datos/SAVI_22_06_84.img", datatype='FLT4S', overwrite = TRUE)
writeRaster(IE_6_84$MSAVI,"../output/datos/MSAVI_22_06_84.img", datatype='FLT4S', overwrite = TRUE)

# INDICES de TEXTURA

# GLCM para la BANDA 4 con ventana de 3x3
# Se seleccionan las bandas que participan,
# los estadísticos deseados
# y la direccion
# por default esta la direccion a 45 grados
# si se requiere y teoricamente se debe de calcular la 
# direccion y su opuesto, por lo que
# se selecciona list(c(1,0), c(-1,0))

library(raster)
library(glcm)

# --- Parámetros generales ---
out_dir <- "../output/datos"
prefijo <- "22_6_84"  # lo que estás usando en los nombres de archivo

stats_glcm <- c("mean", "variance", "homogeneity", "entropy" )

shift_glcm <- list(c(1,0), c(-1,0))
window_glcm <- c(3, 3)


#loop
res_list <- vector("list", 7)

for (i in 1:7) {
  tmp <- glcm_band_to_files(
    r_stack  = costz_6_84R,   # tu raster (stack/brick o SpatRaster convertible)
    band_i   = i,
    out_dir  = out_dir,
    prefijo  = prefijo,
    nombres  = NULL
  )
  res_list[[i]] <- tmp$glcm
  nombres <- tmp$nombres
}


# Funcion para leer las variabels explicativas tratadas
Lista_VX <- list.files(
  "../output/datos/",
  pattern = ".img$",
  full.names = TRUE
)

nlay_por_archivo <- sapply(Lista_VX, function(f) raster::nlayers(raster::brick(f)))

rasVars <- raster::stack(Lista_VX)

# genera nombres por capa, respetando archivos multibanda
nm <- unlist(mapply(function(f, k) {
  base <- tools::file_path_sans_ext(basename(f))
  if (k == 1) base else paste0(base, "_", seq_len(k))
}, Lista_VX, sapply(Lista_VX, function(f) raster::nlayers(raster::brick(f))),
SIMPLIFY = FALSE))

names(rasVars) <- make.names(nm, unique = TRUE)

ta_sf <- sf::st_read("../input/conjunto//TA_1984.shp")

# Pasar a SpatVector
ta_v <- terra::vect(ta_sf)

# Asegurar CRS igual al raster
ta_v <- terra::project(ta_v, terra::crs(rasVars))

# Extraer valores
rasVars_terra <- terra::rast(rasVars)  
ta_vals <- terra::extract(rasVars_terra, ta_v)

# union 
ta_df <- cbind(as.data.frame(ta_v), ta_vals[ , -1, drop = FALSE]) %>% 
  dplyr::select(Cl1984, CostzPr_6_84_1, CostzPr_6_84_2,
  MSAVI_22_06_84, NDVI_22_06_84, SAVI_22_06_84,
  CostzPr_6_84_3, CostzPr_6_84_4,CostzPr_6_84_5,CostzPr_6_84_6,
  entropy_22_6_84_b4, entropy_22_6_84_b5, entropy_22_6_84_b7, 
  homogeneity_22_6_84_b4, homogeneity_22_6_84_b5, homogeneity_22_6_84_b7,
  mean_22_6_84_b4, mean_22_6_84_b5, mean_22_6_84_b7, 
  variance_22_6_84_b4, variance_22_6_84_b5,variance_22_6_84_b7)

summary(ta_df)

write.csv(ta_df, "../output/df_preproc.csv")


