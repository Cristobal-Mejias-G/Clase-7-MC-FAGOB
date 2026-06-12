# Librerías ---------------------------------------------------

library(pacman)
pacman::p_load(
  tidyverse,    # manipulación de datos
  dplyr,        # gramática de manipulación de datos 
  broom,        # resultados de tests como data frame
  kableExtra,   # formateo de tablas
  sjlabelled,   # Etiquetas
  Publish,      # intervalos de confianza legibles
  rempsyc,      # tablas de resultados para tests
  gginference,  # visualización de pruebas de hipótesis
  flextable,    # tablas de resultados
  sjPlot,       # gráficos y tablas
  sjmisc,       # tratamiento de datos
  stargazer,    # Tablas
  car          # función scatterplot y otras de manejo de datos
)
options(scipen = 999)  # Desactivar notificación cinetífica


# Carga de datos ----------------------------------------------------------
base <- read_sav("raw/cep95.sav") |> 
  select ("esc_delincuencia" = "democracia_38",
          "esc_responsabilidad" = "pobreza_63",
          "esc_ingreso_merito" =  "pobreza_62",
          "pos_pol" = "iden_pol_2",
          "sexo",
          "educacion" = "esc_nivel_1_c",
          "democracia" = "democracia_21",
          "lider_autoritario" = "democracia_37_c",
          "aborto" = "religion_14",
          "eutanasia" = "religion_64",
          "confianza_justicia" = "confianza_6_d",
          "confianza_congreso" = "confianza_6_k",
          "confianza_gobierno" = "confianza_6_i"
) |> 
    var_labels(
    esc_delincuencia = "Preferencia: Garantizar libertades vs. Suprimir libertades para controlar delincuencia",
    esc_responsabilidad = "Preferencia: Estado vs. Individuos como responsables del sustento económico",
    esc_ingreso_merito = "Preferencia: Igualdad de ingresos vs. Premiar el esfuerzo individual",
    pos_pol = "Posición política",
    sexo = "Sexo",
    educacion = "Nivel de educación",
    democracia = "Preferencia de régimen político: Democracia vs. Autoritarismo vs. Indiferencia",
    lider_autoritario = "Apoyo o rechazo a líder fuerte sin congreso ni elecciones",
    aborto = "Preferencia sobre el aborto",
    eutanasia = "Preferencia sobre la eutanasia",
    confianza_justicia = "Confianza en Tribunales de justicia",
    confianza_congreso = "Confianza en el Congreso",
    confianza_gobierno = "Confianza en el Gobierno"
  )

tabla_cep <- sjmisc::descr(base,
      show = c("label","range", "mean", "sd", "NA.prc", "n"))%>% # Selecciona estadísticos
  kable(.,"markdown" # Esto es para que se vea bien en quarto
  )

tabla_cep


saveRDS(base, "output/cep95.RData")



# Dependientes:
# Escala Delincuencia: “Se deben garantizar todas las libertades públicas y privadas, aunque eso 
# impida controlar la delincuencia” y 10 significa que “Se deben suprimir todas las 
# libertades públicas y privadas para controlar la delincuencia”.

# Escala Responsabilidad: “la principal responsabilidad por el sustento económico de las personas
#  está en el Estado” y 10 significa “la principal responsabilidad por el sustento económico 
# de las personas está en las personas mismas”?

# Escala Ingreso por mérito: A su juicio, ¿los ingresos deberían hacerse más iguales o debería 
# incentivarse el esfuerzo individual? ¿Dónde se ubicaría Ud. en esta escala, en que 
# 1 significa “los ingresos deberían hacerse más iguales, aunque no se premie el 
# esfuerzo individual” y 10 significa “debería premiarse el esfuerzo individual aunque se 
# produzcan importantes diferencias de ingresos”?

# Independientes:
# Posición política: 1 significa izquierda y 10 significa derecha.

# Sexo: 1 significa hombre y 2 significa mujer.

# Educación: 1 significa "Media completa o inferior", 2 significa "Universitaria incompleta\no Tecnica 
# profesional" y 3 significa "Universitaria completa o superior".

# Preferencia por la democracia: 
# 1 significa "La democracia es preferible a cualquier otra forma de gobierno" 
# 2 significa "En algunas circunstancias, un gobierno autoritario puede ser preferible a uno democrático" 
# y 3 significa "A la gente como uno, le da lo mismo un régimen democrático que uno autoritario".

# Líder autoritario: ¿Cómo evaluaría Ud. cada una de ellas de acuerdo a la escala? Tener un líder fuerte sin congreso ni elecciones

# Aborto: 1 significa "El aborto debe estar siempre prohibido", 
# 2 significa "El aborto sólo debe estar permitido en casos especiales" 
# y 3 significa "El aborto debe ser una opción para las mujeres, en cualquier caso".

# Eutanasia: 1 significa "La eutanasia debe estar siempre permitida", 
# 2 significa "La eutanasia sólo debe estar permitida en casos especiales"
# y 3 significa "La eutanasia no debe estar nunca permitida".

# Confianza en la justicia: 1 significa "Mucha confianza", 2 significa "Bastante confianza", 
# 3 significa "Poca confianza" y 4 significa "Nada de confianza".

# Confianza en el congreso: 1 significa "Mucha confianza", 2 significa "Bastante confianza", 
# 3 significa "Poca confianza" y 4 significa "Nada de confianza".

# Confianza en el gobierno: 1 significa "Mucha confianza", 2 significa "Bastante confianza", 
# 3 significa "Poca confianza" y 4 significa "Nada de confianza".

# Recodificación de variables ----------------------------------------------------------


# Posición política: categorías
base <- base |> 
  mutate(pos_pol_rec = case_when(pos_pol %in% c(1,2,3,4) ~ "1. Izquierda",
                                pos_pol %in% c(5,6) ~ "2. Centro",
                                pos_pol %in% c(7,8,9,10) ~ "3. Derecha",
                                pos_pol %in% c(-8,-9) ~ "4. Sin posición",))

# Posición política: dummys
base <- base |> 
  mutate(
    pol_izquierda = if_else(pos_pol  %in% c(1,2,3,4), 1, 0),
    pol_centro = if_else(pos_pol %in% c(5,6), 1, 0),
    pol_derecha = if_else(pos_pol %in% c(7,8,9,10), 1, 0),
    pol_sin_posicion = if_else(pos_pol %in% c(-8,-9), 1, 0)
  )



# Educación: categorías
base <- base |> 
  mutate(educ_rec = case_when(educacion %in% c(0,1,2,3,4) ~ "1. Media completa o inferior",
                            educacion %in% c(5,6,7) ~ "2. Universitaria incompleta\no Tecnica profesional",
                            educacion %in% c(8,9,10) ~ "3. Universitaria completa o superior",
                            TRUE ~ NA_character_))

# Educación: dummys
base <- base |> 
  mutate(
    educ_media_completa = if_else(educacion %in% c(0,1,2,3,4), 1, 0),
    educ_uni_incompleta = if_else(educacion %in% c(5,6,7), 1, 0),
    educ_uni_completa   = if_else(educacion %in% c(8,9,10), 1, 0)
  )



# Preferencia por la democracia: categorías
base <- base |> 
  mutate(pref_dem_rec = case_when(democracia == 1 ~ "1. Democracia es preferible a cualquier otra forma de gobierno",
                                democracia == 2 ~ "2. En algunas circunstancias, un gobierno autoritario puede ser preferible a uno democrático",
                                democracia == 3 ~ "3. A la gente como uno, le da lo mismo un régimen democrático que uno autoritario",
                                TRUE ~ NA_character_))

# Preferencia por la democracia: dummys
base <- base |>
  mutate(
    pref_democracia = if_else(democracia == 1, 1, 0),
    pref_autoritarismo = if_else(democracia == 2, 1, 0),
    pref_indiferente = if_else(democracia == 3, 1, 0)
  )



# Líder autoritario: categorías
base <- base |> 
  mutate(lider_autoritario_rec = case_when(lider_autoritario %in% c(1,2) ~ "1. Apoyo líder fuerte sin congreso ni elecciones",
                                lider_autoritario %in% c(3,4) ~ "2. Rechazo líder fuerte sin congreso ni elecciones",
                                TRUE ~ NA_character_))
    
# Líder autoritario: dummys
base <- base |> 
  mutate(
    apoyo_lider_autoritario = if_else(lider_autoritario %in% c(1,2), 1, 0),
    rechazo_lider_autoritario = if_else(lider_autoritario %in% c(3,4), 1, 0)
  )

# Aborto: categorías
base <- base |> 
  mutate(aborto_rec = case_when(aborto == 1 ~ "1. El aborto debe estar siempre prohibido",
                                aborto == 2 ~ "2. El aborto sólo debe estar permitido en casos especiales",
                                aborto == 3 ~ "3. El aborto debe ser una opción para las mujeres, en cualquier caso",
                                TRUE ~ NA_character_))

# Aborto: dummys
base <- base |> 
  mutate(aborto_prohibido = if_else(aborto == 1, 1, 0)) |> 
  mutate(aborto_casosespeciales = if_else(aborto == 2, 1, 0)) |> 
  mutate(aborto_libre = if_else(aborto == 3, 1, 0))

# Eutanasia: categorías
base <- base |> 
  mutate(eutanasia_rec = case_when(eutanasia == 1 ~ "1. La eutanasia debe estar siempre permitida",
                                eutanasia == 2 ~ "2. La eutanasia sólo debe estar permitida en casos especiales",
                                eutanasia == 3 ~ "3. La eutanasia no debe estar nunca permitida",
                                TRUE ~ NA_character_))

# Eutanasia: dummys
base <- base |> 
  mutate(eutanasia_permitida = if_else(eutanasia == 1, 1, 0)) |> 
  mutate(eutanasia_casosespeciales = if_else(eutanasia == 2, 1, 0)) |> 
  mutate(eutanasia_prohibida = if_else(eutanasia == 3, 1, 0))



# Confianza en el Estado: 
base <- base |> 
  mutate(
    across(
      c(confianza_justicia, confianza_congreso, confianza_gobierno),
      ~ sjmisc::rec(., rec = "1=4; 2=3; 3=2; 4=1; else=NA"),
      .names = "{.col}_recode"
    )
  ) |> 
  mutate(
    across(
      c(confianza_justicia_recode, confianza_congreso_recode, confianza_gobierno_recode),
      ~ sjlabelled::set_labels(., labels = c("Nada de confianza" = 1, "Poca confianza" = 2, "Bastante confianza" = 3, "Mucha confianza" = 4))
    )
  ) |> 
mutate(
    esc_confianza_estado2 = confianza_justicia_recode + confianza_congreso_recode + confianza_gobierno_recode,
    esc_confianza_estado2 = esc_confianza_estado2 - 2
  ) |> 
  set_labels(
    esc_confianza_estado2,
    labels = c("Nada de confianza" = 1, "Mucha confianza" = 10)
  ) |> 
  var_labels(
    esc_confianza_estado2 = "Escala de Confianza en el Estado"
  )


# Visualización de datos ----------------------------------------------------------


tabla_cep <- sjmisc::descr(base,
      show = c("label","range", "mean", "sd", "NA.prc", "n"))%>% # Selecciona estadísticos
  kable(.,"markdown" # Esto es para que se vea bien en quarto
  )

tabla_cep


# Sobre categorías de referencia ----------------------------------------------------------


# base <- base |> 
#   mutate(posicion_politica = fct_relevel(posicion_politica, "Centro")) # Fijamos Centro como referencia



# Casos perdidos ---------------------------------------------------------------

base_limpia <- na.omit(base)


# Modelos de regresión ----------------------------------------------------------

model1 <- lm(esc_delincuencia ~ pos_pol_rec, data = base_limpia)

## Plus: Modelo de regresión logística binaria ----------------------------------------------------------


# Exportación de tablas ----------------------------------------------------------

screenreg(results_2)
screenreg(list(reg_ind, reg_agg, results_3))


htmlreg(list(reg_ind, reg_agg, results_3), 
    custom.model.names = c("Individual","Agregado","Multinivel"),    
    custom.coef.names = c("Intercepto", "$SES_{ij}$","$Mujer_{ij}$", "$SES_{j}$", "$Sector_{j}$"), 
    custom.gof.names=c(NA,NA,NA,NA,NA,NA,NA, 
                   "Var:id ($\\tau_{00}$)","Var: Residual ($\\sigma^2$)"),
    custom.note = "%stars. Errores estándar en paréntesis",
    caption="Comparación de modelos Individual, Agregado y Multinivel",
    caption.above=TRUE,
    doctype = FALSE)

sjPlot::tab_model(model1, model2, model3, dv.labels = c("Raw Score Model","GMC Model","CMC Model"), show.ci = FALSE)


# Ejercicios Aplicados ----------------------------------------------------------
## Ejercicio 1 ------------------------------------------------------------------

model2 <- lm

## Ejercicio 2 ------------------------------------------------------------------


## Ejercicio 3 ------------------------------------------------------------------





