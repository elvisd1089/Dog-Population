# Paper analysis - canine ecology
# Elvis Diaz

# Library
library(tidyverse)

# calling database
setwd("/Users/pmacs/Desktop/Envío Paper Dinanica/Bases")

# Loading dataset - 2016
dogs <- read.csv("dogs16_data.csv", sep = ";")
houses <- read.csv("houses16_data.csv", sep = ";")

# Loading dataset - Cohort
dogs_cohort <- read.csv("dogs_cohort.csv", sep = ";")
houses_cohort <- read.csv("houses_cohort.csv", sep = ";")

# Periurban
houses_p<-houses_cohort[houses_cohort$TYPE_L=="periurban",] #212

# Urban
houses_u<-houses_cohort[houses_cohort$TYPE_L=="urban",] #673
# Identify cohort Houses
cohort_ids <- houses_cohort$UNICODE

# Select dogs - 2016
dogs16 <- dogs_cohort %>%
  filter(UNICODE %in% cohort_ids, ESTADO == "Seguimiento" | is.na(ESTADO))

# Dogs of 2017
dogs17 <- dogs_cohort %>%
  filter(!is.na(ESTADO), SIGUE_EN_CASA17 == "Si" | ESTADO == "Nuevo",
         UNICODE %in% cohort_ids)

# Separate by area
dogs16_area <- split(dogs16, dogs16$TYPE_L)
dogs17_area <- split(dogs17, dogs17$TYPE_L)

# By group
dogs16_p <- dogs16_area$periurban
dogs16_u <- dogs16_area$urban
dogs17_p <- dogs17_area$periurban
dogs17_u <- dogs17_area$urban

# By area
# Periurban
  houses_p<-houses_cohort[houses_cohort$TYPE_L=="periurban",] #212
  
# Urban
  houses_u<-houses_cohort[houses_cohort$TYPE_L=="urban",] #673
  
#------------------------------------------------------------------------------#
# Table 1.
  #### Proportion of houses with dogs ---- 
  houses_u$HAY_PERROS17[houses_u$HAY_PERROS17==""]<-"No"
  
  # Periurban
  tbl_p <- cbind(table(houses_p$PERROS), table(houses_p$HAY_PERROS17))
  test_p <- chisq.test(tbl_p, correct = TRUE)
  
  # Urban
  tbl_u <- cbind(table(houses_u$PERROS), table(houses_u$HAY_PERROS17))
  test_u <- chisq.test(tbl_u, correct = TRUE)
  
  # Table
  table1_proportion <- data.frame(
    Variable = "Proportion of houses with dogs",
    Periurban_2016 = mean(houses_p$PERROS == "SI", na.rm = TRUE),
    Periurban_2017 = mean(houses_p$HAY_PERROS17 == "Si", na.rm = TRUE),
    P_periurban = test_p$p.value,
    Urban_2016 = mean(houses_u$PERROS == "SI", na.rm = TRUE),
    Urban_2017 = mean(houses_u$HAY_PERROS17 == "Si", na.rm = TRUE),
    P_urban = test_u$p.value)
  
  # Round values
  table1_proportion[-1] <- lapply(table1_proportion[-1], round, 3)
  
  # Display table
  knitr::kable(table1_proportion, caption = "Proportion of houses with dogs")
  
#### Ratio of human to dog ----  
  # Convert variables to numeric
  houses_p <- houses_p %>%
    mutate(CUANTOS = as.numeric(CUANTOS))
  
  houses_u <- houses_u %>%
    mutate(CUANTOS = as.numeric(CUANTOS))
  
  # Replace missing-value code
  houses_p$CANTIDAD_PERSONAS17[houses_p$CANTIDAD_PERSONAS17 == 9999] <- NA
  houses_u$CANTIDAD_PERSONAS17[houses_u$CANTIDAD_PERSONAS17 == 9999] <- NA
  
  # Bootstrap for human-to-dog ratio
  bootstrap_ratio <- function(data, people, dogs,
                              n_people, n_dogs, B = 10000) {
    df_people <- na.omit(data[[people]])
    df_dogs <- na.omit(data[[dogs]])
    replicate(B, {
      sum(sample(df_people, n_people, replace = TRUE)) /
        sum(sample(df_dogs, n_dogs, replace = TRUE))})}
  set.seed(1977)
  
  # Periurban
  ratios_p16 <- bootstrap_ratio(houses_p, "VIVEN_TOTAL", "CUANTOS", 212, 212)
  
  ratios_p17 <- bootstrap_ratio(houses_p, "CANTIDAD_PERSONAS17", "CUANTOS_PERROS17", 211, 212)
  
  # Urban
  ratios_u16 <- bootstrap_ratio(houses_u, "VIVEN_TOTAL", "CUANTOS", 665, 673)
  
  ratios_u17 <- bootstrap_ratio(houses_u, "CANTIDAD_PERSONAS17", "CUANTOS_PERROS17", 670, 673)
  
  # Observed ratios
  ratio_p16 <- sum(houses_p$VIVEN_TOTAL, na.rm = TRUE) /
    sum(houses_p$CUANTOS, na.rm = TRUE)
  ratio_p17 <- sum(houses_p$CANTIDAD_PERSONAS17, na.rm = TRUE) /
    sum(houses_p$CUANTOS_PERROS17, na.rm = TRUE)
  ratio_u16 <- sum(houses_u$VIVEN_TOTAL, na.rm = TRUE) /
    sum(houses_u$CUANTOS, na.rm = TRUE)
  ratio_u17 <- sum(houses_u$CANTIDAD_PERSONAS17, na.rm = TRUE) /
    sum(houses_u$CUANTOS_PERROS17, na.rm = TRUE)
  
  # Bootstrap confidence intervals
  ci_ratio <- function(x) {quantile(x, c(0.025, 0.975), na.rm = TRUE)}
  ci_p16 <- ci_ratio(ratios_p16)
  ci_p17 <- ci_ratio(ratios_p17)
  ci_u16 <- ci_ratio(ratios_u16)
  ci_u17 <- ci_ratio(ratios_u17)
  
  # Bootstrap comparison of ratios
  bootstrap_pvalue <- function(x, y, observed, B = 9999) {
    boot <- replicate(B, {
    mean(sample(x, length(x), replace = TRUE)) /
    mean(sample(y, length(y), replace = TRUE))})
    (1 + sum(boot >= observed)) / (B + 1)}
  
  # Periurban
  ratio_ratios_p <- ratio_p16 / ratio_p17
  p_ratio_p <- bootstrap_pvalue(ratios_p16, ratios_p17, ratio_ratios_p)
  
  # Urban
  ratio_ratios_u <- ratio_u16 / ratio_u17
  p_ratio_u <- bootstrap_pvalue(ratios_u16, ratios_u17, ratio_ratios_u)
  
  # Table 1 - Human-to-dog ratio
  table1_ratio <- data.frame(
    Variable = "Ratio of human to dog",
    Periurban_2016 = paste0(round(ratio_p16, 2), ":1"),
    Periurban_2017 = paste0(round(ratio_p17, 2), ":1"),
    P_periurban = round(p_ratio_p, 3),
    Urban_2016 = paste0(round(ratio_u16, 2), ":1"),
    Urban_2017 = paste0(round(ratio_u17, 2), ":1"),
    P_urban = round(p_ratio_u, 3))
  knitr::kable(table1_ratio,caption = "Human-to-dog")
  
  #### Canine population density ----
  # Number of dogs per household: PERIURBAN
  dogs_house_p <- houses_p %>%
    distinct(UNICODE) %>%
    left_join(
      dogs16_p %>% dplyr::count(UNICODE, name = "dogs_2016"), by = "UNICODE") %>%
    left_join(
      dogs17_p %>% dplyr::count(UNICODE, name = "dogs_2017"), by = "UNICODE") %>%
    mutate(
      dogs_2016 = replace_na(dogs_2016, 0), dogs_2017 = replace_na(dogs_2017, 0))
  
  # Number of dogs per household: URBAN
  dogs_house_u <- houses_u %>%
    distinct(UNICODE) %>%
    left_join(
      dogs16_u %>% dplyr::count(UNICODE, name = "dogs_2016"), by = "UNICODE") %>%
    left_join(dogs17_u %>% dplyr::count(UNICODE, name = "dogs_2017"), by = "UNICODE") %>%
    mutate(dogs_2016 = replace_na(dogs_2016, 0), dogs_2017 = replace_na(dogs_2017, 0))
  
#### Mean dogs per house (SD) ----
  # Convert variables to numeric
  houses_cohort <- houses_cohort %>%
    mutate(CUANTOS = as.numeric(CUANTOS),
      CUANTOS_PERROS17 = as.numeric(CUANTOS_PERROS17))
  
  # Mean dogs per house (SD)
  mean_dogs <- function(data, area) {
    d <- data %>% filter(TYPE_L == area)
    
    # Descriptive statistics
    mean16 <- mean(d$CUANTOS, na.rm = TRUE)
    sd16   <- sd(d$CUANTOS, na.rm = TRUE)
    mean17 <- mean(d$CUANTOS_PERROS17, na.rm = TRUE)
    sd17   <- sd(d$CUANTOS_PERROS17, na.rm = TRUE)
    
    # Paired t-test
    p <- t.test(d$CUANTOS, d$CUANTOS_PERROS17, paired = TRUE)$p.value
    data.frame(Area = area, Mean_2016 = mean16, SD_2016 = sd16,
               Mean_2017 = mean17, SD_2017 = sd17, P_value = p)}
  
  # Apply function
  results_mean_dogs <- bind_rows(
    mean_dogs(houses_cohort, "periurban"),
    mean_dogs(houses_cohort, "urban"))
  results_mean_dogs
  knitr::kable(results_mean_dogs, digits = 2, caption = "Mean dogs per house")
  
#------------------------------------------------------------------------------# 
# Figure 2. Baseline sex and age distribution in urban and periurban areas
  # DOG SEX
  dogs$SEXO[dogs$SEXO == "F"] <- "H"
  dogs$SEX <- factor(dogs$SEXO, levels = c("H", "M"), labels = c("Female", "Male"))
  table(dogs$SEX, useNA = "always")
  
  # DOG AGE
  dogs$EDAD_M[dogs$EDAD_M == "NS"] <- NA
  dogs$EDAD_A[dogs$EDAD_A == "NS"] <- NA
  
  dogs$dogageyear <- as.numeric(as.character(dogs$EDAD_A))
  dogs$dogagemonth <- as.numeric(as.character(dogs$EDAD_M))
  dogs$dogage <- dogs$dogageyear * 12 + dogs$dogagemonth
  dogs$dogage1 <- round(dogs$dogage / 12, digits = 1)
  
  # Separating by area
  peri <- dogs[dogs$TYPE_L == "periurban", ]
  urban <- dogs[dogs$TYPE_L == "urban", ]
  
  # Age intervals
  rangos.valor  <- cut(dogs$dogage1, seq(0, 15, by = 1))
  rangos.valor1 <- cut(peri$dogage1, seq(0, 15, by = 1))
  rangos.valor2 <- cut(urban$dogage1, seq(0, 15, by = 1))
  
  edad_sexo <- table(dogs$SEX, rangos.valor)
  test1 <- data.frame(edad_sexo)
  df0 <- data.frame(Edad = test1$rangos.valor, Sex = test1$Var1, Poblacion = test1$Freq)
  
  # Calculating percentages
  df0$Percentage <- df0$Poblacion / sum(df0$Poblacion) * 100
  
  # Plot
  plot <- ggplot(
    data = df0,
    mapping = aes(
      x = ifelse(Sex == "Male", -Percentage, Percentage),
      y = Edad, fill = Sex)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = c("coral1", "cornflowerblue")) +
    scale_x_continuous(labels = function(x) paste0(abs(x), "%")) +
    labs(x = "Percentage of dogs",y = "Age in years",fill = "Sex") +
    theme_minimal()
  
  plot
  
 # Periurban
  edad_rango1 <- table(peri$SEX, rangos.valor1)
  test1 <- data.frame(edad_rango1)
  df0 <- data.frame(Edad = test1$rangos.valor1,Sex = test1$Var1,Poblacion = test1$Freq)
  df0$Percentage <- df0$Poblacion / sum(df0$Poblacion) * 100

  plot1 <- ggplot(
    data = df0,mapping = aes(
      x = ifelse(Sex == "Male", -Percentage, Percentage),
      y = Edad,fill = Sex)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = c("coral1", "cornflowerblue")) +
    scale_x_continuous(labels = function(x) paste0(abs(x), "%")) +
    labs(x = "Percentage of dogs",y = "Age in years",fill = "Sex") +
    theme_minimal()
  
  plot1
  
# Urban
  edad_rango2 <- table(urban$SEX, rangos.valor2)
  test1 <- data.frame(edad_rango2)
  df0 <- data.frame(Edad = test1$rangos.valor2,
    Sex = test1$Var1, Poblacion = test1$Freq)
  df0$Percentage <- df0$Poblacion / sum(df0$Poblacion) * 100
  
  plot2 <- ggplot(
    data = df0, mapping = aes(
      x = ifelse(Sex == "Male", -Percentage, Percentage),
      y = Edad, fill = Sex)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = c("coral1", "cornflowerblue")) +
    scale_x_continuous(labels = function(x) paste0(abs(x), "%")) +
    labs(x = "Percentage of dogs", y = "Age in years", fill = "Sex") +
    theme_minimal()
  
  plot2
  
#------------------------------------------------------------------------------#  
# Table 2. Origin of dogs that entered the cohort
  dogs17_new <- dogs_cohort %>%
    filter(UNICODE %in% cohort_ids,ESTADO == "Nuevo")
  
  # ORIGIN OF NEW DOGS
  
  # Frequency table
  tabla_procedencia <- table(
    dogs17_new$PROCEDENCIA,
    dogs17_new$TYPE_L
  )
  
  # Absolute frequencies
  tabla_procedencia
  
  # Percentages by TYPE_L
  round(prop.table(tabla_procedencia, margin = 2) * 100, 1) 
  
  fisher.test(tabla_procedencia)
  
  # PLACE OF ORIGIN OF NEW DOGS
  dogs17_new$DONDE_TRAJERON17[
    dogs17_new$PROCEDENCIA == "CRIA DE MIS PERRAS.NACIO AQUI"
  ] <- "Nacio en esta vivienda"
  
  # Frequency table
  tabla_donde_trajeron <- table(
    dogs17_new$DONDE_TRAJERON17,
    dogs17_new$TYPE_L
  )
  
  # Absolute frequencies
  tabla_donde_trajeron
  
  # Percentages by TYPE_L
  round(prop.table(tabla_donde_trajeron, margin = 2) * 100, 1) 
  
  fisher.test(tabla_donde_trajeron)
#------------------------------------------------------------------------------#
# Table 3. Access
 
  # Cleaning empty values
  dogs16_p$ACCESO_CALLE[dogs16_p$ACCESO_CALLE == ""] <- NA
  dogs16_u$ACCESO_CALLE[dogs16_u$ACCESO_CALLE == ""] <- NA
  
  # Building the combined dataset
  acceso16 <- data.frame(
    ACCESO_CALLE = c(dogs16_p$ACCESO_CALLE, dogs16_u$ACCESO_CALLE),
    TYPE_L = factor(c(rep("Periurban", nrow(dogs16_p)),
        rep("Urban", nrow(dogs16_u))),levels = c("Periurban", "Urban")))
  
  # Frequencies and percentages
  tab_acceso16 <- table(acceso16$ACCESO_CALLE, acceso16$TYPE_L)
  
  tab_acceso16
  round(prop.table(tab_acceso16, margin = 2) * 100, 1)
  
  # Chi-square test
  chisq.test(tab_acceso16, correct = FALSE)
  
  # RESTRICTION LEVELS - 2017
  
  # Grouping exit categories
  dogs17$COMO_PERMANECE_CASA17[
    dogs17$COMO_PERMANECE_CASA17 %in% c("sale_dia", "sale_noche")
  ] <- "sale_una_o_dos_veces_al_dia"
  
  # Frequency table
  tab_restriccion17 <- table(dogs17$COMO_PERMANECE_CASA17, dogs17$TYPE_L)
  
  # Percentages by area
  round(prop.table(tab_restriccion17, margin = 2) * 100, 1)
  
  # Fisher
  fisher.test(tab_restriccion17)
  
  # DOG WALKING - 2016
  
  paseo16 <- data.frame(
    PASEAN16 = c(dogs16_p$PASEAN16, dogs16_u$PASEAN16),
    TYPE_L = factor(c(rep("Periurban", nrow(dogs16_p)),
        rep("Urban", nrow(dogs16_u))),
      levels = c("Periurban", "Urban")))
  paseo16$PASEAN16[paseo16$PASEAN16 == ""] <- NA
  
  tab_paseo16 <- table(paseo16$PASEAN16, paseo16$TYPE_L)
  round(prop.table(tab_paseo16, margin = 2) * 100, 1)
  chisq.test(tab_paseo16, correct = FALSE)
  
  #### DOG WALKING - 2017 ####
  tab_paseo17 <- table(dogs17$PASEAN17, dogs17$TYPE_L)
  round(prop.table(tab_paseo17, margin = 2) * 100, 1)
  chisq.test(tab_paseo17, correct = FALSE)
  
  # LEASH USE - 2016

  # Cleaning missing values
  dogs16_p$CORREA16[dogs16_p$CORREA16 %in% c("NS", "")] <- NA
  dogs16_u$CORREA16[dogs16_u$CORREA16 %in% c("NS", "")] <- NA
  
  correa16 <- data.frame(
    CORREA16 = c(dogs16_p$CORREA16, dogs16_u$CORREA16),
    TYPE_L = factor(c(rep("Periurban", nrow(dogs16_p)),
        rep("Urban", nrow(dogs16_u))),
      levels = c("Periurban", "Urban")))
  
  tab_correa16 <- table(correa16$CORREA16, correa16$TYPE_L)
  round(prop.table(tab_correa16, margin = 2) * 100, 1)
  chisq.test(tab_correa16, correct = FALSE)
  
  #### LEASH USE - 2017 ####
  tab_correa17 <- table(dogs17$PASEAN_CORREA17, dogs17$TYPE_L)
  round(prop.table(tab_correa17, margin = 2) * 100, 1)
  chisq.test(tab_correa17, correct = FALSE)

#------------------------------------------------------------------------------#
# Table 4. Dogs exiting the cohort 
  # Loading data
  dogs <- read.csv("dogs_exit.csv", sep = ";")
  
  # Selecting dogs that exited the cohort
  dogs <- dogs[!is.na(dogs$ESTADO) & dogs$ESTADO == "Seguimiento" &
          dogs$SIGUE_EN_CASA17 %in% c("No", "perro_no_es_esta_vivienda"),]
  
  # Checking sample size
  table(dogs$TYPE_L)

  # Sex - Frequencies
  tabla_sexo <- table(dogs$SEXO, dogs$TYPE_L)
  
  # Percentage of females
  round(prop.table(tabla_sexo, 2)["H", ] * 100, 1)
  
  # Chi-square
  chisq.test(tabla_sexo)
  
  # Calculating age in months
  dogs$dogageyear <- as.numeric(as.character(dogs$EDAD_ANIO17))
  dogs$dogagemonth <- as.numeric(as.character(dogs$EDAD_MESES17))
  dogs$dogage <- dogs$dogageyear * 12 + dogs$dogagemonth
  
  # Separating by area
  dogs_periurban <- dogs[dogs$TYPE_L == "periurban", ]
  dogs_urban <- dogs[dogs$TYPE_L == "urban", ]
  
  # Median and IQR
  median(dogs_periurban$dogage, na.rm = TRUE)
  quantile(dogs_periurban$dogage, c(0.25, 0.75), na.rm = TRUE)
  
  median(dogs_urban$dogage, na.rm = TRUE)
  quantile(dogs_urban$dogage, c(0.25, 0.75), na.rm = TRUE)
  
  # Mann-Whitney
  wilcox.test(dogage ~ TYPE_L, data = dogs)
  
  # Mean and standard deviation
  mean(dogs_periurban$dogage, na.rm = TRUE)
  sd(dogs_periurban$dogage, na.rm = TRUE)
  
  mean(dogs_urban$dogage, na.rm = TRUE)
  sd(dogs_urban$dogage, na.rm = TRUE)
  
  # Student t-test
  t.test(dogage ~ TYPE_L, data = dogs) 
  
  # Purebred - Frequencies
  tabla_raza <- table(dogs$RAZA, dogs$TYPE_L)
  
  # Percentage of purebred dogs
  round(prop.table(tabla_raza, 2)["R", ] * 100, 1)
  
  # Chi-square
  chisq.test(tabla_raza)
  
  # Vaccinated - Excluding NS
  dogs$ESTE_ANIO_VACUNARON17[dogs$ESTE_ANIO_VACUNARON17 == "NS"] <- NA
  
  # Frequencies
  tabla_vacuna <- table(dogs$ESTE_ANIO_VACUNARON17,dogs$TYPE_L)
  
  # Percentage vaccinated
  round(prop.table(tabla_vacuna, 2)["Si", ] * 100, 1)
  
  # Chi-square
  chisq.test(tabla_vacuna)
  
  # Access - Excluding blank responses
  dogs$ACCESO_CALLE16[dogs$ACCESO_CALLE16 == ""] <- NA
  
  # Frequencies
  tabla_acceso <- table(dogs$ACCESO_CALLE16,dogs$TYPE_L)
  
  # Percentage with free access
  round(prop.table(tabla_acceso, 2)["SI", ] * 100, 1)
  
  # Chi-square
  chisq.test(tabla_acceso)
  
  # Exit - Frequencies
  tabla_salida <- table(dogs$CAUSA_NO_ESTE17,dogs$TYPE_L)
  
  # Percentages by area
  round(prop.table(tabla_salida, 2) * 100, 1)
  
  # Fisher exact test (hybrid)
  fisher.test(tabla_salida,hybrid = TRUE,workspace = 2e7)
  
  # Emigration - Selecting dogs sent to another household
  send_other <- dogs[
    dogs$CAUSA_NO_ESTE17 == "otra_casa" & !is.na(dogs$CAUSA_NO_ESTE17),]
  
  # Reviewing original destinations
  table(send_other$OTRA_CIUDAD17, useNA = "ifany")
  
  # Replacing "ns" with NA
  send_other$OTRA_CIUDAD17[
    tolower(trimws(send_other$OTRA_CIUDAD17)) == "ns"] <- NA
  
  # Standardizing responses
  send_other$OTRA_CIUDAD17 <- tolower(trimws(send_other$OTRA_CIUDAD17))
  
  # Classifying destinations
  send_other$OTRA_CIUDAD17[
    send_other$OTRA_CIUDAD17 %in%c("aqp", "arequipa", "1.1.78.94")] <- "arequipa"
  
  # Creating destination categories
  send_other$DESTINO <- ifelse(
    send_other$OTRA_CIUDAD17 == "arequipa", "To other districts within Arequipa",
    ifelse(send_other$OTRA_CIUDAD17 == "lima","To Lima",
      ifelse(send_other$OTRA_CIUDAD17 %in% c("cusco", "moquegua"),
        "To other regions",NA)))
  
  # Reviewing classification
  table(send_other$DESTINO, useNA = "ifany")
  
  # Frequencies by area
  tabla_destino <- table(send_other$DESTINO,send_other$TYPE_L)
  
  # Percentages by area
  round(prop.table(tabla_destino, margin = 2) * 100,digits = 1)
  
  # Fisher's exact test
  fisher.test(tabla_destino,hybrid = TRUE,workspace = 2e7)
  
  # Cause of death - Selecting deceased dogs
  death <- dogs[dogs$CAUSA_NO_ESTE17 == "murio" & !is.na(dogs$DE_QUE_MURIO17),]
  
  # Ordering categories
  death$DE_QUE_MURIO17 <- factor(death$DE_QUE_MURIO17, levels = c("veneno",
      "enfermedad", "atropello", "natural_vejez", "otro"))
  
  # Frequencies
  tabla_muerte <- table(death$DE_QUE_MURIO17,death$TYPE_L)
  
  # Percentages by area
  round(prop.table(tabla_muerte, 2) * 100, 1)
  
  # Fisher exact test
  fisher.test(tabla_muerte, hybrid = TRUE, workspace = 2e7)
  
#------------------------------------------------------------------------------#
# Table 5. Transient dogs
  # Reading data from the unified dog database
  PERRO_TEMPORAL_2017<-read.csv("dogs_temporal.csv", sep = ";")
  
  # Identifying urban localities
  localidades_urbanas <- c("1.1.77.", "1.1.78.", "1.1.81.", "1.1.83.")
  
  # Classifying transient dogs
  TEMPORAL_URBANO <- PERRO_TEMPORAL_2017[
    grepl(paste(localidades_urbanas, collapse = "|"),
      PERRO_TEMPORAL_2017$UNICODE_PERRO_TEMPORAL), ]
  
  TEMPORAL_PERI <- PERRO_TEMPORAL_2017[
    !grepl(paste(localidades_urbanas, collapse = "|"),
      PERRO_TEMPORAL_2017$UNICODE_PERRO_TEMPORAL), ]
  
  # Number of records by area
  URBANO_TOTAL <- nrow(TEMPORAL_URBANO)
  PERIURBANO_TOTAL <- nrow(TEMPORAL_PERI)
  
  # Number of households with transient dogs
  TEMPORAL_SI <- PERRO_TEMPORAL_2017[PERRO_TEMPORAL_2017$TUVO_PERRO_TEMPORAL == "Si", ]
  TEMPORAL_SI_U <- TEMPORAL_URBANO[TEMPORAL_URBANO$TUVO_PERRO_TEMPORAL == "Si", ]
  TEMPORAL_SI_PERI <- TEMPORAL_PERI[TEMPORAL_PERI$TUVO_PERRO_TEMPORAL == "Si", ]
  
  # Table showing the presence of transient dogs
  tabla_temporal <- table(
    Area = c(rep("Periurban", nrow(TEMPORAL_PERI)),rep("Urban", nrow(TEMPORAL_URBANO))),
    TUVO = c(TEMPORAL_PERI$TUVO_PERRO_TEMPORAL,TEMPORAL_URBANO$TUVO_PERRO_TEMPORAL))
  tabla_temporal
  
  # Percentage of households with transient dogs
  porcentaje_temporal <- prop.table(tabla_temporal,margin = 1) * 100
  round(porcentaje_temporal, 1)
  
  # Chi-square test
  chisq.test(tabla_temporal)
  
  # Table of reasons
  tabla_motivos <- table(
    Area = c(rep("Periurban", nrow(TEMPORAL_SI_PERI)),
             rep("Urban", nrow(TEMPORAL_SI_U))),
    Motivo = c(TEMPORAL_SI_PERI$QUE_PASO_PERRO_TEMPORAL,
               TEMPORAL_SI_U$QUE_PASO_PERRO_TEMPORAL))
  tabla_motivos
  
  # Percentages by area
  porcentaje_motivos <- prop.table(tabla_motivos,margin = 1) * 100
  round(porcentaje_motivos, 1)
  
  # Fisher's exact test
  fisher.test(tabla_motivos) 

  
