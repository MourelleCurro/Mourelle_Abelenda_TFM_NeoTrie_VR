# ==============================================================================
# SCRIPT DE ANÁLISE DE DATOS: PERCEPCIÓN DE VISORES VR NA AULA
# Autoría/Uso: Traballo Fin de Mestrado
# ==============================================================================

setwd("C:/Users/FX506/Downloads")

# ==============================================================================
# 1. INSTALACIÓN E CARGA DE PAQUETES
# ==============================================================================
# Este bloque encárgase de comprobar se as ferramentas (paquetes) necesarias 
# para procesar os datos e xerar os gráficos están instaladas no ordenador. 
# Se falta algunha, instálaa automaticamente. Finalmente, carga todas as 
# librerías en memoria para poder usalas ao longo do script.
# ==============================================================================
paquetes_necesarios <- c("dplyr", "ggplot2", "tidyr", "stringr", "forcats", 
                         "patchwork", "ggalluvial", "ggcorrplot")

novos_paquetes <- paquetes_necesarios[!(paquetes_necesarios %in% installed.packages()[,"Package"])]
if(length(novos_paquetes)) install.packages(novos_paquetes)

library(dplyr)       # Para a manipulación e transformación de datos
library(ggplot2)     # Para a creación de gráficos
library(tidyr)       # Para reorganizar o formato das táboas de datos
library(stringr)     # Para a manipulación de cadeas de texto
library(forcats)     # Para ordenar as categorías (factores) nos gráficos
library(patchwork)   # Para combinar varios gráficos nunha soa imaxe
library(ggalluvial)  # Para crear gráficos de fluxo (aluviais)
library(ggcorrplot)  # Para crear matrices de correlación visuales

# ==============================================================================
# 2. FUNCIÓNS E VARIABLES GLOBAIS
# ==============================================================================
# Aquí defínense elementos que se van utilizar repetidamente ao longo de todo 
# o código, como a paleta de cores para as preguntas tipo Likert (de 1 a 7) ou
# funcións para limpar os textos, evitando así reescribir código.
# ==============================================================================

# Función para limpar os textos das gráficas (elimina números ao final e parénteses)
limpar_etiquetas <- function(texto) {
  texto <- str_remove(texto, "\\d+$")
  texto <- str_remove(texto, "\\s*\\(.*\\)")
  return(str_trim(texto))
}

# Paleta de cores estándar para escalas Likert (diverxente: do vermello ao verde)
cores_likert <- c("1" = "#D73027", "2" = "#FC8D59", "3" = "#FEE08B", 
                  "4" = "#E0E0E0", 
                  "5" = "#D9EF8B", "6" = "#91CF60", "7" = "#1A9850")

# Crear o cartafol onde se gardarán os gráficos xerados (se non existe previamente)
if(!dir.exists("Graficos_VR_TFM")) {
  dir.create("Graficos_VR_TFM")
}

# ==============================================================================
# 3. CARGA E LIMPEZA INICIAL DOS DATOS
# ==============================================================================
# Nesta sección lense os datos orixinais dende o arquivo CSV descargado dos
# resultados do cuestionario. Ademais, elimínanse aquelas columnas de datos e 
# metadatos internos que non teñen relevancia para o estudo (horas de inicio, 
# puntos, etc.) e aságnanse nomes curtos ás columnas fundamentais.
# ==============================================================================

ficheiro <- "Cuestionario.csv"

# Cargar os datos respectando os acentos e espazos (check.names = FALSE)
datos_brutos <- read.csv(ficheiro, encoding = "UTF-8", check.names = FALSE)

# Limpeza e descarte de columnas innecesarias
datos <- datos_brutos %>%
  select(-contains("Puntos:"), -contains("Comentarios:")) %>%
  select(-c(1:8))

# Renomeado de variables principais segundo a súa posición para un uso máis doado
names(datos)[2] <- "xenero"
names(datos)[3] <- "ano_nacemento"
names(datos)[4] <- "grao"
names(datos)[6] <- "conecia_ivr"
names(datos)[7] <- "empregou_ivr"
names(datos)[8] <- "tipo_experiencia"
names(datos)[102] <- "orde_factores"

# Rexistro do número total de participantes (usarase nos cálculos de porcentaxes)
total_participantes <- nrow(datos)


# ==============================================================================
# 4. ANÁLISE SOCIODEMOGRÁFICA
# ==============================================================================
# Xéranse os primeiros gráficos para ilustrar o perfil das persoas enquisadas: 
# o xénero, o ano de nacemento e os estudos previos (grao universitario).
# Finalmente, combínanse os tres gráficos nunha única presentación visual.
# ==============================================================================

# 4.1 Xénero (Gráfico de barras simple)
grafico_xenero <- datos %>%
  drop_na(xenero) %>%
  count(xenero) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ggplot(aes(x = xenero, y = n, fill = xenero)) +
  geom_col(color = "black", alpha = 0.8, show.legend = FALSE) +
  geom_text(aes(label = paste0(round(pct, 1), "%")), vjust = -0.5, fontface = "bold") +
  scale_fill_manual(values = c("#FF9999", "#99CCFF", "#99FF99")) +
  labs(title = "Xénero dos participantes", x = "", y = "Nº participantes") +
  theme_minimal() +
  scale_y_continuous(breaks = function(x) seq(0, ceiling(x[2]), by = 2), 
                     expand = expansion(mult = c(0, 0.2)))

ggsave("Graficos_VR_TFM/01_Xenero.png", plot = grafico_xenero, width = 6, height = 5, bg = "white")

# 4.2 Ano de nacemento (Histograma)
grafico_ano <- ggplot(datos, aes(x = ano_nacemento)) +
  geom_histogram(binwidth = 1, fill = "#F39C12", color = "black", alpha = 0.8) +
  labs(title = "Ano de Nacemento", x = "Ano", y = "Nº participantes") +
  theme_minimal()

ggsave("Graficos_VR_TFM/02_Ano_Nacemento.png", plot = grafico_ano, width = 6, height = 5, bg = "white")

# 4.3 Grao Estudado (Gráfico de sectores)
grafico_grao <- datos %>%
  select(grao) %>%
  drop_na() %>%
  separate_rows(grao, sep = ";") %>%
  mutate(grao = str_trim(grao)) %>% 
  filter(grao != "") %>%
  count(grao) %>%
  mutate(
    pct = n / sum(n) * 100,
    etiqueta_pct = paste0(round(pct, 1), "%"),
    etiqueta_num = paste0("(", n, ")")
  ) %>%
  ggplot(aes(x = "", y = n, fill = grao)) +
  geom_bar(stat = "identity", width = 1, color = "white", alpha = 0.9) +
  coord_polar(theta = "y", start = 0) +
  geom_text(aes(label = etiqueta_pct), position = position_stack(vjust = 0.5), 
            vjust = -0.2, fontface = "bold", size = 4, color = "black") +
  geom_text(aes(label = etiqueta_num), position = position_stack(vjust = 0.5), 
            vjust = 1.2, fontface = "bold", size = 4, color = "black") +
  scale_fill_brewer(palette = "Set2") + 
  labs(title = "Titulación de orixe", fill = "Grao estudado",
       caption = "*A suma total é igual a 15 por un caso de dobre grao.") +
  theme_void() + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold", margin = margin(b = 15)),
        plot.caption = element_text(color = "black", face = "italic", hjust = 0.5, size = 10, margin = margin(t = 10)),
        legend.position = "right")

ggsave("Graficos_VR_TFM/03_Grao_Estudado.png", plot = grafico_grao, width = 7, height = 4, bg = "white")

# 4.4 Combinación dos gráficos sociodemográficos
figura_combinada <- (grafico_xenero | grafico_ano) | grafico_grao
figura_combinada <- figura_combinada + 
  plot_annotation(title = "Resumo Sociodemográfico e Formativo",
                  theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 10)))

ggsave("Graficos_VR_TFM/00_Sociodemografico_Combinado.png", plot = figura_combinada, width = 10, height = 8, dpi = 300, bg = "white")


# ==============================================================================
# 5. COÑECEMENTO PREVIO E HABILIDADES
# ==============================================================================
# Avalíase o contacto previo da mostra coa tecnoloxía de realidade virtual (VR) 
# e estúdase a súa familiaridade con ferramentas dixitais, videoxogos e o grao 
# de habilidade percibido no seu manexo antes de realizar a sesión.
# ==============================================================================

# 5.1 Coñecía e Empregou VR (Barras agrupadas)
datos_coñecemento <- datos %>%
  select(Coñecía = conecia_ivr, Empregou = empregou_ivr) %>%
  pivot_longer(cols = everything(), names_to = "Pregunta", values_to = "Resposta") %>%
  drop_na() %>%
  count(Pregunta, Resposta)

grafico_conecemento <- ggplot(datos_coñecemento, aes(x = Pregunta, y = n, fill = Resposta)) +
  geom_col(position = "dodge", color = "black", alpha = 0.85) +
  geom_text(aes(label = n), position = position_dodge(width = 0.9), vjust = -0.5) +
  scale_fill_manual(values = c("Si" = "#2ECC71", "Non" = "#E74C3C")) +
  labs(title = "Coñecemento e uso previo da IVR", x = "", y = "Nº participantes") +
  theme_minimal()

ggsave("Graficos_VR_TFM/04_Conecemento_Previo.png", plot = grafico_conecemento, width = 7, height = 5, bg = "white")

# 5.2 Tipo de experiencias con VR
grafico_experiencias <- datos %>%
  select(tipo_experiencia) %>%
  drop_na() %>%
  separate_rows(tipo_experiencia, sep = ";") %>%
  mutate(tipo_experiencia = str_trim(tipo_experiencia)) %>%
  filter(tipo_experiencia != "") %>%
  count(tipo_experiencia) %>%
  mutate(pct = n / sum(n) * 100,
         etiqueta_pct = paste0(round(pct, 1), "%"),
         etiqueta_num = paste0("(", n, ")")) %>%
  ggplot(aes(x = "", y = n, fill = tipo_experiencia)) +
  geom_bar(stat = "identity", width = 1, color = "white", alpha = 0.9) +
  coord_polar(theta = "y", start = 0) +
  geom_text(aes(label = etiqueta_pct), position = position_stack(vjust = 0.5), vjust = -0.2, fontface = "bold", size = 4, color = "black") +
  geom_text(aes(label = etiqueta_num), position = position_stack(vjust = 0.5), vjust = 1.2, fontface = "bold", size = 4, color = "black") +
  scale_fill_brewer(palette = "Set3") + 
  labs(title = "Ámbitos de uso previo da Realidade Virtual", fill = "Tipo de experiencia",
       caption = "Nota: as persoas participante podían escoller máis dunha opción.") +
  theme_void() + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold", margin = margin(b = 15)),
        plot.caption = element_text(color = "black", face = "italic", hjust = 0.5, size = 10, margin = margin(t = 10)),
        legend.position = "right")

ggsave("Graficos_VR_TFM/04b_Experiencias_VR.png", plot = grafico_experiencias, width = 7, height = 5, bg = "white")

figura_previo_combinada <-  grafico_conecemento | grafico_experiencias 
ggsave("Graficos_VR_TFM/00_Conecemento_Experiencias.png", plot = figura_previo_combinada, width = 12, height = 10, dpi = 300, bg = "white")

# 5.3 Habilidade Previa Percibida (Gráfico Diverxente Likert)
orde_habilidades <- c("Control táctiles (smartphone, tablet...)", 
                      "Controladores / mandos de videoxogos", 
                      "Control por movemento (Wii, Kinect...)", 
                      "Visores de Realidade Virtual")

datos_habilidade <- datos %>%
  select("Control táctiles (smartphone, tablet...)" = 13,
         "Controladores / mandos de videoxogos" = 14,
         "Control por movemento (Wii, Kinect...)" = 15,
         "Visores de Realidade Virtual" = 16) %>%
  pivot_longer(everything(), names_to = "Habilidade", values_to = "Puntuacion") %>%
  drop_na() %>%
  mutate(Puntuacion = factor(Puntuacion, levels = as.character(1:7)),
         Habilidade = fct_rev(factor(Habilidade, levels = orde_habilidades))) %>%
  count(Habilidade, Puntuacion, .drop = FALSE) %>%
  group_by(Habilidade) %>% mutate(pct = n / sum(n) * 100) %>% ungroup()

datos_esq <- datos_habilidade %>% filter(Puntuacion %in% c("1", "2", "3", "4")) %>% mutate(pct_plot = ifelse(Puntuacion == "4", -pct / 2, -pct)) %>% filter(pct_plot != 0)
datos_der <- datos_habilidade %>% filter(Puntuacion %in% c("4", "5", "6", "7")) %>% mutate(pct_plot = ifelse(Puntuacion == "4", pct / 2, pct)) %>% filter(pct_plot != 0)

grafico_hab_diverxente <- ggplot() +
  geom_bar(data = datos_esq, aes(x = Habilidade, y = pct_plot, fill = Puntuacion), stat = "identity", color = "white", position = position_stack(reverse = F)) + 
  geom_bar(data = datos_der, aes(x = Habilidade, y = pct_plot, fill = Puntuacion), stat = "identity", color = "white", position = position_stack(reverse = T)) + 
  coord_flip() + 
  geom_hline(yintercept = 0, color = "black", linewidth = 0.8) + 
  scale_fill_manual(values = cores_likert) +
  scale_y_continuous(labels = function(x) paste0(abs(x), "%"), limits = c(-100, 100), breaks = seq(-100, 100, by = 25)) +
  labs(title = "Habilidade previa percibida (Escala 1-7)",
       subtitle = "Esquerda: Menor habilidade (1-3) | Centro: (4) | Dereita: Maior habilidade (5-7)",
       x = "", y = "Porcentaxe de participantes (%)", fill = "Escala (1-7)") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, color = "gray40", size = 10, margin = margin(b = 10)),
        panel.grid.major.y = element_blank(),
        axis.text.y = element_text(size = 14, face = "bold")) + # TAMAÑO AUMENTADO AQUÍ
  guides(fill = guide_legend(nrow = 1))

ggsave("Graficos_VR_TFM/05_Habilidade_Previa_Diverxente.png", plot = grafico_hab_diverxente, width = 11, height = 5, bg = "white")

# 5.4 Frecuencia de uso (Pregunta 9)
niveis_frecuencia <- c("Nunca ou case nunca", "De xeito puntual / algunha vez no ano", "Unha ou varias veces ao mes", "Unha ou varias veces á semana", "Diariamente")
orde_ferramentas <- c("Videoxogos (móbil, consola, PC)", "Controladores / mandos", "Realidade Virtual Inmersiva", "Noticias sobre tecnoloxía")

datos_frecuencia <- datos %>%
  select("Videoxogos (móbil, consola, PC)" = 9, "Controladores / mandos" = 10, "Realidade Virtual Inmersiva" = 11, "Noticias sobre tecnoloxía" = 12) %>%
  pivot_longer(everything(), names_to = "Ferramenta", values_to = "Frecuencia") %>%
  drop_na() %>%
  mutate(Frecuencia = factor(Frecuencia, levels = niveis_frecuencia),
         Ferramenta = fct_rev(factor(Ferramenta, levels = orde_ferramentas))) %>%
  count(Ferramenta, Frecuencia, .drop = FALSE) %>%
  group_by(Ferramenta) %>% mutate(pct = n / sum(n) * 100) %>% ungroup()

grafico_frecuencia <- ggplot(datos_frecuencia, aes(x = Ferramenta, y = pct, fill = Frecuencia)) +
  geom_col(color = "black", alpha = 0.9) +
  coord_flip() +
  scale_fill_brewer(palette = "Blues") + 
  geom_text(aes(label = ifelse(pct > 0, paste0(round(pct, 0), "%"), "")), position = position_stack(vjust = 0.5), size = 4, fontface = "bold") +
  labs(title = "Frecuencia de uso de servizos e ferramentas dixitais", x = "", y = "Porcentaxe de participantes (%)", fill = "Frecuencia de uso") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, margin = margin(b = 15)),
        legend.position = "bottom", legend.text = element_text(size = 9),
        axis.text.y = element_text(size = 14, face = "bold"), # TAMAÑO AUMENTADO AQUÍ
        panel.grid.major.y = element_blank()) +
  guides(fill = guide_legend(reverse = FALSE, nrow = 2, byrow = TRUE))

ggsave("Graficos_VR_TFM/04c_Frecuencia_Uso_Tecnoloxias.png", plot = grafico_frecuencia, width = 11, height = 6, dpi = 300, bg = "white")


# ==============================================================================
# 6. EXPERIENCIA DURANTE A SESIÓN
# ==============================================================================
# Analízase o grao de dificultade que os usuarios percibiron á hora de 
# completar diferentes tarefas co visor de Realidade Virtual, empregando un 
# gráfico de barras apilado dende o centro para ilustrar a avaliación.
# ==============================================================================

nomes_actividades <- c(
  "Construír un triángulo (ou calquera polígono)", "Debuxo", "Editar obxectos", "Agarrar e mover obxectos",
  "Construír un prisma por estrusión", "Construír unha pirámide por estrusión", "Colorear obxectos",
  "Copiar e pegar un obxecto", "Crear un novo obxecto a escala", "Construción de paralelas",
  "Construción de perpendiculares", "Construción dun paralelepípedo/ortoedro", "Achar o punto medio dun segmento",
  "Achar o baricentro dun polígono", "Crear unha pirámide empregando o baricentro"
)

datos_actividades_likert <- datos %>%
  select(any_of(nomes_actividades)) %>% 
  pivot_longer(everything(), names_to = "Actividade", values_to = "Puntuacion") %>%
  drop_na() %>%
  mutate(Puntuacion = factor(Puntuacion, levels = as.character(1:7)),
         Actividade = fct_rev(factor(Actividade, levels = nomes_actividades))) %>%
  count(Actividade, Puntuacion, .drop = FALSE) %>%
  mutate(pct = n / total_participantes * 100) %>% ungroup()

datos_esq <- datos_actividades_likert %>% filter(Puntuacion %in% c("1", "2", "3", "4")) %>% mutate(pct_plot = ifelse(Puntuacion == "4", -pct / 2, -pct)) %>% filter(pct_plot != 0)
datos_der <- datos_actividades_likert %>% filter(Puntuacion %in% c("4", "5", "6", "7")) %>% mutate(pct_plot = ifelse(Puntuacion == "4", pct / 2, pct)) %>% filter(pct_plot != 0)

grafico_dificultade_act <- ggplot() +
  geom_bar(data = datos_esq, aes(x = Actividade, y = pct_plot, fill = Puntuacion), stat = "identity", color = "white", linewidth = 1.2, alpha = 0.95, position = position_stack(reverse = FALSE)) + 
  geom_bar(data = datos_der, aes(x = Actividade, y = pct_plot, fill = Puntuacion), stat = "identity", color = "white", linewidth = 1.2, alpha = 0.95, position = position_stack(reverse = TRUE)) + 
  coord_flip() + geom_hline(yintercept = 0, color = "gray80", linewidth = 0.5) + 
  scale_fill_manual(values = cores_likert) +
  scale_y_continuous(labels = function(x) paste0(abs(x), "%"), limits = c(-100, 100), breaks = seq(-100, 100, by = 25)) +
  labs(title = "Facilidade percibida das actividades realizadas",
       subtitle = "Esquerda: Alta dificultade (1-3) | Centro: (4) | Dereita: Completado sen dificultade (5-7)",
       x = "", y = "Porcentaxe de respostas sobre o total do grupo", fill = "Valoración (1 a 7)") +
  theme_minimal() +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5, color = "gray40", size = 10, margin = margin(b = 10)),
        panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(color = "gray90"),
        axis.text.y = element_text(size = 14, face = "bold")) + # TAMAÑO AUMENTADO AQUÍ
  guides(fill = guide_legend(nrow = 1, reverse = FALSE))

ggsave("Graficos_VR_TFM/06_Dificultade_Actividades_Likert.png", plot = grafico_dificultade_act, width = 12, height = 8, bg = "white")


# ==============================================================================
# 7. EVOLUCIÓN PRE VS POST: DIFICULTADE PROPIA E ALUMNADO
# ==============================================================================
# Estes gráficos constrastan o que pensaban os futuros docentes antes da sesión
# fronte ao que opinan logo de probar a tecnoloxía, avaliando tanto a súa 
# propia dificultade no uso (P11 vs P17) como a que experimentaría o alumnado.
# Emprégase transparencia para distinguir visualmente as dúas avaliacións.
# ==============================================================================

cols_pre <- c("Uso dos controladores/mandos2", "Control por movemento2", "Interfaz e menú2", "Seleccionar elementos con precisión2", "Movemento no espazo tridimensional2", "Creación de elementos xeométricos (puntos, polígonos...)2", "Manipulación de obxectos (edición, agarre)2", "Escalado de obxectos xeométricos2", "Debuxo mediante o control por movemento2")
cols_post <- c("Uso dos controladores", "Control por movemento3", "Interfaz e menú3", "Seleccionar elementos con precisión3", "Movemento no espazo tridimensional3", "Creación de elementos xeométricos (puntos, polígonos...)3", "Manipulación de obxectos (edición, agarre)3", "Escalado de obxectos xeométricos3", "Debuxo mediante o control por movemento3")
nomes_etiquetas <- c("Uso dos controladores", "Control por movemento", "Interfaz e menú", "Seleccionar elementos con precisión", "Movemento 3D", "Creación de elementos xeométricos", "Manipulación de obxectos", "Escalado de obxectos", "Debuxo mediante movemento")

datos_pre <- datos %>% select(all_of(cols_pre)) %>% rename_with(~ nomes_etiquetas, everything()) %>% pivot_longer(everything(), names_to = "Actividade", values_to = "Puntuacion") %>% drop_na() %>% mutate(Momento = "1. Pre-sesión")
datos_post <- datos %>% select(all_of(cols_post)) %>% rename_with(~ nomes_etiquetas, everything()) %>% pivot_longer(everything(), names_to = "Actividade", values_to = "Puntuacion") %>% drop_na() %>% mutate(Momento = "2. Post-sesión")

datos_likert_evolucion <- bind_rows(datos_pre, datos_post) %>%
  mutate(Puntuacion = factor(Puntuacion, levels = as.character(1:7)),
         Actividade = factor(Actividade, levels = nomes_etiquetas),
         Momento = fct_rev(factor(Momento, levels = c("1. Pre-sesión", "2. Post-sesión")))) %>%
  count(Actividade, Momento, Puntuacion, .drop = FALSE) %>% mutate(pct = n / total_participantes * 100) %>% ungroup()

datos_esq <- datos_likert_evolucion %>% filter(Puntuacion %in% c("1", "2", "3", "4")) %>% mutate(pct_plot = ifelse(Puntuacion == "4", -pct / 2, -pct)) %>% filter(pct_plot != 0)
datos_der <- datos_likert_evolucion %>% filter(Puntuacion %in% c("4", "5", "6", "7")) %>% mutate(pct_plot = ifelse(Puntuacion == "4", pct / 2, pct)) %>% filter(pct_plot != 0)

grafico_evolucion <- ggplot() +
  geom_bar(data = datos_esq, aes(x = Momento, y = pct_plot, fill = Puntuacion, alpha = Momento), stat = "identity", color = "white", linewidth = 0.5, position = position_stack(reverse = FALSE)) + 
  geom_bar(data = datos_der, aes(x = Momento, y = pct_plot, fill = Puntuacion, alpha = Momento), stat = "identity", color = "white", linewidth = 0.5, position = position_stack(reverse = TRUE)) + 
  coord_flip() + geom_hline(yintercept = 0, color = "black", linewidth = 0.8) + 
  facet_grid(Actividade ~ ., switch = "y") +
  scale_fill_manual(values = cores_likert) +
  scale_alpha_manual(values = c("1. Pre-sesión" = 0.45, "2. Post-sesión" = 1)) +
  scale_y_continuous(labels = function(x) paste0(abs(x), "%"), limits = c(-100, 100), breaks = seq(-100, 100, by = 25)) +
  labs(title = "Evolución da autopercepción da dificultade", subtitle = "Cores claras: Avaliación Pre-sesión | Cores sólidas: Avaliación Post-sesión", x = "", y = "Porcentaxe de respostas sobre o total do grupo", fill = "Valoración Likert (1 a 7)", alpha = "Momento") +
  theme_minimal() +
  theme(legend.position = "bottom", legend.box = "vertical", plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5, color = "gray40", size = 10, margin = margin(b = 10)),
        panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(color = "gray90"),
        strip.text.y.left = element_text(angle = 0, face = "bold", size = 14, hjust = 1), # TAMAÑO AUMENTADO AQUÍ
        strip.placement = "outside", axis.text.y = element_blank(), panel.spacing = unit(0.5, "lines")) +
  guides(fill = guide_legend(nrow = 1, reverse = FALSE, order = 1), alpha = guide_legend(nrow = 1, reverse = TRUE, order = 2))

ggsave("Graficos_VR_TFM/07_Evolucion_PrePost_DobreLikert.png", plot = grafico_evolucion, width = 12, height = 9, bg = "white")

# 7.2 Evolución Dificultade Alumnado (P12 vs P18)
cols_post_alumnado <- c("Uso dos controladores2", "Control por movemento4", "Interfaz e menú4", "Seleccionar elementos con precisión4", "Movemento no espazo tridimensional4", "Creación de elementos xeométricos (puntos, polígonos...)4", "Manipulación de obxectos (edición, agarre)4", "Escalado de obxectos xeométricos4", "Debuxo mediante o control por movemento4")

datos_pre_alum <- datos %>% select(all_of(cols_pre)) %>% rename_with(~ nomes_etiquetas, everything()) %>% pivot_longer(everything(), names_to = "Mecanica", values_to = "Puntuacion") %>% drop_na() %>% mutate(Momento = "1. Pre-sesión")
datos_post_alum <- datos %>% select(all_of(cols_post_alumnado)) %>% rename_with(~ nomes_etiquetas, everything()) %>% pivot_longer(everything(), names_to = "Mecanica", values_to = "Puntuacion") %>% drop_na() %>% mutate(Momento = "2. Post-sesión")

datos_likert_alum <- bind_rows(datos_pre_alum, datos_post_alum) %>%
  mutate(Puntuacion = factor(Puntuacion, levels = as.character(1:7)),
         Mecanica = factor(Mecanica, levels = nomes_etiquetas),
         Momento = fct_rev(factor(Momento, levels = c("1. Pre-sesión", "2. Post-sesión")))) %>%
  count(Mecanica, Momento, Puntuacion, .drop = FALSE) %>% mutate(pct = n / total_participantes * 100) %>% ungroup()

datos_esq_alum <- datos_likert_alum %>% filter(Puntuacion %in% c("1", "2", "3", "4")) %>% mutate(pct_plot = ifelse(Puntuacion == "4", -pct / 2, -pct)) %>% filter(pct_plot != 0)
datos_der_alum <- datos_likert_alum %>% filter(Puntuacion %in% c("4", "5", "6", "7")) %>% mutate(pct_plot = ifelse(Puntuacion == "4", pct / 2, pct)) %>% filter(pct_plot != 0)

grafico_evolucion_alum <- ggplot() +
  geom_bar(data = datos_esq_alum, aes(x = Momento, y = pct_plot, fill = Puntuacion, alpha = Momento), stat = "identity", color = "white", linewidth = 0.5, position = position_stack(reverse = FALSE)) + 
  geom_bar(data = datos_der_alum, aes(x = Momento, y = pct_plot, fill = Puntuacion, alpha = Momento), stat = "identity", color = "white", linewidth = 0.5, position = position_stack(reverse = TRUE)) + 
  coord_flip() + geom_hline(yintercept = 0, color = "black", linewidth = 0.8) + 
  facet_grid(Mecanica ~ ., switch = "y") +
  scale_fill_manual(values = cores_likert) + scale_alpha_manual(values = c("1. Pre-sesión" = 0.45, "2. Post-sesión" = 1)) +
  scale_y_continuous(labels = function(x) paste0(abs(x), "%"), limits = c(-100, 100), breaks = seq(-100, 100, by = 25)) +
  labs(title = "Evolución da dificultade percibida para o alumnado", subtitle = "Cores claras: Expectativa Pre-sesión | Cores sólidas: Avaliación Post-sesión", x = "", y = "Porcentaxe de respostas sobre o total do grupo", fill = "Valoración Likert (1 a 7)", alpha = "Momento") +
  theme_minimal() +
  theme(legend.position = "bottom", legend.box = "vertical", plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5, color = "gray40", size = 10, margin = margin(b = 10)),
        panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(color = "gray90"),
        strip.text.y.left = element_text(angle = 0, face = "bold", size = 14, hjust = 1), # TAMAÑO AUMENTADO AQUÍ
        strip.placement = "outside", axis.text.y = element_blank(), panel.spacing = unit(0.5, "lines")) +
  guides(fill = guide_legend(nrow = 1, reverse = FALSE, order = 1), alpha = guide_legend(nrow = 1, reverse = TRUE, order = 2))

ggsave("Graficos_VR_TFM/09_Evolucion_Alumnado_DobreLikert.png", plot = grafico_evolucion_alum, width = 12, height = 9, bg = "white")


# ==============================================================================
# 8. APTITUDE DOCENTE PERCIBIDA E SENTIDOS MATEMÁTICOS
# ==============================================================================
# Aquí mídese a confianza do futuro docente na súa capacidade para implementar
# de forma efectiva estas ferramentas no ensino (manexo de hardware/software,
# deseño de aulas, etc.). Ademais, visualízase en que sentidos matemáticos 
# consideran que podería e debería aplicarse a IVR.
# ==============================================================================

# 8.1 Aptitude Docente
cols_aptitude <- c("Ensinar ao alumnado a empregar o hardware propio da IVR.", "Ensinar ao alumnado a manexar o software (NeoTrie VR neste caso).", "Investigar sobre actividades xa deseñadas en NeoTrie VR", "Importar, cargar e executar actividades xa existentes en NeoTrie VR", "Deseñar conceptualmente actividades novas para realizar en NeoTrie VR", "Deseñar tecnolóxicamente actividades novas para realizar en NeoTrie VR.", "Organizar a aula durante as sesións onde se aplicaría esta ferramenta.")
nomes_etiquetas_aptitude <- c("Ensinar ao alumnado a usar o hardware", "Ensinar ao alumnado a usar o software", "Investigar actividades xa deseñadas", "Importar e executar actividades existentes", "Deseñar conceptualmente novas actividades", "Deseñar tecnoloxicamente novas actividades", "Organizar a aula durante as sesións")

datos_aptitude <- datos %>%
  select(all_of(cols_aptitude)) %>% rename_with(~ nomes_etiquetas_aptitude, everything()) %>% pivot_longer(everything(), names_to = "Aptitude", values_to = "Puntuacion") %>% drop_na() %>%
  mutate(Puntuacion = factor(Puntuacion, levels = as.character(1:7)), Aptitude = fct_rev(factor(Aptitude, levels = nomes_etiquetas_aptitude))) %>%
  count(Aptitude, Puntuacion, .drop = FALSE) %>% mutate(pct = n / total_participantes * 100) %>% ungroup()

datos_esq_aptitude <- datos_aptitude %>% filter(Puntuacion %in% c("1", "2", "3", "4")) %>% mutate(pct_plot = ifelse(Puntuacion == "4", -pct / 2, -pct)) %>% filter(pct_plot != 0)
datos_der_aptitude <- datos_aptitude %>% filter(Puntuacion %in% c("4", "5", "6", "7")) %>% mutate(pct_plot = ifelse(Puntuacion == "4", pct / 2, pct)) %>% filter(pct_plot != 0)

grafico_aptitude <- ggplot() +
  geom_bar(data = datos_esq_aptitude, aes(x = Aptitude, y = pct_plot, fill = Puntuacion), stat = "identity", color = "white", linewidth = 0.8, alpha = 0.95, position = position_stack(reverse = FALSE)) + 
  geom_bar(data = datos_der_aptitude, aes(x = Aptitude, y = pct_plot, fill = Puntuacion), stat = "identity", color = "white", linewidth = 0.8, alpha = 0.95, position = position_stack(reverse = TRUE)) + 
  coord_flip() + geom_hline(yintercept = 0, color = "gray60", linewidth = 0.8) + 
  scale_fill_manual(values = cores_likert) +
  scale_y_continuous(labels = function(x) paste0(abs(x), "%"), limits = c(-100, 100), breaks = seq(-100, 100, by = 25)) +
  labs(title = "Nivel de aptitude percibida polo futuro docente", subtitle = "Esquerda: Baixa capacidade (1-3) | Centro: (4) | Dereita: Alta capacidade (5-7)", x = "", y = "Porcentaxe sobre o total de participantes", fill = "Valoración (1 a 7)") +
  theme_minimal() +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5, color = "gray40", size = 10, margin = margin(b = 10)),
        panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(color = "gray90", linetype = "dashed"),
        axis.text.y = element_text(size = 14, face = "bold")) + # TAMAÑO AUMENTADO AQUÍ
  guides(fill = guide_legend(nrow = 1, reverse = FALSE))

ggsave("Graficos_VR_TFM/10_Aptitude_Docente_Likert.png", plot = grafico_aptitude, width = 11, height = 6, bg = "white")

# 8.2 Sentidos Matemáticos (P20 vs P21)
orde_orixinal <- c("Sentido numérico", "Sentido da medida", "Sentido espacial", "Sentido alxébrico", "Sentido estocástico", "Sentido Socioafectivo")
nomes_curtos <- c("numérico", "da medida", "espacial", "alxébrico", "estocástico", "socioafectivo")

datos_q20 <- datos %>% select(Respostas = contains("sería posible traballar")) %>% drop_na() %>% separate_rows(Respostas, sep = ";") %>% mutate(Respostas = str_trim(Respostas)) %>% filter(Respostas != "") %>% count(Respostas, name = "Frecuencia") %>% mutate(Pregunta = "É posible traballar (P20)")
datos_q21 <- datos %>% select(Respostas = contains("debería incluír o uso")) %>% drop_na() %>% separate_rows(Respostas, sep = ";") %>% mutate(Respostas = str_trim(Respostas)) %>% filter(Respostas != "") %>% count(Respostas, name = "Frecuencia") %>% mutate(Pregunta = "Debería incluírse (P21)")

datos_sentidos <- bind_rows(datos_q20, datos_q21) %>% complete(Respostas = orde_orixinal, Pregunta, fill = list(Frecuencia = 0)) %>% filter(!is.na(Pregunta)) %>% mutate(Respostas = factor(Respostas, levels = orde_orixinal, labels = nomes_curtos), Pregunta = factor(Pregunta, levels = c("É posible traballar (P20)", "Debería incluírse (P21)")))
cores_sentidos <- c("numérico" = "#3A8EDB", "da medida" = "#199A1A", "espacial" = "#9374C3", "alxébrico" = "#27979E", "estocástico" = "#DA0082", "socioafectivo" = "#6582F7")

grafico_sentidos <- ggplot(datos_sentidos, aes(x = Respostas, y = Frecuencia, fill = Respostas, alpha = Pregunta)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7, color = "white") +
  scale_fill_manual(values = cores_sentidos, guide = "none") + scale_alpha_manual(values = c("É posible traballar (P20)" = 0.45, "Debería incluírse (P21)" = 1)) +
  scale_y_continuous(limits = c(0, 15), breaks = seq(0, 15, by = 5)) +
  labs(title = "Sentidos matemáticos e IVR", subtitle = "En cales sería posible incluíla (P20) e en cales se debería (P21)", x = "", y = "Nº respostas", alpha = "", caption = "*Pregunta de resposta múltiple") +
  theme_minimal() +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5, color = "gray40", size = 10, margin = margin(b = 15)),
        plot.caption = element_text(hjust = 0, face = "italic", color = "gray50", size = 9, margin = margin(t = 15)),
        axis.text.x = element_text(angle = 0, face = "bold", size = 11, color = "black"), axis.text.y = element_text(face = "bold", size = 10),
        panel.grid.major.x = element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.y = element_line(color = "gray85", linetype = "solid")) +
  guides(alpha = guide_legend(nrow = 1, reverse = FALSE))

ggsave("Graficos_VR_TFM/11_Sentidos_Matematicos_Cores.png", plot = grafico_sentidos, width = 10, height = 6, bg = "white")


# ==============================================================================
# 9. IDONEIDADE METODOLÓXICA E ORGANIZACIÓN
# ==============================================================================
# Analízase o grao no que as distintas metodoloxías pedagóxicas se ven 
# axeitadas para traballar con IVR, contrastándoo coa predisposición xeral do 
# propio docente a empregar esas metodoloxías.
# ==============================================================================

cols_pre_metod <- c("Clase Maxistral", "Aprendizaxe Baseada en Proxectos (AbP)", "Aprendizaxe Cooperativa", "Flipped Classroom (Clase Invertida)", "Resolución de problemas.", "Aprendizaxe por descubrimento", "Gamificación")
cols_post_metod <- c("Clase Maxistral2", "Aprendizaxe Baseada en Proxectos (AbP)2", "Aprendizaxe Cooperativa2", "Flipped Classroom (Clase Invertida)2", "Resolución de problemas.2", "Aprendizaxe por descubrimento2", "Gamificación2")
nomes_etiquetas_metod <- c("Clase Maxistral", "Aprendizaxe Baseada en Proxectos", "Aprendizaxe Cooperativa", "Flipped Classroom", "Resolución de problemas", "Aprendizaxe por descubrimento", "Gamificación")

datos_pre_metod <- datos %>% select(all_of(cols_pre_metod)) %>% rename_with(~ nomes_etiquetas_metod, everything()) %>% pivot_longer(everything(), names_to = "Metodoloxia", values_to = "Puntuacion") %>% drop_na() %>% mutate(Momento = "Compatibilidade coa IVR (P23)")
datos_post_metod <- datos %>% select(all_of(cols_post_metod)) %>% rename_with(~ nomes_etiquetas_metod, everything()) %>% pivot_longer(everything(), names_to = "Metodoloxia", values_to = "Puntuacion") %>% drop_na() %>% mutate(Momento = "Disposición a aplicalo (P24)")

datos_likert_metod <- bind_rows(datos_pre_metod, datos_post_metod) %>%
  mutate(Puntuacion = factor(Puntuacion, levels = as.character(1:7)), Metodoloxia = factor(Metodoloxia, levels = nomes_etiquetas_metod), Momento = fct_rev(factor(Momento, levels = c("Compatibilidade coa IVR (P23)", "Disposición a aplicalo (P24)")))) %>%
  count(Metodoloxia, Momento, Puntuacion, .drop = FALSE) %>% mutate(pct = n / total_participantes * 100) %>% ungroup()

datos_esq_metod <- datos_likert_metod %>% filter(Puntuacion %in% c("1", "2", "3", "4")) %>% mutate(pct_plot = ifelse(Puntuacion == "4", -pct / 2, -pct)) %>% filter(pct_plot != 0)
datos_der_metod <- datos_likert_metod %>% filter(Puntuacion %in% c("4", "5", "6", "7")) %>% mutate(pct_plot = ifelse(Puntuacion == "4", pct / 2, pct)) %>% filter(pct_plot != 0)

grafico_metodoloxias <- ggplot() +
  geom_bar(data = datos_esq_metod, aes(x = Momento, y = pct_plot, fill = Puntuacion, alpha = Momento), stat = "identity", color = "white", linewidth = 0.5, position = position_stack(reverse = FALSE)) + 
  geom_bar(data = datos_der_metod, aes(x = Momento, y = pct_plot, fill = Puntuacion, alpha = Momento), stat = "identity", color = "white", linewidth = 0.5, position = position_stack(reverse = TRUE)) + 
  coord_flip() + geom_hline(yintercept = 0, color = "black", linewidth = 0.8) + 
  facet_grid(Metodoloxia ~ ., switch = "y") +
  scale_fill_manual(values = cores_likert) + scale_alpha_manual(values = c("Compatibilidade coa IVR (P23)" = 0.45, "Disposición a aplicalo (P24)" = 1)) +
  scale_y_continuous(labels = function(x) paste0(abs(x), "%"), limits = c(-100, 100), breaks = seq(-100, 100, by = 25)) +
  labs(title = "Métodos de ensino", subtitle = "Compatibilidade coa IVR (P23) vs Disposición a aplicar dito método na aula (P24)", x = "", y = "Porcentaxe de respostas sobre o total do grupo", fill = "Valoración Likert (1 a 7)", alpha = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom", legend.box = "vertical", plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5, color = "gray40", size = 10, margin = margin(b = 10)), panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(color = "gray90"),
        strip.text.y.left = element_text(angle = 0, face = "bold", size = 14, hjust = 1), # TAMAÑO AUMENTADO AQUÍ
        strip.placement = "outside", axis.text.y = element_blank(), panel.spacing = unit(0.5, "lines")) +
  guides(fill = guide_legend(nrow = 1, reverse = FALSE, order = 1), alpha = guide_legend(nrow = 1, reverse = TRUE, order = 2))

ggsave("Graficos_VR_TFM/12_Idoneidade_Metodoloxica_Likert.png", plot = grafico_metodoloxias, width = 12, height = 8, bg = "white")


# ==============================================================================
# 10. APLICACIÓN NA AULA (SESIÓNS E ORGANIZACIÓN)
# ==============================================================================
# Visualiza a cantidade de sesións dedicadas á VR por curso, e cal é o formato
# de organización dos equiposVR na aula (docente, grupo reducido, alumno sherpa).
# ==============================================================================

# 10.1 Número de sesións por materia
cols_sesions <- c("1º", "2º", "3º", "4º A - Matemáticas Aplicadas", "4º B - Matemáticas Académicas")
nomes_materias <- c("1º ESO", "2º ESO", "3º ESO", "4º ESO (Aplicadas)", "4º ESO (Académicas)")

datos_sesions <- datos %>% select(all_of(cols_sesions)) %>% rename_with(~ nomes_materias, everything()) %>% pivot_longer(everything(), names_to = "Materia", values_to = "Sesions") %>% drop_na() %>%
  mutate(Sesions = factor(Sesions, levels = c("0", "1", "2", "3", "4", "5", "Máis de 5")), Materia = fct_rev(factor(Materia, levels = nomes_materias))) %>%
  count(Materia, Sesions, .drop = FALSE) %>% group_by(Materia) %>% mutate(pct = n / sum(n) * 100, texto_pct = ifelse(pct >= 5, paste0(round(pct), "%"), "")) %>% ungroup()

cores_sesions <- c("0" = "#F7FBFF", "1" = "#DEEBF7", "2" = "#C6DBEF", "3" = "#9ECAE1", "4" = "#6BAED6", "5" = "#3182BD", "Máis de 5" = "#08519C")

grafico_sesions <- ggplot(datos_sesions, aes(x = pct, y = Materia, fill = Sesions)) +
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE), color = "black", linewidth = 0.5, width = 0.7) +
  geom_text(aes(label = texto_pct), position = position_stack(reverse = TRUE, vjust = 0.5), size = 4, fontface = "bold", color = "black") +
  scale_fill_manual(values = cores_sesions) + scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 25)) +
  labs(title = "Número de sesións que adicarían á IVR por curso", x = "Porcentaxe de respostas (%)", y = "", fill = "Número de sesións") +
  theme_minimal() +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold", hjust = 0.5, size = 14, margin = margin(b = 15)),
        axis.text.y = element_text(face = "bold", size = 14, color = "black"), # TAMAÑO AUMENTADO AQUÍ
        axis.text.x = element_text(size = 10), panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(color = "gray85")) +
  guides(fill = guide_legend(nrow = 1, reverse = FALSE))

ggsave("Graficos_VR_TFM/13_Numero_Sesions_Materia.png", plot = grafico_sesions, width = 11, height = 6, bg = "white")

# 10.2 Organización da aula (Boxplot)
respostas_brutas <- c('10-70-20', '10-60-30', '15-60-25', '20-30-50', '10-60-30', '20-50-30', '50-25-25', '25-75-0', '10-70-20', '10-50-40', '10-70-20', '25-65-10', '50-50-0', '20-70-10')
datos_modos <- data.frame(Respostas = respostas_brutas) %>% separate(Respostas, into = c("Modo_1", "Modo_2", "Modo_3"), sep = "-", convert = TRUE) %>% mutate(ID = row_number()) %>% pivot_longer(cols = starts_with("Modo"), names_to = "Modo_Raw", values_to = "Porcentaxe")
etiquetas_modos <- c("Modo_1" = "Docente co visor", "Modo_2" = "Grupos reducidos", "Modo_3" = "Alumno sherpa")
datos_modos <- datos_modos %>% mutate(Modo_Limpo = factor(Modo_Raw, levels = c("Modo_1", "Modo_2", "Modo_3"), labels = etiquetas_modos))
cores_modos <- c("Docente co visor" = "#FC8D59", "Grupos reducidos" = "#91BFDB", "Alumno sherpa" = "#99D594")

grafico_organizacion <- ggplot(datos_modos, aes(x = Modo_Limpo, y = Porcentaxe, fill = Modo_Limpo)) +
  stat_boxplot(geom = "errorbar", width = 0.2, color = "black", linewidth = 0.6) +
  geom_boxplot(alpha = 0.7, width = 0.5, color = "black", linewidth = 0.6) +
  scale_fill_manual(values = cores_modos) + scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 25), labels = function(x) paste0(x, "%")) +
  labs(title = "Organización da aula e os visores VR", subtitle = "Porcentaxes asignadas a cada modelo", x = "", y = "Porcentaxe de tempo asignado (%)") +
  theme_minimal() +
  theme(legend.position = "none", plot.title = element_text(face = "bold", hjust = 0.5, size = 15),
        plot.subtitle = element_text(hjust = 0.5, color = "gray40", size = 11, margin = margin(b = 15)),
        axis.text.x = element_text(face = "bold", size = 14, color = "black"), axis.text.y = element_text(face = "bold", size = 11, color = "black"),
        axis.title.y = element_text(margin = margin(r = 15)), panel.grid.major.x = element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.y = element_line(color = "gray85", linetype = "dashed"))

ggsave("Graficos_VR_TFM/18_Organizacion_Aula_Boxplot.png", plot = grafico_organizacion, width = 9, height = 7, bg = "white")


# ==============================================================================
# 11. ANÁLISE CUALITATIVA: ASPECTOS POSITIVOS, NEGATIVOS E CONTIDOS
# ==============================================================================
# Resúmense visualmente as respostas libres dadas polos participantes sobre 
# a súa experiencia positiva ou negativa tras o seu paso polas gafas VR, 
# ademais dos contidos teóricos propostos para as clases.
# ==============================================================================

datos_positivos <- data.frame(Tema = c("Visualización 3D e redución da abstracción", "Carácter lúdico, interactivo e motivador", "Manipulación e experimentación directa", "Potencial pedagóxico e innovación", "Funcionamento intuitivo do software"), Frecuencia = c(7, 4, 4, 3, 1)) %>% mutate(Tema = fct_reorder(Tema, Frecuencia))
datos_negativos <- data.frame(Tema = c("Dificultades co desprazamento", "Curva de aprendizaxe dos controis", "Problemas de saúde (Miopía/Mareos)", "Xestión do tempo e loxística", "Deseño estético da interface"), Frecuencia = c(6, 6, 4, 3, 1), Etiqueta_Texto = c("6", "6", "4 (3+1)", "3", "1")) %>% mutate(Tema = fct_reorder(Tema, Frecuencia))

grafico_pos <- ggplot(datos_positivos, aes(x = Frecuencia, y = Tema, alpha = Frecuencia)) +
  geom_bar(stat = "identity", fill = "#1A9850", width = 0.7, color = "#005a32") +
  geom_text(aes(label = Frecuencia), hjust = -0.5, fontface = "bold", size = 4.5, color = "black", alpha = 1) +
  scale_alpha_continuous(range = c(0.35, 1), guide = "none") + scale_x_continuous(limits = c(0, 8.5), breaks = seq(0, 8, by = 2)) +
  labs(title = "Aspectos máis positivos", subtitle = "Pregunta 15", x = "", y = "") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 12, color = "#1A9850"), plot.subtitle = element_text(color = "gray40", size = 10, margin = margin(b = 10)),
        axis.text.y = element_text(face = "bold", size = 14, color = "black"), # TAMAÑO AUMENTADO AQUÍ
        axis.text.x = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.x = element_blank())

grafico_neg <- ggplot(datos_negativos, aes(x = Frecuencia, y = Tema, alpha = Frecuencia)) +
  geom_bar(stat = "identity", fill = "#D73027", width = 0.7, color = "#a50f15") +
  geom_text(aes(label = Etiqueta_Texto), hjust = -0.2, fontface = "bold", size = 4.5, color = "black", alpha = 1) +
  scale_alpha_continuous(range = c(0.35, 1), guide = "none") + scale_x_continuous(limits = c(0, 8.5), breaks = seq(0, 8, by = 2)) +
  labs(title = "Aspectos máis negativos ou limitantes", subtitle = "Pregunta 16", x = "Número de mencións", y = "") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 12, color = "#D73027"), plot.subtitle = element_text(color = "gray40", size = 10, margin = margin(b = 10)),
        axis.text.y = element_text(face = "bold", size = 14, color = "black"), # TAMAÑO AUMENTADO AQUÍ
        panel.grid.major.y = element_blank(), panel.grid.minor.x = element_blank())

grafico_cualitativo_combinado <- grafico_pos / grafico_neg + plot_annotation(title = "Aspectos positivos/negativos detectados durante a sesión", theme = theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15)))
ggsave("Graficos_VR_TFM/14_Analise_Cualitativa_Aberta.png", plot = grafico_cualitativo_combinado, width = 11, height = 7.5, bg = "white")

# Contidos (P22)
datos_contidos <- data.frame(Tema = c("Xeometría tridimensional", "Xeometría plana", "Escalas, semellanza e proporcionalidade", "Cálculo de magnitudes (Áreas, volumes...)", "Xeometría analítica (rectas, planos...)", "Representación gráfica de funcións"), Frecuencia = c(10, 8, 5, 5, 4, 2)) %>% mutate(Tema = fct_reorder(Tema, Frecuencia))

grafico_contidos <- ggplot(datos_contidos, aes(x = Frecuencia, y = Tema, alpha = Frecuencia)) +
  geom_bar(stat = "identity", fill = "#3182BD", width = 0.7, color = "#08519C") +
  geom_text(aes(label = Frecuencia), hjust = -0.5, fontface = "bold", size = 4.5, color = "black", alpha = 1) +
  scale_alpha_continuous(range = c(0.4, 1), guide = "none") + scale_x_continuous(limits = c(0, 11), breaks = seq(0, 10, by = 2)) +
  labs(title = "Propostas de contidos para traballar con IVR", subtitle = "Pregunta 22", x = "Número de mencións", y = "") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14, color = "#08519C", hjust = 0.5), plot.subtitle = element_text(color = "gray40", size = 10, hjust = 0.5, margin = margin(b = 15)),
        axis.text.y = element_text(face = "bold", size = 14, color = "black"), # TAMAÑO AUMENTADO AQUÍ
        panel.grid.major.y = element_blank(), panel.grid.minor.x = element_blank())

ggsave("Graficos_VR_TFM/15_Analise_Cualitativa_Contidos.png", plot = grafico_contidos, width = 11, height = 5, bg = "white")


# ==============================================================================
# 12. ANÁLISE CUALITATIVA: DIFICULTADES E FACTORES DE INFLUENCIA
# ==============================================================================
# Resumo das principais barreiras e retos percividos polos participantes á 
# hora de introducir a VR e a orde de prioridade dos factores limitantes.
# ==============================================================================

# Dificultades da IVR (P25)
datos_dificultades <- data.frame(Tema = c("Escasez e custo do material", "Xestión do tempo e loxística", "Organización da aula e ratios", "Formación do profesorado e soporte técnico", "Atención á diversidade", "Curva de aprendizaxe do alumnado"), Frecuencia = c(8, 7, 6, 5, 4, 4)) %>% mutate(Tema = fct_reorder(Tema, Frecuencia))
grafico_dificultades <- ggplot(datos_dificultades, aes(x = Frecuencia, y = Tema, alpha = Frecuencia)) +
  geom_bar(stat = "identity", fill = "#D73027", width = 0.7, color = "#a50f15") +
  geom_text(aes(label = Frecuencia), hjust = -0.5, fontface = "bold", size = 4.5, color = "black", alpha = 1) +
  scale_alpha_continuous(range = c(0.4, 1), guide = "none") + scale_x_continuous(limits = c(0, 9), breaks = seq(0, 8, by = 2)) +
  labs(title = "Dificultades percibidas para aplicar a IVR na aula", subtitle = "Pregunta 25", x = "Número de mencións", y = "") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14, color = "#D73027", hjust = 0.5), plot.subtitle = element_text(color = "gray40", size = 10, hjust = 0.5, margin = margin(b = 15)),
        axis.text.y = element_text(face = "bold", size = 14, color = "black"), # TAMAÑO AUMENTADO AQUÍ
        panel.grid.major.y = element_blank(), panel.grid.minor.x = element_blank())

ggsave("Graficos_VR_TFM/16_Analise_Cualitativa_Dificultades.png", plot = grafico_dificultades, width = 11, height = 5, bg = "white")

# Factores de influencia (P27)
datos_brutos_factores <- data.frame(Factor = c("Cantidade de visores no centro", "Número de alumnos por clase", "Comportamento do alumnado", "Atención á diversidade", "Tamaño/disposición da aula", "Idade do alumnado", "Currículo do curso"), Rango_1 = c(6, 3, 4, 0, 1, 0, 0), Rango_2 = c(2, 5, 1, 3, 1, 1, 1), Rango_3 = c(2, 2, 2, 3, 1, 4, 0), Rango_4 = c(3, 1, 3, 3, 3, 0, 1), Rango_5 = c(1, 1, 4, 2, 2, 2, 2), Rango_6 = c(0, 2, 0, 2, 4, 4, 2), Rango_7 = c(0, 0, 0, 1, 2, 3, 8))
datos_grafico <- datos_brutos_factores %>% pivot_longer(cols = starts_with("Rango_"), names_to = "Rango", values_to = "Frecuencia") %>%
  mutate(Rango_Limpo = paste0(gsub("Rango_", "", Rango), "º"), Rango_Limpo = factor(Rango_Limpo, levels = paste0(1:7, "º")), pct = Frecuencia / total_participantes * 100, Factor = factor(Factor, levels = rev(c("Cantidade de visores no centro", "Número de alumnos por clase", "Comportamento do alumnado", "Atención á diversidade", "Tamaño/disposición da aula", "Idade do alumnado", "Currículo do curso")))) %>% filter(Frecuencia > 0)
cores_rangos_claras <- c("1º" = "#74C476", "2º" = "#A1D99B", "3º" = "#9ECAE1", "4º" = "#BCBDDC", "5º" = "#FDD0A2", "6º" = "#FAE9AA", "7º" = "#F7F7F7")

grafico_ordenacion <- ggplot(datos_grafico, aes(x = pct, y = Factor, fill = Rango_Limpo)) +
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE), color = "black", linewidth = 0.4, width = 0.7) +
  geom_text(aes(label = ifelse(pct >= 5, paste0(round(pct), "%"), "")), position = position_stack(reverse = TRUE, vjust = 0.5), size = 3.5, fontface = "bold", color = "black") +
  scale_fill_manual(values = cores_rangos_claras) + scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 25)) +
  labs(title = "Factores de influencia na implantación da IVR", subtitle = "Ordenación factores por parte das persoas enquisadas", x = "Porcentaxe de respostas (%)", y = "", fill = "Nivel de prioridade (1º a 7º)") +
  theme_minimal() +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold", hjust = 0.5, size = 14), plot.subtitle = element_text(hjust = 0.5, color = "gray40", size = 10, margin = margin(b = 15)),
        axis.text.y = element_text(face = "bold", size = 14, color = "black"), # TAMAÑO AUMENTADO AQUÍ
        axis.text.x = element_text(size = 10), panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(color = "gray85")) +
  guides(fill = guide_legend(nrow = 1, reverse = FALSE))

ggsave("Graficos_VR_TFM/17_Ordenacion_Factores_Influencia_Claro.png", plot = grafico_ordenacion, width = 12, height = 6, bg = "white")


# ==============================================================================
# 13. FLUXO DE PROPOSTAS E CORRELACIÓNS
# ==============================================================================
# Úsase un Gráfico Aluvial (para a proposta de actividades libres da pregunta 29)
# que amosa a conexión entre Curso, Contido e Actividade. Pechase cun correlograma 
# inferencial de Spearman que busca cruzar diferentes respostas entre si.
# ==============================================================================

# 13.1 Gráfico Aluvial Único (P29)
datos_fluxo <- data.frame(Curso = c("1º ESO", "2º ESO", "1º ESO", "2º ESO", "4º ESO", "2º ESO", "2º BACH", "2º ESO", "3º ESO", "3º ESO", "2º ESO", "1º ESO", "2º ESO", "3º ESO"), Contidos = c("Xeometría\nPlana", "Xeometría\nEspacial", "Xeometría\nMixta\n(2D/3D)", "Xeometría\nPlana", "Xeometría\nAnalítica", "Xeometría\nEspacial", "Xeometría\nAnalítica", "Xeometría\nMixta\n(2D/3D)", "Xeometría\nMixta\n(2D/3D)", "Xeometría\nMixta\n(2D/3D)", "Xeometría\nEspacial", "Xeometría\nPlana", "Xeometría\nEspacial", "Xeometría\nEspacial"), Actividade = c("Visualización\ne Exploración", "Construción\ne Modelado", "Construción\ne Modelado", "Visualización\ne Exploración", "Visualización\ne Exploración", "Traballo\nCooperativo\n/ AbP", "Descubrimento\ne Retos", "Traballo\nCooperativo\n/ AbP", "Visualización\ne Exploración", "Construción\ne Modelado", "Visualización\ne Exploración", "Descubrimento\ne Retos", "Descubrimento\ne Retos", "Traballo\nCooperativo\n/ AbP"))
datos_alluvial <- datos_fluxo %>% group_by(Curso, Contidos, Actividade) %>% summarise(Freq = n(), .groups = 'drop')
datos_alluvial$Curso <- factor(datos_alluvial$Curso, levels = c("1º ESO", "2º ESO", "3º ESO", "4º ESO", "2º BACH"))

grafico_aluvial <- ggplot(datos_alluvial, aes(y = Freq, axis1 = Curso, axis2 = Contidos, axis3 = Actividade)) +
  geom_alluvium(aes(fill = Curso), width = 1/12, color = "darkgray", alpha = 0.7) + geom_stratum(width = 1/6, fill = "#F7F7F7", color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3.2, fontface = "bold") +
  scale_fill_manual(values = c("1º ESO" = "#A1D99B", "2º ESO" = "#9ECAE1", "3º ESO" = "#FDD0A2", "4º ESO" = "#BCBDDC", "2º BACH" = "#FAE9AA")) +
  scale_x_discrete(limits = c("Curso", "Contidos", "Tipo de actividade"), expand = c(.05, .05)) +
  labs(title = "Fluxo das propostas didácticas coa IVR", subtitle = "Relación entre o curso, contido e tipoloxía de actividade (P29)", x = "", y = "") +
  theme_minimal() +
  theme(legend.position = "none", plot.title = element_text(face = "bold", hjust = 0.5, size = 15), plot.subtitle = element_text(hjust = 0.5, color = "gray40", size = 11, margin = margin(b = 15)),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), axis.text.x = element_text(face = "bold", size = 12, color = "black"), panel.grid.major.x = element_blank())

ggsave("Graficos_VR_TFM/19A_Propostas_Aluvial_Axustado.png", plot = grafico_aluvial, width = 11, height = 7, bg = "white")

# 13.2 Correlacións (P19 Deseño Conceptual vs Resto)
col_p19_especifica <- "Deseñar conceptualmente actividades novas para realizar en NeoTrie VR"
cols_p14_filtradas <- c("Construír un triángulo (ou calquera polígono)", "Debuxo", "Editar obxectos", "Agarrar e mover obxectos", "Construír un prisma por estrusión", "Construír unha pirámide por estrusión", "Colorear obxectos", "Copiar e pegar un obxecto", "Crear un novo obxecto a escala", "Construción de paralelas", "Construción de perpendiculares", "Construción dun paralelepípedo/ortoedro", "Achar o punto medio dun segmento", "Achar o baricentro dun polígono", "Crear unha pirámide empregando o baricentro")

datos_indices_especifico <- datos %>%
  mutate(across(c(all_of(col_p19_especifica), all_of(cols_post), any_of(cols_p14_filtradas), all_of(cols_pre), 13:16), as.numeric)) %>%
  rowwise() %>%
  mutate(Aptitude_Deseno_P19 = get(col_p19_especifica),
         Facilidade_Post_P17 = mean(c_across(all_of(cols_post)), na.rm = TRUE),
         Facilidade_Tarefas_P14 = mean(c_across(any_of(cols_p14_filtradas)), na.rm = TRUE),
         Facilidade_Pre_P11 = mean(c_across(all_of(cols_pre)), na.rm = TRUE),
         Habilidade_Tecnoloxica_P10 = mean(c_across(13:16), na.rm = TRUE)) %>%
  ungroup() %>%
  select(Aptitude_Deseno_P19, Facilidade_Post_P17, Facilidade_Tarefas_P14, Facilidade_Pre_P11, Habilidade_Tecnoloxica_P10)
colnames(datos_indices_especifico) <- c("Deseño Conceptual (P19)", "Facilidade Post-sesión (P17)", "Facilidade nas Tarefas (P14)", "Expectativa Pre-sesión (P11)", "Habilidade Tecnolóxica (P10)")

matriz_cor_esp <- cor(datos_indices_especifico, method = "spearman", use = "pairwise.complete.obs")
matriz_p_esp <- cor_pmat(datos_indices_especifico, method = "spearman")

grafico_correlacion_esp <- ggcorrplot(matriz_cor_esp, method = "square", type = "lower", lab = TRUE, lab_size = 4, colors = c("#D73027", "white", "#1A9850"), p.mat = matriz_p_esp, sig.level = 0.05, insig = "pch", pch.col = "black", pch.cex = 6) +
  labs(title = "Correl. entre capacidade de deseño conceptual e outras respostas", subtitle = "Correlación de Spearman (X indica que non hai significación estatística, p > 0.05)", fill = "Coeficiente\nSpearman", x = NULL, y = NULL) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 14), plot.subtitle = element_text(hjust = 0.5, color = "gray40", size = 10, margin = margin(b = 15)), axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", color = "black"), axis.text.y = element_text(face = "bold", color = "black"), panel.grid = element_blank())

ggsave("Graficos_VR_TFM/21_Correlograma_Deseno_Conceptual_Limpio.png", plot = grafico_correlacion_esp, width = 9, height = 8, bg = "white")
