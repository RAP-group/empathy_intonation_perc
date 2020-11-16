# Build 2afc stim trials ------------------------------------------------------
#
# Last update: 2020/11/13
#
# - Create .csv and .xlsx files for the following: 
#    - targets
#    - filler
# - Save files to exp/stim dir
#
# -----------------------------------------------------------------------------


# Source libs and helpers -----------------------------------------------------

source(here::here("scripts", "01_helpers.R"))

# -----------------------------------------------------------------------------




# Build stim data frame -------------------------------------------------------

stim_list <- tribble(
  ~"stim_n",  ~"sentence_type", ~"condition", ~"prompt",  ~"sentence", 
  0,  " ", " ", "Instrucciones", "Por favor lea las siguientes frases.", 
  1,  "match", "declarative_broad_focus", " ", "Daniel iba a Bolivia.", 
  2,  "match", "declarative_broad_focus", " ", "David leía el libro.", 
  3,  "match", "declarative_broad_focus", " ", "Emilio ama la marcha.", 
  4,  "match", "declarative_broad_focus", " ", "Mariano habla del tiempo.", 
  5,  "match", "declarative_broad_focus", " ", "Ana lleva el abrigo.", 
  6,  "match", "declarative_broad_focus", " ", "María bebe el vino.", 
  7,  "match", "declarative_broad_focus", " ", "Marta abre el regalo.", 
  8,  "match", "declarative_broad_focus", " ", "Manuela vende el carro.", 
  9,  "match", "declarative_narrow_focus", "¿A dónde iba Daniel?",     "Daniel iba a Bolivia.", 
  10, "match", "declarative_narrow_focus", "¿Qué leía David?",         "David leía el libro.", 
  11, "match", "declarative_narrow_focus", "¿Qué ama Emilio?",         "Emilio ama la marcha.", 
  12, "match", "declarative_narrow_focus", "¿De qué habla Mariano?",   "Mariano habla del tiempo.", 
  13, "match", "declarative_narrow_focus", "¿Qué lleva Ana?",          "Ana lleva el abrigo.", 
  14, "match", "declarative_narrow_focus", "¿Qué bebe María?",         "María bebe el vino.", 
  15, "match", "declarative_narrow_focus", "¿Qué abre Marta?",         "Marta abre el regalo.", 
  16, "match", "declarative_narrow_focus", "¿Qué vende Manuela?",      "Manuela vende el carro.", 
  17, "match", "interrogative_total_yn", " ", "¿Daniel iba a Bolivia?",
  18, "match", "interrogative_total_yn", " ", "¿David leía el libro?",
  19, "match", "interrogative_total_yn", " ", "¿Emilio ama la marcha?",
  20, "match", "interrogative_total_yn", " ", "¿Mariano habla del tiempo?",
  21, "match", "interrogative_total_yn", " ", "¿Ana lleva el abrigo?",
  22, "match", "interrogative_total_yn", " ", "¿María bebe el vino?",
  23, "match", "interrogative_total_yn", " ", "¿Marta abre el regalo?",
  24, "match", "interrogative_total_yn", " ", "¿Manuela vende el carro?",
  25, "match", "interrogative_partial_wh", " ", "¿Por qué iba a Bolivia?",
  26, "match", "interrogative_partial_wh", " ", "¿Cuándo leía el libro?", 
  27, "match", "interrogative_partial_wh", " ", "¿Por qué ama la navidad?", 
  28, "match", "interrogative_partial_wh", " ", "¿Por qué hablaba del agua?", 
  29, "match", "interrogative_partial_wh", " ", "¿Cuándo lleva el abrigo?", 
  30, "match", "interrogative_partial_wh", " ", "¿Cuándo bebía el vino?", 
  31, "match", "interrogative_partial_wh", " ", "¿Por qué abre el regalo?", 
  32, "match", "interrogative_partial_wh", " ", "¿Cuándo vendía el carro?", 
  33, "mismatch", "declarative_broad_focus", " ", "El hombre mira la luna.", 
  34, "mismatch", "declarative_broad_focus", " ", "La niña lava el plato.", 
  35, "mismatch", "declarative_broad_focus", " ", "Mi madre come la fruta.", 
  36, "mismatch", "declarative_broad_focus", " ", "El niño oye el río.", 
  37, "mismatch", "declarative_broad_focus", " ", "Mi tía odia la lluvia.", 
  38, "mismatch", "declarative_broad_focus", " ", "El bebé comía muy bien.", 
  39, "mismatch", "declarative_broad_focus", " ", "La maestra vive en Paris.", 
  40, "mismatch", "declarative_broad_focus", " ", "Mi novio viene del lago.", 
  41, "mismatch", "declarative_narrow_focus", "¿Qué mira el hombre?",      "El hombre mira la luna.", 
  42, "mismatch", "declarative_narrow_focus", "¿Qué lava la niña?",        "La niña lava el plato.", 
  43, "mismatch", "declarative_narrow_focus", "¿Qué come tu madre?",       "Mi madre come la fruta.", 
  44, "mismatch", "declarative_narrow_focus", "¿Qué oye el niño?",         "El niño oye el río.", 
  45, "mismatch", "declarative_narrow_focus", "¿Qué odia tu tía?",         "Mi tía odia la lluvia.", 
  46, "mismatch", "declarative_narrow_focus", "¿Cómo comía el bebé? ",     "El bebé comía muy bien.", 
  47, "mismatch", "declarative_narrow_focus", "¿Dónde vive la maestra?",   "La maestra vive en Paris.", 
  48, "mismatch", "declarative_narrow_focus", "¿De dónde viene tu novio?", "Mi novio viene del lago.", 
  49, "mismatch", "interrogative_total_yn", " ", "¿El hombre mira la luna?", 
  50, "mismatch", "interrogative_total_yn", " ", "¿La niña lava el plato?", 
  51, "mismatch", "interrogative_total_yn", " ", "¿Mi madre come la fruta?", 
  52, "mismatch", "interrogative_total_yn", " ", "¿El niño oye el río?", 
  53, "mismatch", "interrogative_total_yn", " ", "¿Mi tía odia la lluvia?", 
  54, "mismatch", "interrogative_total_yn", " ", "¿El bebé comía muy bien?", 
  55, "mismatch", "interrogative_total_yn", " ", "¿La maestra vive en Paris?", 
  56, "mismatch", "interrogative_total_yn", " ", "¿Mi novio viene del lago?", 
  57, "mismatch", "interrogative_partial_wh", " ", "¿Cuándo miraba la luna?", 
  58, "mismatch", "interrogative_partial_wh", " ", "¿Cuándo lavaba el plato?", 
  59, "mismatch", "interrogative_partial_wh", " ", "¿Cuándo comía la fruta?", 
  60, "mismatch", "interrogative_partial_wh", " ", "¿Por qué oía el río?", 
  61, "mismatch", "interrogative_partial_wh", " ", "¿Por qué odiaba la lluvia?", 
  62, "mismatch", "interrogative_partial_wh", " ", "¿Por qué desayuna muy bien?", 
  63, "mismatch", "interrogative_partial_wh", " ", "¿Por qué vivía en Paris?", 
  64, "mismatch", "interrogative_partial_wh", " ", "¿Por qué venía del lago?") %>% 
  mutate(color = if_else(prompt == " ", "white", "blue"))

# -----------------------------------------------------------------------------




# Build filler data frame -----------------------------------------------------

filler_list <- tribble(
  ~"stim_n", ~"sentence_type", ~"condition", ~"prompt", ~"sentence", 
  1,  "statement", "A", "filler", "Roberto cocina por la mañana.", 
  2,  "statement", "B", "filler", "Ana practicaba el yoga.", 
  3,  "statement", "A", "filler", "La niña pinta con su amiga.", 
  4,  "statement", "C", "filler", "Las chicas dan una fiesta hoy.", 
  5,  "statement", "A", "filler", "Isabel duerme por muchas horas.", 
  6,  "statement", "B", "filler", "Alberto nunca se afeita.", 
  7,  "statement", "A", "filler", "La madre pasa la aspiradora.", 
  8,  "statement", "C", "filler", "El estudiante odia la tarea.", 
  9,  "statement", "A", "filler", "El equipo practica mucho.", 
  10, "statement", "B", "filler", "Juana barría el piso.", 
  11, "statement", "A", "filler", "La abuela plancha la ropa.", 
  12, "statement", "C", "filler", "Los abuelos compran un espejo.", 
  13, "statement", "A", "filler", "El niño habla con su madre.", 
  14, "statement", "B", "filler", "Hago ejercicio todos los días. ", 
  15, "statement", "A", "filler", "La vista de la casa es bonita.", 
  16, "statement", "B", "filler", "El baño no es muy grande. ", 
  17, "statement", "A", "filler", "Compro un regalo para Navidad.", 
  18, "statement", "C", "filler", "Tu cumpleaños es mañana.", 
  19, "statement", "C", "filler", "Nadamos en la piscina por la tarde.", 
  20, "statement", "A", "filler", "Karina siempre anda en bicicleta.", 
  21, "statement", "B", "filler", "Los niños ganan el partido.", 
  22, "statement", "A", "filler", "Cocinabas por la noche.", 
  23, "statement", "A", "filler", "Dibujabas con tu amiga.", 
  24, "statement", "C", "filler", "El hombre da un paseo. ", 
  25, "statement", "A", "filler", "Compramos en el supermercado.", 
  26, "statement", "B", "filler", "Mateo piensa en las compras.", 
  27, "statement", "A", "filler", "Emilia va al hospital.", 
  28, "statement", "C", "filler", "El hotel tiene muchas habitaciones. ", 
  29, "statement", "A", "filler", "El abrigo es muy barato. ", 
  30, "statement", "B", "filler", "La discoteca es muy buena.", 
  31, "statement", "A", "filler", "La ciudad tiene dos bancos.", 
  32, "statement", "C", "filler", "Luis trabaja todo el día.", 
  33, "question", "C", "filler", "¿Ella compra una mochila?", 
  34, "question", "A", "filler", "¿Mi padre cantaba bien?", 
  35, "question", "B", "filler", "¿La vecina sale muy temprano? ", 
  36, "question", "A", "filler", "¿Mi amiga mira la luna?", 
  37, "question", "B", "filler", "¿Lucía va a la iglesia?", 
  38, "question", "A", "filler", "¿Mi vecino habla mucho?", 
  39, "question", "C", "filler", "¿El estudiante toma apuntes?", 
  40, "question", "A", "filler", "¿La chica abre su libro?", 
  41, "question", "B", "filler", "¿Sandra corre en el parque?", 
  42, "question", "A", "filler", "¿Mi amigo no comía pollo?", 
  43, "question", "C", "filler", "¿Carlos escribe una carta?", 
  44, "question", "A", "filler", "¿Jose visitaba la playa?", 
  45, "question", "B", "filler", "¿Mi hermano cierra la puerta?", 
  46, "question", "A", "filler", "¿El chico escribía una carta?", 
  47, "question", "C", "filler", "¿Mi vecino trae comida?", 
  48, "question", "A", "filler", "¿Carlos trae a sus hijos? ", 
  49, "question", "B", "filler", "¿Jaime tenía una tienda?", 
  50, "question", "A", "filler", "¿Juan quiere un bistec?", 
  51, "question", "A", "filler", "¿Carolina baila flamenco? ", 
  52, "question", "B", "filler", "¿Mi familia cenaba tarde?", 
  53, "question", "C", "filler", "¿Pablo lavaba su coche? ", 
  54, "question", "A", "filler", "¿El equipo gana la competencia?", 
  55, "question", "C", "filler", "¿Rogelio cuida a los niños? ", 
  56, "question", "A", "filler", "¿Maria se peina en la mañana?", 
  57, "question", "B", "filler", "¿Lucía compra el jabón?", 
  58, "question", "A", "filler", "¿A Jaime le gusta su piso?", 
  59, "question", "C", "filler", "¿Isabel puede apagar la luz?", 
  60, "question", "A", "filler", "¿Felipe no tiene contraseña?", 
  61, "question", "B", "filler", "¿El estudiante practica deportes?", 
  62, "question", "A", "filler", "¿Sofia no quiere la revista?", 
  63, "question", "C", "filler", "¿Mi esposo conoce al chico?", 
  64, "question", "A", "filler", "¿Mi hija prefiere los perros?") %>% 
  mutate(color = "white")

# -----------------------------------------------------------------------------




# Save as csv's and xlsx files ------------------------------------------------

write_csv(stim_list, here("exp", "stim", "trials", "stim_list.csv"))
write_xlsx(stim_list, here("exp", "stim", "trials", "stim_list.xlsx"), 
  format_headers = F)

write_csv(filler_list, here("exp", "stim", "trials", "filler_list.csv"))
write_xlsx(filler_list, here("exp", "stim", "trials", "filler_list.xlsx"), 
  format_headers = F)

# -----------------------------------------------------------------------------
