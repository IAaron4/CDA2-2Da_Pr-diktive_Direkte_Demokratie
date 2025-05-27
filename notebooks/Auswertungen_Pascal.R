library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(purrr)
library(broom)

setwd("~/data")

abst <- read_xlsx("/raw/DATASET XLSX 09-02-2025.xlsx")
abst_4 <- abst %>%  filter(d1e1 %in% c(4,6))


abst_4_1 <- abst_4 %>% slice(15:152)
abst_4_2 <- abst_4 %>% slice(1:14)
abst_4_2$datum <- as.Date(abst_4_2$datum, format = "%d.%m.%Y")
abst_4_2$datum <- format(abst_4_2$datum, "%d.%m.%Y")
abst_4_1$datum <- as.Date(as.numeric(abst_4_1$datum), origin = "1899-12-30")
abst_4_1$datum <- format(abst_4_1$datum, "%d.%m.%Y")
abst_4 <- bind_rows(abst_4_1, abst_4_2)

abst_4$`p-fdp` <- as.numeric(abst_4$`p-fdp`)
abst_4$`p-fdp` <- ifelse(abst_4$`p-fdp` > 2, NA, abst_4$`p-fdp`)
abst_4$`p-fdp` <- ifelse(abst_4$`p-fdp`==2, 0, abst_4$`p-fdp`)




abst_4_lm <- abst_4 %>% select(anr, datum, titel_kurz_d, `p-fdp`, contains("annahme"), `volkja-proz`)
abst_4_lm$`ju-annahme` <- ifelse(abst_4_lm$`ju-annahme`==".", NA, abst_4_lm$`ju-annahme`)

abst_4_lm_clean <- abst_4_lm %>%
  mutate(across(ends_with("-annahme"), as.character))

# 2. In Long-Format überführen
abst_long <- abst_4_lm_clean %>%
  pivot_longer(
    cols = ends_with("-annahme"),
    names_to = "kanton",
    values_to = "kt_annahme",
    names_transform = list(kanton = ~ sub("-annahme", "", .x))
  )

# 3. Versuchen, die Werte zu numerisieren (nicht-konvertierbare werden NA)
abst_long <- abst_long %>%
  mutate(annahme = readr::parse_number(annahme))

abst_long$`p-fdp`<- as.numeric(abst_long$`p-fdp`)
abst_long$kt_annahme <- as.numeric(abst_long$kt_annahme)

abst_long$econ_fr <-  abst_long$`p-fdp`*abst_long$kt_annahme

abst_long <- abst_long %>%
  mutate(
    datum = dmy(datum),
    jahr = year(datum),
    
    # 3. Jahrzehnt berechnen
    jahrzehnt = floor(jahr / 10) * 10
  )

### Religionsdatensatz ####

# Verschiedene Jahres-Datensätze einlesen und unbekannte Regionen filtern, 
# damit keine doppelten/unzuweisbaren Einträge entstehen
dat_1850 <- read_xlsx("1850.xlsx", skip=3) %>% filter(!is.na(Regionsname))
dat_1860 <- read_xlsx("1860.xlsx", skip=3) %>% filter(!is.na(Regionsname))
dat_1870 <- read_xlsx("1870.xlsx", skip=3) %>% filter(!is.na(Regionsname))
dat_1880 <- read_xlsx("1880.xlsx", skip=3) %>% filter(!is.na(Regionsname))
dat_1890 <- read_xlsx("1890.xlsx", skip=3) %>% filter(!is.na(Regionsname))
dat_1900 <- read_xlsx("1900.xlsx", skip=3) %>% filter(!is.na(Regionsname))
dat_1910 <- read_xlsx("1910.xlsx", skip=3) %>% filter(!is.na(Regionsname))
dat_1920 <- read_xlsx("1920.xlsx", skip=3) %>% filter(!is.na(Regionsname))
dat_1930 <- read_xlsx("1930.xlsx", skip=3) %>% filter(!is.na(Regionsname))
dat_1940 <- read_xlsx("1940.xlsx", skip=3) %>% filter(!is.na(Regionsname))
dat_1950 <- read_xlsx("1950.xlsx", skip=3) %>% filter(!is.na(Regionsname))
dat_1960 <- read_xlsx("1960.xlsx", skip=3) %>% filter(!is.na(Regionsname))
dat_1970 <- read_xlsx("1970.xlsx", skip=3) %>% filter(!is.na(Regionsname))
dat_1980 <- read_xlsx("1980.xlsx", skip=3) %>% filter(!is.na(Regionsname))
dat_1990 <- read_xlsx("1990.xlsx", skip=3) %>% filter(!is.na(Regionsname))
dat_2000 <- read_xlsx("2000.xlsx", skip=3) %>% filter(!is.na(Regionsname))
dat_2014 <- read_xlsx("2014.xlsx", skip=3) %>% filter(!is.na(Regionsname))

# Die unterschiedlichen Jahresdatensätze zu einem grossen Datensatz zusammenfügen
# Merging-Variablen sind die Regions-ID's (BFS) und Regions-Namen
full_dat <- right_join(dat_1850, dat_1860, by=c("Regions-ID", "Regionsname"))
full_dat <- full_dat %>% rename(y_1850=...3.x) %>% rename(y_1860=...3.y)
full_dat <- right_join(full_dat, dat_1870, by=c("Regions-ID", "Regionsname"))
full_dat <- full_dat %>% rename(y_1870=...3)
full_dat <- right_join(full_dat, dat_1880, by=c("Regions-ID", "Regionsname"))
full_dat <- full_dat %>% rename(y_1880=...3)
full_dat <- right_join(full_dat, dat_1890, by=c("Regions-ID", "Regionsname"))
full_dat <- full_dat %>% rename(y_1890=...3)
full_dat <- right_join(full_dat, dat_1900, by=c("Regions-ID", "Regionsname"))
full_dat <- full_dat %>% rename(y_1900=...3)
full_dat <- right_join(full_dat, dat_1910, by=c("Regions-ID", "Regionsname"))
full_dat <- full_dat %>% rename(y_1910=...3)
full_dat <- right_join(full_dat, dat_1920, by=c("Regions-ID", "Regionsname"))
full_dat <- full_dat %>% rename(y_1920=...3)
full_dat <- right_join(full_dat, dat_1930, by=c("Regions-ID", "Regionsname"))
full_dat <- full_dat %>% rename(y_1930=...3)
full_dat <- right_join(full_dat, dat_1940, by=c("Regions-ID", "Regionsname"))
full_dat <- full_dat %>% rename(y_1940=...3)
full_dat <- right_join(full_dat, dat_1950, by=c("Regions-ID", "Regionsname"))
full_dat <- full_dat %>% rename(y_1950=...3)
full_dat <- right_join(full_dat, dat_1960, by=c("Regions-ID", "Regionsname"))
full_dat <- full_dat %>% rename(y_1960=...3)
full_dat <- right_join(full_dat, dat_1970, by=c("Regions-ID", "Regionsname"))
full_dat <- full_dat %>% rename(y_1970=...3)
full_dat <- right_join(full_dat, dat_1980, by=c("Regions-ID", "Regionsname"))
full_dat <- full_dat %>% rename(y_1980=...3)
full_dat <- right_join(full_dat, dat_1990, by=c("Regions-ID", "Regionsname"))
full_dat <- full_dat %>% rename(y_1990=...3)
full_dat <- right_join(full_dat, dat_2000, by=c("Regions-ID", "Regionsname"))
full_dat <- full_dat %>% rename(y_2000=...3)
full_dat <- right_join(full_dat, dat_2014, by=c("Regions-ID", "Regionsname"))
full_dat <- full_dat %>% rename(y_2014=...3)
full_dat <- full_dat %>% rename("Regions_ID"="Regions-ID")

# Spezialfall Glarus behandeln. Da im Jahr 2010 die Gemeinden des Kantons Glarus zu drei Grossgemeinden
# fusioniert wurden, erhalten wir für alle Gemeinden (aufgrund des Matchings aus dem Gemeindedatensatz des BFS) NA-Werte und alle Daten
# für den Kanton GL gehen verloren. Daher muss dieser manuell zum Datensatz hinzugefügt werden.

# Alle Gemeinden des Kantons Glarus filtern
gl <- full_dat %>% filter(Regions_ID>=1602 & Regions_ID<=1629)
gl %>% select(y_1850:y_2014)
# Zu long-Format wechseln
gl_long <- gl %>% pivot_longer(cols = starts_with("y_"), names_to = "jahr", values_to = "wert")
# Ausprägungen der Religionen in den verschiedenen Glarner Gemeinden zählen und die Maximalwerte bestimmen
gl_count <- gl_long %>%
  count(jahr, wert) %>%               
  group_by(jahr) %>%
  slice_max(order_by = n, n = 1, with_ties = TRUE) %>%
  ungroup()
# Max-Zahlen wieder entfernen und zurück in wide-format bringen
gl_count <- gl_count %>% select(jahr, wert)
gl_count <- gl_count %>% pivot_wider(names_from = jahr, values_from = wert)

# Datensatz des BFS mit den Gemeinde-Codes laden
num <- read_xlsx("Gemeindestand.xlsx")
num <- num %>% rename("Regions_ID"="BFS Gde-nummer")
full_dat$Regions_ID <- as.numeric(full_dat$Regions_ID)

# Die Gemeinde-Codierungen mit dem full_dat Datensatz matchen
full_dat <- right_join(full_dat, num, by="Regions_ID")
full_dat$Kanton <- as.factor(full_dat$Kanton)

cols <- c("y_1850", "y_1860","y_1870","y_1880","y_1890","y_1900","y_1910","y_1920","y_1930","y_1940","y_1950","y_1960","y_1970","y_1980","y_1990","y_2000","y_2014")
full_dat[cols] <- lapply(full_dat[cols], factor)

# Erneut Spezialfall Glarus: Nun müssen die oben berechneten Religionswerte für die drei Gemeinden des kantons GL eingefügt werden
gl_values <- as.list(unlist(gl_count[1,]))
names(gl_values) <- cols
full_dat[which(full_dat$Kanton == "GL"), names(gl_values)] <- gl_values


# Datensatz in Long-Format bringen und Jahresvariable richtig formatieren
full_dat <- full_dat %>% pivot_longer(cols=cols, names_to = "year", values_to = "rel")
full_dat <- full_dat %>% mutate(year=substr(full_dat$year, 3,6))
full_dat$year <- as.numeric(full_dat$year)


write.csv(full_dat, "/Users/pascaltrosch/Downloads/rel_gemeinde.csv")


df_kanton_agg <- full_dat %>%
  group_by(Kanton, year) %>%
  summarise(rel = {
    x <- rel[!is.na(rel)]
    ux <- unique(x)
    if (length(ux) == 0) {
      NA
    } else {
      ux[which.max(tabulate(match(x, ux)))]
    }
  }, .groups = "drop")

# 3. Pivotieren: Kantone als Zeilen, Jahre als Spalten
df_pivot <- df_kanton_agg %>%
  pivot_wider(names_from = year, values_from = rel)

df_pivot$`2020` <- df_pivot$`2014`

gem <- df_pivot

#### weiterfahren mit Data Wrangling #####


gem_long <- gem %>%
  pivot_longer(
    cols = !starts_with("K"),
    names_to = "Jahr",
    values_to = "Religion"
  ) %>%
  mutate(
    Jahr = as.integer(sub("^X", "", Jahr))
  )


gem_long <- gem_long %>% 
  mutate(rel_num=case_when(Religion=="Mehrheit katholisch: ≥ 80,0 %" ~ 0,
                           Religion=="Mehrheit Katholisch: ≥ 80,0 %" ~ 0,
                           Religion=="Mehrheit katholisch: 60,0 - 79,9 %" ~0.2,
                           Religion=="Mehrheit Katholisch: 60,0 - 79,9 %" ~0.2,
                           Religion=="Mehrheit katholisch: 40,0 - 59,9 %" ~0.4,
                           Religion=="Mehrheit Katholisch: 40,0 - 59,9 %" ~0.4,
                           Religion=="Mehrheit reformiert: 40,0 - 59,9 %" ~0.6,
                           Religion=="Mehrheit Reformiert: 40,0 - 59,9 %" ~0.6,
                           Religion=="Mehrheit reformiert: 60,0 - 79,9 %" ~0.8,
                           Religion=="Mehrheit Reformiert: 60,0 - 79,9 %" ~0.8,
                           Religion=="Mehrheit reformiert: ≥ 80,0 %" ~ 1,
                           Religion=="Mehrheit Reformiert: ≥ 80,0 %" ~ 1,
                           Religion=="Mehrheit mit anderer oder ohne Religionszugehörigkeit: 40,0 - 59,9 %"~0.5))

abst_long$kanton <- toupper(abst_long$kanton)
gem_long <- gem_long %>% rename(kanton=Kanton)
gem_long <- gem_long %>% rename(jahrzehnt=Jahr)
gem_long$jahrzehnt <- ifelse(gem_long$jahrzehnt==2014, 2010, gem_long$jahrzehnt)

abst_long <- left_join(abst_long, gem_long, by=c("jahrzehnt", "kanton"))

# Wie oft stimmt econ_fr mit abstimmungsresultat auf ch-ebene überein?
abst_long$same_ch <- abst_long$annahme*abst_long$econ_fr

abst_long$rel_dichotom <- ifelse(abst_long$rel_num<0.1, 0, 1)
# Welcher kanton stimmt besonders oft wirtschaftsfreundlich oder wirtschaftsunfreundlich?
same_ch_tab <- abst_long %>% count(kanton, econ_fr) %>% arrange(desc(n))
jahrzehnt_ch_tab <- abst_long %>% count(kanton, jahrzehnt, econ_fr)

jahrzehnt_ch_tab <- na.omit(jahrzehnt_ch_tab)
jahrzehnt_ch_tab %>% arrange(desc(n))



mod1 <- glm(econ_fr~rel_num, data=abst_long)
summary(mod1)
mod2 <- glm(econ_fr ~ rel_num, data = abst_long, family = binomial(link = "logit"))
summary(mod2)
mod3 <- glm(econ_fr ~ rel_num+kanton, data = abst_long, family = binomial(link = "logit"))
summary(mod3)


coef(mod2)

# Wahrscheinlichkeit für rel_num = 0
plogis(coef(mod2)[1])

# Wahrscheinlichkeit für rel_num = 1
plogis(coef(mod2)[1] + coef(mod2)[2])

# Werte für moderat reformiert(0.6) und moderat katholisch(0.4)
plogis(coef(mod2)[1] + coef(mod2)[2] * 0.6)
plogis(coef(mod2)[1] + coef(mod2)[2] * 0.4)

plogis(coef(mod2)[1] + coef(mod2)[2] * 0.8)
plogis(coef(mod2)[1] + coef(mod2)[2] * 0.2)





summary(abst_long$econ_fr)

## Religion und Abstimmung gruppiert nach Jahrzehnt
# Daten vorbereiten mit Prozentanteilen
plot_data <- abst_long %>%
  count(jahrzehnt, rel_dichotom, econ_fr) %>%
  filter(!is.na(jahrzehnt), !is.na(rel_dichotom), !is.na(econ_fr)) %>%
  group_by(jahrzehnt, rel_dichotom) %>%
  mutate(prozent = n / sum(n) * 100) %>%
  ungroup()

# Plot mit Prozentzahlen als Text
plot_rel_jahrzehnt <- ggplot(plot_data, aes(x = factor(jahrzehnt), y = n, fill = factor(econ_fr))) +
  geom_col(position = "stack", width = 0.6) +
  geom_text(
    aes(label = paste0(round(prozent, 1), "%")),
    position = position_stack(vjust = 0.5),
    size = 3,
    color = "black"
  ) +
  facet_grid(. ~ rel_dichotom, labeller = labeller(rel_dichotom = c(
    `0` = "Katholische Kantone",
    `1` = "Reformierte Kantone"
  ))) +
  scale_fill_manual(
    values = c("0" = "lightgray", "1" = "steelblue"),
    labels = c("0" = "Nicht wirtschaftsfreundlich", "1" = "Wirtschaftsfreundlich")
  ) +
  labs(x = "Jahrzehnt", y = "Anzahl", fill = "Abstimmungsresultat") +
  theme_minimal()

plot_rel_jahrzehnt

















modelle_pro_kanton <- abst_long %>%
  group_by(kanton) %>%
  nest() %>%
  mutate(
    model = map(data, ~ glm(econ_fr ~ rel_num, data = ., family = binomial)),
    koeffizienten = map(model, tidy)
  ) %>%
  unnest(koeffizienten)
print(modelle_pro_kanton, n = Inf)

#abst_4$`p-fdp` <- as.factor(abst_4$`p-fdp`)
#abst_4$annahme <- as.factor(abst_4$annahme)
abst_4 <- abst_4 %>%
  mutate(across(c(anr, datum, titel_kurz_d,`p-fdp`, annahme, `be-annahme`, `vs-annahme`), as.factor))
abst_4 <- abst_4 %>%
  mutate(across(all_of(names(abst_4)[655:860]), as.factor))

abst4_lm <- abst_4 %>%
  select(anr, datum, titel_kurz_d, `p-fdp`, contains("japroz"), contains("annahme"))

abst_long <- abst4_lm %>%
  pivot_longer(
    cols = ends_with("-japroz"),  # oder: matches("^[a-z]{2}-japroz$")
    names_to = "kanton",
    names_pattern = "(.*)-japroz",
    values_to = "japroz"
  )
abst_long <- abst_long %>%
  pivot_longer(
    cols = ends_with("-annahme"),
    names_to = "kanton2",
    names_pattern = "(.*)-annahme",
    values_to = "annahme_kanton"
  ) %>%
  filter(kanton == kanton2) %>%  # sicherstellen, dass kanton und kanton2 übereinstimmen
  select(-kanton2)

abst_long <- abst_long %>%
  mutate(across(c(kanton, annahme_kanton), as.factor))
abst_long <- abst_long %>%
  mutate(across(japroz, as.numeric))
model_vd <- glm(japroz ~ `p-fdp`, data = abst_long, subset = kanton == "vd")
model_fr <- glm(japroz ~ `p-fdp`, data = abst_long, subset = kanton == "fr")
summary(model_vd)
summary(model_fr)





abst_4_bevs <- abst_4 %>% select(c(anr, datum, titel_kurz_d, `be-japroz`, `be-annahme`, `vs-japroz`, `vs-annahme`, `p-fdp`, annahme))
abst_4_bevs <- abst_4_bevs %>% rename("be_proz"=`be-japroz`, "vs_proz"=`vs-japroz`)
abst_4_bevs <- pivot_longer(abst_4_bevs, cols=c(4:7), names_to="cantons", values_to=c("japroz", "annahme"))
abst_4_bevs$japroz <- as.numeric(abst_4_bevs$japroz)

plot <- ggplot(abst_4_bevs, aes(datum, japroz, color=`p-fdp`))+geom_point()+facet_grid(cols=vars(cantons))

model1 <- lm(japroz~`p-fdp`, data=abst_4_bevs, subset =(cantons))
model2 <- glm(`p-fdp` ~ cantons, data = abst_4_bevs, family = binomial())

abst_3$datum[5:51] <- as.Date(as.integer(abst_3$datum[5:51]), origin = "1899-12-30")


t.test(japroz ~ cantons, data = abst_4_bevs, var.equal = TRUE)


