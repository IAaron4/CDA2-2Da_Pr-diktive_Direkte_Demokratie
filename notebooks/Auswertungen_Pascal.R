library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(purrr)
library(broom)

### Data Wrangling ###

#Swissvotes-Datensatz laden
abst <- read_xlsx("data/raw/DATASET XLSX 09-02-2025.xlsx", sheet=2)
#Swissvotes-Datensatz nach Wirtschafts- und Finanzpolitischen Vorlagen filtern
abst_4 <- abst %>%  filter(d1e1 %in% c(4,6))


#Alle Datumsformate angleichen (Daten vor 1900 werden durch read_xlsx falsch importiert)
abst_4_1 <- abst_4 %>% slice(15:152)
abst_4_2 <- abst_4 %>% slice(1:14)
abst_4_2$datum <- as.Date(abst_4_2$datum, format = "%d.%m.%Y")
abst_4_2$datum <- format(abst_4_2$datum, "%d.%m.%Y")
abst_4_1$datum <- as.Date(as.numeric(abst_4_1$datum), origin = "1899-12-30")
abst_4_1$datum <- format(abst_4_1$datum, "%d.%m.%Y")
abst_4 <- bind_rows(abst_4_1, abst_4_2)

# Empfehlungsvariable FDP richtig codieren und als numerisch abspeichern (NA's entfernen)
abst_4$`p-fdp` <- as.numeric(abst_4$`p-fdp`)
abst_4$`p-fdp` <- ifelse(abst_4$`p-fdp` > 2, NA, abst_4$`p-fdp`)
abst_4$`p-fdp` <- ifelse(abst_4$`p-fdp`==2, 0, abst_4$`p-fdp`)



# Relevante Variablen aus dem Gesamtdatensatz extrahieren
abst_4_lm <- abst_4 %>% select(anr, datum, titel_kurz_d, `p-fdp`, contains("annahme"), `volkja-proz`)
# Fehlende Werte für den Kanton Jura zu NA umcodieren
abst_4_lm$`ju-annahme` <- ifelse(abst_4_lm$`ju-annahme`==".", NA, abst_4_lm$`ju-annahme`)

#Dichotome Variablen für die Abstimmungsannahme in Datensatz inkludieren
abst_4_lm_clean <- abst_4_lm %>%
  mutate(across(ends_with("-annahme"), as.character))

# In Long-Format überführen
abst_long <- abst_4_lm_clean %>%
  pivot_longer(
    cols = ends_with("-annahme"),
    names_to = "kanton",
    values_to = "kt_annahme",
    names_transform = list(kanton = ~ sub("-annahme", "", .x))
  )

#  Werte in numeric-Format konvertieren (nicht-konvertierbare werden NA)
abst_long <- abst_long %>%
  mutate(annahme = readr::parse_number(annahme))
abst_long$`p-fdp`<- as.numeric(abst_long$`p-fdp`)
abst_long$kt_annahme <- as.numeric(abst_long$kt_annahme)

# Econ-Friendly (für wirtschaftsfreundliches Abstimmen) generieren
# Abstimmung wird als wirtschaftsfreundlich codiert, wenn Abstimmungsresultat mit FDP-Empfehlung übereinstimmt.
abst_long$econ_fr <-  abst_long$`p-fdp`*abst_long$kt_annahme

# Jahrzehnte aufgrund der Datumsvariable generieren
abst_long <- abst_long %>%
  mutate(
    datum = dmy(datum),
    jahr = year(datum),
    

    jahrzehnt = floor(jahr / 10) * 10
  )

### Religiöse Ausrichtung der kantone ####

# Verschiedene Jahres-Datensätze einlesen und unbekannte Regionen filtern, 
# damit keine doppelten/unzuweisbaren Einträge entstehen
# Daten stammen vom Bundesamt für Statistik und konnten nur pro Jahrzehnt einzeln als Excel-Datei heruntergeladen werden
dat_1850 <- read_xlsx("data/raw/religion/1850.xlsx", skip=3) %>% filter(!is.na(Regionsname))
dat_1860 <- read_xlsx("data/raw/religion/1860.xlsx", skip=3) %>% filter(!is.na(Regionsname))
dat_1870 <- read_xlsx("data/raw/religion/1870.xlsx", skip=3) %>% filter(!is.na(Regionsname))
dat_1880 <- read_xlsx("data/raw/religion/1880.xlsx", skip=3) %>% filter(!is.na(Regionsname))
dat_1890 <- read_xlsx("data/raw/religion/1890.xlsx", skip=3) %>% filter(!is.na(Regionsname))
dat_1900 <- read_xlsx("data/raw/religion/1900.xlsx", skip=3) %>% filter(!is.na(Regionsname))
dat_1910 <- read_xlsx("data/raw/religion/1910.xlsx", skip=3) %>% filter(!is.na(Regionsname))
dat_1920 <- read_xlsx("data/raw/religion/1920.xlsx", skip=3) %>% filter(!is.na(Regionsname))
dat_1930 <- read_xlsx("data/raw/religion/1930.xlsx", skip=3) %>% filter(!is.na(Regionsname))
dat_1940 <- read_xlsx("data/raw/religion/1940.xlsx", skip=3) %>% filter(!is.na(Regionsname))
dat_1950 <- read_xlsx("data/raw/religion/1950.xlsx", skip=3) %>% filter(!is.na(Regionsname))
dat_1960 <- read_xlsx("data/raw/religion/1960.xlsx", skip=3) %>% filter(!is.na(Regionsname))
dat_1970 <- read_xlsx("data/raw/religion/1970.xlsx", skip=3) %>% filter(!is.na(Regionsname))
dat_1980 <- read_xlsx("data/raw/religion/1980.xlsx", skip=3) %>% filter(!is.na(Regionsname))
dat_1990 <- read_xlsx("data/raw/religion/1990.xlsx", skip=3) %>% filter(!is.na(Regionsname))
dat_2000 <- read_xlsx("data/raw/religion/2000.xlsx", skip=3) %>% filter(!is.na(Regionsname))
dat_2014 <- read_xlsx("data/raw/religion/2014.xlsx", skip=3) %>% filter(!is.na(Regionsname))

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
num <- read_xlsx("data/raw/religion/Gemeindestand.xlsx")
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


#write.csv(full_dat, "/Users/pascaltrosch/Downloads/rel_gemeinde.csv")

# Aggregierter Datensatz mit Religionsvariablen erstellen
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

# Pivotieren: Kantone als Zeilen, Jahre als Spalten
df_pivot <- df_kanton_agg %>%
  pivot_wider(names_from = year, values_from = rel)

df_pivot$`2020` <- df_pivot$`2014`

gem <- df_pivot




#### Gesamtdatensatz für die Auswertungen erstellen #####

# Gemeinde-Datensatz für die Religionsauswertung Pivotieren (wide to long)
gem_long <- gem %>%
  pivot_longer(
    cols = !starts_with("K"),
    names_to = "Jahr",
    values_to = "Religion"
  ) %>%
  mutate(
    Jahr = as.integer(sub("^X", "", Jahr))
  )

# Character-Werte der Religionszugehörigkeit skalieren. Hierbei wird die Religionsskala von 0 (stark katholisch) bis 1 (stark reformiert verwendet)
# Datne des BFS sind teilweise "schmutzig" mancham ist katholisch/reformiert gross geschrieben, manchmal klein
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

# Variablen-Namen in beiden Datensätzen angleichen
abst_long$kanton <- toupper(abst_long$kanton)
gem_long <- gem_long %>% rename(kanton=Kanton)
gem_long <- gem_long %>% rename(jahrzehnt=Jahr)
gem_long$jahrzehnt <- ifelse(gem_long$jahrzehnt==2014, 2010, gem_long$jahrzehnt)

# Merging des Abstimmungs-Datensatzes mit dem Religions-Datensatz
abst_long <- left_join(abst_long, gem_long, by=c("jahrzehnt", "kanton"))

# Neue Variable generieren: Wie oft stimmt econ_fr mit abstimmungsresultat auf ch-ebene überein?
abst_long$same_ch <- abst_long$annahme*abst_long$econ_fr

# Dichotome Religionsvariable generieren (0=katholisch, 1=reformiert)
abst_long$rel_dichotom <- ifelse(abst_long$rel_num<0.1, 0, 1)
# Welcher kanton stimmt besonders oft wirtschaftsfreundlich oder wirtschaftsunfreundlich?
same_ch_tab <- abst_long %>% count(kanton, econ_fr) %>% arrange(desc(n))
jahrzehnt_ch_tab <- abst_long %>% count(kanton, jahrzehnt, econ_fr)
jahrzehnt_ch_tab <- na.omit(jahrzehnt_ch_tab)
jahrzehnt_ch_tab %>% arrange(desc(n))


# Lineare/Logistische Regressionsmodelle für gesamte Schweiz rechnen
# Model 1: Lineare Regression Einfluss Religion auf wirtschaftsfreundliche Abstimmungen
mod1 <- glm(econ_fr~rel_num, data=abst_long)
summary(mod1)
# Model 2: Logistische Regression Einfluss Religion auf wirtschaftsfreundliche Abstimmungen
mod2 <- glm(econ_fr ~ rel_num, data = abst_long, family = binomial(link = "logit"))
summary(mod2)
# Model 3: Logistische Regression unter Einbeziehungen kantonaler Unterschiede
mod3 <- glm(econ_fr ~ rel_num+kanton, data = abst_long, family = binomial(link = "logit"))
summary(mod3)



# Interpretation der Koeffizienten der logistischen Regression
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


## Religion und Abstimmung gruppiert nach Jahrzehnt
# Daten vorbereiten mit Prozentanteilen
plot_data <- abst_long %>%
  count(jahrzehnt, rel_dichotom, econ_fr) %>%
  filter(!is.na(jahrzehnt), !is.na(rel_dichotom), !is.na(econ_fr)) %>%
  group_by(jahrzehnt, rel_dichotom) %>%
  mutate(prozent = n / sum(n) * 100) %>%
  ungroup()

# Abbildung 4: Plot Vergleich Abstimmungen zwischen ref und kath Kantone mit Prozentzahlen als Text
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




## Vergleich der Kantone AR/AI und AG ##

# Datensatz nach Kantonen AR/AI filtern
ar_ai <- abst_long %>% filter(kanton=="AR" | kanton=="AI")

# Abbildung 7: Vergleich Abstimmungen AR/AI
plot_arai_1 <- ar_ai %>% count(kanton, econ_fr) %>%
  filter(!is.na(econ_fr)) %>%
  ggplot(aes(x = kanton, y = n, fill = as.factor(econ_fr))) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_text(
    aes(label = n),
    position = position_dodge(width = 0.9),
    vjust = -0.3,
    size = 3.5
  ) +
  labs(
    x = "Kanton",
    y = "Anzahl",
    fill = "Abstimmungsresultat"
  ) +
  scale_fill_manual(
    values = c("0" = "lightgray", "1" = "steelblue"),
    labels = c("0" = "Nicht wirtschaftsfreundlich", "1" = "Wirtschaftsfreundlich")) +
  theme_minimal()
plot_arai_1

# Abbildung 8: Abstimmungen AR/AI über Zeit
plot_arai_2 <- ar_ai %>%
  count(jahrzehnt, kanton, econ_fr) %>% filter(econ_fr==1) %>% 
  ggplot(aes(x = jahrzehnt, y = n, color = kanton, group = kanton)) +
  geom_point(position = position_jitter(width = 0.5, height = 0), size = 3) +
  labs(x = "Jahrzehnt", y = "Anzahl", color = "Kanton") +
  scale_color_manual(
    values = c("AI" = "royalblue", "AR" = "peachpuff"))+
  theme_minimal()

plot_arai_2



# Abbildung 9: Abstimmung KT AG vs. Schweizer Durchschnitt
ag <- abst_long %>% filter(kanton=="AG")


plot_data_ag2 <- abst_long %>%
  count(jahrzehnt, econ_fr) %>%
  filter(!is.na(jahrzehnt), !is.na(econ_fr)) %>%
  mutate(n_pro_25 = n / 25) %>% 
  filter(econ_fr==1)
plot_data_ag2$ag <- ag %>% count(jahrzehnt, econ_fr) %>% filter(!is.na(econ_fr)) %>% filter(econ_fr==1)

plot_ag_2 <- ggplot(plot_data_ag2, aes(x=jahrzehnt))+
  geom_point(aes(y=n_pro_25, color = "Durchschnitt Schweiz"), size = 3)+
  geom_point(aes(y=ag$n, color="Kanton Aargau"), size = 3, shape = 17)+
  scale_color_manual(
    name = "Abstimmungsergebnis",
    values = c("Durchschnitt Schweiz" = "steelblue", "Kanton Aargau" = "firebrick")
  ) +
  labs(x = "Jahrzehnt", y = "Anzahl") +
  theme_minimal()+
  geom_vline(xintercept = c(1920, 1960), linetype = "dashed", color = "gray40", linewidth = 0.7)

plot_ag_2











