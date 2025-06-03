# CDA2-2Da_Prädiktive_Direkte_Demokratie

## Überblick

Dieses Repository enthält die Datenanalysen und Visualisierungen zu direktdemokratischen Prozessen in der Schweiz, basierend auf dem Datensatz von swissvotes.ch. Das Projekt wurde im Rahmen der Challenge "Direkte Demokratie" an der FHNW im Frühlingssemester 2025 durchgeführt.

## Projektziel

Ziel dieses Projekts ist die Analyse und aussagekräftige Darstellung von Volksabstimmungsdaten der Schweiz seit 1848. Dabei werden historische, kulturelle, soziologische und politologische Kontexte berücksichtigt, um die Daten entsprechend einzuordnen. Die Ergebnisse wurden in Form eines wissenschaftlichen Blogbeitrags für ein allgemeines Publikum aufbereitet.

## Inhaltsstruktur

- `data/`: Enthält die verwendeten Datensätze
    - `raw/`: Rohdaten von swissvotes.ch
    - `processed/`: Aufbereitete Datensätze für die Analyse
- `notebooks/`: Jupyter Notebooks mit den durchgeführten Analysen
    - `01_exploratory_analysis.ipynb`: Explorative Datenanalyse
    - `01_a_cantons_analyzed.ipynb`: Explorative Datenanalyse - Kantone analysiert
    - `01_b_cantons_categorized.ipynb`: Explorative Datenanalyse - Kantone kategorisiert
    - `02_religion_analysis.ipynb`: Analyse der Religionsdaten
    - `03_logistic_regression_model.ipynb`: Logistische Regressionsmodelle
    - `04_additional_evaluation.R`: Weitere Auswertungen und Modelle in R
    - `05_additional_visualizations_blog.ipynb`: Weitere Visualisierungen für den wissenschaftlichen Blog-Artikel
- `visualizations/`: Generierte Grafiken und Visualisierungen
- `presentations/`: Präsentationsmaterialien
- `docs/`: Zusätzliche Dokumentation und Ressourcen


## Methodik

Das Projekt kombiniert verschiedene Methoden der Datenanalyse:

- Explorative Datenanalyse (EDA)
- Statistische Auswertungen
- Lineare- / Logistische Regressionsmodelle (LLR)
- Multiple Lineare- / Logistische Regressionsmodelle (MLR)
- Visualisierung von zeitlichen Trends und thematischen Schwerpunkten
- Web-Datenbeschaffung und Analyse (für 4Da-Studierende)

## Thematischer Fokus

Lassen sich Unterschiede zwischen protestantischen und katholischen Gebieten feststellen, wenn
es um politische Entscheide geht? Für eine solche Untersuchung eignet sich die Schweiz besonders gut, da die
Bevölkerung dank der direkten Demokratie regelmässig die Gelegenheit hat, ihre politischen Ansichten kundzu-
tun. Wir untersuchen Unterschiede im Abstimmungsverhalten von protestantischen und katholischen Gebiete
und ergründen, ob die zunehmende Säkularisierung in jüngeren Jahren zu einer Veränderung im Abstimmungs-
verhalten geführt hat.

## Installation und Ausführung


``` bash
# Repository klonen
git clone https://github.com/IAaron4/CDA2-2Da_Pr-diktive_Direkte_Demokratie.git
cd swissvotes-analysis

# Virtuelle Umgebung erstellen und aktivieren
python -m venv venv
source venv/bin/activate  # Unter Windows: venv\Scripts\activate

# Abhängigkeiten installieren
pip install -r requirements.txt

# Jupyter Notebook starten
jupyter notebook

```

## Abhängigkeiten

Alle packages sind im requirements.txt definiert.

- Python 3.9+
- pandas
- numpy
- matplotlib
- seaborn
- scikit-learn
- jupyter
- plotly

## Team

- Annabelle McPherson
- Pascal Trösch
- Aaron Studer