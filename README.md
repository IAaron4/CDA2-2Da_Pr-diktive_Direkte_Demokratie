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
    - `02_thematic_analysis.ipynb`: Themenspezifische Analysen
    - `03_predictive_models.ipynb`: Prädiktive Modelle (für 4Da-Studierende)
- `scripts/`: Python-Skripte für Datenverarbeitung und -analyse
- `visualizations/`: Generierte Grafiken und Visualisierungen
- `blog/`: Quellcode des wissenschaftlichen Blogbeitrags
- `presentations/`: Präsentationsmaterialien
- `docs/`: Zusätzliche Dokumentation und Ressourcen


## Methodik

Das Projekt kombiniert verschiedene Methoden der Datenanalyse:

- Explorative Datenanalyse (EDA)
- Statistische Auswertungen
- Visualisierung von zeitlichen Trends und thematischen Schwerpunkten
- Anwendung des Satzes von Bayes für einfache Prognosen
- Web-Datenbeschaffung und Analyse (für 4Da-Studierende)

## Thematischer Fokus

In diesem Projekt konzentrieren wir uns auf [THEMA EINSETZEN, z.B. "die Entwicklung der schweizerischen Umwelt- und Energiepolitik anhand von Volksabstimmungen"]. Der Fokus wurde gewählt, um [BEGRÜNDUNG EINSETZEN, z.B. "aktuelle Debatten zur Energiewende im historischen Kontext zu betrachten"].

## Installation und Ausführung


``` bash
# Repository klonen
git clone https://github.com/username/swissvotes-analysis.git
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

- Python 3.9+
- pandas
- numpy
- matplotlib
- seaborn
- scikit-learn
- jupyter
- plotly

## Ergebnisse
Die Hauptergebnisse unserer Analyse sind:

[HAUPTERKENNTNIS 1]
[HAUPTERKENNTNIS 2]
[HAUPTERKENNTNIS 3]

Der vollständige wissenschaftliche Blogbeitrag ist unter blog/final_blog_post.md zu finden.

## Team

- Annabelle McPherson
- Pascal Trösch
- Aaron Studer