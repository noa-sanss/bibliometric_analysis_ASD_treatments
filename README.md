# Bibliometric Analysis of Research on Side Effects of Pharmacological Treatments for Children with Autism Spectrum Disorder

## Description
This project investigates trends and developments in research on the side effects of pharmacological treatments for children with Autism Spectrum Disorder (ASD) from 2010 to 2024. Using bibliometric data, the study explores publication trends, key contributors, and thematic focuses to better understand the landscape of pharmacological research and its impact.

## Features
- Temporal analysis of research trends (2010–2024).
- Identification of influential journals, authors, and collaborations.
- Visualization of geographical contributions and research themes.
- Use of bibliometric tools and techniques to highlight trends and challenges in pharmacological treatments for ASD.

## Technologies Used
- **R Programming Language**: For data analysis and visualization.
- **Packages**:
  - `ggplot2`: Graphical representation of trends.
  - `dplyr`: Data manipulation.
  - `treemap` and `treemapify`: Treemap visualizations.
  - `wordcloud`: Word cloud generation.
  - `viridis`: Color palettes for enhanced visualization.

## Dataset
The dataset used in this analysis was sourced from the **Web of Science** database using the following query:
("Autis* Spectrum Disorder*") AND children AND "treatment" AND (pharmacological OR drug OR Medicinal) AND (side effects OR Adverse effects OR Secondary effects OR Adverse reactions)

Only publications with at least 11 citations between 2010 and 2024 were included in the analysis.

## Visualizations and Results
- **Temporal Trends**: Peaks in research publications observed in 2016, 2019, and 2020, with a decline after 2020.
- **Geographical Analysis**: The United States led the research output, followed by countries like the Netherlands and Australia.
- **Thematic Analysis**: Emphasis on clinical treatments, with emerging interest in biological foundations and genetic studies.
- **Influential Journals and Authors**: Highlighted key journals like *Journal of Child and Adolescent Psychopharmacology* and researchers like Dr. Richard E. Frye.
