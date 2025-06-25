# ecosphereMismatch

# Phenological mismatch and reproductive success in bighorn ewes

This repository contains the processed data and supporting code for the manuscript:

**"Trophic mismatch alters allocation to mass gain in female bighorn sheep (Ovis canadensis)"** (submitted to *Ecosphere*)
manuscript ID: ECS25-0363

## 📂 Repository Contents

- `data/`
  - `ECS25-0363_unscaledDat.csv`: full individual-level data (names to be changed in R codes)
  - `ECS25-0363_massGainDat.csv`: subset for mass gain analyses (names to be changed in R codes)
  - `ECS25-0363_breedingDat.csv`: subset for reproductive analyses (names to be changed in R codes)

- `dataTidyingV2.R`: script to process and anonymize data
- `analyses.R`: script to run models of mass gain and carry-over effects 
- `predictionFigures.R`: script to generate figures as they appear in the ms
- `.gitignore`: files excluded from version control
- `README.md`: this file

## 📊 Data Notes

- All individual IDs are anonymized.
- All numerical values rounded to 3 decimal places.
- Variables not used in published analyses (e.g. lamb survival) have been removed.
- Data originate from long-term monitoring of the **Ram Mountain bighorn sheep** population in Alberta, Canada.

## 🧪 Reproducibility

The file `dataTidyingV2.R`:
- Loads the original `.RData` object (not included in this repo)
- Applies cleaning and rounding
- Writes the 3 final CSV datasets

These are the files used in the analyses shown in the manuscript.

## 📜 License

Data and code are shared under the **CC BY 4.0** license.  
Please cite the manuscript if reusing this material.

## 👩‍🔬 Contact

If you have questions or need clarification, please contact:

**Limoilou-Amelie Renaud**  
Postdoctoral researcher, Université du Québec en Abitibi-Témiscamingue  
[limoilouAmelie.renaud@uqat.ca]
[limoilou.renaud@gmail.com]
