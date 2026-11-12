# NBA Shots Data

An R final project on every shot from the 2014–2015 NBA season (Kaggle, 128,069 shots). It uses decision tree and random forest models to predict whether a shot goes in.

## Repository Contents

| File | Description |
|---|---|
| `nba shots project final draft.R` | R script: cleaning, visualizations, and decision tree and random forest models |
| `nba_shot_data.csv` | Shot-level data: game, period, shot clock, dribbles, touch time, shot distance, shot type, closest defender and result |
| `Word Doc NBA Decision Tree and Random Forest Final Project.docx` | Final project write-up |

## Commit History

### `3507c98`: Upload of the R script, dataset and write-up (2022-05-18)
- **Research questions:**
  - which variables most affect whether a shot goes in?
  - is a random forest a good model for predicting made shots?
- **Random forest demo:** explained random forests and demonstrated them on the `GlaucomaM` dataset (about 83% accuracy for glaucoma, 88% for normal).
- **Cleaning:** removed about 6,000 shots with a missing shot clock value.
- **Visualizations:** a boxplot of shot distance at home vs. away, split by made vs. missed shots.
- **Decision tree (`rpart`):** predicts `SHOT_MAKE` from location, shot number, period, shot clock, dribbles, shot distance, shot type, closest defender distance and player. It identified missed shots with 82% accuracy but made shots with only 35%.
- **Random forest:** built as a follow-up to try to improve accuracy.
- **Write-up:** `.docx` with explanations of each step and visualization.

## Tools
R, RStudio, tidyverse, ggplot2, rpart, randomForest, caTools
