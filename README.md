Bot or Not? 🤖
Using Machine Learning to Distinguish Assisted and Unassisted Wordle Play
Overview

Every day, millions of people share their Wordle results using the game's emoji-based summary format. While these summaries do not reveal the actual guesses made, they still contain information about a player's decision-making process.

This project investigates a simple question:

Can Wordle result summaries be used to distinguish statistically plausible human play from play assisted by external tools such as Wordle solvers?

Because real-world Wordle data does not contain labels indicating whether a game was assisted, this project develops a synthetic data generation framework that simulates both valid (human-like) and invalid (solver-assisted) gameplay. Machine learning models are then trained on these simulated games and applied to real Wordle data.

Research Question

Can gameplay features extracted solely from Wordle emoji summaries be used to classify games as:

Valid (unassisted, human-like play)
Invalid (solver-assisted play)
Methodology
1. Wordle Simulation Engine

A custom Wordle simulator was developed to reproduce the mechanics of the original game.

The simulator:

Selects target words from the official Wordle solution list
Generates guesses sequentially
Computes Wordle feedback (green, yellow, gray tiles)
Tracks remaining candidate words after each guess

Two gameplay strategies were simulated:

Valid Play

Human-like heuristic strategy:

Prioritizes information gathering
Makes imperfect decisions
Does not always choose the optimal guess
Invalid Play

Solver-assisted strategy:

Has access to candidate word information
Selects guesses that maximize expected information gain
Reduces uncertainty much more efficiently
2. Synthetic Dataset Generation

Thousands of simulated games were generated under both conditions.

For each game, summary-level features were extracted, including:

Mean entropy reduction per guess
Maximum entropy reduction
Early-game entropy reduction
Fraction of green tiles
Fraction of yellow tiles
Guess efficiency metrics
Feedback progression metrics

These features formed the training dataset for the machine learning models.

3. Real Wordle Dataset

To evaluate whether the learned patterns generalize beyond simulation, the models were applied to a large public Wordle dataset:

Source: Scarcalvetsis (2022), Kaggle Wordle Games Dataset

Dataset characteristics:

~6.9 million Wordle results
Collected from publicly shared Twitter posts
Contains Wordle summaries and metadata
No labels indicating whether games were assisted

A random subset of the dataset was used for analysis.

4. Machine Learning Pipeline

The synthetic dataset was split into:

70% Training
30% Testing

Using k-fold cross-validation, the following models were trained and compared:

Logistic Regression
Elastic Net
Random Forest
XGBoost (Gradient Boosted Trees)

Preprocessing included:

Feature scaling
Dummy encoding
Missing-value handling
Removal of uninformative variables
Results
Synthetic Test Data

Among all candidate models, XGBoost achieved the strongest performance on held-out synthetic data.

Feature importance analysis showed that the most informative predictors were primarily:

Entropy-based metrics
Green-tile accumulation rates
Information-gain measures

These findings support the hypothesis that solver-assisted play produces distinct information acquisition patterns.

Real-World Application

The best-performing model was applied to real Wordle games.

Key observations:

Predicted probabilities showed a bimodal distribution.
Most games strongly resembled the simulated valid strategy.
A smaller subset resembled the simulated solver-assisted strategy.

Importantly:

The model does not identify individual players as cheaters.

Instead, it detects gameplay patterns that statistically resemble the assisted strategy used during simulation.

Repository Structure
.
├── data/
│   ├── synthetic_data.csv
│   ├── real_wordle_data.csv
│
├── scripts/
│   ├── simulation_engine.R
│   ├── feature_extraction.R
│   ├── model_training.R
│   └── evaluation.R
│
├── figures/
│   ├── eda/
│   ├── model_performance/
│   └── feature_importance/
│
├── Final-Project.html
├── Final-Project.Rmd
└── README.md
Technologies Used
R
tidymodels
tidyverse
xgboost
ranger
ggplot2
patchwork
yardstick
Key Takeaways
Wordle summaries contain meaningful information about decision-making processes.
Entropy-based features are highly informative for distinguishing gameplay styles.
Machine learning models can successfully separate simulated assisted and unassisted play.
Synthetic data provides a practical solution when labeled real-world data is unavailable.
Future Work

Potential extensions include:

More realistic models of human gameplay
Additional cheating/assistance strategies
Sequence-based models (RNNs, Transformers)
User-level longitudinal analysis
Calibration against experimentally collected labeled gameplay data
Author

Pratyush Rallapally

Statistics & Data Science + Biology
University of California, Santa Barbara

Disclaimer

This project is intended as an exploration of gameplay behavior and machine learning classification. Model predictions should not be interpreted as evidence that any individual player cheated.
