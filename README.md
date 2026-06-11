# Bot or Not? 🤖

Using machine learning to distinguish assisted and unassisted Wordle play.

## Overview

Wordle result summaries contain surprisingly rich information about a player's decision-making process. Although these summaries do not reveal the actual guesses used, they preserve patterns of information acquisition that may differ between human players and those using external solver tools.

This project investigates whether machine learning models can distinguish between:

- **Valid play** (unassisted, human-like gameplay)
- **Invalid play** (solver-assisted gameplay)

Because no labeled dataset of assisted Wordle games exists, this project develops a simulation framework to generate synthetic examples of both gameplay styles and uses them to train classification models.

---

## Research Question

Can features extracted solely from Wordle result summaries be used to classify whether a game was played with or without external assistance?

---

## Methodology

### 1. Wordle Simulation Engine

A custom Wordle simulator was developed to reproduce the game's mechanics and generate synthetic gameplay data.

Two strategies were simulated:

#### Valid Play

- Human-like guessing behavior
- Imperfect information use
- Exploration-oriented decisions
- Realistic variability in performance

#### Invalid Play

- Solver-assisted guessing
- Maximization of expected information gain
- Rapid reduction of uncertainty
- Near-optimal decision making

---

### 2. Feature Engineering

For each simulated game, summary-level features were extracted, including:

- Mean entropy reduction per guess
- Maximum entropy reduction
- Early-game information gain
- Fraction of green tiles
- Fraction of yellow tiles
- Guess efficiency metrics
- Feedback progression statistics

These features were designed to capture differences in how information is acquired throughout the game.

---

### 3. Model Training

Synthetic games were split into training and testing sets.

Several machine learning models were evaluated:

- Logistic Regression
- Elastic Net
- Random Forest
- XGBoost

Model tuning was performed using cross-validation within the `tidymodels` framework.

---

### 4. Real-World Evaluation

The trained models were applied to a large public dataset of Wordle results collected from Twitter.

Dataset:

- Approximately 6.9 million Wordle games
- Publicly shared Wordle summaries
- No labels indicating assisted or unassisted play
---



