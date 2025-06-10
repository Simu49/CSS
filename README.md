# Swiss Media Coverage of the Refugee Issue: A Sentiment Analysis

This repository contains the R code and documentation for a sentiment analysis project conducted as part of a seminar at the University of Bern (Fall Semester 2023). The project analyzes the tonality of Swiss-German media coverage on asylum migration over the period 2010–2022 using NLP methods.

## Project Summary

We collected ~188,000 articles from major Swiss media sources using the Swissdox@LiRI API, filtered them using Latent Dirichlet Allocation (LDA) for topic relevance, and applied sentiment analysis using the SentiMerge lexicon. We examined trends in sentiment over time and tested correlations with monthly asylum immigration statistics via linear regression.

### Key Steps:
- Data acquisition via Swissdox@LiRI API
- Text preprocessing using `quanteda`
- Topic modeling with LDA (unsupervised)
- Sentiment analysis using SentiMerge lexicon
- Correlation and regression analysis with Swiss asylum statistics

## Research Questions

- How has media sentiment about asylum migration changed over the past decade?
- Is sentiment correlated with actual refugee immigration figures?
- Does sentiment differ based on the origin continent of asylum seekers?

## Main Findings

- A slight trend toward more positive sentiment post-2021, especially during Ukraine war.
- No significant correlation between sentiment and total asylum figures.
- Sentiment varied slightly by origin, with Europe, America, and Oceania receiving more positive coverage.

## Files Included

| File / Folder | Description |
|---------------|-------------|
| `CSS_Project_Simon_Kevin` | Contains R script for data preprocessing, topic modeling, and sentiment analysis |
| `README.md` | Project overview |
| `Seminararbeit_CSS_Simon_Kevin.pdf` | Final seminar paper |

## References

- Emerson & Declerck (2014) – SentiMerge Sentiment Lexicon
- Swissdox@LiRI API
- SEM (Swiss Asylum Statistics)

## Authors

- **Simon Bernhard** | simon.bernhard@students.unibe.ch  
- **Kevin Jan Schläpfer** | kevin.schlaepfer@students.unibe.ch  
University of Bern, Department of Social Sciences

