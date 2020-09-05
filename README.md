# fraud-detection

The Repo contains project prepared as an assessment for the course [Advanced Programming in R](https://usosweb.wne.uw.edu.pl/kontroler.php?_action=katalog2/przedmioty/pokazPrzedmiot&prz_kod=2400-DS1APR) on my Master Studies - Data Science and Business Analytics @ [Faculty of Economic Sciences, University of Warsaw](www.wne.uw.edu.pl).

### The goal of the assessment

The aim of the project was to prepare R functions that cover following topics:
- defensive programming
- loops 
- functions from dplyr family

and had to be prepated in two variants:
1) using aggregation function
2) using loops for aggregation data



### FraudDetection() functions

I attempted to build simple function that can detect potential fraud transactions. Obviously, financial institutions use much complex tools for fraud detection. Following project is based on some basic assumptions and had to show ability to use R programming skills.

Data from [PKDD'99 Challenge](https://sorry.vse.cz/~berka/challenge/pkdd1999/berka.html) was used.

Function is built of three parts:
1) detect transactions that are much higher than usuall transactions of chosen user
2) detect users that make a number of transactions between own accounts
3) detect users that made fast outgoing transactions of similar amount after one big incoming transaction

