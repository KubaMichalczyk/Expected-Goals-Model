# Expected Goals Model
- this is to present my attempt in modelling the most well known statistics in Football/Soccer Analytics community - Expected Goals*
- logistic regression is applied to model the probabilities of scoring a goal dependent on a chance's characteristics
- covariates of the resulting model includes: defensive pressure, number of Defensive Players in a DIRECT line of goal from the shooting player, game state, type of attack as well as distance and angle (the latter two are transformed using restricted cubic splines)

Open __expected_goals_model.md__ to see a description of my modelling proccess.

\* For those who came here from outside from a football analytics community: Expected goals is a method of estimating quality of a chance that a football team creates. It assigns a probability of a shot being a goal given the features of this shot (ex. location, whether proceeded by set piece/open play etc.).
