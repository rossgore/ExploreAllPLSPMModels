Required Libraries
require(plspm)
require(iterpc)
require(R.utils)
require(stringr)
require(AlgDesign)
Example of input interaction
------------------------------------------------------------
What is the filename for your .csv data? dataset.csv
How many latent variables are in your model? 4
What is the name of latent variable 1 ? Belief.In.God
For latent variable Belief.In.God provide the corresponding column numbers separated by a comma with no spaces: 1,2
What is the name of latent variable 2 ? Supernatural.Beliefs
For latent variable Supernatural.Beliefs provide the corresponding column numbers separated by a comma with no spaces: 3,4,5,6
What is the name of latent variable 3 ? Religious.Formation
For latent variable Religious.Formation provide the corresponding column numbers separated by a comma with no spaces: 7,8,9
What is the name of latent variable 4 ? Religious.Practice
For latent variable Religious.Practice provide the corresponding column numbers separated by a comma with no spaces: 10,11,12
-------------------------------------------------------------
Output is written to output.csv file in the same directory as the program. Format is csv. First column is a pretty print of the model, second is goodness of fit.
-------------------------------------------------------------