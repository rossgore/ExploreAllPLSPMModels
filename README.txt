Built and Tested with R 3.2 and 3.3
Required Libraries
require(plspm)
require(lavan)
require(iterpc)
require(R.utils)
require(stringr)
require(AlgDesign)
require(doParallel)
-----------------------------------------------------------
Choose which type of modeling methodology you would like to employ.
test-all-lavan-models.r does the testing for using the lavan package.
test-all-plspm-models.r does the test using the plspm package.
-----------------------------------------------------------
Example of input interaction (same for both scripts)
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
Output is written to output.csv file in the same directory as the program. Format is csv. First column is a pretty print of the model, all others columns reflect goodness of fit.
-------------------------------------------------------------
