require(lavaan)
require(iterpc)
require(R.utils)
require(stringr)
require(doParallel)
require(AlgDesign)
addModel <- function(listOfNames, regressionModel, rowIndex)
{
	toReturn = paste0(listOfNames[rowIndex], " ~ ")
	termAdded = FALSE
	for (i in 1:length(regressionModel))
	{
		if (1 == regressionModel[i] &  termAdded == TRUE)
		{
			toReturn = paste0(toReturn, " + ", listOfNames[i], " ")
		}
		else if (1 == regressionModel[i] & termAdded == FALSE )
		{
			toReturn = paste0(toReturn, listOfNames[i], " ")
			termAdded = TRUE
		}
		
	}
	if (1 %in% regressionModel)
	{
		return (paste(toReturn, "\n"))
	}
	else
	{
			return ("")
	}
}
runModel <- function(modelData, numberOfFactors, outerModelDetails, innerModelDetails, indexDetails, modelBasics)
{
		currentRow = outerModelDetails
		inner = matrix(, nrow=numberOfFactors, ncol=numberOfFactors)
		inner[1,] = rep(0, numberOfFactors)
		for (k in 2:numberOfFactors)
		{
			index = indexDetails[k]
			inner[k,] = innerModelDetails[[k]][[k]][index,]
		}
		innerNamesList = list()
		for (k in 1:length(currentRow))
		{
			innerNamesList = c(innerNamesList, namesList[currentRow[k]])
		}
		rownames(inner) = innerNamesList
		colnames(inner) = innerNamesList
		# added unlist
		namesMap = unlist(getListOfData(currentRow, listOfColLists, namesList))
		model = modelBasics
		for (i in 2:nrow(inner))
		{
			model = paste(model, addModel(listOfNames=namesMap, regressionModel=inner[i,], rowIndex=i))
		}
		tryCatch({
			fit <- sem(model, data=modelData)
			score = fitMeasures(fit, c("all"))
		return (c(formatModel(inner), score))
	    }, error=function(e){})
		return (c(NA, NA))
}
reverseString <- function(a)
{
	paste(rev(substring(a,1:nchar(a),1:nchar(a))),collapse="")
}
getListOfData <- function(rows, listOfListIndicies, labels)
{
	dataList = list()
	for (i in 1:length(rows))
	{
		for (j in 1:length(rows))
		{
			if (rows[i] == j)
			{
				dataList = c(dataList, list(prepColumnNumbersForUse(listOfListIndicies[j])))
			}
		}

	}
	return (dataList)
}
generateRow <- function(rowIndex, numberOfDigits)
{
	if (rowIndex == 0)
	{
		return (c(0, 0, 0, 0))
	}
	finalComboToGen = (2^rowIndex)-1
	listOfRows = list()
	for(i in 0:finalComboToGen)
	{
		thisRow = as.numeric(unlist(strsplit(reverseString(str_pad(as.character(intToBin(i)), numberOfDigits, pad="0")), "")))
		if (i == 0)
		{
			listOfRows = thisRow
		}
		else
		{
			listOfRows = rbind(listOfRows, thisRow)
		}
	}
	rownames(listOfRows) <- c()
	return (as.matrix(listOfRows))
}
formatModel <- function(X, score)
{
	formattedModel = ""
	for (i in 1:nrow(X))
	{
		for (j in 1:ncol(X))
		{
			if (X[i,j] != 0 )
			{
				formattedModel = paste(formattedModel, paste(colnames(X)[j], "->", rownames(X)[i], "||"))
			}	
		}
	}
	return(substr(formattedModel, start=2, stop=nchar(formattedModel)-3))
}
prepColumnNumbersForUse <- function (stringOfColNumbers)
{
	myList = unlist(strsplit(as.character(stringOfColNumbers), ","))
	myList = as.numeric(myList)
	listToReturn = c()
	for (i in 1:length(myList))
	{
		listToReturn = c(listToReturn, namesList[myList])
	}
	return (listToReturn)
}
cat("What is the filename for your .csv data? ")
x <- readLines(file("stdin"),1)
closeAllConnections()
data = read.csv(file=x, header=T)
# end prep of data frame

# determine number of manifests
cat("How many latent variables are in your model? ")
numberOfLatents <- readLines(file("stdin"),1)
closeAllConnections()
numberOfLatents <- as.integer(numberOfLatents)

listOfColLists = list()
namesList = list()
modelBase = ""
for (i in 1:numberOfLatents)
{
	cat(paste("What is the name of latent variable", i, "? "))
	name = readLines(file("stdin"),1)
	namesList = c(namesList, name)
	modelBase = paste(modelBase, name, "=~ ")
	cat(paste("For latent variable", name, "provide the corresponding column numbers separated by a comma with no spaces: "))
	colIndicies = as.numeric(as.character(unlist(strsplit(readLines(file("stdin"),1),","))))
	for (j in 1:length(colIndicies))
	{
		if (j != length(colIndicies))
		{
			modelBase = paste0(modelBase, colnames(data)[colIndicies[j]], "+")
		}
		else
		{
			modelBase = paste0(modelBase, colnames(data)[colIndicies[j]], "\n")
		}
	}
	listOfColLists = c(listOfColLists, colIndicies)
}
closeAllConnections()
# call function to form base model namesList[i]
myRows = list()
numberOfEntries = rep(1, numberOfLatents)
for (i in 1:numberOfLatents)
{
	myRows[[i]] = unlist(list(myRows, list(generateRow(i-1, numberOfLatents))), recursive=FALSE)
	if (i > 1)
	{
		numberOfEntries[i] = nrow(myRows[[i]][[i]])
	}	
}

I = iterpc(numberOfLatents, numberOfLatents, ordered=TRUE)
matrixOfChoices = getall(I)
indexMatrix = as.matrix(gen.factorial(numberOfEntries, length(numberOfEntries), center=FALSE))
colnames(indexMatrix) <- c()

numCores = detectCores()-1
cl <- makeCluster(numCores)
registerDoParallel(cl, cores = numCores)
df <- data.frame(Model.Description=character(), Goodness.Of.Fit=numeric(), stringsAsFactors=FALSE) 

for (i in 1:nrow(matrixOfChoices))
{
	output = foreach(n = 1:nrow(indexMatrix), .packages=c("lavaan"), .combine=rbind) %dopar% 
	{
		runModel(modelData=data, numberOfFactors=numberOfLatents, outerModelDetails=matrixOfChoices[i,], innerModelDetails=myRows, indexDetails=indexMatrix[n,], modelBasics=modelBase)
	}
	df = rbind(df, output)
}
df = df[complete.cases(df),]
rownames(df) = c()
colnames(df) = c("Model.Description", "npar", "fmin", "chisq", "df", "pvalue", "baseline.chisq", "baseline.df", "baseline.pvalue", "cfi", "tli", "nnfi", "rfi", "nfi", "pnfi", "ifi", "rni", "logl", "unrestricted.logl", "aic", "bic", "ntotal", "bic2", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "rmsea.pvalue", "rmr", "rmr_nomean", "srmr", "srmr_bentler", "srmr_bentler_nomean", "srmr_bollen", "srmr_bollen_nomean", "srmr_mplus", "srmr_mplus_nomean", "cn_05", "cn_01", "gfi", "agfi", "pgfi", "mfi", "ecvi")
write.csv(df, file="output.csv", row.names=F, quote=FALSE)