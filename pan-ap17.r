# Including needed libraries
library(qdap)
library(XML)
library(tm)
library(splitstackshape)
library(caret)
library(ngram)

start.time <- Sys.time()

# **************************************************************************
# ***********************   PARAMETROS     *********************************
# **************************************************************************
# Preparacion de parametros, estaticos
lang <- "es"
path_training <- "C:/Users/ASUS/Documents/Master Big Data/Text Mining en Social Media/pan-ap17-bigdata/training"		# Your training path
path_test <- "C:/Users/ASUS/Documents/Master Big Data/Text Mining en Social Media/pan-ap17-bigdata/test"							# Your test path

# Preparacion de parametros, estaticos
n <- 150
k <- 4
r <- 2

#swlang lista predefinida de stop words que no queremos que esten para español pasar el string "es"
# En esta implementación el vocabulario se genera según el genero
# Probaremos a no eliminar acentos ni convertir a minusculas para "diferenciar" también el tipo de escritura
# 
param.eliminarAcentos <- FALSE
param.lowcase <- FALSE
param.punctuations <- TRUE
param.numbers <- TRUE
param.whitespaces <- TRUE
param.swlang <- "es"
param.swlist <- c("si","q","d","via")
param.verbose <- FALSE
param.genero <- ""

# **************************************************************************
# **************************************************************************
# **************************************************************************



# Auxiliar functions
# * GenerateVocabulary: Given a corpus (training set), obtains the n most frequent words
# * GenerateBoW: Given a corpus (training or test), and a vocabulary, obtains the bow representation

# GenerateVocabulary: Given a corpus (training set), obtains the n most frequent words


GenerateVocabulary <- function(path, n = 1000, eliminarAcentos = FALSE, lowcase = FALSE, punctuations = FALSE, numbers = FALSE, whitespaces = FALSE, swlang = "", swlist = "", verbose = TRUE, genero = "") {
  setwd(path)
  
  
  if (swlang!="")	{
    forbiWords <-  stopwords(swlang)
    
  }
  
  # Reading corpus list of files
  files = list.files(pattern="*.xml")
  
  # Reading the truth file
  truth <- read.csv("truth.txt", sep=":", header=FALSE)
  truth <- truth[,c(1,4,7)]
  colnames(truth) <- c("author", "gender", "variety")
  
  # Reading files contents and concatenating into the corpus.raw variable
  corpus.raw <- NULL
  i <- 0
  for (file in files) {
    xmlfile <- xmlTreeParse(file, useInternalNodes = TRUE)
    # Obtaining truth information for the current author
    author <- gsub(".xml", "", file)
    variety <- truth[truth$author==author,"variety"]
    gender <- truth[truth$author==author,"gender"]
    
    # Diferenciamos por genero
    if( genero != "") {
      if(gender == genero){
        valor <- xpathApply(xmlfile, "//document", function(x) xmlValue(x))
        #Hacemos las transformaciones en el momento de leer el documento para optimizar el problema.
        valor<-unlist(valor)
        #Transformacion a UTF-8
        print(valor)
        valor<-iconv( valor, from = "UTF-8",to="UTF-8")
        print(valor)
        
        if(eliminarAcentos){
          # print(typeof(corpus.preprocessed))
          valor <- iconv(valor, to="ASCII//TRANSLIT")

        }
        
        if (lowcase) {
          valor <- tolower(valor)
        }
        
        if (punctuations) {
          valor <- removePunctuation(valor)
        }
        
        if (numbers) {
          valor <- removeNumbers(valor)
        }
        
        if (whitespaces) {
          valor <- stripWhitespace(valor)
        }
        
        if (swlang!="")	{
          valor <- removeWords(valor, forbiWords )

        }
        
        if (swlist!="") {
          valor <- removeWords(valor, swlist)
          
        }
        
        
        corpus.raw <- c(corpus.raw, valor)
        i <- i + 1
        if (verbose) print(paste(i, " ", file))
      }
      
    }else{
      
      valor <- xpathApply(xmlfile, "//document", function(x) xmlValue(x))
      valor<-unlist(valor)
      #Transformacion a UTF-8
      valor<-iconv( valor, from = "UTF-8",to="UTF-8")
      
      if(eliminarAcentos){
        # print(typeof(corpus.preprocessed))
        valor <- iconv(valor, to="ASCII//TRANSLIT")
        
      }
      
      if (lowcase) {
        valor <- tolower(valor)
      }
      
      if (punctuations) {
        valor <- removePunctuation(valor)
      }
      
      if (numbers) {
        valor <- removeNumbers(valor)
      }
      
      if (whitespaces) {
        valor <- stripWhitespace(valor)
      }
      
      if (swlang!="")	{
        valor <- removeWords(valor, forbiWords)
        
      }
      
      if (swlist!="") {
        valor <- removeWords(valor, swlist)
        
      }
      corpus.raw <- c(corpus.raw, valor)
      i <- i + 1
      
    }
    
    
  }
  
  # Preprocessing the corpus
  corpus.preprocessed <- corpus.raw
  
  corpus.raw <- NULL


  
  # Generating the vocabulary as the n most frequent terms
  if (verbose) print("Generating frequency terms")
  corpus.frequentterms <- freq_terms(corpus.preprocessed,n)
  if (verbose) plot(corpus.frequentterms)
  
  return(corpus.frequentterms)

  
}

#Esta funcion no devuelve valores absolutos devuelve frecuencias relativas
# GenerateBoW: Given a corpus (training or test), and a vocabulary, obtains the bow representation
GenerateBoW <- function(path, vocabulary, n = 100000, eliminarAcentos = FALSE, lowcase = FALSE, punctuations = FALSE, numbers = FALSE, whitespaces = FALSE, swlang = "", swlist = "", class="gender", verbose = FALSE) {
  setwd(path)
  
  # Reading the truth file
  truth <- read.csv("truth.txt", sep=":", header=FALSE)
  truth <- truth[,c(1,4,7)]
  colnames(truth) <- c("author", "gender", "variety")
  
  i <- 0
  bow <- NULL
  # Reading the list of files in the corpus
  files = list.files(pattern="*.xml")
  for (file in files) {
    # Obtaining truth information for the current author
    author <- gsub(".xml", "", file)
    variety <- truth[truth$author==author,"variety"]
    gender <- truth[truth$author==author,"gender"]
    
    # Reading contents for the current author
    xmlfile <- xmlTreeParse(file, useInternalNodes = TRUE)
    valor <- xpathApply(xmlfile, "//document", function(x) xmlValue(x))

    valor<-unlist(valor)
    #Transformacion a UTF-8
    valor<-iconv( valor, from = "UTF-8",to="UTF-8")
    
    if(eliminarAcentos){
      # print(typeof(corpus.preprocessed))
      valor <- iconv(valor, to="ASCII//TRANSLIT")
      
    }
    
    if (lowcase) {
      valor <- tolower(valor)
    }
    
    if (punctuations) {
      valor <- removePunctuation(valor)
    }
    
    if (numbers) {
      valor <- removeNumbers(valor)
    }
    
    if (whitespaces) {
      valor <- stripWhitespace(valor)
    }
# 
#     if (swlang!="")	{
#       valor <- removeWords(valor, stopwords(swlang))
# 
#     }
# 
#     if (swlist!="") {
#       valor <- removeWords(valor, swlist)
# 
#     }
    
    #Quizas una funcion para CONTAR el numero de palabras dichas por vocabulario destacado de hombres o mujeres.
    
    # Building the vector space model. For each word in the vocabulary, it obtains the frequency of occurrence in the current author.
    line <- author
    freq <- freq_terms(valor, n)
    sumfreq <- sum(freq[2])
    for (word in vocabulary$WORD) {
      thefreq <- 0
      if (length(freq[freq$WORD==word,"FREQ"])>0) {
        
        thefreq <- freq[freq$WORD==word,"FREQ"]/sumfreq
      }
      line <- paste(line, ",", thefreq, sep="")
    }
    
    # Concatenating the corresponding class: variety or gender
    if (class=="variety") {
      line <- paste(variety, ",", line, sep="")
    } else {
      line <- paste(gender, ",", line, sep="")
    }
    
    # New row in the vector space model matrix
    bow <- rbind(bow, line)
    i <- i + 1
    
    
    if (verbose) {
      if (class=="variety") {
        print(paste(i, author, variety))
      } else {
        print(paste(i, author, gender))
      }
    }
  }
  
  return (bow)
}



# GENERATE VOCABULARY
vocabulary <- GenerateVocabulary(path_training, n,verbose = FALSE ,eliminarAcentos = param.eliminarAcentos, lowcase = param.lowcase, punctuations = param.punctuations, numbers = param.numbers, whitespaces = param.whitespaces, swlang = param.swlang, swlist = param.swlist)
#vocabulary <- GenerateVocabulary(path_training, n,verbose = FALSE)
end.time <- Sys.time()
time.taken <- end.time - start.time
print(as.numeric(time.taken,units = "secs"))

# GENDER IDENTIFICATION
#######################
# GENERATING THE BOW FOR THE GENDER SUBTASK FOR THE TRAINING SET
bow_training_gender <- GenerateBoW(path_training, vocabulary, class="gender",verbose = FALSE,eliminarAcentos = param.eliminarAcentos, lowcase = param.lowcase, punctuations = param.punctuations, numbers = param.numbers, whitespaces = param.whitespaces, swlang = param.swlang, swlist = param.swlist)
#bow_training_gender <- GenerateBoW(path_training, vocabulary, class="gender",verbose = FALSE)
end.time2 <- Sys.time()
time.taken <- end.time2 - end.time
print(as.numeric(time.taken,units = "secs"))

# PREPARING THE VECTOR SPACE MODEL FOR THE TRAINING SET
training_gender <- concat.split(bow_training_gender, "V1", ",")
training_gender <- cbind(training_gender[,2], training_gender[,4:ncol(training_gender)])
names(training_gender)[1] <- "theclass"

# Learning a SVM and evaluating it with k-fold cross-validation
train_control <- trainControl( method="repeatedcv", number = k , repeats = r)
model_SVM_gender <- train( theclass~., data= training_gender, trControl = train_control, method = "svmLinear")
#Kappa una medida para ver como de significativo es tu modelo contra un random
print(model_SVM_gender)

# Learning a SVM with the whole training set and without evaluating it
#train_control <- trainControl(method="none")
#model_SVM_gender <- train( theclass~., data= training_gender, trControl = train_control, method = "svmLinear")

# GENERATING THE BOW FOR THE GENDER SUBTASK FOR THE TEST SET 
bow_test_gender <- GenerateBoW(path_test, vocabulary, class="gender",verbose = FALSE,eliminarAcentos = param.eliminarAcentos, lowcase = param.lowcase, punctuations = param.punctuations, numbers = param.numbers, whitespaces = param.whitespaces, swlang = param.swlang, swlist = param.swlist)
#bow_test_gender <- GenerateBoW(path_test, vocabulary, class="gender",verbose = FALSE)
# Preparing the vector space model and truth for the test set
test_gender <- concat.split(bow_test_gender, "V1", ",")
truth_gender <- unlist(test_gender[,2])
test_gender <- test_gender[,4:ncol(test_gender)]

# Predicting and evaluating the prediction
pred_SVM_gender <- predict(model_SVM_gender, test_gender)
results <- confusionMatrix(pred_SVM_gender, truth_gender)


end.time <- Sys.time()
time.taken <- end.time - start.time

print(paste("Tiempo empleado :",  time.taken))
print(paste("Accuracy :",  results$overall["Accuracy"]))
print(paste("Kappa :",  results$overall["Kappa"]))
