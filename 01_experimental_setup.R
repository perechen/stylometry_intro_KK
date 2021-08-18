library(tidyverse)
library(tidytext)
library(stylo)


## utilizing stylo library to sample things
stylo_features_freq = function(path,feature,nsize,mfw,sample_n=10000,sample_method="no.sampling") {
  texts = load.corpus.and.parse(corpus.dir=path,
                                corpus.lang = "Other",
                                features = feature,
                                ngram.size = nsize,
                                sample.size = sample_n,
                                sampling = sample_method,
                                number.of.samples = 1)
  
  freq.list = make.frequency.list(texts, head=mfw)
  
  word.frequencies = make.table.of.frequencies(corpus=texts, features=freq.list)
  
}

## initialize corpus
dirs=c(list.dirs("estonian_1900_1930/", recursive = F))
corpus_list = tibble(corpus = c("est30"), path = dirs)

########
### 1. Authorship attribution: number of most frequent features
########

### initialize variables and experimental setup

mfw_vector = c(10, 30, 50, 70, 80, seq(100, 5000, by=100))

methods = c("delta","delta","delta", "delta", "delta")
distances = c("delta", "cosine", "cosine", "euclidean", "manhattan")
method_label = c("Burrows' Delta", "Cosine Delta", "Cosine", "Euclidean", "Manhattan")

## variables for holding results
matrices = vector(mode="list", length=nrow(corpus_list))
names(matrices) = corpus_list$corpus
results_df = NULL


for (c in 1:nrow(corpus_list)) {
  cmatrix = 0
  print(c("Now at:", corpus_list$path[c]))
  
  dtm = stylo_features_freq(corpus_list$path[c],
                            feature = "w",
                            nsize = 1,
                            mfw = 5000)
  rownames(dtm) = str_remove(rownames(dtm), "\\..*$")

  
  
  for (m in 1:length(methods)) {
    print(paste("currently performing:", method_label[m]))
    
    results_total = vector(length=length(mfw_vector))
    kappa = vector(length=length(mfw_vector))
    
    dtm_perform = dtm
    
    for (mfw in 1:length(mfw_vector)) {
      
      #dtm_perform = dtm
      
      ## scale matrix if this is a cosine delta
      if(method_label[m]  == "Cosine Delta") {
        dtm_perform = scale(dtm_perform)
      }
      

      cv_results = crossv(training.set = dtm_perform[,1:mfw_vector[mfw]],
                            cv.mode="leaveoneout",
                            classification.method = methods[m],
                            distance=distances[m])
      
      results_total[mfw] = sum(cv_results$y)/nrow(dtm_perform)
      cmatrix = cmatrix + cv_results$confusion_matrix
      cm=caret::confusionMatrix(cv_results$predicted, cv_results$expected)
      kappa[mfw] = cm$overall[2]
    }
    
    ## combine results
    df = tibble(mfw = mfw_vector, accuracy = results_total, kappa=kappa, method=method_label[m],corpus=corpus_list$corpus[c])
    
    results_df = rbind(results_df, df)
    
  }
  ## confusion matrices
  matrices[[c]] = cmatrix
}

save(matrices, file="conf_matrix.rda")

results_words = results_df
save(results_words, file="res_words.rda")



###########
### 2. Sample size testing
###########

sample_size = c(100, 500, 1000, 1500, seq(2000, 10000, by=1000))
iterations = 100

results_df = NULL

for(s in sample_size) {
  print(paste("Now at:", s, "samples"))
  mfw_vector = 200
  
  results_total = vector(length=iterations)
  kappa = vector(length=iterations)
  for (it in 1:iterations) {
    dtm = stylo_features_freq("estonian_1900_1930/soned/",
                              feature = "w",
                              nsize = 1,
                              sample_method = "random.sampling",
                              sample_n = s,
                              mfw = 2000) %>% scale()
    
    rownames(dtm) = str_remove(rownames(dtm), "\\..*$")
    
    
    cv_results = crossv(training.set = dtm[,1:mfw_vector],
                              cv.mode="leaveoneout",
                              classification.method = "delta",
                              distance="cosine")
        
    results_total[it] = sum(cv_results$y)/nrow(dtm) 
    cm=caret::confusionMatrix(cv_results$predicted, cv_results$expected)
    kappa[it] = cm$overall[2]
    
  }
  df = tibble(accuracy = results_total,sample_size=s,kappa=kappa, i=it)
  results_df = rbind(results_df, df)
  
}
    

results_samples = results_df

save(results_samples, file="results_samples.rda")



