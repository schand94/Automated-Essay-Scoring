
####    Section 1: Installing and initializing packages  ####
rm(list = ls())
install.packages('readxl')
install.packages("stringr", dependencies = TRUE)
install.packages('tokenizers')
install.packages('xgboost')
install.packages('dplyr')
install.packages('hunspell')
install.packages('stopwords')

install.packages('openNLP')
install.packages('udpipe')
install.packages('textreadr')
install.packages('standardize')
install.packages('caret')
install.packages('rpart')
install.packages('rpart.plot')
install.packages('e1071') 
install.packages('textstem')

library('textstem')
library(e1071)
library('rpart')
library('rpart.plot')
library('dplyr')

library('textreadr')
library('readxl')
library('dplyr')
library('stringr')
library('tokenizers')
library('hunspell')
library('caret')

library('udpipe')
library('xgboost')
library('stopwords')
library('standardize')

library(randomForest)
library(caret)
library(ggplot2)


#########################################################################
####    Section 2: Preprocess data    ####

####    Filter texts based on min length > 150, rating diff > 1 or 6    ####
#Function to round off marks
marks_scaled_fn <- function(x){
  if (x<0.5) {mark <- 0}
  else if(x>=0.5 & x<1){mark <- 1}
  else if(x>=1 & x<1.5){mark <- floor(x)}
  else if(x>=1.5 & x<2){mark <- ceiling(x)}
  else if(x>=2 & x<2.5){mark <- floor(x)}
  else if(x>=2.5 & x<3){mark <- ceiling(x)}
  else if(x>=3 & x<3.5){mark <- floor(x)}
  else if(x>=3.5 & x<4){mark <- ceiling(x)}
  else if(x>=4 & x<4.5){mark <- floor(x)}
  else if(x>=4.5 & x<5){mark <- ceiling(x)}
  else if(x>=5 & x<5.5){mark <- floor(x)}
  else if(x>=5.5 & x<6){mark <- ceiling(x)}
  else {mark <- x}
  return(mark)
}


#Read the data in
text <- read_excel('C:\\Users\\asus\\Desktop\\UIC\\Spring 2020\\IDS 560\\Capstone Project\\Capstone Project\\asap-aes\\Essay_Collection_10th_grade.xlsx')
View(text)
sample_essays <- read_excel('C:\\Users\\asus\\Desktop\\UIC\\Spring 2020\\IDS 560\\Capstone Project\\Capstone Project\\Grammar Final\\CTA Samples (with grammar scores).xlsx')
View(sample_essays)



#Remove essays that:
#1. Have a word count of <150 (arbitrarily chosen)
#2. Have a difference in ratings of >1(6 pt scale) or >6 (30 pt scale)
#3. Have a score of 1 (after choosing the minimum of rater's scores)

#Remove texts having word count <150
tokenized_words <- tokenize_words(text$essay)
len <- lapply(tokenized_words,lengths)
len <- lapply(len,sum)

text<- text[which(len>150),]

#Remove texts having difference in score > 6 for essay set 8 and >1 for other sets
text$Diff <- abs(text$rater1_domain1 - text$rater2_domain1)
text <- text[!((text$essay_set==8&text$Diff>6)|(text$essay_set!=8&text$Diff>1)),]
View(text)

#Remove "diff" column
text<- text[,!tolower(names(text)) %in% "diff"]
View(text)

#Put min(rater) scores into a "marks_final" variable

text$Min_Marks <-mapply(min, text$rater1_domain1, text$rater2_domain1)
View(text)
text$Marks_Scaled <- ifelse(text$essay_set==8, text$Min_Marks/5,text$Min_Marks)
View(text)
text$Marks_Final <- mapply(marks_scaled_fn,text$Marks_Scaled)
View(text)
text <- text[text$Marks_Final>1,]
View(text)

marks_final <- text$Marks_Final

#Look at the number of essays for each score
count_per_score <- data.frame(table(marks_final))
View(count_per_score)
sum(count_per_score$Freq) #To verify all scores are entered correctly

#Tokenize text into 
#1. Sentence tokens and
#2. Word tokens
temp1 <- tokenize_sentences(text$essay)
sample_sentence_tokens <- tokenize_sentences(sample_essays$essay)


sentence_words <- lapply(temp1, tokenize_words)
sample_word_tokens <- lapply(sample_sentence_tokens,tokenize_words) 


####    Calculate number of sentences & Avg Sentence Length   ####

num_sentences <- list()
sample_num_sentences <- list()



for (i in 1:length(sentence_words)){
  num_sentences[i] <- length(sentence_words[[i]])
}

for(i in 1:length(sample_word_tokens)){
  sample_num_sentences[i] <- length(sample_word_tokens[[i]])
}



# Calculate average sentence length

avg_sentence_length <- list()
sample_avg_sentence_length <- list()
 
for (i in 1:length(sentence_words)){
  avg_sentence_length[i] <- sum(lengths(sentence_words[[i]]))/length(sentence_words[[i]])
}
length(sample_word_tokens)

for(i in 1:length(sample_word_tokens)){
  
  sample_avg_sentence_length[i] <- sum(lengths(sample_word_tokens[[i]]))/length(sample_word_tokens[[i]])
}




#Convert to lower case

lower_text <- tolower(text$essay)
lower_text <- gsub("@\\w+ *", "", lower_text)

sample_lower_text <- tolower(sample_essays$essay)
sample_lower_text <- gsub("@\\w+ *", "", sample_lower_text)


####    Check for spellings, get Spell Score, Essay Length   ####

#Function to score text
score <- function(essay){
  #Check for mistakes and print
  bad <- hunspell(essay, ignore = list_of_words)
  
  tot_mistakes <- sum(lengths(bad))
  
  
  #Tokenize text into words
  all_words <- hunspell_parse(essay)
  
  len <- sum(lengths(all_words))
  x <- (tot_mistakes/len)*100
  
  if(x==0){
    res = 6
    
  }
  else if(x<=1.5)
  {
    res = 5
  }

  else if(x<=2.5){
    res = 4
  }
  else if(x<=3.5){
    res = 3
  }
  else if(x<=4.5){
    res = 2
  }
  else {res=1}
    return(c(res,x, len,tot_mistakes))
}



#List of words to ignore
list_of_words <- c('CTA', 'CFD', 'CPD','servicer')

essay_details <- lapply(lower_text,score)
sample_essay_details <- lapply(sample_lower_text, score)




spell_mist_percent = list()
spell_score  = list()
essay_lengths <- list()

sample_spell_mist_percent <- list()
sample_spell_score <- list()
sample_essay_lengths <- list()


for(i in 1:length(essay_details)){
  spell_mist_percent[i] <- essay_details[[i]][2]
  spell_score[i] <- essay_details[[i]][1]
  essay_lengths[i] <- essay_details[[i]][3]
}

for(i in 1:length(sample_essay_details)){
  sample_spell_mist_percent[i] <- sample_essay_details[[i]][2]
  sample_spell_score[i] <- sample_essay_details[[i]][1]
  sample_essay_lengths[i] <- sample_essay_details[[i]][3]
}

####    Remove stop words, extra spaces, get unique # of tokens ####
#Remove everything that is not a number or letter
temp <- str_replace_all(lower_text,"[^a-zA-Z\\s]", " ")
sample_temp <- str_replace_all(sample_lower_text,"[^a-zA-Z\\s]", " ")
class(temp)




#Remove all extra white space
temp <- str_replace_all(temp,"[\\s]+", " ")
sample_temp <- str_replace_all(sample_temp,"[\\s]+", " ")

#Split it
temp_new <- tokenize_words(temp)
sample_temp_new <- tokenize_words(sample_temp)




#Get number of unique tokens in each essay
sw <- stopwords("en")

stop_w <- function(x){
  if((x %in% sw) == FALSE){
    return(x)
  }
}

unique_token = temp_new
sample_unique_token <- sample_temp_new

#Get unique words
unique_token <- lapply(unique_token,unique)
sample_unique_token <- lapply(sample_unique_token, unique)

#Remove stop words
unique_token_wo_sw <- lapply(unique_token, function(x){lapply(x,stop_w)})
unique_token_wo_sw <- lapply(unique_token_wo_sw,function(x){x[-which(sapply(x,is.null))]})

sample_unique_token_wo_sw <- lapply(sample_unique_token, function(x){lapply(x,stop_w)})

sample_unique_token_wo_sw <- lapply(sample_unique_token_wo_sw, function(x){x[-which(sapply(x,is.null))]})




check_length <- function(x){
  if(nchar(x)>1){
    return(x)
    
  }
}

#Reassign final values to 'temp_new'
temp_new <- lapply(unique_token_wo_sw,function(x){lapply(x,check_length)})
sample_temp_new <- lapply(sample_unique_token_wo_sw, function(x){lapply(x,check_length)})

#Number of "unique" words in essay
num_words <- list()
sample_num_words <- list()

for (i in 1:length(temp_new)){
  num_words[i] <- length(temp_new[[i]])
}

for(i in 1:length(sample_temp_new)){
  sample_num_words[i] <- length(sample_temp_new[[i]])
}

#Find average word length per essay
avg_word_length <- list()
for(i in 1:length(temp_new)){
  avg_word_length[i]<- sum(nchar(temp_new[[i]]))/length(temp_new[[i]])
}

sample_avg_word_length <- list()
for(i in 1:length(sample_temp_new)){
  sample_avg_word_length[i] <- sum(nchar(sample_temp_new[[i]])/length(sample_temp_new[[i]]))
}

####    POS Tagging   ####

#Get number of nouns, verbs & adjectives in text
udmodel <- udpipe_download_model(language = "english")
udmodel <- udpipe_load_model(udmodel$file_model)


#Function to identify POS
num_POS <- function(essay){
  x <- udpipe(essay, object=udmodel)
  noun <- x %>% select(token) %>% filter(x$xpos=='NN'|x$xpos=='NNS' )
  verb <- x %>% select(token) %>% filter(grepl("VB",x$xpos)&x$upos!='AUX')
  adj <- x %>% select(token) %>% filter(x$xpos=='JJ' )
  n_noun <- length(noun$token)
  n_verb <- length(verb$token)
  n_adj <- length(adj$token)
  mylist <- c(n_noun, n_verb, n_adj)
  return(mylist)
}


#NOTE: The two commands below take a LONG time to run

sample_POS_Tag  <- lapply(sample_temp,num_POS)
length(sample_POS_Tag)
#POS_Tag <- lapply(temp, num_POS)
POS_Tag <- read.csv('C:\\Users\\asus\\Desktop\\UIC\\Spring 2020\\IDS 560\\Capstone Project\\Capstone Project\\Grammar Final\\Code Latest Mar 10, 2020\\POS_Tag.csv')


#Remove index column 
POS_Tag <- POS_Tag[-1]

length(POS_Tag) #Should be 4081


####    Topic Pertinence    ####

#Function to get the number of features for each topic
get_feat <- function(essay,lemma){
  
  feat_1_1 <- sum(str_count(tolower(essay), tolower(leadership_terms)))
  feat_1_2 <- sum(unique(lemmatize_words(lemma)) %in% tolower(leadership_terms_lemma))
  feat_1 <- max(feat_1_1,feat_1_2)

  #Feature 2 assesses whether an opinion is expressed
  feat_2_1 <- sum(str_count(tolower(essay),tolower(opinion_terms)))
  feat_2_2 <- sum(unique(lemmatize_words(lemma)) %in% opinion_terms_lemma)
  feat_2 <- max(feat_2_1, feat_2_2)
  
  #Feature 3 addresses communication and expectations
  feat_3_1 <- sum(str_count(tolower(essay),tolower(comm_terms)))
  feat_3_2 <- sum(unique(lemmatize_words(lemma)) %in% tolower(comm_terms_lemma))
  feat_3 <- max(feat_3_1, feat_3_2)
  
  #Feature 4 addresses developing staff
  
  feat_4_1 <- sum(str_count(tolower(essay), tolower(staff_dev_terms)))
  feat_4_2 <- sum(unique(lemmatize_words(lemma)) %in% tolower(staff_dev_terms_lemma))
  feat_4 <- max(feat_4_1 , feat_4_2)
  
  
  # Feature 5 addresses relationship with staff 
  feat_5_1 <- sum(str_count(tolower(essay),tolower(staff_rel_terms)))
  feat_5_2 <- sum(unique(lemmatize_words(lemma)) %in% staff_rel_terms_lemma)
  feat_5 <- max(feat_5_1 , feat_5_2)
  
  feat <- c(feat_1,feat_2, feat_3, feat_4, feat_5)
  
  return(feat)
  
}


#Function to score essays 
topic_score <- function(feat_list){
  pert_score_1 <- if(feat_list[1] > 0){
    (1.2)
  }
  pert_score_2 <- if(feat_list[2] > 0){
    (1.2)
  }
  pert_score_3 <- if(feat_list[3] > 0){
    (1.2)
  }
  pert_score_4 <- if(feat_list[4] > 0){
    (1.2)
  }
  pert_score_5 <- if(feat_list[5] > 0){
    (1.2)
  }
  
  final_pert_score <- sum(pert_score_1, pert_score_2, pert_score_3, pert_score_4, pert_score_5)
  
  return(final_pert_score)
}





#List of terms across different metrics of scoring
leadership_terms <- c('leader',
                      'leadership',
                      'manage',
                      'manager',
                      'management',
                      'leadership style',
                      'style',
                      'boss')

leadership_terms_lemma <- unique(lemmatize_words(leadership_terms))

opinion_terms <- c('l believe',
                   'I think',
                   'I feel',
                   'my opinion',
                   'should',
                   'must',
                   'has to',
                   'have to',
                   'cannot',
                   'can not',
                   'good',
                   'bad',
                   'strong',
                   'weak')

opinion_terms_lemma <- unique(lemmatize_words(opinion_terms))

comm_terms <- c('communicate',
                'expectations',
                'expectation',
                'clear',
                'clearly',
                'information',
                'succeed',
                'success')
comm_terms_lemma <- unique(lemmatize_words(comm_terms))


staff_dev_terms <- c('develop',
                     'grow',
                     'foster',
                     'development',
                     'nurture',
                     'help',
                     'assist')
staff_dev_terms_lemma <- unique(lemmatize_words(staff_dev_terms))

staff_rel_terms <- c('relationship',
                     'relationships',
                     'interpersonal',
                     'personal',
                     'open',
                     'team',
                     'staff',
                     'employee')
staff_rel_terms_lemma <- unique(lemmatize_words(staff_rel_terms))

sample_topic_score <- list()


for(i in 1:length(sample_lower_text)){
  
  sample_topic_score[i] <- topic_score(get_feat(sample_lower_text[i], sample_temp_new[i]))
}



####    Put all data into a final dataset before modeling ####

rm(final_dataset)
#Read final dataset & Remove first column
#final_dataset <- read.csv('final_dataset.csv')
#final_dataset <- final_dataset[-1]

####    Do NOT run if csv is available    ####
#Collating all data into one data frame
final_dataset <- data.frame(Rater_Score = integer(),
                            Num_of_words = integer(),
                            Num_Sentences = integer(),
                            Avg_word_Len = double(),
                            Avg_sent_length = double(),
                            noun_percent = double(),
                            verb_percent = double(),
                            adj_percent = double(),
                            spell_mistake_percent = double(),
                            Grammar_Score = double()
                            
)



for (i in 1:length(temp)){
  final_dataset <- rbind(final_dataset,c(marks_final[i],num_words[i],num_sentences[i], avg_word_length[i], avg_sentence_length[i],POS_Tag[[i]][1]/essay_lengths[[i]]*100,POS_Tag[[i]][2]/essay_lengths[[i]]*100 ,POS_Tag[[i]][3]/essay_lengths[[i]]*100, spell_mist_percent[i],text$grammar_score[i]))
}

colnames(final_dataset) <- c('Examiner_Score', 'Essay_Length','Num_Sentences','Avg_Word_Length', 'Avg_Sentence_Length','Noun_Percentage','Verb_Percentage', 'Adjective_Percentage', 'Spell_Mistake_Percent','Grammar_Score')

final_dataset <- unique(final_dataset)
final_dataset$Grammar_Score <- final_dataset$Grammar_Score*1000000


####    1. View Kaggle essay details ;  2. Assign sample dataset ; 3. Summary of Kaggle numbers   ####
View(final_dataset)




rm(sample_final_dataset)
sample_final_dataset <- data.frame(Num_of_words = integer(),
                                   Num_Sentences = integer(),
                                   Avg_word_Len = double(),
                                   Avg_sent_length = double(),
                                   noun_percent = double(),
                                   verb_percent = double(),
                                   adj_percent = double(),
                                   spell_mistake_percent = double(),
                                   Grammar_Score = double()
                                   
)

for(i in 1:length(sample_temp)){
  sample_final_dataset <- rbind(sample_final_dataset, c(sample_num_words[i],sample_num_sentences[i], sample_avg_word_length[i], sample_avg_sentence_length[i], sample_POS_Tag[[i]][1]/sample_essay_lengths[[i]]*100,sample_POS_Tag[[i]][2]/sample_essay_lengths[[i]]*100,sample_POS_Tag[[i]][3]/sample_essay_lengths[[i]]*100, sample_spell_mist_percent[i], sample_essays$ll_value[i] ))
}

colnames(sample_final_dataset) <- c('Essay_Length','Num_Sentences','Avg_Word_Length', 'Avg_Sentence_Length','Noun_Percentage','Verb_Percentage', 'Adjective_Percentage', 'Spell_Mistake_Percent','Grammar_Score')

sample_final_dataset <- unique(sample_final_dataset)

View(sample_final_dataset)


#Get summary for each score
rez <- final_dataset %>% group_by(Examiner_Score) %>% summarize(mean(Essay_Length),mean(Avg_Word_Length),mean(Num_Sentences), mean(Avg_Sentence_Length),mean(Noun_Percentage),mean(Verb_Percentage),mean(Adjective_Percentage),mean(Spell_Mistake_Percent),mean(Grammar_Score),n())
View(rez)


#########################################################################
####    Section3: Modeling    ####

####    Sampleing: Stratified/Regular   ####
# Creating a duplicate dataset for modeling
full_data <- final_dataset[!is.na(final_dataset$Examiner_Score),]
View(full_data)
#write.csv(full_data, file = "Full_data.csv")




sample_type <- readline(prompt = "Enter 1: Stratified sampling ; 2: Regular Sampling: ")



rm(list = ls(pattern = "train"))
rm(list = ls(pattern = "test"))

if(sample_type==1){
# Stratified Sampling
flag=0
set.seed(3721)

for(i in 1:6){
  nam <- paste("fd",i,sep = "_")
  full_var <- assign(nam,full_data[full_data$Examiner_Score==i,])

  
  smp_size = floor(0.80*nrow(full_var))
  set.seed(3721)
  
  nam2 = paste("train",i,sep = "_")
  ind <- sample(seq_len(nrow(full_var)),size = smp_size)


  
  train_var <- assign(nam2,full_var[ind,])

  nam3 <- paste("test",i,sep="_")
  test_var <- assign(nam3,full_var[-ind,])
  


  if(nrow(train_var)>0 && flag==0){
    flag=1
    train_full <- train_var
    test_full <- test_var
    
  }
  else if(flag==1){
    train_full <- rbind(train_full, train_var)
    test_full <- rbind(test_full,test_var)
                  }
  
}

#train_full <- cbind(unlist(train_ind),train_full)



train_ind <- as.integer(rownames(train_full))
train_full <- cbind(train_ind,train_full)
View(train_full)

test_ind <- as.integer(rownames(test_full))
test_full <- cbind(test_ind,test_full)
View(test_full)

set.seed(3721)
rows <- sample(nrow(train_full))
train_full <- train_full[rows,]

View(train_full)
train_ind <- train_full[,1]
train_full<- train_full[,-1]

rows <- sample(nrow(test_full))
test_full <- test_full[rows,]


test_ind <- test_full[,1]
test_full <- test_full[,-1]

}


if(sample_type==2){

print("Regular Sampling")
reg <- 0
smp_size <- floor(0.80*nrow(full_data))
set.seed(3721)


#Preparing "train" and "test" data
smp_size <- floor(0.80*nrow(full_data))
set.seed(3721)


train_ind <- sample(seq_len(nrow(full_data)), size = smp_size)
train_full <- full_data[train_ind,]
test_full <- full_data[-train_ind,]
}


#Assign train, test labels
train_label <- train_full[,1]
test_label <- test_full[,1]


####    Modeling using XGBOOST    ####


train_matrix <- data.matrix(train_full[,-1])

colnames(train_matrix)
View(train_matrix)


test_matrix <- data.matrix(test_full[,-1])
View(test_matrix)

#1. Cross validation to choose the best parameters
#NOTE: This takes a LONG time to run
#Skip to "Default Values" to avoid running this

cv_res <- data.frame(n_rounds = integer(),
                     max_depth = integer(),
                     eta = integer(),
                     nthread = integer(),
                     test_rmse_mean = double())


#Choose a range of values for each parameter and find MSE for "EVERY" combination of param values
#This means, there are a total of 10*3*4*4 = "480" possible combinations to choose from
nr <- c(50,70,90,100,120,140,150,170,190,200)
max_depth <- c(1,2,3)
eta <- c(0.1,0.3,0.5,0.7)
nthread <- c(1,2,3,4)

for(i in 1:length(nr)){
  for(j in 1:length(max_depth)){
    for(k in 1:length(eta)){
      for(l in 1:length(nthread)){
  cv <- xgb.cv(data = train_matrix, nfold = 5,label = train_label, max_depth=max_depth[j],eta = eta[k], 
               nthread = nthread[l], nrounds = nr[i], verbose = FALSE)

  cv_res <- rbind(cv_res,c(nr[i],max_depth[j],eta[k],nthread[l],mean(cv[[4]][,4]$test_rmse_mean)))
}
  }
    }
      }
colnames(cv_res) <- c('nrounds','max_depth','eta','nthread','Test_MSE')

write.csv(cv_res,"XGBOOST_CV_RESULTS.csv")
View(cv_res)

cv_res[which.min(cv_res$Test_MSE),]
nrounds <- cv_res[which.min(cv_res$Test_MSE),][[1]]
max_depth <- cv_res[which.min(cv_res$Test_MSE),][[2]]
eta <- cv_res[which.min(cv_res$Test_MSE),][[3]]
nthread <- cv_res[which.min(cv_res$Test_MSE),][[4]]

#Default Values (Based on previous instance of running above loop)
nrounds <- 150
max_depth <- 2
eta <- 0.5
nthread <- 4




#2. Using the parameters chosen from above results

bst <- xgboost(data = train_matrix, train_label,
               max_depth = max_depth, eta = eta, nthread = nthread, nrounds = nrounds)


#Check variable importance
xgb.importance(colnames(train), model = bst)

#Predict scores of test data
pred <- predict(bst, test_matrix)

test_prediction <- pred

#Find Accuracy and Incorrectly predicted scores from the model alone
#Accuracy due to the model
length(test_prediction[round(test_prediction)==test_label])/length(test_label)*100


#Incorrect predictions from the model
incorrect_ans <- data.frame(test_label[test_label!=round(test_prediction)],test_prediction[test_label!=round(test_prediction)], test_label[test_label!=round(test_prediction)]-test_prediction[test_label!=round(test_prediction)])
colnames(incorrect_ans) <- c('test_label','test_prediction','difference')
View(incorrect_ans)
sum(incorrect_ans$difference)

#Number of incorrect predictions across scores
incorrect_rez <- incorrect_ans %>% group_by(incorrect_ans$test_label) %>% summarize(n())
incorrect_rez


#Getting cumulative score
rm(final_score)
final_score <- data.frame(Spelling_Score = double(),
                          Model_Score = double(),
                          Final_Score = double(),
                          Actual_Score = double(),
                          Score_Diff = double())




final_score<- cbind(unlist(spell_score[as.integer(rownames(test_full))]),unlist(test_prediction),(0.3*unlist(spell_score[as.integer(rownames(test_full))])+0.7*unlist(test_prediction)), unlist(test_label), (0.3*unlist(spell_score[as.integer(rownames(test_full))])+0.7*unlist(test_prediction))- unlist(test_label))
class(final_score)
final_score <- as.data.frame(final_score)
colnames(final_score) <- c('Spelling_Score', 'Model_Score', 'Final_Score','Actual_Score', 'Score_Diff')
View(final_score)


#Get sum of differenes to know "cumulatively" how much the model makes a mistake by
score_diff <- final_score$Final_Score - final_score$Actual_Score
sum(score_diff)


#Predict scores of sample essays
sample_matrix <- data.matrix(sample_final_dataset)
sample_pred <- predict(bst, sample_matrix)
sample_pred

#Total Score
#Weightage
w_spell <- 0.1
w_model <- 0.6
w_topic <- 0.15
w_grammar <- 0.15

sample_final_score <- w_spell*unlist(sample_spell_score) + w_model*sample_pred + w_topic*unlist(sample_topic_score) + w_grammar*sample_essays$grammar_score
sample_final_score

rm(sample_final_score_dataset)
sample_final_score_dataset <- data.frame(Spell_Score <- double(),
                                         Model_Score <- double(),
                                         Topic_Pert_Score <- double(),
                                         Grammar_Score <- double(),
                                         Final_Weighted_Score <- double()
                                         )
sample_final_score_dataset <- cbind(unlist(sample_spell_score), sample_pred, unlist(sample_topic_score),sample_essays$grammar_score,w_spell*unlist(sample_spell_score) + w_model*sample_pred + w_topic*unlist(sample_topic_score) + w_grammar*sample_essays$grammar_score)
colnames(sample_final_score_dataset) <- c('Spell Score', 'Model Score', 'Topic Pert Score', 'Grammar Score', 'Final Weighted Score')
View(sample_final_score_dataset)
#Get essay with the "max" +ve and -ve difference between actual score and overall calculated score
write.csv(sample_final_score_dataset,'XgBoost_modified_Weights.csv')

#1. Positive difference (ie the actual score is "lower" than the overall score)
ind <- which.max(final_score$Score_Diff)
ind
rownames(test_matrix)[ind]
#Print original essay
text$essay[as.integer(rownames(test_matrix)[ind])]

#Print the list of unique words (without stop words) 
#along with the final dataset and compare :-
# a) Examiner score 
# b) Essay length
# c) First few words of the actual essay with unique tokens
final_score$Actual_Score[ind]
text[as.integer(rownames(test_matrix)[ind]),]

length(as.vector(unlist(unique_token_wo_sw[as.integer(rownames(test_matrix))[ind]])))
final_dataset[as.integer(rownames(test_matrix))[ind],]
substr(text$essay[as.integer(rownames(test_matrix))[ind]],1,100)
as.vector(unlist(unique_token_wo_sw[as.integer(rownames(test_matrix))[ind]]))
hunspell(lower_text[as.integer(rownames(test_matrix))[ind]])

# Negative Difference (ie the actual score is "higher" than the overall score)
ind <- which.min(final_score$Score_Diff)
ind


#Print original essay
text$essay[as.integer(rownames(test_matrix)[ind])]

#Print the list of unique words (without stop words) 
#along with the final dataset and compare :-
# a) Examiner score 
# b) Essay length
# c) First few words of   the actual essay with unique tokens
final_score$Actual_Score[ind]
text[as.integer(rownames(test_matrix)[ind]),]
length(as.vector(unlist(unique_token_wo_sw[as.integer(rownames(test_matrix))[ind]])))
final_dataset[as.integer(rownames(test_matrix))[ind],]
substr(text$essay[as.integer(rownames(test_matrix))[ind]],1,100)
as.vector(unlist(unique_token_wo_sw[as.integer(rownames(test_matrix))[ind]]))
hunspell(lower_text[as.integer(rownames(test_matrix))[ind]])









####    Modeling with RF    ####
#Check linear regression assumptions. IS target normally distributed?

#LAsso
#Ridge


# Random forest
# 

ggplot(full_data, aes(Examiner_Score)) + geom_density(fill="blue")
#The distribution is clearly multimodal.  Linear Regression is not an option. 
#Could try multinomial regression, but will be treating the target variable as categorical.
#Seems like regression not best choice for this data.


#Start decision tree on full set  to visualize decisions

#making my own dataset

new_set <- full_data
new_set <- new_set %>%
  rename("score" = "Examiner_Score")

fit <- rpart(score~.,
             method="anova", data= new_set)

printcp(fit) # display the results
plotcp(fit) # visualize cross-validation results
summary(fit) # detailed summary of splits

# plot tree show number of obs. at each node and % of obs. 
rpart.plot(fit, uniform=TRUE,
     main="Regression Tree for Examiner Score ", extra=101)
#print rules
rules <- rpart.rules(fit, cover = TRUE)
rules                            

### Test Random Forest Accuracy

#Use same train and test as above from xgb

train_rf <- full_data[train_ind,]


test_rf <- full_data[-train_ind,]


View(train_rf)

  

set.seed(3721)
start <- proc.time()[3]
model.rf <- train(Examiner_Score ~ .,
                  data = train_rf,
                  method = "rf",
                  ntree = 50) # How many trees to grow in total?
              
end <- proc.time()[3]
print(paste("This took ", round(end-start,digits = 1), " seconds", sep = ""))
print(model.rf)
start <- proc.time()[3]
View(train_rf)
#Use 500 trees instead of 50 (THIS TAKES 20 MINUTES on JL MACHINE with 500 TREES)
model.rf_2 <- train(Examiner_Score ~ .,
                  data = train_rf,
                  method = "rf",
                  ntree = 501) # How many trees to grow in total?

end <- proc.time()[3]
print(paste("This took ", round(end-start,digits = 1), " seconds", sep = ""))
print(model.rf_2)
#more trees reduced RMSE to .5873

varImp(model.rf_2)

#Predict scores of test data
pred <- predict(model.rf_2, test_full[-1])

test_prediction <- pred


#Find Accuracy and Incorrectly predicted scores from the model alone
#Accuracy due to the model
length(test_prediction[round(test_prediction)==test_label])/length(test_label)*100

#Incorrect predictions from the model
incorrect_ans <- data.frame(test_label[test_label!=round(test_prediction)],test_prediction[test_label!=round(test_prediction)], test_label[test_label!=round(test_prediction)]-test_prediction[test_label!=round(test_prediction)])
colnames(incorrect_ans) <- c('test_label','test_prediction','difference')
View(incorrect_ans)
sum(incorrect_ans$difference)



rm(final_score)
final_score <- data.frame(Spelling_Score = double(),
                          Model_Score = double(),
                          Final_Score = double(),
                          Actual_Score = double(),
                          Score_Diff = double())




final_score<- cbind(unlist(spell_score[as.integer(rownames(test_full))]),unlist(test_prediction),(0.3*unlist(spell_score[as.integer(rownames(test_full))])+0.7*unlist(test_prediction)), unlist(test_label), (0.3*unlist(spell_score[as.integer(rownames(test_full))])+0.7*unlist(test_prediction))- unlist(test_label))
class(final_score)
final_score <- as.data.frame(final_score)
colnames(final_score) <- c('Spelling_Score', 'Model_Score', 'Final_Score','Actual_Score', 'Score_Diff')
View(final_score)


#Sample 
sample_pred <- predict(model.rf_2, sample_final_dataset)

sample_pred



#Total Score
#Weightage
w_spell <- 0.1
w_model <- 0.6
w_topic <- 0.15
w_grammar <- 0.15

sample_final_score <- w_spell*unlist(sample_spell_score) + w_model*sample_pred + w_topic*unlist(sample_topic_score) + w_grammar*sample_essays$grammar_score
sample_final_score

rm(sample_final_score_dataset)
sample_final_score_dataset <- data.frame(Spell_Score <- double(),
                                         Model_Score <- double(),
                                         Topic_Pert_Score <- double(),
                                         Grammar_Score <- double(),
                                         Final_Weighted_Score <- double()
)
sample_final_score_dataset <- cbind(unlist(sample_spell_score), sample_pred, unlist(sample_topic_score),sample_essays$grammar_score,w_spell*unlist(sample_spell_score) + w_model*sample_pred + w_topic*unlist(sample_topic_score) + w_grammar*sample_essays$grammar_score)
colnames(sample_final_score_dataset) <- c('Spell Score', 'Model Score', 'Topic Pert Score', 'Grammar Score', 'Final Weighted Score')
View(sample_final_score_dataset)
write.csv(sample_final_score_dataset, "RF_Modified_Weights.csv")


#Get essay with the "max" +ve and -ve difference between actual score and overall calculated score

#1. Positive difference (ie the actual score is "lower" than the overall score)
max(final_score$Score_Diff)
ind <- which.max(final_score$Score_Diff)
ind
rownames(test_matrix)[ind]
#Print original essay
text$essay[as.integer(rownames(test_matrix)[ind])]

#Print the list of unique words (without stop words) 
#along with the final dataset and compare :-
# a) Examiner score 
# b) Essay length
# c) First few words of the actual essay with unique tokens
final_score$Actual_Score[ind]
text[as.integer(rownames(test_matrix)[ind]),]

length(as.vector(unlist(unique_token_wo_sw[as.integer(rownames(test_matrix))[ind]])))
final_dataset[as.integer(rownames(test_matrix))[ind],]
substr(text$essay[as.integer(rownames(test_matrix))[ind]],1,100)
as.vector(unlist(unique_token_wo_sw[as.integer(rownames(test_matrix))[ind]]))
hunspell(lower_text[as.integer(rownames(test_matrix))[ind]])

# Negative Difference (ie the actual score is "higher" than the overall score)
ind <- which.min(final_score$Score_Diff)
ind
rownames(test_full)[ind]

#Print original essay
text$essay[as.integer(rownames(test_matrix)[ind])]

#Print the list of unique words (without stop words) 
#along with the final dataset and compare :-
# a) Examiner score 
# b) Essay length
# c) First few words of   the actual essay with unique tokens
final_score$Actual_Score[ind]
text[as.integer(rownames(test_matrix)[ind]),]
length(as.vector(unlist(unique_token_wo_sw[as.integer(rownames(test_matrix))[ind]])))
final_dataset[as.integer(rownames(test_matrix))[ind],]
substr(text$essay[as.integer(rownames(test_matrix))[ind]],1,100)
as.vector(unlist(unique_token_wo_sw[as.integer(rownames(test_matrix))[ind]]))
hunspell(lower_text[as.integer(rownames(test_matrix))[ind]])













#####   Modeling with SVM   #####
View(train_full)
classifier <- svm(formula=Examiner_Score~., 
                  data=train_full, 
                  type='eps-regression',
                  kernel='radial')


View(test_full)
test_prediction <- predict(classifier, test_full[-1])
View(test_prediction)


View(sample_final_dataset)

#cm <- table(test_prediction, test_label$Examiner_Score)
#confusionMatrix(cm)
test_prediction <- as.numeric(as.character(test_prediction))
test_prediction

#Accuracy
length(test_prediction[round(test_prediction)==test_label])/length(test_label)*100

#Incorrect predictions from the model
incorrect_ans <- data.frame(test_label[test_label!=round(test_prediction)],test_prediction[test_label!=round(test_prediction)], test_label[test_label!=round(test_prediction)]-test_prediction[test_label!=round(test_prediction)])
colnames(incorrect_ans) <- c('test_label','test_prediction','difference')
View(incorrect_ans)
sum(incorrect_ans$difference)



#Getting cumulative score
rm(final_score)
final_score <- data.frame(Spelling_Score = double(),
                          Model_Score = double(),
                          Final_Score = double(),
                          Actual_Score = double(),
                          Score_Diff = double())



final_score<- cbind(unlist(spell_score[as.integer(rownames(test_full))]),unlist(test_prediction),(0.3*unlist(spell_score[as.integer(rownames(test_full))])+0.7*unlist(test_prediction)), unlist(test_label), (0.3*unlist(spell_score[as.integer(rownames(test_full))])+0.7*unlist(test_prediction))- unlist(test_label))
class(final_score)
final_score <- as.data.frame(final_score)
colnames(final_score) <- c('Spelling_Score', 'Model_Score', 'Final_Score','Actual_Score', 'Score_Diff')
View(final_score)





View(sample_final_dataset)
sample_pred <- predict(classifier, sample_final_dataset)

sample_pred


#Total Score
#Weightage
w_spell <- 0.1
w_model <- 0.6
w_topic <- 0.15
w_grammar <- 0.15

sample_final_score <- w_spell*unlist(sample_spell_score) + w_model*sample_pred + w_topic*unlist(sample_topic_score) + w_grammar*sample_essays$grammar_score
sample_final_score

rm(sample_final_score_dataset)
sample_final_score_dataset <- data.frame(Spell_Score <- double(),
                                         Model_Score <- double(),
                                         Topic_Pert_Score <- double(),
                                         Grammar_Score <- double(),
                                         Final_Weighted_Score <- double()
)
sample_final_score_dataset <- cbind(unlist(sample_spell_score), sample_pred, unlist(sample_topic_score),sample_essays$grammar_score,w_spell*unlist(sample_spell_score) + w_model*sample_pred + w_topic*unlist(sample_topic_score) + w_grammar*sample_essays$grammar_score)
colnames(sample_final_score_dataset) <- c('Spell Score', 'Model Score', 'Topic Pert Score', 'Grammar Score', 'Final Weighted Score')
View(sample_final_score_dataset)


write.csv(sample_final_score_dataset, 'SVM_Modified_Weights.csv')





#Get essay with the "max" +ve and -ve difference between actual score and overall calculated score

#1. Positive difference (ie the actual score is "lower" than the overall score)
ind <- which.max(final_score$Score_Diff)
ind
rownames(test_full)[ind]

#Print original essay
text$essay[as.integer(rownames(test_full)[ind])]

#Print the list of unique words (without stop words) 
#along with the final dataset and compare :-
# a) Examiner score 
# b) Essay length
# c) First few words of the actual essay with unique tokens
final_score$Actual_Score[ind]
text[as.integer(rownames(test_matrix)[ind]),]

length(as.vector(unlist(unique_token_wo_sw[as.integer(rownames(test_matrix))[ind]])))
final_dataset[as.integer(rownames(test_matrix))[ind],]
substr(text$essay[as.integer(rownames(test_matrix))[ind]],1,100)
as.vector(unlist(unique_token_wo_sw[as.integer(rownames(test_matrix))[ind]]))
hunspell(lower_text[as.integer(rownames(test_matrix))[ind]])

# Negative Difference (ie the actual score is "higher" than the overall score)
ind <- which.min(final_score$Score_Diff)
ind
rownames(test_full)[ind]

#Print original essay
text$essay[as.integer(rownames(test_matrix)[ind])]

#Print the list of unique words (without stop words) 
#along with the final dataset and compare :-
# a) Examiner score 
# b) Essay length
# c) First few words of   the actual essay with unique tokens
final_score$Actual_Score[ind]
text[as.integer(rownames(test_matrix)[ind]),]
length(as.vector(unlist(unique_token_wo_sw[as.integer(rownames(test_matrix))[ind]])))
final_dataset[as.integer(rownames(test_matrix))[ind],]
substr(text$essay[as.integer(rownames(test_matrix))[ind]],1,100)
as.vector(unlist(unique_token_wo_sw[as.integer(rownames(test_matrix))[ind]]))
hunspell(lower_text[as.integer(rownames(test_matrix))[ind]])







