## Gesture Data Analysis 
### Authors: Anon 
library(readr)
library(caret)
library(ggplot2)
library(cowplot)

find_type <- function(target){
  types <- c("span","container","sweep")
  for(type in types){
    if(grepl(type, target, fixed=TRUE)) return(type)
  }
  return("none")
}

## Data Loading
ChatGPT_GestureType <- read_csv("C:/Users/nutju/Downloads/Gestures/gpt-3.5-turbo_GestureType-v7.csv")
GPT4_GestureType    <- read_csv("C:/Users/nutju/Downloads/Gestures/gpt-4_GestureType-v7.csv")


ChatGPT_Physical<- read_csv("C:/Users/nutju/Downloads/Gestures/gpt-3.5-turbo_PhysicalGesture-v7.csv")
GPT4_Physical    <- read_csv("C:/Users/nutju/Downloads/Gestures/gpt-4_PhysicalGesture-v7.csv")

ChatGPT_Semantic <- read_csv("C:/Users/nutju/Downloads/Gestures/gpt-3.5-turbo_SemanticDescription-v7.csv")
GPT4_Semantic    <- read_csv("C:/Users/nutju/Downloads/Gestures/gpt-4_SemanticDescription-v7.csv")

## Data Cleaning - removing "."

ChatGPT_GestureType <-  as.data.frame(sapply(ChatGPT_GestureType, function(x) gsub("\\.","",x)))
GPT4_GestureType <-  as.data.frame(sapply(GPT4_GestureType, function(x) gsub("\\.","",x)))


ChatGPT_Physical <-  as.data.frame(sapply(ChatGPT_Physical, function(x) gsub("\\.","",x)))
GPT4_Physical <-  as.data.frame(sapply(GPT4_Physical, function(x) gsub("\\.","",x)))


ChatGPT_Semantic <-  as.data.frame(sapply(ChatGPT_Semantic, function(x) gsub("\\.","",x)))
GPT4_Semantic <-  as.data.frame(sapply(GPT4_Semantic, function(x) gsub("\\.","",x)))



## Extra
ChatGPT_Semantic$Annotation_semantic <- ChatGPT_Semantic$Annotation
ChatGPT_Semantic$`num=2_context=True_semantic` <- ChatGPT_Semantic$`num=2_context=True`
ChatGPT_Semantic$`num=4_context=True_semantic` <- ChatGPT_Semantic$`num=4_context=True`
ChatGPT_Semantic$`num=6_context=True_semantic` <- ChatGPT_Semantic$`num=6_context=True`
ChatGPT_Semantic$`num=-1_context=True_semantic` <- ChatGPT_Semantic$`num=-1_context=True`

ChatGPT_Semantic$Annotation_semantic <- gsub("span","",ChatGPT_Semantic$Annotation_semantic)
ChatGPT_Semantic$Annotation_semantic <- gsub("sweep","",ChatGPT_Semantic$Annotation_semantic)
ChatGPT_Semantic$`num=2_context=True_semantic` <- gsub("span","",ChatGPT_Semantic$`num=2_context=True_semantic`)
ChatGPT_Semantic$`num=2_context=True_semantic` <- gsub("sweep","",ChatGPT_Semantic$`num=2_context=True_semantic`)
ChatGPT_Semantic$`num=4_context=True_semantic` <- gsub("span","",ChatGPT_Semantic$`num=4_context=True_semantic`)
ChatGPT_Semantic$`num=4_context=True_semantic` <- gsub("sweep","",ChatGPT_Semantic$`num=4_context=True_semantic`)
ChatGPT_Semantic$`num=6_context=True_semantic` <- gsub("span","",ChatGPT_Semantic$`num=6_context=True_semantic`)
ChatGPT_Semantic$`num=6_context=True_semantic` <- gsub("sweep","",ChatGPT_Semantic$`num=6_context=True_semantic`)
ChatGPT_Semantic$`num=-1_context=True_semantic` <- gsub("span","",ChatGPT_Semantic$`num=-1_context=True_semantic`)
ChatGPT_Semantic$`num=-1_context=True_semantic` <- gsub("sweep","",ChatGPT_Semantic$`num=-1_context=True_semantic`)


GPT4_Semantic$Annotation_semantic <- GPT4_Semantic$Annotation
GPT4_Semantic$`num=2_context=True_semantic` <- GPT4_Semantic$`num=2_context=True`
GPT4_Semantic$`num=4_context=True_semantic` <- GPT4_Semantic$`num=4_context=True`
GPT4_Semantic$`num=6_context=True_semantic` <- GPT4_Semantic$`num=6_context=True`
GPT4_Semantic$`num=-1_context=True_semantic` <- GPT4_Semantic$`num=-1_context=True`

GPT4_Semantic$Annotation_semantic <- gsub("span","",GPT4_Semantic$Annotation_semantic)
GPT4_Semantic$Annotation_semantic <- gsub("sweep","",GPT4_Semantic$Annotation_semantic)
GPT4_Semantic$`num=2_context=True_semantic` <- gsub("span","",GPT4_Semantic$`num=2_context=True_semantic`)
GPT4_Semantic$`num=2_context=True_semantic` <- gsub("sweep","",GPT4_Semantic$`num=2_context=True_semantic`)
GPT4_Semantic$`num=4_context=True_semantic` <- gsub("span","",GPT4_Semantic$`num=4_context=True_semantic`)
GPT4_Semantic$`num=4_context=True_semantic` <- gsub("sweep","",GPT4_Semantic$`num=4_context=True_semantic`)
GPT4_Semantic$`num=6_context=True_semantic` <- gsub("span","",GPT4_Semantic$`num=6_context=True_semantic`)
GPT4_Semantic$`num=6_context=True_semantic` <- gsub("sweep","",GPT4_Semantic$`num=6_context=True_semantic`)
GPT4_Semantic$`num=-1_context=True_semantic` <- gsub("span","",GPT4_Semantic$`num=-1_context=True_semantic`)
GPT4_Semantic$`num=-1_context=True_semantic` <- gsub("sweep","",GPT4_Semantic$`num=-1_context=True_semantic`)



## Confusion Matrix 

chatConfusion <- as.data.frame(table(ChatGPT_GestureType$Annotation, ChatGPT_GestureType$`num=-1_context=True`))
gpt4Confusion <- as.data.frame(table(GPT4_GestureType$Annotation, GPT4_GestureType$`num=-1_context=True`))

chatConfusion$Var1 <- factor(chatConfusion$Var1, levels = rev(levels(chatConfusion$Var1))) 
gpt4Confusion$Var1 <- factor(gpt4Confusion$Var1, levels = rev(levels(gpt4Confusion$Var1)))

g_chat_conf <- ggplot(chatConfusion, aes(x = Var2,y = Var1, fill= Freq)) +
   geom_tile() + geom_text(aes(label=Freq), size = 5) +
   scale_fill_gradient(low="white", high="#009194", limit=c(0,10), breaks=c(0,2,4,6,8,10)) +
   labs(y = "Reference",x = "Prediction") + ggtitle("ChatGPT")

 
g_gpt4_conf <- ggplot(gpt4Confusion, aes(x = Var2,y = Var1, fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq), size = 5) +
  scale_fill_gradient(low="white", high="#009194", limit=c(0,10), breaks=c(0,2,4,6,8,10)) +
  labs(y = "Reference",x = "Prediction") + ggtitle("GPT-4")

plot_grid(g_chat_conf, g_gpt4_conf, labels = "AUTO", ncol=1)


## Accuracy plots


### Accuracy Calculation 

cal_acc <- function(target, predict, mode){
  total <- length(target)
  if(mode == "complete"){
    return(sum(target == predict)/total)
  }
  if(mode == "partial"){
    score <- 0
    for(i in c(1:total)){
      type <- find_type(target[i])
      score <- score + 0.5*grepl(type, predict[i], fixed=TRUE)
      prefix <- gsub(type,"",target[i])
      if(prefix == ""){
        score <- score + 0.5*(target[i]==predict[i])  
      }else{
        score <- score + 0.5*grepl(prefix, predict[i],fixed=TRUE)
      }
    }
    return(score/total)
  }
}

acc_dat_type <- data.frame(
  accuracy = c(cal_acc(ChatGPT_GestureType$Annotation,ChatGPT_GestureType$`num=2_context=True`,"complete"),
               cal_acc(ChatGPT_GestureType$Annotation,ChatGPT_GestureType$`num=4_context=True`,"complete"),
               cal_acc(ChatGPT_GestureType$Annotation,ChatGPT_GestureType$`num=6_context=True`,"complete"),
               cal_acc(ChatGPT_GestureType$Annotation,ChatGPT_GestureType$`num=-1_context=True`,"complete"),
               cal_acc(GPT4_GestureType$Annotation,GPT4_GestureType$`num=2_context=True`,"complete"),
               cal_acc(GPT4_GestureType$Annotation,GPT4_GestureType$`num=4_context=True`,"complete"),
               cal_acc(GPT4_GestureType$Annotation,GPT4_GestureType$`num=6_context=True`,"complete"),
               cal_acc(GPT4_GestureType$Annotation,GPT4_GestureType$`num=-1_context=True`,"complete")
               ),
  num_examples = rep(c("2","4","6","-1"), 2),
  model = rep(c("ChatGPT","GPT-4"),each = 4)
)
acc_dat_type$num_examples <- factor(acc_dat_type$num_examples, levels = c("2","4","6","-1"), ordered=TRUE)

acc_dat_physical <- data.frame(
  accuracy = c(cal_acc(ChatGPT_Physical$Annotation,ChatGPT_Physical$`num=2_context=True`,"complete"),
               cal_acc(ChatGPT_Physical$Annotation,ChatGPT_Physical$`num=4_context=True`,"complete"),
               cal_acc(ChatGPT_Physical$Annotation,ChatGPT_Physical$`num=6_context=True`,"complete"),
               cal_acc(ChatGPT_Physical$Annotation,ChatGPT_Physical$`num=-1_context=True`,"complete"),
               cal_acc(GPT4_Physical$Annotation,GPT4_Physical$`num=2_context=True`,"complete"),
               cal_acc(GPT4_Physical$Annotation,GPT4_Physical$`num=4_context=True`,"complete"),
               cal_acc(GPT4_Physical$Annotation,GPT4_Physical$`num=6_context=True`,"complete"),
               cal_acc(GPT4_Physical$Annotation,GPT4_Physical$`num=-1_context=True`,"complete")
  ),
  num_examples = rep(c("2","4","6","-1"), 2),
  model = rep(c("ChatGPT","GPT-4"),each = 4)
)
acc_dat_physical$num_examples <- factor(acc_dat_physical$num_examples, levels = c("2","4","6","-1"), ordered=TRUE)


acc_dat_semantic<- data.frame(
  accuracy = c(cal_acc(ChatGPT_Semantic$Annotation,ChatGPT_Semantic$`num=2_context=True`,"complete"),
               cal_acc(ChatGPT_Semantic$Annotation,ChatGPT_Semantic$`num=4_context=True`,"complete"),
               cal_acc(ChatGPT_Semantic$Annotation,ChatGPT_Semantic$`num=6_context=True`,"complete"),
               cal_acc(ChatGPT_Semantic$Annotation,ChatGPT_Semantic$`num=-1_context=True`,"complete"),
               cal_acc(GPT4_Semantic$Annotation,GPT4_Semantic$`num=2_context=True`,"complete"),
               cal_acc(GPT4_Semantic$Annotation,GPT4_Semantic$`num=4_context=True`,"complete"),
               cal_acc(GPT4_Semantic$Annotation,GPT4_Semantic$`num=6_context=True`,"complete"),
               cal_acc(GPT4_Semantic$Annotation,GPT4_Semantic$`num=-1_context=True`,"complete")
  ),
  num_examples = rep(c("2","4","6","-1"), 2),
  model = rep(c("ChatGPT","GPT-4"),each = 4)
)
acc_dat_semantic$num_examples <- factor(acc_dat_semantic$num_examples, levels = c("2","4","6","-1"), ordered=TRUE)


acc_dat_semantic_alone<- data.frame(
  accuracy = c(cal_acc(ChatGPT_Semantic$Annotation_semantic,ChatGPT_Semantic$`num=2_context=True_semantic`,"complete"),
               cal_acc(ChatGPT_Semantic$Annotation_semantic,ChatGPT_Semantic$`num=4_context=True_semantic`,"complete"),
               cal_acc(ChatGPT_Semantic$Annotation_semantic,ChatGPT_Semantic$`num=6_context=True_semantic`,"complete"),
               cal_acc(ChatGPT_Semantic$Annotation_semantic,ChatGPT_Semantic$`num=-1_context=True_semantic`,"complete"),
               cal_acc(GPT4_Semantic$Annotation_semantic,GPT4_Semantic$`num=2_context=True_semantic`,"complete"),
               cal_acc(GPT4_Semantic$Annotation_semantic,GPT4_Semantic$`num=4_context=True_semantic`,"complete"),
               cal_acc(GPT4_Semantic$Annotation_semantic,GPT4_Semantic$`num=6_context=True_semantic`,"complete"),
               cal_acc(GPT4_Semantic$Annotation_semantic,GPT4_Semantic$`num=-1_context=True_semantic`,"complete")
  ),
  num_examples = rep(c("2","4","6","-1"), 2),
  model = rep(c("ChatGPT","GPT-4"),each = 4)
)
acc_dat_semantic_alone$num_examples <- factor(acc_dat_semantic$num_examples, levels = c("2","4","6","-1"), ordered=TRUE)


### Accuracy ploting 

g_type <- ggplot(acc_dat_type, aes(y = accuracy, x = num_examples, group = model, fill = model)) + 
  geom_bar(stat='identity', position="dodge") + ggtitle("Gesture Type") + theme(legend.position = "none") + ylim(c(0,.7)) + 
  geom_hline(yintercept=1/length(unique(ChatGPT_GestureType$Annotation)), linetype="dashed")

g_physical <- ggplot(acc_dat_physical, aes(y = accuracy, x = num_examples, group = model, fill = model)) + 
  geom_bar(stat='identity', position="dodge") + ggtitle("Physical Gesture") + theme(legend.position = "none") + ylim(c(0,.7)) + 
  geom_hline(yintercept=1/length(unique(ChatGPT_Physical$Annotation)), linetype="dashed")

g_semantic <- ggplot(acc_dat_semantic, aes(y = accuracy, x = num_examples, group = model, fill = model)) + 
  geom_bar(stat='identity', position="dodge") + ggtitle("Semantic Gesture") + ylim(c(0,.7)) + 
  geom_hline(yintercept=1/length(unique(ChatGPT_Semantic$Annotation)), linetype="dashed") + theme(legend.position = "bottom")
legend <- get_legend(g_semantic)
g_semantic <- g_semantic + theme(legend.position = "none")

g_semantic_alone <- ggplot(acc_dat_semantic_alone, aes(y = accuracy, x = num_examples, group = model, fill = model)) + 
  geom_bar(stat='identity', position="dodge") + ggtitle("Semantic Only") + ylim(c(0,.7)) + theme(legend.position = "none") + 
  geom_hline(yintercept=1/length(unique(ChatGPT_Semantic$Annotation_semantic)), linetype="dashed")


g_all <- plot_grid(g_type, g_physical, g_semantic, g_semantic_alone, labels="AUTO", nrow=1)
g <- plot_grid(g_all, legend, nrow=2, rel_heights = c(1,.1))

print(g)
