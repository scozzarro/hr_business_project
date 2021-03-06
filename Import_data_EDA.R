#######################################################################
# HR  Project 
# 26 Dic 2020
# Gabrielmaria Scozzarro
#    
#######################################################################

# 0.1 Tools ----
library(tidyverse)
library(corrplot)
library(kableExtra)
library(scales)
library(lubridate)
library(reshape2)
library(plyr)
library(RColorBrewer)
library(corrplot)
library(caret)
library(igraph)
library(ggraph)


nb.cols <- 26
mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(nb.cols)


# 1.0 Import data ----
hr_data<- read.csv("Data/HRDataset_v14.csv")


# 1.1 Data summary ----
glimpse(hr_data)
summary(hr_data)

# 1.2 Fix data ---- 
hr_data$ManagerID[is.na(hr_data$ManagerID)]<- 0

hr_data$MaritalDesc<- as_factor(hr_data$MaritalDesc)

useless_col<- c("GenderID", "MaritalStatusID")
hr_data<- hr_data[, -which(colnames(hr_data) %in% useless_col)]

hr_data$DateofHire<- as.POSIXlt(hr_data$DateofHire,format="%m/%d/%Y")
hr_data$DateofHire<- as.Date(hr_data$DateofHire)

hr_data$DateofTermination<- as.POSIXlt(hr_data$DateofTermination,format="%m/%d/%Y")
hr_data$DateofTermination<- as.Date(hr_data$DateofTermination)

hr_data$DOB<- as.Date(hr_data$DOB, "%m/%d/%y")
hr_data$DOB<- as.Date(ifelse(hr_data$DOB > "2020-01-01", format(hr_data$DOB, "19%y-%m-%d"), format(hr_data$DOB)))

hr_data$Age<- year(Sys.Date())-year(hr_data$DOB)
# 2.0 EDA ----

# 2.1 Gender analysis
hr_data %>% ggplot(aes(Sex, fill = Sex)) +
            geom_bar(stat = "count") +
            ggtitle("Gender presence") +
            theme_minimal() +
            theme(legend.position =  "none")

  
hr_data %>% ggplot(aes(Age, fill = Sex)) +
            geom_density(alpha = 0.3) +
            theme_minimal()

all_roles = unique(hr_data$Position)
hr_data$Position = factor(hr_data$Position, levels = all_roles)
hr_data$order = as.numeric(hr_data$Position)/100 

hr_data %>% filter(!is.na(DateofTermination)) %>%
            ggplot(aes(x = factor(order), fill = Sex)) + 
            geom_bar(subset=.(Sex == "M")) + 
            geom_bar(subset=.(Sex == "F"), aes(y=..count..)) + 
            scale_x_discrete(labels = all_roles) +
            xlab("Role") +
            coord_flip() +
            theme(text = element_text(size = 16)) +
            ggtitle("Gender presence by role up to date") +
            theme_minimal()

hr_data %>% ggplot(aes(x = factor(order), fill = Sex)) + 
            geom_bar(subset=.(Sex == "M")) + 
            geom_bar(subset=.(Sex == "F"), aes(y=..count..)) + 
            scale_x_discrete(labels = all_roles) +
            xlab("Role") +
            coord_flip() +
            theme(text = element_text(size = 16)) +
            facet_wrap(~year(DateofHire)) +
            ggtitle("Enrolment by role and gender since the incorporation") +
            theme_minimal()

hr_data %>% group_by(Sex, year = year(DateofHire)) %>%
            dplyr::summarise(n = n()) %>%
            ggplot(aes(year, n, color = Sex)) +
            geom_line(aes(linetype=Sex), size = 1) +
            geom_point(size = 2) +
            ggtitle("Hiring trend by gender") +
            theme_minimal()


hr_data %>% group_by(Sex, Position) %>%
            dplyr::summarise(avgsal = mean(Salary)) %>%
            ggplot(aes(avgsal, Position, fill = Sex)) +
            geom_bar(stat = "identity", position = "dodge") +
            ggtitle("Average salary by gender and role") +
            theme_minimal()

hr_data %>% filter(!is.na(DateofTermination)) %>%
            group_by(Sex) %>%
            dplyr::summarise(n = n()) %>%
            ggplot(aes(n, Sex, fill = Sex)) +
            geom_bar(stat = "identity") +
            ggtitle("Employes contract terminated by gender") +
            theme_minimal() +
            theme(legend.position = "none")

hr_data %>% group_by(Sex, RecruitmentSource) %>%
            dplyr::summarise(n = n()) %>%
            ggplot(aes(n, RecruitmentSource, fill = Sex)) +
            geom_bar(stat = "identity", position = "dodge") +
            ggtitle("Recruitment source performance by gender") +
            theme_minimal()

hr_data %>% filter(!is.na(DateofTermination)) %>%
            group_by(Sex, TermReason) %>%
            dplyr::summarise(n = n()) %>%
            ggplot(aes(n, TermReason, fill = Sex)) +
            geom_bar(stat = "identity", position = "dodge") +
            ggtitle("Contract termination reasons by gender") +
            theme_minimal()


hr_data %>% group_by(Sex) %>%
            ggplot(aes(Sex, EmpSatisfaction, fill = Sex)) +
            geom_violin(width=1, alpha = 0.7) +
            geom_boxplot(width=0.1, color="black", alpha=0.2) +
            theme_minimal() +
            theme(legend.position="none") +
            ggtitle("Employes satisfaction by gender")

hr_data$Salary_level<- 0

for (i in 1:nrow(hr_data)) {
  if (hr_data$Salary[i] > mean(hr_data$Salary) + 2*sd(hr_data$Salary)) {
    hr_data$Salary_level[i]<- "High"
  }
  else if (hr_data$Salary[i] < mean(hr_data$Salary) + sd(hr_data$Salary)) {
    hr_data$Salary_level[i]<- "Medium"
  } 
  else {
    hr_data$Salary_level[i]<- "Low"
  }
  }

hr_data %>% group_by(Sex, Salary_level) %>%
            ggplot(aes(Salary_level, EmpSatisfaction, fill = Sex)) +
            geom_boxplot() +
            ggtitle("Employes satisfaction level by gender and salary level")

# 2.2 Manager and performance analysis ----
kable(unique(hr_data$ManagerName), format = 'html', booktabs = T) %>% kable_styling(bootstrap_options = 'striped')
length(unique(hr_data$ManagerName))

hr_data %>% ggplot(aes(ManagerName, fill = ManagerName)) +
            geom_bar() +
            geom_text(stat='count', aes(label=..count..), vjust=0, nudge_y = 0.5) +
            scale_fill_manual(values = mycolors) +
            coord_flip() +
            ggtitle('Employees under each manager') +
            theme_minimal() +
            theme(legend.position = 'none')

hr_data %>% ggplot(aes(PerformanceScore, fill = PerformanceScore)) +
            geom_bar() +
            geom_text(stat='count', aes(label=..count..), vjust=-0.2) +
            facet_wrap(~ManagerName) +
            theme(axis.text.x=element_blank()) +
            ggtitle('Employess performance by manager')

hr_data %>% filter(EmploymentStatus != 'Active') %>%
            ggplot(aes(TermReason, fill = TermReason)) +
            geom_bar() +
            geom_text(stat='count', aes(label=..count..), vjust=0, nudge_y = 0.5) +
            facet_wrap(~ManagerName) +
            scale_fill_manual(values = mycolors) +
            theme(axis.text.x=element_blank()) +
            ggtitle('Termination reason by manager')

hr_data %>% group_by(ManagerName) %>%
            dplyr::summarise(Special_Projects = sum(SpecialProjectsCount)) %>%
            ggplot(aes(Special_Projects, ManagerName, fill = ManagerName)) +
            geom_bar(stat = 'identity') +
            geom_text(aes(label=Special_Projects, vjust=0.5)) +
            scale_fill_manual(values = mycolors) +
            ggtitle('Special projects by manager') +
            theme_minimal() +
            theme(legend.position = 'none')

hr_data %>% group_by(ManagerName) %>%
            dplyr::summarise(avgsatisf = mean(EmpSatisfaction)) %>%
            ggplot(aes(sort(avgsatisf, decreasing = F), ManagerName, fill = ManagerName)) +
            geom_bar(stat = 'identity') +
            scale_fill_manual(values = mycolors) +
            ggtitle('Average employees satisfaction by manager') +
            theme_minimal() +
            theme(legend.position = 'none')


# 2.3 Termination for salary analysis ----
hr_data %>% filter(TermReason == 'more money') %>% 
            group_by(Position) %>% 
            dplyr::summarise(n = n()) %>%
            ggplot(aes(n, Position, fill = Position)) +
            geom_bar(stat = 'identity') +
            scale_fill_manual(values = mycolors) +
            ggtitle('Postion and quantity terminated for more money') +
            theme_minimal()

hr_data %>% filter(TermReason == 'more money') %>%
            group_by(Position) %>% 
            dplyr::summarise(avgsatisfaction = mean(EmpSatisfaction)) %>%
            ggplot(aes(avgsatisfaction, Position, fill = Position)) +
            geom_bar(stat = 'identity') +
            scale_fill_manual(values = mycolors) +
            ggtitle('Satiosfaction lev of who terminated for more money') +
            theme_minimal()

termpos<- unique(as.character(hr_data$Position[which(hr_data$TermReason == 'more money')]))

avgsal_active<- hr_data %>% group_by(Position) %>%
                filter(EmploymentStatus == 'Active') %>%
                dplyr::summarise(avgsal = mean(Salary)) %>%
                filter(Position %in% termpos)

avgsal_terminated<- hr_data %>% group_by(Position) %>%
                    filter(EmploymentStatus != 'Active') %>%
                    dplyr::summarise(avgsal = mean(Salary)) %>%
                    filter(Position %in% termpos)

avgsal_active$status<- c('Active','Active')
avgsal_terminated$status<- c('Terminated','Terminated')

avsal_frame<- full_join(x = avgsal_active, y = avgsal_terminated)

avsal_frame %>% ggplot(aes(avgsal, Position, fill = status)) +
                geom_bar(stat = 'identity', position = 'dodge') +
                scale_fill_manual(values = mycolors) +
                ggtitle('Salary difference between employees and whom terminted for more money') +
                theme_minimal()

hr_data %>% group_by(RecruitmentSource) %>%
            dplyr::summarise(avgsal = mean(Salary)) %>%
            ggplot(aes(sort(avgsal, decreasing = F), RecruitmentSource, fill = RecruitmentSource)) +
            geom_bar(stat = 'identity') +
            scale_fill_manual(values = mycolors) +
            ggtitle('Average salary by recruitment source') +
            theme_minimal()

# 2.4 Correlation ----

hr_data_numeric<- hr_data%>% dplyr::select(where(is.numeric))
useless_col_numeric<- c("EmpID", "Zip", 'order')
hr_data_numeric<- hr_data_numeric[, -which(colnames(hr_data_numeric) %in% useless_col_numeric)]

corrplot(cor(hr_data_numeric), method = 'circle')

hr_data %>% ggplot(aes(Salary, PerfScoreID, fill = EngagementSurvey)) +
            geom_point()

with(hr_data, qplot(Salary, PerfScoreID, colour = EngagementSurvey, cex=2))


 # 3.0 Performance Prediction ----

hr_data$PerfScoreID<- as.factor(hr_data$PerfScoreID)

val_index<- createDataPartition(hr_data$PerfScoreID, p = 0.2, list = FALSE)

validation<- hr_data[val_index,]
train<- hr_data[-val_index,]

hr.tree = train(PerfScoreID ~ EngagementSurvey +
                              EmpSatisfaction +
                              Age +
                              Salary +
                              Absences +
                              ManagerID, 
                              data=train, 
                              method="rpart", 
                              trControl = trainControl(method = "cv"))
hr.tree

suppressMessages(library(rattle))

fancyRpartPlot(hr.tree$finalModel)

varImp(hr.tree)

hr.pred = predict(hr.tree, newdata = validation)

table(hr.pred, validation$PerfScoreID)

error.rate = round(mean(hr.pred != validation$PerfScoreID),2)

error.rate


#second tree model


hr.tree_2 = train(PerfScoreID ~ EngagementSurvey +
                  EmpSatisfaction +
                  Age +
                  Salary +
                  Absences +
                  ManagerID +
                  Absences, 
                  data=train, 
                  method="rf",
                  metric = 'Accuracy',
                  trControl = trainControl(method = "cv", number=10, repeats=3, search = 'random'))

hr.tree_2

plot(hr.tree_2)

RfGrid <-  expand.grid(mtry = seq(from = 4, to = 4.5, length.out = 5))

hr.tree_2 = train(PerfScoreID ~ EngagementSurvey +
                    EmpSatisfaction +
                    Age +
                    Salary +
                    Absences +
                    ManagerID, 
                    data=train, 
                    method="rf",
                    metric = 'Accuracy',
                    trControl = trainControl(method = "cv", number=10, repeats=3, search = 'grid'))

hr.tree_2

plot(hr.tree_2)
tree_func <- function(final_model, 
                      tree_num) {
  
  # get tree by index
  tree <- randomForest::getTree(final_model, 
                                k = tree_num, 
                                labelVar = TRUE) %>%
    tibble::rownames_to_column() %>%
    # make leaf split points to NA, so the 0s won't get plotted
    mutate(`split point` = ifelse(is.na(prediction), `split point`, NA))
  
  # prepare data frame for graph
  graph_frame <- data.frame(from = rep(tree$rowname, 2),
                            to = c(tree$`left daughter`, tree$`right daughter`))
  
  # convert to graph and delete the last node that we don't want to plot
  graph <- graph_from_data_frame(graph_frame) %>%
    delete_vertices("0")
  
  # set node labels
  V(graph)$node_label <- gsub("_", " ", as.character(tree$`split var`))
  V(graph)$leaf_label <- as.character(tree$prediction)
  V(graph)$split <- as.character(round(tree$`split point`, digits = 2))
  
  # plot
  plot <- ggraph(graph, 'dendrogram') + 
    theme_bw() +
    geom_edge_link() +
    geom_node_point() +
    geom_node_text(aes(label = node_label), na.rm = TRUE, repel = TRUE) +
    geom_node_label(aes(label = split), vjust = 2.5, na.rm = TRUE, fill = "white") +
    geom_node_label(aes(label = leaf_label, fill = leaf_label), na.rm = TRUE, 
                    repel = TRUE, colour = "white", fontface = "bold", show.legend = FALSE) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_blank(),
          plot.background = element_rect(fill = "white"),
          panel.border = element_blank(),
          axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_text(size = 18))
  
  print(plot)
}

hr_tree_num<- which(hr.tree_2$finalModel$forest$ndbigtree == min(hr.tree_2$finalModel$forest$ndbigtree))
tree_func(final_model = hr.tree_2$finalModel, hr_tree_num)



hr.pred_2 = predict(hr.tree_2, newdata = validation)

table(hr.pred_2, validation$PerfScoreID)

error.rate2 = round(mean(hr.pred_2 != validation$PerfScoreID),2)

error.rate2
