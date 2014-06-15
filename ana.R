# Temporal Analysis

download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", destfile="pml-training.csv", method="curl");
download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", destfile="pml-testing.csv", method="curl");

# library
library(caret)

# functions
calc_range <- function(x, ...){
	my_range <- range(x, ...)
	return(diff(my_range))
}

exp_input <- function(x){
	res = x[1];
	for (i in 2:length(x)){
			res <- paste(res, " + ", x[i], sep="");
	}
	return(res);
}

# missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}

missClass <- function(values, prediction){
	sum( prediction != values )/length(values)
}

raw_training <- read.csv("pml-training.csv");
testing  <- read.csv("pml-testing.csv");


# features:
# - belt
### # avg_roll_belt, var_roll_belt, NA is so much and remove these features
# max_accel_belt, rng_accel_belt, var_accel_belt,
# var_gyros_belt, var_magnet_belt
# - arm
# var_accel_arm, max_magnet_arm, min_magnet_arm
# - dumbbel
# max_accel_dumbbell, var_gyros_dumbbell, max_magnet_dumbbell,
# min_magnet_dumbbell
# - glove
# picth_forearm, max_gyros_forearm, min_gyros_forearm
#raw_training$max_accel_belt <- apply(raw_training[,grep("accel_belt_(x|y|z)",
#	names(raw_training))],1,max,na.rm=T)
#raw_training$min_accel_belt <- apply(raw_training[,grep("accel_belt_(x|y|z)",
#	names(raw_training))],1,min,na.rm=T)	
#raw_training$rng_accel_belt <- apply(raw_training[,grep("accel_belt_(x|y|z)",
#	names(raw_training))],1,calc_range,na.rm=T)
#raw_training$var_accel_belt <- apply(raw_training[,grep("accel_belt_(x|y|z)",
#	names(raw_training))],1,var,na.rm=T)
#	
#raw_training$max_gyros_belt <- apply(raw_training[,grep("gyros_belt_(x|y|z)",
#	names(raw_training))],1,max,na.rm=T)
#raw_training$min_gyros_belt <- apply(raw_training[,grep("gyros_belt_(x|y|z)",
#	names(raw_training))],1,min,na.rm=T)
#raw_training$var_gyros_belt <- apply(raw_training[,grep("gyros_belt_(x|y|z)",
#	names(raw_training))],1,var,na.rm=T)
#	
#raw_training$max_magnet_belt <- apply(raw_training[,grep("magnet_belt_(x|y|z)",
#	names(raw_training))],1,max,na.rm=T)
#raw_training$min_magnet_belt <- apply(raw_training[,grep("magnet_belt_(x|y|z)",
#	names(raw_training))],1,min,na.rm=T)
#raw_training$var_magnet_belt <- apply(raw_training[,grep("magnet_belt_(x|y|z)",
#	names(raw_training))],1,var,na.rm=T)
#	
#raw_training$max_accel_arm <- apply(raw_training[,grep("accel_arm_(x|y|z)",
#	names(raw_training))],1,max,na.rm=T)
#raw_training$min_accel_arm <- apply(raw_training[,grep("accel_arm_(x|y|z)",
#	names(raw_training))],1,min,na.rm=T)	
#raw_training$var_accel_arm <- apply(raw_training[,grep("accel_arm_(x|y|z)",
#	names(raw_training))],1,var,na.rm=T)
#	
#raw_training$max_magnet_arm <- apply(raw_training[,grep("magnet_arm_(x|y|z)",
#	names(raw_training))],1,max,na.rm=T)
#raw_training$min_magnet_arm <- apply(raw_training[,grep("magnet_arm_(x|y|z)",
#	names(raw_training))],1,min,na.rm=T)
#raw_training$var_magnet_arm <- apply(raw_training[,grep("magnet_arm_(x|y|z)",
#	names(raw_training))],1,var,na.rm=T)
#	
#raw_training$max_accel_dumbbell <- apply(raw_training[,grep("accel_dumbbell_(x|y|z)",
#	names(raw_training))],1,max,na.rm=T)
#raw_training$min_accel_dumbbell <- apply(raw_training[,grep("accel_dumbbell_(x|y|z)",
#	names(raw_training))],1,min,na.rm=T)	
#raw_training$var_accel_dumbbell <- apply(raw_training[,grep("accel_dumbbell_(x|y|z)",
#	names(raw_training))],1,var,na.rm=T)		
#
#raw_training$max_gyros_dumbbell <- apply(raw_training[,grep("gyros_dumbbell_(x|y|z)",
#	names(raw_training))],1,max,na.rm=T)
#raw_training$min_gyros_dumbbell <- apply(raw_training[,grep("gyros_dumbbell_(x|y|z)",
#	names(raw_training))],1,min,na.rm=T)	
#raw_training$var_gyros_dumbbell <- apply(raw_training[,grep("gyros_dumbbell_(x|y|z)",
#	names(raw_training))],1,var,na.rm=T)
#	
#raw_training$max_magnet_dumbbell <- apply(raw_training[,grep("magnet_dumbbell_(x|y|z)",
#	names(raw_training))],1,max,na.rm=T)
#raw_training$min_magnet_dumbbell <- apply(raw_training[,grep("magnet_dumbbell_(x|y|z)",
#	names(raw_training))],1,min,na.rm=T)
#raw_training$var_magnet_dumbbell <- apply(raw_training[,grep("magnet_dumbbell_(x|y|z)",
#	names(raw_training))],1,var,na.rm=T)

#raw_training$max_gyros_forearm <- apply(raw_training[,grep("gyros_forearm_(x|y|z)",
#	names(raw_training))],1,max,na.rm=T)
#raw_training$min_gyros_forearm <- apply(raw_training[,grep("gyros_forearm_(x|y|z)",
#	names(raw_training))],1,min,na.rm=T)
#raw_training$var_gyros_forearm <- apply(raw_training[,grep("gyros_forearm_(x|y|z)",
#	names(raw_training))],1,var,na.rm=T)



set.seed(1)
inTrain <- createDataPartition(y=raw_training$classe,p=0.6, list=F)
training <- raw_training[inTrain, ]
cv       <- raw_training[-inTrain, ]



p <- qplot(new_window, num_window, col=classe, data=training, alpha=0.1);
p # plot 1

p <- qplot(roll_belt, pitch_belt, col=classe, data=training, alpha=0.1);
p # plot 2

p <- qplot(yaw_belt, total_accel_belt, col=classe, data=training, alpha=0.1);
p # plot 3

p <- qplot(roll_arm, pitch_arm, col=classe, data=training, alpha=0.1);
p # plot 4

p <- qplot(yaw_arm, total_accel_arm, col=classe, data=training, alpha=0.1);
p # plot 5

p <- qplot(roll_dumbbell, pitch_dumbbell, col=classe, data=training, alpha=0.1);
p # plot cand

p <- qplot(yaw_dumbbell, total_accel_dumbbell, col=classe, data=training, alpha=0.1);
p 

p <- qplot(total_accel_dumbbell, yaw_dumbbell, col=classe, data=training, alpha=0.1);
p 

p <- qplot(roll_forearm, pitch_forearm, col=classe, data=training, alpha=0.1);
p # plot 6

p <- qplot(yaw_forearm, total_accel_forearm, col=classe, data=training, alpha=0.1);
p 

# additional variables
#p <- qplot(max_accel_belt, min_accel_belt, col=classe, data=training, alpha=0.1);
#p
#p <- qplot(max_gyros_belt, min_gyros_belt, col=classe, data=training, alpha=0.1);
#p # bad
#p <- qplot(max_magnet_belt, min_magnet_belt, col=classe, data=training, alpha=0.1);
#p # bad
#p <- qplot(rng_accel_belt, var_accel_belt, col=classe, data=training, alpha=0.1);
#p








# old
#p <- qplot(max_accel_belt, min_accel_belt, col=classe, data=training, alpha=0.1);
#p
#p <- qplot(rng_accel_belt, var_accel_belt, col=classe, data=training, alpha=0.1);
#p
#p <- qplot(max_gyros_belt, min_gyros_belt, col=classe, data=training, alpha=0.1));
#p
#p <- qplot(var_gyros_belt, var_magnet_belt, col=classe, data=training, alpha=0.1);
#p
#
#p <- qplot(max_magnet_arm, min_magnet_arm, col=classe, data=training, alpha=0.1);
#p # plot 1
#
#p <- qplot(max_accel_dumbbell, min_accel_dumbbell, col=classe, data=training, alpha=0.1);
#p # plot 2
#
#p <- qplot(var_gyros_dumbbell, max_magnet_dumbbell, col=classe, data=training, alpha=0.1);#
#
#p <- qplot(max_magnet_dumbbell, min_magnet_dumbbell, col=classe, data=training, alpha=0.1);
#p # plot 3#
#
#p <- qplot(max_gyros_forearm, min_gyros_forearm, col=classe, data=training, alpha=0.1);
#p
     
# ---

input_vars_list <- c("new_window", "num_window", "roll_belt", "pitch_belt", "yaw_belt", 
	"total_accel_belt", "roll_arm", "pitch_arm", "yaw_arm", "total_accel_arm", 
	"roll_dumbbell", "pitch_dumbbell", "yaw_dumbbell", "total_accel_dumbbell",
	"roll_forearm", "pitch_forearm", "yaw_forearm", 
	"total_accel_forearm");
input_vars <- exp_input(input_vars_list)

set.seed(2);

st <- Sys.time();
modFit <- train( eval(parse(text=paste("classe ~", input_vars, sep="")))
	,data=training, method="rf")
ed <- Sys.time();

st - ed;


#modFit_gbm <- train( eval(parse(text=paste("classe ~", input_vars, sep="")))
#	,data=training, method="gbm")

# too long...
#modFit_rf2 <- train( eval(parse(text=paste("classe ~", input_vars, sep="")))
#	,data=training, method="rf", prox=TRUE)




#gbm_control <- expand.grid(n.trees=(1:30)*20,interaction.depth=c(1,5,9),
#	shrinkage=0.1);
#modFit_gbm <- train( eval(parse(text=paste("classe ~", input_vars, sep="")))
#	,data=training, method="gbm", tuneGrid=gbm_control);

missClass(training$classe, predict(modFit, training))
# 0
missClass(cv$classe, predict(modFit, cv))
# [1] [1] 0.002039256 # rf


# test sample
answers <- predict(modFit, testing)
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
pml_write_files(answers)





#################################################################################
# _endline