##################################################################
###########										      ############
###########      DRIVER TO MAKE MODEL MATRIX:         ############
#																 #
# This function creates the set of features that are used to     #
# drive the learning algorithm									 #
#																 #
# Calls ingame feature scripts, and historical features scripts  #
#																 #
#########										       ###########
#########										       ###########
##################################################################

library(pitchRx)
library(arrayhelpers)
library(dplyr)
library(glmnet)
library(SGL)
library(zoo)
filter = dplyr::filter
setwd('/Volumes/TRANSCEND/MLB')
source('~/Box Sync/School/2016 Fall/MIT 9.520/Final/in_game_features.R', chdir = TRUE)
source('~/Box Sync/School/2016 Fall/MIT 9.520/Final/handed_features.R', chdir = TRUE)
source('~/Box Sync/School/2016 Fall/MIT 9.520/Final/functions.R', chdir = TRUE)
set.seed(200)


######################################################################
#Load pitching database

db = src_sqlite('/Volumes/TRANSCEND/MLB/pitchfx.sqlite3.2012_on')
atbat = tbl(db, 'atbat')

######################################################################

############ GLOBAL VARIABLES ##############

#Pitch types - each pitch belongs to a category of pitches
#Fastball, offspeed, breaking ball, uncategorized

pitch_types = list(c("FA","FF","FT","FC", "FS", "SI", "SF"), 
					c("SL","CH"),
					c("CB","CU","KC","KN","EP"),
					c("UN","XX","PO","FO")
					)

pitch_category = c("fastball","offspeed","breaking","unknown")

pitch_type_df = data.frame(pitch_type = c("FA","FF","FT","FC", "FS", "SI", "SF", "SL", "CH", 
									"CB","CU","KC","KN","EP","UN","XX","PO","FO"),
							pitch_category = c(rep("fastball", length(pitch_types[[1]])), 
										rep("offspeed", length(pitch_types[[2]])),
										rep("breaking", length(pitch_types[[3]])),
										rep("unknown", length(pitch_types[[4]]))))


######################################################################	

run_pitcher_fitting = function(pitcher_nam){
	
	print(paste("Running for", pitcher_nam))

	######################################################################
	####
	#### SOURCE FEATURES
	####
	######################################################################
	
	
	#Make training/testing dataframes by looping over all game id's
	#and combining datasets
	
	inputs = make_inputs(pitcher_nam = pitcher_nam)
	train_gids = inputs[[1]]
	test_gids = inputs[[2]]
	
	#Make Training Set
	historical_train = load_hist_data(test_gids, pitcher_nam = pitcher_nam)
	handed = handed_features(historical_train)
	training = NULL
	for(i in 1:length(train_gids)){
		game_pitches = load_game_pitches(train_gids[i])
		handed_merge = merge(game_pitches, handed, by="stand", all.x=T)
		handed_merge = handed_merge[order(handed_merge[,"num_pitches"]),]
		handed_merge = handed_merge[,colnames(handed)[colnames(handed) != "stand"]]
		ingame = get_ingame_features(game_pitches, historical_train)
		ifeatures = ingame #cbind(ingame, handed_merge)
		training = rbind(training, ifeatures)
		print(paste("building training: game", i, "of", length(train_gids)))
	}
	
	#Clean up training set
	training_features = training[, !(colnames(training) %in% "pitch_type")]
	training_features = sapply(as.data.frame(training_features), function(x)as.numeric(as.character(x)))
	training_response = training[, (colnames(training) %in% "pitch_type")]
	
	#Remove features that are all zeros
	sel_zeros = apply(training_features, 2, function(x)sum(x==0, na.rm=T) == length(x)) | grepl("unknown", colnames(training_features))
	training_features = as.data.frame(training_features[,!sel_zeros])
	
	#Remove "unknown" pitches from model
	unknown_pitches = training_response != "unknown"
	training_features = training_features[unknown_pitches,]
	training_response = training_response[unknown_pitches]
	training_features = as.matrix(training_features)
	sel_comp = complete.cases(training_features)
	training_features = training_features[sel_comp,]
	training_response = training_response[sel_comp]
	
	#Balance
	balanced_data = balance_classes(training_features, training_response)
	balanced_features = as.matrix(balanced_data[[1]])
	balanced_response = balanced_data[[2]]
	
	#Make Test Set
	historical_test = load_hist_data(train_gids, pitcher_nam = pitcher_nam)
	handed = handed_features(historical_test)
	testing = NULL
	for(i in 1:length(test_gids)){
		game_pitches = load_game_pitches(test_gids[i])
		handed_merge = merge(game_pitches, handed, by="stand", all.x=T)
		handed_merge = handed_merge[order(handed_merge[,"num_pitches"]),]
		handed_merge = handed_merge[,colnames(handed)[colnames(handed) != "stand"]]
		ingame = get_ingame_features(game_pitches, historical_test)
		ifeatures = ingame #cbind(ingame, handed_merge)
		testing = rbind(testing, ifeatures)
		print(paste("building testing: game", i, "of", length(test_gids)))
	}
	testing_features = testing[, !(colnames(testing) %in% "pitch_type")]
	testing_response = testing[, (colnames(testing) %in% "pitch_type")]
	testing_features = as.data.frame(testing_features[,!sel_zeros])
	
	#Remove "unknown" pitches from model
	unknown_pitches2 = testing_response != "unknown"
	testing_features = testing_features[unknown_pitches2,]
	testing_response = testing_response[unknown_pitches2]
	testing_features = as.matrix(testing_features)
	sel_comp2 = complete.cases(testing_features)
	testing_features = testing_features[sel_comp2,]
	testing_response = testing_response[sel_comp2]
	
	######################################################################
	####
	#### FIT MODEL
	####
	######################################################################
	
	
	######### FIT ON ENTIRE DATASET #############
	
	#Make groups based on features
	# 1 = Traditional (Pitch Count, Inning, Outs, Runners on base, # Pitches thrown)
	# 2 = Pitch Frequency (% Fastball, Breaking, Offspeed)
	# 3 = In-Game pitch performace (Speed/Distance/etc. per pitch deviation)
	# 4 = Matchup vs handedness
	# 5 = Matchup vs player
	
	
	########## ELASTIC NET ###########
	
	#Fit function over range of alphas: 0-ridge regression, 1-lasso regression
	#Compare error and testing accuracy for each alpha
	
	#Define alpha array
	alpha_seq = seq(0,1,length=10)
	
	#Loop and fit model over all alphas - save results
	en_coefs = array(dim=c(length(colnames(training_features))+1, length(alpha_seq), 3))
	dimnames(en_coefs) = list(c("Intercept",colnames(training_features)), alpha_seq, 1:3)
	test_acc = NULL
	train_acc = NULL
	cv_err = NULL
	for(i in 1:length(alpha_seq)){
		glmdat = run_glmnet(family_type="multinomial", lambda_type="lambda.min", alpha_type=alpha_seq[i], training_features = balanced_features, training_response = balanced_response, testing_features=testing_features, testing_response = testing_response)
		en_coefs[,i,] = cbind(as.matrix(glmdat[[3]][[1]]), as.matrix(glmdat[[3]][[2]]), as.matrix(glmdat[[3]][[3]]))
		cv_err[i] = glmdat[[4]]
		train_acc[i] = glmdat[[5]]
		test_acc[i] = glmdat[[6]]
		print(paste("Cross-validating: alpha", i, "of",length(alpha_seq)))	
	}
	
	return(list(en_coefs, test_acc, train_acc, cv_err, testing_response))
}

######################################################

#Get list of pitchers over last few years
#Set threshold of number of pitches thrown
#Subsample pitchers from list
#Running fitting algorithm over pitchers
grouped1 = group_by(atbat, pitcher_name)
counted1 = summarise(grouped1, pitcher_name, total_pitches= sum(num))
top_pitches_df = as.data.frame(counted1)

#Get top pitchers (by number of pitches thrown)
top_pitchers = top_pitches_df[order(top_pitches_df[,"total_pitches"], decreasing=T), "pitcher_name"][1:20]

#Run Elastic Net for each of these pitchers
pitcher_output = NULL
for(i in 1:length(top_pitchers)){
	pitcher_output[[i]] = list(pitcher_name = top_pitchers[i], run_pitcher_fitting(top_pitchers[i]))
}

save(pitcher_output, file="elastic_net_pitchers.rda")



########################################################

#Plot coefficients


#Fastball coefficients
#plot_coef_en(1)

#Breaking coefficients
#plot_coef_en(2)

#Offspeed coefficients
#plot_coef_en(3)


#Plot errors/accuracy
#plot(alpha_seq, train_acc, ylim=c(.4,.7), type="o", col="dodgerblue", lwd=3, pch=21, bg="dodgerblue")
#lines(alpha_seq, test_acc, type="o", col="indianred2", lwd=3, pch=21, bg="indianred2")
#abline(h=base_class(testing_response), lwd=3, lty=2, col=adjustcolor(1,.5))
#par(new=T)
#yylim=range(cv_err)+2*c(-sd(cv_err), sd(cv_err))
#plot(alpha_seq, cv_err, axes=F, ylim=yylim, type="o", col=adjustcolor("darkgreen",.5), lwd=3, pch=21, bg=adjustcolor("darkgreen",.5))
#axis(4, round(seq(yylim[1], yylim[2], length=4),2), col.axis="darkgreen", col="darkgreen")








######### FIT ON SPECIFIC INNING #############



















