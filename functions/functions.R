############# INPUTS to FEATURE FUNCTIONS

#To get features, need to have a pitcher name and a game id
make_inputs = function(pitcher_nam = "Clayton Kershaw"){
	#Select pitcher
		
	#Get all game IDs for a pitcher
	pitch_atbat = filter(atbat, pitcher_name==pitcher_nam)
	gids = unique(as.data.frame(pitch_atbat)[,"gameday_link"])
	
	#Partition into testing/training set of games
	smp_size = floor(.75 * length(gids))
	train = sample(1:length(gids), size=smp_size)
	
	train_gids = gids[train]
	test_gids = gids[-train]
	
	gid_sel = train_gids[1]
	test_train_gids = test_gids
	
	return(list(train_gids, test_gids))
}


######################################################################
####
#### LOAD HISTORICAL DATA FROM PITCHRX DATABASE
####
######################################################################

#Want test_train_gids to be opposite the matrix you are building
#i.e. you don't want the historical testing data to be mixed with 
#historical training data

#Takes too long to run for each gid - no gid_sel is embedded in
#historical data - ideally this would not be the case
load_hist_data = function(test_train_gids, pitcher_nam="Clayton Kershaw"){
	#Load all pitch data for pitcher
	atbat = tbl(db, 'atbat')
	ipitcher2 = filter(atbat, pitcher_name==pitcher_nam, !gameday_link %in% test_train_gids)
	ipitcher2_nums = as.data.frame(select(ipitcher2, num))
	
	#Load all pitches for all gameday links
	allpitches = tbl(db, 'pitch')
	allpitches_games = filter(allpitches, !gameday_link %in% test_train_gids, num %in% unique(ipitcher2_nums)$num, !is.na(start_speed))
	
	#Join pitcher with pitches to get just pitches by pitcher
	allpitches_pitcher = inner_join(allpitches_games, ipitcher2, by=c("num", "url"))
	historical_df = as.data.frame(allpitches_pitcher)
	historical_pitches = merge(historical_df, pitch_type_df, by="pitch_type", all=T)
	
	historical_pitches = historical_pitches[!is.na(historical_pitches[,"pitcher_name"]),]
	
	return(historical_pitches)
}

######################################################################
####
#### LOAD GAME DATA FROM PITCHRX DATABASE
####
######################################################################

#For each game ID in either testing or training set, get pitch data
load_game_pitches = function(gid_sel){
	#Get one game - one pitcher performance
	ipitcher = filter(atbat, pitcher_name==pitcher_nam, gameday_link == gid_sel)
	pitches = filter(tbl(db, 'pitch'), gameday_link == gid_sel)
	
	#Convert selections to data.frames - they should be small so SQL unnecessary
	pitches_df = as.data.frame(pitches)
	ipitcher_df = as.data.frame(ipitcher)
	
	#Merge pitch/atbat dataframe to get total in-game performance for a pitcher
	pitcher_pitches = merge(pitches_df, ipitcher_df, by = c('num'))
	
	pitcher_pitches = data.frame(pitcher_pitches)
	pitcher_pitches = pitcher_pitches[order(pitcher_pitches[,"id"]),]
	pitcher_pitches[,"num_pitches"] = 1:dim(pitcher_pitches)[1]

	
	return(pitcher_pitches)
}

#Function to balance features
#Balance by undersampling majority classes to minority class
balance_classes = function(training_features, training_response){
	type_counts = tapply(training_response, training_response, length)
	type_counts = type_counts[!is.na(type_counts)]
	min_ind = type_counts == min(type_counts)
	min_class = type_counts[min_ind]
	
	#Sample to other classes to size of min_class
	new_features = training_features[training_response == names(type_counts)[min_ind],]
	new_response = names(type_counts)[training_response[training_response == names(type_counts)[min_ind]]]
	for(i in which(!min_ind)){
		iindex = which(training_response == names(type_counts)[i])
		smpl = sample(iindex, size=min_class)
		new_features = rbind(new_features, training_features[smpl,])
		new_response = c(new_response, names(type_counts)[training_response[smpl]])
	}
	return(list(new_features, as.factor(new_response)))
}

class_encoder = function(responses){
	encodes = rep(NA, length(responses))
	encodes[responses == "fastball"] = 1
	encodes[responses == "breaking"] = 2
	encodes[responses == "offspeed"] = 3
	return(encodes)
}


#Make baseline classification
base_class = function(responses){
	type_counts = tapply(responses, responses, length)
	type_counts = type_counts[!is.na(type_counts)]
	most_pitches = which(type_counts ==max(type_counts))
	pred_base = type_counts[most_pitches] / length(responses)
	return(pred_base)
}

#Create function to fit learning algorithm based on glmnet
#Feed function type of loss, and type of norm
run_glmnet = function(family_type, lambda_type, alpha_type, training_features, training_response, testing_features, testing_response){
	
	#Make multi-response matrix for glmnet
	nc_mat = 1*cbind(training_response=="fastball", training_response=="breaking", training_response=="offspeed")
	
	#Fit model and cross validate: 10-fold
	cv_glm1 = cv.glmnet(training_features, y=nc_mat, family=family_type, alpha=alpha_type)
	test_pred = predict(cv_glm1, newx = testing_features, s = lambda_type, type = "class")
	train_pred = predict(cv_glm1, newx = training_features, s = lambda_type, type = "class")
	coefs = coef(cv_glm1, s=lambda_type)
	cv_error = cv_glm1$cvm[cv_glm1$lambda == cv_glm1[lambda_type]]
	
	#Encode training/test to match output of glmnet
	encode_test = class_encoder(testing_response)
	encode_train = class_encoder(training_response)
	
	#Get accuracy
	train_accuracy = sum(train_pred == encode_train, na.rm=T) / length(train_pred)
	test_accuracy = sum(test_pred == encode_test, na.rm=T) / length(test_pred)
	
	return(list(test_pred, train_pred, coefs, cv_error, train_accuracy, test_accuracy))
}

#Plot coefficients
plot_coef_en = function(which_ind){
	ind_names = c("fastball","breaking","offspeed")
	which_nam = ind_names[which_ind]
	
	which_zeros = apply(en_coefs, 1, function(x)sum(abs(x)<.1))
	non_sparse = names(which_zeros)[which_zeros != 30]
	nsind = dimnames(en_coefs)[[1]] %in% non_sparse 
	N = length(non_sparse)
	
	colbar = colorRampPalette(c("darkorchid4","dodgerblue","indianred2","firebrick4"),space="rgb")(length(alpha_seq))
	par(mar=c(10,5,5,5))
	plot(1:N, rep(NA, N) , ylim=c(-4,4), xlab="", ylab="", xaxt="n")
	axis(1, at=1:N, dimnames(en_coefs)[[1]][nsind], las=2)
	abline(v = (1:N)-.5, lty=3, col=adjustcolor(1,.2))
	title(paste(which_nam,"coefficients"))
	for(i in 1:length(alpha_seq)){
		#bplt = barplot(en_coefs[,i,1], xaxt="n", border=NA, col=adjustcolor(colbar[i],.5))
		lines(jitter(1:N), en_coefs[nsind,i,which_ind], type="h", col=adjustcolor(colbar[i],.5), lwd=2)		
	}
}
