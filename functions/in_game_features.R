
get_ingame_features = function(pitcher_pitches, historical_pitches){
			
	######################################################################
	####															 #####
	####                SELECT RELEVANT IN-GAME FEATURES			 #####
	####															 #####
	######################################################################	
	
	########
	# FEATURES: Pitch Count, In-game performace, runners on base
	########
	
	
	#Initialize ingame feature matrix - will add column by column
	ingame_features = data.frame(num_pitches = pitcher_pitches[,"num_pitches"])
	
	
	########## Pitch Count ###########
	
	# Create index that determines favorability of count
	# -1 = 0-2 count (pitchers count)
	# +1 = 3-0 count (hitters count)
	# Use table on http://www.theoleballgame.com/batting-average-analysis.html
	# for historical count averages - determines how to set index
	
	count_types = c("0-2","1-2","2-2","3-2","0-1","1-1","0-0","2-1","1-0","3-1","2-0","3-0")
	num_of_diff_counts = length(count_types)
	hist_count_av = c(.156, .171, .189, .233, .317, .332, .338, .339, .340, .352, .368, .395)
	
	#Make sequence based on historical count averages
	count_map = 1+(2*(hist_count_av - max(hist_count_av)) / (max(hist_count_av) - min(hist_count_av)))
	count_df = data.frame(count_map, count=count_types)
	
	#Map to dataframe
	count_merge = merge(count_df, pitcher_pitches, by="count", all.y=T)
	count_merge = count_merge[order(count_merge[,"num_pitches"]),]
	
	#Append to feature dataframe
	ingame_features[,"pitch_count"] = count_merge[,"count_map"]
	
	
	########## Runners on Base ###########
	
	# Create index that determines if a runner is on base, and where
	# -1 = no runners on base
	# +1 = bases loaded
	# Make this table linear - could expand to historical performace of pitcher with runners on
	
	#All types of runners on base configurations
	runner_types = c("", "first", "second", "firstsecond", "firstthird", "secondthird", "firstsecondthird")
	runner_map = seq(-1,1, length=length(runner_types))
	runner_map_df = data.frame(runner_types, runner_map)
	
	#Get data from pitchrx data and turn it into a form that matches with our map
	runner_dat = pitcher_pitches[,c("on_1b", "on_2b", "on_3b")]
	runner_dat[is.finite(runner_dat[,"on_1b"]),"on_1b"] = "first"
	runner_dat[is.finite(runner_dat[,"on_2b"]),"on_2b"] = "second"
	runner_dat[is.finite(runner_dat[,"on_3b"]),"on_3b"] = "third"
	runner_dat[is.na(runner_dat)] = ""
	
	#Merge this data back onto pitchrx data
	runners_df = data.frame(runner_types = paste(runner_dat[,1], runner_dat[,2], runner_dat[,3], sep=""), num_pitches = pitcher_pitches[,"num_pitches"])
	runner_merge = merge(runner_map_df, runners_df, by="runner_types", all.y=T)
	runner_merge = runner_merge[order(runner_merge[,"num_pitches"]),]
	
	#Append to feature dataframe
	ingame_features[,"runners"] = runner_merge[,"runner_map"]
	
	
	########## Number of Outs ###########
	
	#Number of outs in the inning
	ingame_features[,"outs"] = pitcher_pitches[,"o"] - 1
	
	
	########## Inning ###########
	
	#Number of outs in the inning
	ingame_features[,"inning"] = pitcher_pitches[,"inning.y"]
		
	########## Percent of Certain Pitch Type ###########
	
	#Percentage of pitches thrown in each of 4 categories during game
	pitch_type_merge = merge(pitcher_pitches, pitch_type_df, by="pitch_type", all.x=T)
	pitch_type_merge = pitch_type_merge[order(pitch_type_merge[,"num_pitches"]),]
	
	
	#Find percentage of certain pitch type as game advances
	pitch_perc = data.frame(array(dim=c(dim(pitch_type_merge)[1], 4)))
	for(i in pitch_type_merge[,"num_pitches"]){
		idist = tapply(pitch_type_merge[1:i,"pitch_category"], pitch_type_merge[1:i,"pitch_category"], 
					function(x, num_pitch){out = length(x)/num_pitch; out=ifelse(is.na(out),0,out)}, i)
		
		if(i!=max(pitch_type_merge[,"num_pitches"])){
			pitch_perc[i+1,] = idist
		}
	}
	pitch_perc[is.na(pitch_perc)] = 0
	colnames(pitch_perc) = paste("perc_",names(idist),sep="")
	
	#Add to ingame features dataframe
	ingame_features = cbind(ingame_features, pitch_perc)
	ingame_features[,"pitch_type"] = pitch_type_merge[,"pitch_category"]
	
	
	########## In-Game Pitching Performance ###########
	
	#Select raw pitchrx cartesian features
	#Horizontal movement, vertical movement, left/right distance, distance of greatest break, break angle, break length, spin, velocity
	cartesian_features = c("pfx_x", "pfx_z", "px", "break_y", "break_angle", "break_length", "spin_rate", "start_speed")
	cart_dat = historical_pitches[,cartesian_features]
	cart_dat = sapply(cart_dat, function(x)as.numeric(as.character(x)))
	
	#Find the mean of a pitcher's movement by pitch type
	cart_means = aggregate(cart_dat, by=list(historical_pitches[,"pitch_type"]), mean, na.rm=T)
	
	#Update deviations from history as more pitches are thrown
	pitch_perf = data.frame(array(dim=c(dim(pitcher_pitches)[1], rep(length(cartesian_features) * length(pitch_category)))))
	for(i in pitcher_pitches[,"num_pitches"]){
		#Select cartesian for just the game
		game_means = aggregate(pitcher_pitches[1:i,cartesian_features], by=list(pitcher_pitches[1:i,"pitch_type"]), mean, na.rm=T)
		cart_means_sel = cart_means[cart_means[,"Group.1"] %in% game_means[,"Group.1"],]
		
		#Need to make features of each cartesian for each pitch type
		#Find the deviation of each type
		if(dim(cart_means_sel)[1] == 0){
			devs_all = rep(NA, length(cartesian_features))
			names(devs_all) = cartesian_features
						
		}else{
			devs_all = cart_means_sel[,cartesian_features] - game_means[,cartesian_features]
		}
		
		#Group into the major pitch categories
		devs_collapse = NULL
		dev_names = NULL
		for(j in 1:length(pitch_category)){
			icategory = pitch_category[j]
			
			if(dim(cart_means_sel)[1] == 0){
				mean_cat = devs_all
				inames = paste(names(devs_all),icategory,sep="_")
			}else{
				sub_cat = cart_means_sel[,"Group.1"] %in% pitch_types[[j]]
				mean_cat = apply(devs_all[sub_cat,], 2, mean, na.rm=T)
				inames = paste(names(mean_cat),icategory,sep="_")
			}
			
			dev_names = c(dev_names, inames)
			devs_collapse = c(devs_collapse, mean_cat)
		}
		
		if(i != max(pitcher_pitches[,"num_pitches"])){
			pitch_perf[i+1,] = devs_collapse
		}
		
	}
	colnames(pitch_perf) = dev_names
	pitch_perf[is.na(pitch_perf)] = 0

	
	#Merge with ingame feature dataset
	ingame_features = cbind(ingame_features, pitch_perf)
	
	
	############ Return feature matrix ################
	
	return(ingame_features)
}








