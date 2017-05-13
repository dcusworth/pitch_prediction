handed_features = function(historical_pitches){
	
	##############################################################
	##															##
	## Get matchup of pitcher vs. batters of certain handedness ##
	##															##
	##############################################################

	#pitcher_pitches = game_pitches
	#historical_pitches = historical_test
	
	#swing strike, called strike, foul, ball, in play
	hand_outcome = tapply(historical_pitches[,"des"], list(historical_pitches[,"des"], historical_pitches[,"stand"], historical_pitches[,"pitch_category"]), length)
	hand_size = tapply(historical_pitches[,"des"], list(historical_pitches[,"stand"], historical_pitches[,"pitch_category"]), length)

	#Map outcome descriptions to names
	outcomes_map = list(c("Swinging Strike", "Swinging Strike (Blocked)"), c("Called Strike"), c("Foul", "Foul (Runner Going)", "Foul Bunt", "Foul Tip"), c("Ball", "Ball In Dirt", "Intent Ball", "Pitchout", "Hit By Pitch"),c("In play, no out", "In play, out(s)", "In play, run(s)"))
	
	#Count how much outcome occurs for pitcher
	swing_strike = apply(hand_outcome[dimnames(hand_outcome)[[1]] %in% outcomes_map[[1]], ,], 2:3, sum, na.rm=T)
	called_strike = hand_outcome[dimnames(hand_outcome)[[1]] %in% outcomes_map[[2]],, ]
	foul = apply(hand_outcome[dimnames(hand_outcome)[[1]] %in% outcomes_map[[3]],, ], 2:3, sum, na.rm=T)
	ball = apply(hand_outcome[dimnames(hand_outcome)[[1]] %in% outcomes_map[[4]], ,], 2:3, sum, na.rm=T)
	inplay = apply(hand_outcome[dimnames(hand_outcome)[[1]] %in% outcomes_map[[5]],, ], 2:3, sum, na.rm=T)
	
	#Find percentage of outcome by hand
	ss_perc = swing_strike / hand_size
	cs_perc = called_strike / hand_size
	fl_perc = foul / hand_size
	bl_perc = ball / hand_size
	ip_perc = inplay / hand_size
	
	out_cbind = cbind(ss_perc,cs_perc,fl_perc,bl_perc,ip_perc)
	outcomes = c("ss", "cs", "fl", "bl", "ip")
	colnames(out_cbind) = paste(outcomes, colnames(out_cbind),sep="_")
	
	return(data.frame(stand=rownames(out_cbind),out_cbind))

}