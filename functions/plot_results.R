##################################################################
###########           ANALYZE MODEL OUTPUT            ############
##################################################################

library(pitchRx)
library(arrayhelpers)
library(dplyr)
library(glmnet)
library(SGL)
library(zoo)
filter = dplyr::filter
setwd('/n/home00/dcusworth/Baseball/Final')
source('/n/home00/dcusworth/Baseball/Final/in_game_features.R', chdir = TRUE)
source('/n/home00/dcusworth/Baseball/Final/handed_features.R', chdir = TRUE)
source('/n/home00/dcusworth/Baseball/Final/functions.R', chdir = TRUE)
set.seed(200)


######################################################################
#Load pitching database

db = src_sqlite('/n/home00/dcusworth/Baseball/Final/pitchfx.sqlite3.2012_on')
atbat = tbl(db, 'atbat')

##################### MULTINOMIAL CASE ########################################
#Load results from model
load("elastic_net_pitchers_hands.rda")
hands = pitcher_output

load("elastic_net_pitchers.rda")
nohand = pitcher_output

#Pitcher names
pitch_names = NULL
for(i in 1:length(hands)){
	pitch_names = c(pitch_names, hands[[i]][[1]])
}

plot_test_data = function(pitcher_dat, doplot=F){
	
	en_coef = pitcher_dat[1][[1]]
	alpha_seq = as.numeric(dimnames(en_coef)[[2]])
	test_acc = pitcher_dat[2][[1]]
	train_acc = pitcher_dat[3][[1]]
	testing_response = pitcher_dat[5][[1]]

	if(doplot){
		#Fastball coefficients
		plot_coef_en(1, en_coef)

		#Breaking coefficients
		plot_coef_en(2, en_coef)

		#Offspeed coefficients
		plot_coef_en(3, en_coef)

	}
	#Does model outperform baseline?
	does_outperform = max(test_acc) > base_class(testing_response)

	return(list(does_outperform, max(test_acc), base_class(testing_response),mean(alpha_seq[test_acc == max(test_acc)])))

}

#See if classifier outperforms baseline
#See by how much classifier performs compared to baseline
#See which alpha gives best accuracy on test set
perform_df = NULL
for(i in 1:length(nohand)){
	inohand = plot_test_data(nohand[[i]][[2]])

	if(length(inohand[[1]]) !=0){	
	idat = data.frame(pitcher = nohand[[i]][[1]], outper = inohand[[1]], tacc = inohand[[2]],  
				base = inohand[[3]],  alpha = inohand[[4]])

	perform_df = rbind(perform_df, idat)
	}
}


#For pitchers who outperform baseline,
#plot % above baseline and alpha level
outperformers = perform_df[perform_df[,"outper"] == T,]
outperformers[,"lift"] = outperformers[,"tacc"] - outperformers[,"base"]
outperformers = outperformers[order(outperformers[,"lift"], decreasing=T),]
print(paste("There are", dim(outperformers)[1],"outperformers of", dim(perform_df)[1]))
print(paste("Thus", 100*dim(outperformers)[1]/dim(perform_df)[1],"outperformed"))

png("multi_outperform.png", res=200, height=8, width=15, units="in")

N = 1:dim(outperformers)[1]
par(mar=c(15,5,5,5))
plot(N, 100*(outperformers[,"tacc"] - outperformers[,"base"]), col="dodgerblue",type="h", xaxt="n", xlab="", lwd=10, xlim=c(1,dim(outperformers)[1]+1), ylab="", cex.axis=1.25, ylim=c(0,4.5))
axis(1, N+.1, outperformers[,"pitcher"], las=2, cex.axis=1.25)
par(new=T)
plot(N+.2, outperformers[,"alpha"], axes=F, lwd=10, xlab="",ylab="", col="indianred2", type="h", ylim=c(0,1), xlim=c(1,dim(outperformers)[1]+1))
axis(4, seq(0,1,length=3), col="indianred2", col.axis="indianred2", cex.axis=1.25)

dev.off()


#Plot coefficients for a few pitchers
pitcher1 = "A.J. Burnett"
which_ind = which(perform_df[,"pitcher"] == pitcher1)
pdat = nohand[[which_ind]]

png("aj_fastball.png", res=200, height=8, width=15, units="in")
plot_coef_en(1, pdat[[2]][[1]])
dev.off()

png("aj_breaking.png", res=200, height=8, width=15, units="in")
plot_coef_en(2, pdat[[2]][[1]])
dev.off()

png("aj_offspeed.png", res=200, height=8, width=15, units="in")
plot_coef_en(3, pdat[[2]][[1]])
dev.off()


 
################# BINOMAL CASE ############################

#Load results from model
load("binomial_hands.rda")
bhands = pitcher_output

load("binomial_no_hands.rda")
bnohand = pitcher_output


get_binom_dat = function(pitcher_dat){
	
	en_coef = pitcher_dat[[1]]
	alpha_seq = as.numeric(dimnames(en_coef)[[2]])
	test_acc = pitcher_dat[[2]]
	train_acc = pitcher_dat[[3]]
	testing_response = pitcher_dat[[5]]
	
	test_resp = as.character(testing_response[testing_response!="unknown"]);
	pitch_count = tapply(test_resp, test_resp, length)
	most_pitched = names(pitch_count)[pitch_count == max(pitch_count,na.rm=T)][1]
	if(most_pitched == "fastball"){
		base_pred = 1*(as.character(testing_response) == "fastball")
	}else{
		base_pred = 1*(as.character(testing_response) != "fastball")
	}
	base_acc = sum(base_pred) / length(base_pred)

	#Does model outperform baseline?
	does_outperform = max(test_acc) > base_acc

	return(list(does_outperform, max(test_acc), base_acc, mean(alpha_seq[test_acc == max(test_acc)])))

}


#See if classifier outperforms baseline
#See by how much classifier performs compared to baseline
#See which alpha gives best accuracy on test set
bperform_df = NULL
for(i in 1:length(bnohand)){
        inohand = get_binom_dat(bnohand[[i]][[2]])

        if(length(inohand[[1]]) !=0){
        idat = data.frame(pitcher = bnohand[[i]][[1]], outper = inohand[[1]], tacc = inohand[[2]],
                                base = inohand[[3]],  alpha = inohand[[4]])

        bperform_df = rbind(bperform_df, idat)
        }
}


#Sort outperformers and plot top
boutperformers = bperform_df[bperform_df[,"outper"] == T,]
boutperformers[,"lift"] = boutperformers[,"tacc"] - boutperformers[,"base"]
print(paste("There are", dim(boutperformers)[1],"outperformers of", dim(bperform_df)[1]))
print(paste("Thus", 100*dim(boutperformers)[1]/dim(bperform_df)[1],"outperformed"))


top_binom = boutperformers[order(boutperformers[,"lift"], decreasing=T),]
top_binom = top_binom[1:12,]


#Plot top performers
png("binom_outperform.png", res=200, height=8, width=15, units="in")

N = 1:dim(top_binom)[1];
par(mar=c(15,5,5,5));
plot(N, 100*(top_binom[,"lift"]), col="dodgerblue",type="h", xaxt="n", xlab="", lwd=10, xlim=c(1,dim(top_binom)[1]+1), ylab="", cex.axis=1.25, ylim=c(0,19));
axis(1, N+.1, top_binom[,"pitcher"], las=2, cex.axis=1.25);
par(new=T);
plot(N+.2, top_binom[,"alpha"], axes=F, lwd=10, xlab="",ylab="", col="indianred2", type="h", ylim=c(0,1), xlim=c(1,dim(top_binom)[1]+1));
axis(4, seq(0,1,length=3), col="indianred2", col.axis="indianred2", cex.axis=1.25)

dev.off()



#Plot pitcher coefficients
png("felix_binom.png", res=200, height=8, width=15, units="in")
pitcher1 = "Felix Hernandez"
which_ind = which(bperform_df[,"pitcher"] == pitcher1)
pdat = bnohand[[which_ind]]
plot_coef_bi(pdat[[2]][[1]])
dev.off()

png("clayton_binom.png", res=200, height=8, width=15, units="in")
pitcher1 = "Clayton Kershaw"
which_ind = which(bperform_df[,"pitcher"] == pitcher1)
pdat = bnohand[[which_ind]]
plot_coef_bi(pdat[[2]][[1]])
dev.off()


#Plot colorbar
library(fields)
png("color_scale.png", res=200, height=8, width=15, units="in")
colbar = colorRampPalette(c("darkblue","dodgerblue","indianred2","firebrick4"),space="rgb")(10)
image.plot(matrix(1:10,nrow=2), col=adjustcolor(colbar,.5))
dev.off()





