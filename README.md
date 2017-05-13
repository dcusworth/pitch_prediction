# Predicting a MLB pitcher's next pitch using PITCHf/x
## Daniel Cusworth

This repository contains a learning algorithm to predict a pitcher’s next pitch given a particular pitcher’s in-game and historical pitching performances [(PITCHf/x)](http://www.sportvision.com/baseball/pitchfx). I use a logistic loss function with an elastic net penalty, and train a unique model on several pitchers individually (with 10-fold cross-validation) while varying the elastic net parameter. I find that for binary classification (fastball, non-fastball), 42 out of 70 trained pitchers outperformed their baseline models. For multi-class classification (fastball, offspeed, breaking), only 12 models/pitchers outperformed the baseline. I find relatively small influence of the elastic net parameter in improving accuracy of the model, and see that even the non-sparse algorithm has a similar fitted weight structure as sparse solutions.


All functions used to build the classifier and query PITCHf/x are contained in the "functions" subdirectory. For a more formal presentation of methods and results, consult the "paper" and "poster" subdirectories.


Below are the results of the top baseline-beating pitchers for binary classification. We see that many elite pitchers are considered predictable by this model. One interpretation of this result is that elite pitchers have such good stuff, that they do not need to worry too much about pitch sequencing to deceive hitters.

<img src="https://github.com/dcusworth/pitch_prediction/blob/master/img/bin1.png" alt="binary" WIDTH="700"/>


The fitted weights of the binary classifier (i.e., factors determining whether or not a fastball will be thrown next) for Clayton Kershaw are shown below. An interpretation of these results is that if the break in the y-direction of his offspeed pitch (here principally the slider) is deviating largely from its historical mean, then he is more likely to go to his fastball. Also, the cofficients indicate that he is less likely to go to the fastball if he has been favoring the pitch (and his slider) too much during the course of the game. 


<img src="https://github.com/dcusworth/pitch_prediction/blob/master/img/clayton1.png" alt="clayton" WIDTH="700"/>


The code in this repository can be used in a similar fashion to find fitted weights for any major league pitcher. Enjoy!
