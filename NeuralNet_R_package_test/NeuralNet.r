install.packages('neuralnet')
library("neuralnet")



CTRcdi <- cdi[which(cdi$Trt=="CTR"),]
Cpmeans <-  GroupSummary(X=c("Cp"), FOR=c("Plvl"), DATA=CTRcdi, SUMMARY=c("mean"), SUBSETBY=NULL, EXCELTAB=FALSE)[[1]]$Aggregated_Data$mean



mynet <- neuralnet(rR~Cp+Tps,CTRcdi, hidden=c(5,5,5,5,5,5,5,5,5), threshold=0.01)


#Test the neural network on some training data
maxt <- 10000
testdata <- data.frame(Cp=rep(Cpmeans, each=(maxt+1)), Tps=rep(0:maxt,length(Cpmeans))) #Generate some squared numbers
net.results <- compute(mynet, testdata) #Run them through the neural network
 
#Lets see what properties net.sqrt has
# ls(net.results)
 
#Lets see the results
# print(net.results$net.result)
 
#Lets display a better version of the results
cleanoutput <- cbind(testdata,
                         as.data.frame(net.results$net.result))
colnames(cleanoutput) <- c("Cp","Tps","net_rR")
# print(cleanoutput)

plot(rR ~ Tps, data=CTRcdi, col=as.numeric(as.factor(as.character(Plvl))), ylim=c(0,1), xlim=c(0,max(cleanoutput$Tps)))
points(y=cleanoutput$net_rR, x=cleanoutput$Tps, col=rep(c(1:5), each=(maxt+1)), cex=.5)

