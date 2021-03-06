#Female movement model to test choice-copy behaviour


#Create a grid witn NxN cells to represent lek arena, each cell is a territory (value at each cell represents, no. of females at that cell)

N=5    # keeping it close to number of males in the videos
lek = matrix(0,nrow=N,ncol=N)

#Declare a vector representing each female's current territory, length of vector is M=total no. of females

M=10  #Number of females
stateM = vector(length=M)

#attraction factor (copying parameter)

#cp = 0.5  #introduce copying strength

T=1000 # Number of time-steps for the simulation

Tot = 0  ##Keep appending no. of females in all the terrirtories, to generate histogram in the end

for(t in 1:T){
lekP= matrix(runif(N*N),nrow=N,ncol=N)    #baseline uniform distribution, no preference for location or males
if(sum(lek)!=0){
  femP= lek/sum(lek)   #probability increment due to copying effect
}else{
  femP=1
}

##For null model just set femP=1
#femP=1
comP=  (lekP*femP)/(lekP*femP+(1-lekP)*(1-femP))h
comP=as.vector(comP)
##Now each female chooses a territory to move to from above probability distribution(synchronous updat)

stateM=sample(1:(N*N),size=M,prob=comP,replace=TRUE)

##subtraction from cells
if(t>1){
  lek[as.integer(names(freq_p))]=lek[as.integer(names(freq_p))]-freq_p  
}
freq_c=table(stateM)  #frequency for current step
lek[as.integer(names(freq_c))]=lek[as.integer(names(freq_c))]+freq_c  #addition to cells
freq_p = freq_c  # frequency for last state  
Tot=append(Tot,as.vector(lek))

#DISPLAY A GRID
plot(0,0,xlim=c(1,N+1),ylim=c(1,N+1),xaxt="n",xlab="",yaxt="n",ylab="")
grid(nx=N,ny=N, lwd = 4) # grid only in y-direction
#Plot female movement
ny=as.integer(stateM/N)+1
nx=stateM%%N
nx[which(nx==0)]=N
points(nx-runif(M)+1,ny-runif(M)+1,pch=13,col="blue",cex=2)
Sys.sleep(0.5) 

}

hist(Tot)



