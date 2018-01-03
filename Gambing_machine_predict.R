# An entrepreneur has devised a gambling machine that chooses two independent random variables x and y that are uniformly and independently distributed between 0 and 100. He plans to tell any customer the value of x and to ask him whether y > x or x > y. 

# If the customer guesses correctly, he is given y dollars. If x = y, he’s given y/2 dollars. And if he’s wrong about which is larger, he’s given nothing. 

# The entrepreneur plans to charge his customers $40 for the privilege of playing the game. Would you play? 

N<-100000 
x<-sample.int(101,N,replace=TRUE)-1 
y<-sample.int(101,N,replace=TRUE)-1 

expected<-rep(NA,101) 
for (cutoff in 0:100) 
{ 
  expected[cutoff+1]<-mean((((x>=cutoff)==(x>y))&(x!=y))*y+(x==y)*y/2) 
} 

plot(expected) 
