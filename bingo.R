bingo <- function(number.sheets=10, filename="bingo.pdf", directory=getwd(), bank=NA){
	if(is.na(bank)){
		warning("No word bank specified. Using default, selected for the 2016 presidential debates. 
		        To specify a different word bank, create a character with at least 30 phrases in quotations, 
		        and call this object in the bank option of the bingo() command. Since 24 phrases are used on each sheet, 
		        at least 50 are recommended.", immediate.=TRUE)
	
	w<-0
	#Choose bingo words
    
	  #Inane personal attacks
	  w[1] <- "Pneumonia"
	  w[2] <- "Email"
	  w[3] <- "Liar"
	  w[4] <- "Birther"
	  w[5] <- "Putin"
	  w[6] <- "Bigot"
	  w[7] <- "Corrupt"
	  w[8] <- "Bankrupt"
	  w[9]<-"Politically correct"
	
		#Other countries
		w[10]<-"Turkey"
		w[11]<-"Iran"
		w[12]<-"China"
		w[13]<-"Syria"
		w[14]<-"Mexico"

		#Feable attempts at discussing domestic policy
		w[15]<-"Climate change"
		w[16]<-"Income inequality"
		w[17]<-"Budget"
		w[18]<-"Unemployment"
		w[19]<-"Wall Street"
		w[20]<-"Social security" 
		w[21]<-"Uninsured" 
		w[22]<-"Gun violence" 
		w[23]<-"Second amendment"
		w[24]<-"National debt"
		w[25]<-"Tax cut"
		
		#Even more feable attempts at discussing foreign policy
		w[26]<-"Diplomacy"
		w[27]<-"NATO" 
		w[28]<-"Strategy"
		w[29]<-"Al Qaeda"
		w[30]<-"Fundamentalist"
		w[31]<-"Nuclear"
		
		#Fear mongering, overt racism, paranoia
		w[32]<-"Muslim"
		w[33] <- "Wall (not street)"
		w[34]<- "Islamic terrorism"
		w[35]<- "Illegal immigrant"

    #Things they should talk about but probably won't
		w[36]<-"Black Lives"
		w[37]<-"Trans-Pacific" 
		w[38]<-"Poverty"
		w[39]<-"College"
		
		#Meaningless calls to action
		w[40]<-"Vision"
		w[41]<-"Future"
		w[42]<-"Peace"
		w[43]<-"Leadership"
		w[44]<-"Constitution"
		w[45]<-"Reform"
		
		#Platitudes
		w[46]<-"Believe me"
		w[47]<-"Great again"
		w[48]<-"God"
		w[49]<-"Freedom"
		w[50]<-"Democracy"
  
	} else if(!is.character(bank)){
		stop("Word bank must be a character vector of length at least 30.  Be sure that all elements of the vector are in quotes.")
	} else if(length(bank)<30){
		stop("The word bank must contain at least 30 elements.")
	} else w <- bank
	
	setwd(directory)
	pdf(filename, width=11, height=8.5)
	for(k in 1:number.sheets) {
		rand<-runif(length(w), min=0, max=1)
		rw<-order(rand)
		z<-0
		for(i in 1:24)z[i]<-w[rw[i]]
		y<-matrix(append(z,"Free Square!",after=12),5,5)
		
		
		b<- 25
		a<- -b
		r<-seq(a,b,length=5000)
		
		x<-rep( a,5000)
		plot(x,r,type="l",axes=F,xlim=c(a,b),ylim=c(a,b),xlab="",ylab="")
		par(new=T)
		plot(r,x,type="l",axes=F,xlim=c(a,b),ylim=c(a,b),xlab="",ylab="")
		for (i in 1:5) {
			par(new=T)
			x<-rep( a+((b-a)/5)*i,5000)
			plot(x,r,type="l",axes=F,xlim=c(a,b),ylim=c(a,b),xlab="",ylab="")
			par(new=T)
			plot(r,x,type="l",axes=F,xlim=c(a,b),ylim=c(a,b),xlab="",ylab="")
		}
		for(i in 1:5) {
			for(j in 1:5) {
				text(a+((b-a)/10)*(2*i-1),a+((b-a)/10)*(2*j-1),y[i,j],cex=.9)
			}
		}
	}
	dev.off()
}

