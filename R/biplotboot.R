biplotboot <-
function(x)
{

#############################################################################
#########	libraries
#############################################################################

tclRequire("BWidget")

mientorno <- new.env()

symbols <- c("*",".", "o","O","0","+","-","|","%","#")
tipo<-"RCMP" 
nejes<-3  
dim1<-1
dim2<-2
dim3<-3 
niter<-100
alphaic <- 95
indicei<-NULL
Namei<-NULL
Cexi<-1
NameCexi<-NULL
colori<-NULL
simChoicei<-NULL
Namei<-NULL
colores<-c()  
indicev<-NULL
Namev<-NULL
NameValv<-NULL
Cexv<-1
NameCexv<-NULL  
colorv<-NULL
colorder<-NULL
Nameder<-NULL
Cexder<- NULL
atrvariables <- c("indicev", "colorv")
atrindividuos <- c("indicei", "colori")
descom<-NULL
inerciatot<-NULL
msginertia<-NULL
nejes<-NULL
cbVal<-NULL
cgfVal <- NULL
ccrVal <- NULL
ceiVal <- NULL
cavarVal <- NULL
cavarjVal <- NULL
#cvcooVal <- NULL
ccrtjVal <- NULL
ccrejfqVal <- NULL
ccrfqejVal <- NULL
#clvVal <- NULL
anteriorx <- NULL
anteriory <- NULL
xCoords <- NULL
yCoords <- NULL
zCoords <- NULL
datos <- NULL
textos <- NULL
indexClosest <- NULL
indexLabeled <- NULL                           
indexLabeledaux <- NULL
parPlotSize <- NULL
usrCoords <- NULL
tChoice <- "Raw data"
img <- NULL
imgbar <- NULL
covartotal <- NULL
bonajuste <- NULL
sumaRvalprop <- NULL
CRTj <- NULL
calcol <- NULL
lonprin <- NULL
CREjFq <- NULL
CRFqEj <- NULL
covartotalb <- NULL

muestras <- c()
muestrasstd <- c()
covar <- c()
covarb <- c()
coind <- c()
angulo <- c()
anguloeje <- c()
autovalores <- c()
CRTjv <- c()
CREjFqv <- c()
CRFqEjv <- c()
bondades <- c()
calidadcolumnas <- c()
indices <- NULL

##############################################################################
####	We create vectors of the colors
##############################################################################

		colvariables<-rep("blue",times = dim(x)[2])		
		colindividuos<-rep("green",times = dim(x)[1])
    
  
##############################################################################
####	We create vectors of the colors
##############################################################################

		textvariables<-colnames(x)
		textindividuos<-rownames(x)


##############################################################################
####	We create vectors of the character size
##############################################################################

		cexvariables<-rep(1,times = dim(x)[2])		
		cexindividuos<-rep(1,times = dim(x)[1])
		
    
##############################################################################
####	We create vectors of the symbols
##############################################################################

		simvariables<-rep(" ",times = dim(x)[2])		
		simindividuos<-rep("+",times = dim(x)[1])



    for (i in 1:dim(x)[1])
		{
			colindividuos[i]<-paste("colori",i, sep = "")
			assign(colindividuos[i],"green", envir = mientorno)
			textindividuos[i]<-paste("labeli",i, sep = "")
			assign(textindividuos[i],rownames(x)[i], envir = mientorno)
			cexindividuos[i]<-paste("cexi",i, sep = "")
			assign(cexindividuos[i],1, envir = mientorno)
			simindividuos[i]<-paste("simi",i, sep = "")
			assign(simindividuos[i]," ", envir = mientorno)
    }
	 

   for (i in 1:dim(x)[2])
		{
			colvariables[i]<-paste("colorv",i, sep = "")
			assign(colvariables[i],"blue", envir = mientorno)
			textvariables[i]<-paste("labelv",i, sep = "")
			assign(textvariables[i],colnames(x)[i], envir = mientorno)
			cexvariables[i]<-paste("cexv",i, sep = "")
			assign(cexvariables[i],1, envir = mientorno)
			simvariables[i]<-paste("simv",i, sep = "")
			assign(simvariables[i],"+", envir = mientorno)
    }
    
    
#############################################################################
### Informative window
#############################################################################
   
winfor<-tktoplevel()
tkwm.title(winfor,"Classical Biplots")
#### Frames

  framewi<-tkframe(winfor, relief = "flat", borderwidth = 2,
  background = "white")

  framewi1<-tkframe(framewi, relief = "ridge", borderwidth = 2, 
        background = "white")

  framewi2<-tkframe(framewi, relief = "ridge", borderwidth = 2, 
        background = "white")

  framewi21<-tkframe(framewi2, relief = "ridge", borderwidth = 2, 
        background = "white")

	framewi21i<-tkframe(framewi21, relief = "ridge", borderwidth = 2, 
        background = "white")
        
 framewi21a<-tkframe(framewi21, relief = "ridge", borderwidth = 2, 
        background = "white")
        
 framewi22<-tkframe(framewi2, relief = "flat", borderwidth = 2, 
        background = "white")

  framewi22c<-tkframe(framewi22, relief = "flat", borderwidth = 2, 
        background = "white")
        
  framewi22l<-tkframe(framewi22, relief = "flat", borderwidth = 2, 
        background = "white")
        
  framewi221c<-tkframe(framewi22c, relief = "flat", borderwidth = 2, 
        background = "white")

  framewi222c<-tkframe(framewi22c, relief = "flat", borderwidth = 2, 
        background = "white")

  framewi223c<-tkframe(framewi22c, relief = "flat", borderwidth = 2, 
        background = "white")

  framewi224c<-tkframe(framewi22c, relief = "flat", borderwidth = 2, 
        background = "white")

  framewi225c<-tkframe(framewi22c, relief = "flat", borderwidth = 2, 
        background = "white")

  framewi226c<-tkframe(framewi22c, relief = "flat", borderwidth = 2, 
        background = "white")

  framewi227c<-tkframe(framewi22c, relief = "flat", borderwidth = 2, 
        background = "white")

  framewi228c<-tkframe(framewi22c, relief = "flat", borderwidth = 2, 
        background = "white")

  framewi229c<-tkframe(framewi22c, relief = "flat", borderwidth = 2, 
        background = "white")

  framewi2210c<-tkframe(framewi22c, relief = "flat", borderwidth = 2, 
        background = "white")

  framewi2211c<-tkframe(framewi22c, relief = "flat", borderwidth = 2, 
        background = "white")

  framewi221l<-tkframe(framewi22l, relief = "flat", borderwidth = 2, 
        background = "white")
  
  framewi222l<-tkframe(framewi22l, relief = "flat", borderwidth = 2, 
        background = "white")
  
  framewi223l<-tkframe(framewi22l, relief = "flat", borderwidth = 2, 
        background = "white")
  
  framewi224l<-tkframe(framewi22l, relief = "flat", borderwidth = 2, 
        background = "white")
  
  framewi225l<-tkframe(framewi22l, relief = "flat", borderwidth = 2, 
        background = "white")
  
  framewi226l<-tkframe(framewi22l, relief = "flat", borderwidth = 2, 
        background = "white")
  
  framewi227l<-tkframe(framewi22l, relief = "flat", borderwidth = 2, 
        background = "white")
  
  framewi228l<-tkframe(framewi22l, relief = "flat", borderwidth = 2, 
        background = "white")
  
  framewi229l<-tkframe(framewi22l, relief = "flat", borderwidth = 2, 
        background = "white")
  
  framewi2210l<-tkframe(framewi22l, relief = "flat", borderwidth = 2, 
        background = "white")
  
  framewi2211l<-tkframe(framewi22l, relief = "flat", borderwidth = 2, 
        background = "white")
  
  framewigr<-tkframe(winfor, relief = "flat", borderwidth = 2, 
        background = "white")


fontHeading <- tkfont.create(family="times",size=24,weight="bold",slant="italic")
fontFixedWidth <- tkfont.create(family="courier",size=12)
tkpack(tklabel(framewi1, text="    "), expand = "TRUE", side="left",expand="TRUE", fill = "both")
tkpack(tklabel(framewi1,text="BOOTSTRAP ON CLASSICAL BIPLOTS",font=fontHeading, foreground = "blue"), expand = "TRUE", side="left",expand="TRUE", fill = "both")
tkpack(tklabel(framewi1, text="    "), expand = "TRUE", side="left",expand="TRUE", fill = "both")
	

######Iterations   ###################################
      Niter <- tclVar(niter)
      entry.Niter <-tkentry(framewi21i,width=10,textvariable=Niter)
      tkconfigure(entry.Niter,textvariable=Niter)
			
			
	tkpack(tklabel(framewi21i, text="Number of iterations"),entry.Niter, expand = "TRUE", side="left", fill = "both")

######alpha confidence intervals ###################################
      Nalpha <- tclVar(alphaic)
      entry.Nalpha <-tkentry(framewi21a,width=10,textvariable=Nalpha)
      tkconfigure(entry.Nalpha,textvariable=Nalpha)
			
			
	tkpack(tklabel(framewi21a, text="Confidence Level     "),entry.Nalpha, expand = "TRUE", side="left", fill = "both")
    	tkpack(framewi21i, framewi21a, expand = "TRUE",side="top", fill="both")


###### Parameters to estimate   ###################################
  	tkpack(tklabel(framewi221l, text="Calculate confidence intervals for:"), expand = "TRUE", side="left",expand="TRUE", fill = "both")

   	tkpack(tklabel(framewi221c, text=" "), expand = "TRUE", side="left",expand="TRUE", fill = "both")

##### Checkbox Godness of fit  #######

	cgf <- tkcheckbutton(framewi222c)
	cgfValue <- tclVar("0")
	tkconfigure(cgf,variable=cgfValue)
  
##### Checkbox quality of representation  #######

	ccr <- tkcheckbutton(framewi223c)
	ccrValue <- tclVar("0")
	tkconfigure(ccr,variable=ccrValue)
  
##### Checkbox eigenvalues  #######

	cei <- tkcheckbutton(framewi224c)
	ceiValue <- tclVar("0")
	tkconfigure(cei,variable=ceiValue)
 

  ##### Checkbox angles between variables  #######

	cavar <- tkcheckbutton(framewi225c)
	cavarValue <- tclVar("0")
	tkconfigure(cavar,variable=cavarValue)
  
 ##### Checkbox angles between variables  #######

	cavarj <- tkcheckbutton(framewi226c)
	cavarjValue <- tclVar("0")
	tkconfigure(cavarj,variable=cavarjValue)
  
   ##### Checkbox variable coordinates #######

	#cvcoo <- tkcheckbutton(framewi227c)
	#cvcooValue <- tclVar("0")
	#tkconfigure(cvcoo,variable=cvcooValue)
  
  ##### Checkbox CRTj #######

	ccrtj <- tkcheckbutton(framewi228c)
	ccrtjValue <- tclVar("0")
	tkconfigure(ccrtj,variable=ccrtjValue)
  
##### Checkbox CREjFq #######

	ccrejfq <- tkcheckbutton(framewi229c)
	ccrejfqValue <- tclVar("0")
	tkconfigure(ccrejfq,variable=ccrejfqValue)
  
##### Checkbox CREFqEj #######

	ccrfqej <- tkcheckbutton(framewi2210c)
	ccrfqejValue <- tclVar("0")
	tkconfigure(ccrfqej,variable=ccrfqejValue)
  
##### Checkbox length of variables #######

#	clv <- tkcheckbutton(framewi2211c)
#	clvValue <- tclVar("0")
#	tkconfigure(clv,variable=clvValue)
  
  	tkpack( tklabel(framewi222l, text="-Goodness of fit", anchor="nw"), 
      tklabel(framewi223l, text="-Quality of approximation for columns", anchor="nw"), 
      tklabel(framewi224l, text="-Eigenvalues", anchor="nw"),
      tklabel(framewi225l, text="-Angles between variables", anchor="nw"),
      tklabel(framewi226l, text="-Angles between variables and axes", anchor="nw"),
   #  tklabel(framewi227l, text="-Variable coordinates", anchor="nw"),
      tklabel(framewi228l, text="-Relative contribution to total variability of the column element j", anchor="nw"),
      tklabel(framewi229l, text="-Relative contribution of the column element j to the q-th factor", anchor="nw"),
      tklabel(framewi2210l, text="-Relative contribution of the q-th factor to column element j", anchor="nw"),
 #    tklabel(framewi2211l, text="-Variable lengths", anchor="nw"),
         expand = "FALSE", side="top",expand="TRUE", fill = "both")

  	tkpack(cgf, ccr, cei, cavar, cavarj, #cvcoo, 
  	ccrtj, ccrejfq, ccrfqej, 	#clv,
   	expand = "TRUE",side="top", fill="both")
  	tkpack(framewi221l,framewi222l,framewi223l,framewi224l,framewi225l,framewi226l,framewi227l,framewi228l,framewi229l,framewi2210l,framewi2211l, expand = "TRUE",side="top", fill="both")
  	tkpack(framewi221c,framewi222c,framewi223c,framewi224c,framewi225c,framewi226c,framewi227c,framewi228c,framewi229c,framewi2210c,framewi2211c, expand = "TRUE",side="top", fill="both")
  	tkpack(framewi22c, framewi22l, expand = "TRUE",side="left", fill="both")


OnOKinf <- function()
			{

	   tkdestroy(winfor) 
	   Xpon <- array(data=unlist(x), dim=dim(x))
     	   niter <<- tclvalue(Niter)
	   alphaic <<- tclvalue(Nalpha)
	   cgfVal <<- as.character(tclvalue(cgfValue))
	   ccrVal <<- as.character(tclvalue(ccrValue))
	   ceiVal <<- as.character(tclvalue(ceiValue))
	   cavarVal <<- as.character(tclvalue(cavarValue))
	   cavarjVal <<- as.character(tclvalue(cavarjValue))
	   #cvcooVal <<- as.character(tclvalue(cvcooValue))
	   ccrtjVal <<- as.character(tclvalue(ccrtjValue))
	   ccrejfqVal <<- as.character(tclvalue(ccrejfqValue))
	   ccrfqejVal <<- as.character(tclvalue(ccrfqejValue))
	#  clvVal <<- as.character(tclvalue(clvValue))

	   
##############################################################################
#####	Window to change labels and colors and select the biplot 
##############################################################################

		tt<-tktoplevel()
		tkwm.title(tt,"Options")

#####Dropdown menu#############################

		topMenutt <- tkmenu(tt)
		tkconfigure(tt,menu=topMenutt)
		fileMenutt <- tkmenu(topMenutt,tearoff=FALSE)
  		fileMenutrans <- tkmenu(topMenutt, tearoff=FALSE)
    	     
	
		tkadd(fileMenutt,"command",label="HJ-biplot",command=function() tipo<<-"RCMP")
		tkadd(fileMenutt,"command",label="GH-biplot",command=function() tipo<<-"CMP")
      	tkadd(fileMenutt,"command",label="JK-biplot",command=function() tipo<<-"RMP")
			
    		tkadd(topMenutt,"cascade",label="Biplot",menu=fileMenutt)
 
     		tkadd(fileMenutrans,"command",label="Subtract the global mean",command=function() tChoice<<-"Subtract the global mean")
		tkadd(fileMenutrans,"command",label="Column centering",command=function() tChoice<<-"Column centering")
		tkadd(fileMenutrans,"command",label="Standardize columns",command=function() tChoice<<-"Standardize columns")
		tkadd(fileMenutrans,"command",label="Row centering",command=function() tChoice<<-"Row centering")
		tkadd(fileMenutrans,"command",label="Standardize rows",command=function() tChoice<<-"Standardize rows")
		tkadd(fileMenutrans,"command",label="Raw data",command=function() tChoice<<-"Raw data")
    
    		tkadd(topMenutt,"cascade",label="Transformations",menu=fileMenutrans)
		
#### Frames

  	framett<-tkframe(tt, relief = "flat", borderwidth = 2,
  		background = "white")

  	framett1<-tkframe(framett, relief = "ridge", borderwidth = 2, 
        background = "white")

  	framett2<-tkframe(framett, relief = "ridge", borderwidth = 2, 
        background = "white")

  #framett3<-tkframe(framett, relief = "ridge", borderwidth = 2, 
   #     background = "white")


  	framet1<-tkframe(framett1, relief = "ridge", borderwidth = 2, 
        background = "white")

	frametext1<-tkframe(framett1, relief = "flat", borderwidth = 2, 
        background = "white")


	frameok1<-tkframe(framett1, relief = "ridge", borderwidth = 2, 
        background = "white")

	framecol1<-tkframe(framett1, relief = "flat", borderwidth = 2, 
        background = "white")

	framecol11<-tkframe(framecol1, relief = "flat", borderwidth = 2, 
        background = "white")
        
  	framecol12<-tkframe(framecol1, relief = "flat", borderwidth = 2, 
        background = "white")

  	framename1<-tkframe(framett1, relief = "flat", borderwidth = 2, 
        background = "white")

	framename11<-tkframe(framename1, relief = "flat", borderwidth = 2, 
        background = "white")

  	framename12<-tkframe(framename1, relief = "flat", borderwidth = 2, 
        background = "white")

  	framecex1<-tkframe(framett1, relief = "flat", borderwidth = 2, 
        background = "white")

	framecex11<-tkframe(framecex1, relief = "flat", borderwidth = 2, 
        background = "white")

  	framecex12<-tkframe(framecex1, relief = "flat", borderwidth = 2, 
        background = "white")
        
  	frames1<-tkframe(framett1, relief = "flat", borderwidth = 2, 
        background = "white")
                                
  	frames11<-tkframe(frames1, relief = "flat", borderwidth = 2, 
        background = "white")
        
  	frames12<-tkframe(frames1, relief = "flat", borderwidth = 2, 
        background = "white")
	
  	framet2<-tkframe(framett2, relief = "ridge", borderwidth = 2, 
        background = "white")

	frametext2<-tkframe(framett2, relief = "flat", borderwidth = 2, 
        background = "white")

	frameok2<-tkframe(framett2, relief = "ridge", borderwidth = 2, 
        background = "white")

	framecol2<-tkframe(framett2, relief = "flat", borderwidth = 2, 
        background = "white")

	framecol21<-tkframe(framecol2, relief = "flat", borderwidth = 2, 
        background = "white")

  	framecol22<-tkframe(framecol2, relief = "flat", borderwidth = 2, 
        background = "white")

  	framename2<-tkframe(framett2, relief = "flat", borderwidth = 2, 
        background = "white")

	framename21<-tkframe(framename2, relief = "flat", borderwidth = 2, 
        background = "white")

  	framename22<-tkframe(framename2, relief = "flat", borderwidth = 2, 
        background = "white")

  	framecex2<-tkframe(framett2, relief = "flat", borderwidth = 2, 
        background = "white")

	framecex21<-tkframe(framecex2, relief = "flat", borderwidth = 2, 
        background = "white")

  	framecex22<-tkframe(framecex2, relief = "flat", borderwidth = 2, 
        background = "white")
        
  	frames2<-tkframe(framett2, relief = "flat", borderwidth = 2, 
        background = "white")
        
  #frames21<-tkframe(frames2, relief = "flat", borderwidth = 2, 
 #       background = "white")
        
  #frames22<-tkframe(frames2, relief = "flat", borderwidth = 2, 
 #       background = "white")
  
  	framegraphic<-tkframe(tt, relief = "flat", borderwidth = 2, 
        background = "white")

##### Checkbox to show the axes or not #######

	cb <- tkcheckbutton(frames2)
	cbValue <- tclVar("0")
	tkconfigure(cb,variable=cbValue)

##############################################################################
##### 	List of individuals
##############################################################################

	
    

		scri <- tkscrollbar(framet1, repeatinterval=5,
				   command=function(...)tkyview(tli,...))

		tli<-tklistbox(framet1,height=6,width=42,selectmode="multiple",yscrollcommand=function(...)tkset(scri,...),background="white")
	
		tkpack(tklabel(frametext1,text="Individuals"),side="left",expand = "TRUE",fill="both")



		for (i in 1:(dim(Xpon)[1]))
		{
    			tkinsert(tli,"end",get(textindividuos[i], envir = mientorno))
		}
		tkselection.set(tli,0) #  Indexing starts at zero.

		OnOKi <- function()
		{
			Choicei <- get(textindividuos[as.numeric(tkcurselection(tli))+1], envir = mientorno)


##### Color of the selected individual  #############

			assign(atrindividuos[1], as.numeric(tkcurselection(tli))+1, envir = mientorno)
			assign(atrindividuos[2], get(colindividuos[as.numeric(tkcurselection(tli))+1], envir = mientorno), envir = mientorno)
      		tkconfigure(canvasi, bg=get(atrindividuos[2], envir = mientorno))
  


##### Text of the selected individual  #############
      
			Namei <<- tclVar(get(textindividuos[get(atrindividuos[1], envir = mientorno)], envir = mientorno))
			tkconfigure(entry.Namei,textvariable=Namei)
			
##### Size of the selected variable  #############

			Cexi <<- tclVar(get(cexindividuos[get(atrindividuos[1], envir = mientorno)], envir = mientorno))
			tkconfigure(entry.Cexi,textvariable=Cexi)
    
		}
	
		OK.buti <-tkbutton(frameok1,text="    OK    ",command=OnOKi)
		
		tkpack(tli,scri,expand = "TRUE", side="left", fill = "both")
		tkpack.configure(scri,side="left")

		tkpack(OK.buti,expand = "TRUE", side="left", fill = "both")
		



#######Color#######################################
	
     assign(atrindividuos[1], as.numeric(tkcurselection(tli))+1, envir = mientorno)
     assign(atrindividuos[2], get(colindividuos[get(atrindividuos[1], envir = mientorno)], envir = mientorno), envir = mientorno)
     canvasi <- tkcanvas(framecol11,width="57",height="20",bg=get(atrindividuos[2], envir = mientorno))

		ChangeColori <- function()
		{
			assign(atrindividuos[2], tclvalue(tcl("tk_chooseColor",initialcolor=get(atrindividuos[2], envir = mientorno))), envir = mientorno)
		 	if (nchar(get(atrindividuos[2], envir = mientorno))>0)
    				{
					tkconfigure(canvasi,bg=get(atrindividuos[2], envir = mientorno))
		 			assign(colindividuos[get(atrindividuos[1], envir = mientorno)], get(atrindividuos[2], envir = mientorno), envir = mientorno)
				}  
		}

		ChangeColor.buttoni<- tkbutton(framecol12,text="Change Color",command=ChangeColori,width=4)
		tkpack(canvasi,ChangeColor.buttoni,expand = "TRUE", side="left", fill = "both")

 

######Labels   ###################################
      Namei <- get(textindividuos[get(atrindividuos[1], envir = mientorno)], envir = mientorno)
      entry.Namei <-tkentry(framename11,width=10,textvariable=Namei)
      
		OnOKli <- function()
		{
		  
      NameVali <- tclvalue(Namei)
			assign(textindividuos[get(atrindividuos[1], envir = mientorno)],NameVali, envir = mientorno)

#####Values of listbox###############################
		
			for (i in 1:dim(Xpon)[1])
			{
				tkdelete(tli,0)
			}

			for (i in 1:(dim(Xpon)[1]))
			{
   				tkinsert(tli,"end",get(textindividuos[i], envir = mientorno))
			}


		}

		OK.butli <-tkbutton(framename12,text=" Change label",command=OnOKli,width=4)
		tkbind(entry.Namei, "<Return>",OnOKli)
		tkpack(entry.Namei,OK.butli,expand = "TRUE", side="left", fill = "both")
	
###### Sizes   ###################################
		Cexi <- get(cexindividuos[get(atrindividuos[1], envir = mientorno)], envir = mientorno)
		entry.Cexi <-tkentry(framecex11,width=10,textvariable=Cexi)

		OnOKci <- function()
		{
			NameCexi <- tclvalue(Cexi)
			assign(cexindividuos[get(atrindividuos[1], envir = mientorno)],NameCexi, envir = mientorno )
		}

		OK.butci <-tkbutton(framecex12,text=" Change size",command=OnOKci,width=4)
		tkbind(entry.Cexi, "<Return>",OnOKci)
		tkpack(entry.Cexi,OK.butci,expand = "TRUE", side="left", fill = "both")
	
  	
######Symbols  ###################################

		comboBoxi <- tkwidget(frames11,"ComboBox",editable=FALSE,values=symbols, width=7)

		chang.symi <- function()
		{
      		simChoicei <- symbols[as.numeric(tclvalue(tcl(comboBoxi,"getvalue")))+1]
		  	assign(simindividuos[get(atrindividuos[1], envir = mientorno)], simChoicei, envir = mientorno)
		}
		Change.symboli <-tkbutton(frames12,text="   Change symbol   ",command=chang.symi,width=4, height=1)
		tkpack(comboBoxi,Change.symboli,side="left",expand="TRUE", fill="both")




##### List of variables ###########################

		


		scrv <- tkscrollbar(framet2, repeatinterval=5,
				   command=function(...)tkyview(tlv,...))

		tlv<-tklistbox(framet2,height=6,width=42,selectmode="multiple",yscrollcommand=function(...)tkset(scrv,...),background="white")
	
		tkpack(tklabel(frametext2,text="Variables"),side="left",expand = "TRUE",fill="both")


		for (i in 1:dim(Xpon)[2])
		{
    			tkinsert(tlv,"end",get(textvariables[i], envir = mientorno))
		}
		tkselection.set(tlv,0) #  Indexing starts at zero.

		OnOKv <- function()
		{
			Choicev <- get(textvariables[as.numeric(tkcurselection(tlv))+1], envir = mientorno)
			
##### Color of the selected variable  #############
		assign(atrvariables[1], as.numeric(tkcurselection(tlv))+1, envir = mientorno)
		assign(atrvariables[2], get(colvariables[get(atrvariables[1], envir = mientorno)], envir = mientorno), envir = mientorno)
      	tkconfigure(canvasv,bg=get(atrvariables[2], envir = mientorno))


##### Text of the selected variable  #############
			
      Namev <<- tclVar(get(textvariables[get(atrvariables[1], envir = mientorno)], envir = mientorno))
			tkconfigure(entry.Namev,textvariable=Namev)
			
      
##### Size of the selected variable  #############
      Cexv <<- tclVar(get(cexvariables[get(atrvariables[1], envir = mientorno)], envir = mientorno))
			tkconfigure(entry.Cexv,textvariable=Cexv)
   
   
		}
	
		OK.butv <-tkbutton(frameok2,text="    OK    ",command=OnOKv)
		tkpack(OK.butv,expand = "TRUE", side="left", fill = "both")


		tkpack(tlv,scrv,expand = "TRUE", side="left", fill = "both")
		tkpack.configure(scrv,side="left")

		tkpack(OK.butv,expand = "TRUE", side="left", fill = "both")
		tkfocus(tt)



#######Color#######################################
	   	assign(atrvariables[1], as.numeric(tkcurselection(tlv))+1, envir = mientorno)
     		assign(atrvariables[2], get(colvariables[get(atrvariables[1], envir = mientorno)], envir = mientorno), envir = mientorno)
     		canvasv <- tkcanvas(framecol21,width="57",height="20",bg=get(atrvariables[2], envir = mientorno))

		ChangeColorv <- function()
		{
			assign(atrvariables[2], tclvalue(tcl("tk_chooseColor",initialcolor=get(atrvariables[2], envir = mientorno),title="Choose a color")), envir = mientorno)
		 	if (nchar(get(atrvariables[2], envir = mientorno))>0)
    				{
					tkconfigure(canvasv,bg=get(atrvariables[2], envir = mientorno))
		 			assign(colvariables[get(atrvariables[1], envir = mientorno)], get(atrvariables[2], envir = mientorno), envir = mientorno)
				}  
		}

		ChangeColor.buttonv<- tkbutton(framecol22,text="Change Color",command=ChangeColorv,width=4)
		tkpack(canvasv,ChangeColor.buttonv,expand = "TRUE", side="left", fill = "both")
 


######Labels  ###################################
	    
      Namev <- get(textvariables[get(atrvariables[1], envir = mientorno)], envir = mientorno)
      entry.Namev <-tkentry(framename21,width=10, textvariable=Namev)

		OnOKlv <- function()
		{
			NameValv <- tclvalue(Namev)
			assign(textvariables[get(atrvariables[1], envir = mientorno)], envir = mientorno)

#####Values of listbox###############################
	
			for (i in 1:(dim(Xpon)[2]))
			{
				tkdelete(tlv,0)
			}

			for (i in 1:(dim(Xpon)[2]))
			{
   				tkinsert(tlv,"end",get(textvariables[i], envir = mientorno))
			}


		}

		OK.butlv <-tkbutton(framename22,text=" Change label",command=OnOKlv,width=4)
		tkbind(entry.Namev, "<Return>",OnOKlv)
		tkpack(entry.Namev,OK.butlv,expand = "TRUE", side="left", fill = "both")


  ###### Sizes  ###################################
	
		Cexv <- get(cexvariables[get(atrvariables[1], envir = mientorno)], envir = mientorno)
		entry.Cexv <-tkentry(framecex21,width=10, textvariable=Cexv)

		OnOKcv <- function()
		{
			NameCexv <- tclvalue(Cexv)
			assign(cexvariables[get(atrvariables[1], envir = mientorno)],NameCexv, envir = mientorno)

		}

		OK.butcv <-tkbutton(framecex22,text=" Change size",command=OnOKcv,width=4)
		tkbind(entry.Cexv, "<Return>",OnOKcv)
		tkpack(entry.Cexv,OK.butcv,expand = "TRUE", side="left", fill = "both")


	
		Graphics <- function()
		{
		
    if (tChoice=="Subtract the global mean"){			
      Xstd<-Xpon
      media<-mean(Xstd)
      
			for(j in 1:dim(Xstd)[2])
				{for(i in 1:dim(Xstd)[1])
					{
						Xstd[i,j]<-(Xstd[i,j]-media)		
					}
				}
      }

		if (tChoice=="Column centering"){
      Xstd<-Xpon
      mediav<-colMeans(Xstd)
	  
			for(j in 1:dim(Xstd)[2])
				{for(i in 1:dim(Xstd)[1])
					{
						Xstd[i,j]<-(Xstd[i,j]-mediav[j])	
					}
				}
      }

		if (tChoice=="Standardize columns"){	
      Xstd<-Xpon
      mediav<-colMeans(Xstd)
			desvvar<-sqrt(diag(var(Xstd)))
		
			for(j in 1:dim(Xstd)[2])
				{for(i in 1:dim(Xstd)[1])
					{
						Xstd[i,j]<-(Xstd[i,j]-mediav[j])/desvvar[j]
					}
				}
      }
	

		if (tChoice=="Row centering"){		
		 Xstd<-Xpon     
    mediav<-rowMeans(Xstd)
     
			for(j in 1:dim(Xstd)[2])
				{for(i in 1:dim(Xstd)[1])
					{
						Xstd[i,j]<-(Xstd[i,j]-mediav[i])
					}
				}	
      }


		if (tChoice=="Standardize rows"){
      Xstd<-Xpon
      
     	mediav<-rowMeans(Xstd)
			desvvar<-mediav
			for (i in 1:dim(Xstd)[1])
			{
				desvvar[i]<-sqrt(var(Xstd[i,]))
			}

			for(j in 1:dim(Xstd)[2])
				{for(i in 1:dim(Xstd)[1])
					{
						Xstd[i,j]<-(Xstd[i,j]-mediav[i])/desvvar[i]
					}
				}
      }
	
		if (tChoice=="Raw data"){
		Xstd<-Xpon
		}
    Xpon<-Xstd
  
			barvp<-tktoplevel()
      tkwm.title(barvp,"Eigenvalues")
	    
	    plotbar<-function()
	    {
        descom <<- La.svd(Xpon)
        sumaRvalprop<-sum((descom$d)^2)
        #sumaRvalprop4<<-sum((descom$d)^4)
				inerciatot<<-(descom$d[1:length(descom$d)])^2/sumaRvalprop
        barplot(descom$d, col="blue", xlab="", ylab="", names.arg=round(inerciatot, digits=2))
        msginertia<<-"Proportion of inertia explained by each axis:"
        for (i in 1:length(descom$d))
          {
           msginertia<<-paste(msginertia, "\n",i, "\t", round(inerciatot[i]*100, digits=2), "%")
          } 
      }
	    
     imgbar <<- tkrplot(barvp,fun=plotbar,hscale=1.5,vscale=1.5)
     tk2tip(imgbar, msginertia)
      
      
  	Onaxis <- function()
	   {
		nejes <- tclvalue(numaxis)
		nejes <-as.numeric(nejes)
	
    if (nejes > length(descom$d))
		 {
        msg <- paste("The maximum number of dimensions is ",length(descom$d))
		    tkmessageBox(message=msg)
       
		 }else{
		
		tkdestroy(barvp)
		nejes <- as.integer(nejes)
   
	
	
			Myhscale <- 1.5    # Horizontal scaling
			Myvscale <- 1.5    # Vertical scaling

	
##############################################################################
#####		Coordinates
##############################################################################
		ejes<-c()
    for (i in 1:nejes)
    {
      ejes<-c(ejes, paste("Axis",i))
    }
    descom <<- La.svd(Xpon)
    sumaRvalprop<-sum((descom$d)^2)
        			
    descom<-La.svd(Xpon,nu=nejes,nv=nejes)
    if (tipo == "RMP"){

				coindividuos<-descom$u%*%diag(descom$d[1:nejes])
				covariables<-t(descom$v)				
        

##############################################################################
#####		Contributions, goodness of fit and qualities of representation
##############################################################################
	
				suma2valprop<-sum((descom$d[1:nejes])^2)
				inercia<-(descom$d[1:nejes])^2/sumaRvalprop
				cuminer<-cumsum(inercia)		

				bonajuste<<-(suma2valprop/sumaRvalprop)*100
					
				calcol<-nejes/length(inerciatot)*100
				calfilas<-(suma2valprop/sumaRvalprop)*100
			
			} 
    
    if (tipo == "CMP"){

				coindividuos<-descom$u
				covariables<-t(descom$v)%*%diag(descom$d[1:nejes])				
       
##############################################################################
#####		Contributions, goodness of fit and qualities of representation
##############################################################################
	
				suma2valprop<-sum((descom$d[1:nejes])^2)
				inercia<-(descom$d[1:nejes])^2/sumaRvalprop
				cuminer<-cumsum(inercia)
 
				bonajuste<<-(suma2valprop/sumaRvalprop)*100
					
			  	calfilas<-nejes/length(inerciatot)*100
				calcol<-(suma2valprop/sumaRvalprop)*100
			
			
			} 
      
      
      if (tipo=="RCMP")
      {

				coindividuos<-descom$u%*%diag(descom$d[1:nejes])
				covariables<-t(descom$v)%*%diag(descom$d[1:nejes])
       
##############################################################################
#####		Contributions, goodness of fit and qualities of representation
##############################################################################
	
				suma2valprop<-sum((descom$d[1:nejes])^2)
				sumaRvalprop<-sum((descom$d)^2)
        			inercia<-(descom$d[1:nejes])^2/sumaRvalprop
				cuminer<-cumsum(inercia)
					
					
				calcol<-(suma2valprop/sumaRvalprop)*100
				calfilas<-(suma2valprop/sumaRvalprop)*100
			}
	
		########################  Reescalamiento
    
    		#sumaindividuos<<-sum(coindividuos^2)
		#sumavariables<<-sum(covariables^2)



		#sind<<-sumaindividuos/(dim(coindividuos)[1])
		#svar<<-sumavariables/(dim(covariables)[1])

		#scf<<-((sind/svar)^(1/2))^(1/2)
		

		#coindividuos<<-coindividuos/scf
		#covariables<<-covariables*scf
		
    
    				covartotal<<-covariables		
	      
				coindividuosnam<-as.data.frame(coindividuos)
				for (i in 1:length(textindividuos))
				{
				  rownames(coindividuosnam)[i]<-get(textindividuos[i], envir = mientorno)
				}
        			colnames(coindividuosnam)<-ejes

				covariablesnam<-as.data.frame(covariables)
				for (i in 1:length(textvariables))
				{
				  rownames(covariablesnam)[i]<-get(textvariables[i], envir = mientorno)
				}
				colnames(covariablesnam)<-ejes

				coindivcuad<-coindividuos^2
				CRTi<-rowSums(coindivcuad)
				CRTi<-(CRTi*1000)/suma2valprop
				CRTi<-as.data.frame(CRTi)
				rownames(CRTi)<-rownames(coindividuosnam)

				covarcuad<-covariables^2
				CRTj<-rowSums(covarcuad)
				CRTj<-(CRTj*1000)/suma2valprop
				CRTj<-as.data.frame(CRTj)
				rownames(CRTj)<-rownames(covariablesnam)

##############################################################################
#####		Length of variables
##############################################################################
        
        longitudprin<<-array(dim=dim(CRTj))
        

        for (i in 1:dim(covartotal)[1])
        {
	         lonprin<-sqrt(covartotal[i,1]^2 + covartotal[i,2]^2)
	         longitudprin[i,1]<<-lonprin
        }
				
	      longitudprin <<- as.data.frame (longitudprin)
        	rownames(longitudprin)<-rownames(CRTj)
    
				CREiFq<-array(dim=dim(coindividuos))
				
				CREjFq<-array(dim=dim(covariables))
				sumavar<-rowSums(covarcuad)
				
				CRFqEi<-coindivcuad
				sumaindi<-rowSums(coindivcuad)
				
				CRFqEj<-covarcuad

				
				for(i in 1:nejes)
				{
				  CREiFq[,i]<-((coindivcuad)[,i]*1000)/((descom$d[i])^2)
				  
				  CREjFq[,i]<-((covarcuad)[,i]*1000)/((descom$d[i])^2)
				  
				  CRFqEi[,i]<-((coindivcuad)[,i]*1000)/(sumaindi)
				  
				  CRFqEj[,i]<-((covarcuad)[,i]*1000)/(sumavar)
        }
				
        			CREiFq<-as.data.frame(CREiFq)
				rownames(CREiFq)<-rownames(coindividuosnam)
				colnames(CREiFq)<-ejes
					
				CREjFq<-as.data.frame(CREjFq)
				rownames(CREjFq)<-rownames(covariablesnam)
				colnames(CREjFq)<-ejes

				CRFqEi<-as.data.frame(CRFqEi)
				rownames(CRFqEi)<-rownames(coindividuosnam)
				colnames(CRFqEi)<-ejes

				CRFqEj<-as.data.frame(CRFqEj)
				rownames(CRFqEj)<-rownames(covariablesnam)
				colnames(CRFqEj)<-ejes


######cálculo de ángulos entre variables

n <- dim(covartotal)[1]
o <- c(0,0)
ang <- array(dim = c(n,n)) 
 
for (z in 1:n)
  {
    for(j in z:n)
      {
        ang[z,j] <- (atan2(covartotal[z,1], covartotal[z,2]) - atan2(covartotal[j,1], covartotal[j,2])) * 180/pi
       
	  if (ang[z,j] < 0)
		  ang[z,j] <- -ang[z,j]
	 	if (ang[z,j] > 180)
			ang[z,j] <- -(ang[z,j] - 360)
		
    ang[j,z] <- ang[z,j]
    }  
  }
  ang<-as.data.frame(ang)
  rownames(ang)<-rownames(CRTj)
  colnames(ang)<-rownames(CRTj)
  
######cálculo de ángulos entre variables y ejes
  
n <- dim(covartotal)[1]
o <- c(0,0)
axisx <- c(1,0)
axisy <- c(0,1)
angax <- array(dim = c(n,2)) 
 
for (z in 1:n)
  {
    angax[z,1] <- (atan2(covartotal[z,1], covartotal[z,2]) - atan2(axisx[1], axisx[2])) * 180/pi
    angax[z,2] <- (atan2(covartotal[z,1], covartotal[z,2]) - atan2(axisy[1], axisy[2])) * 180/pi
       
    for (j in 1:2)
    {   
	  if (angax[z,j] < 0)
		  angax[z,j] <- -angax[z,j]
	 	if (angax[z,j] > 180)
			angax[z,j] <- -(angax[z,j] - 360)
	  if (angax[z,j] > 90)
			angax[z,j] <- -(angax[z,j] - 180)
	  }
  }
  angax<-as.data.frame(angax)
  rownames(angax)<-rownames(CRTj)
  colnames(angax)<-c("Axis 1", "Axis 2")
  
  


			cat("File saved in:    ",file="Results.txt")
			cat(getwd(),file="temp.txt")					
			file.append("Results.txt","temp.txt")	
			cat("\n",file="temp.txt")					
			file.append("Results.txt","temp.txt")
			cat("\n",file="temp.txt")					
			file.append("Results.txt","temp.txt")		
			cat("CONTRIBUTIONS:\n",file="temp.txt")					
			file.append("Results.txt","temp.txt")	
			cat("\n",file="temp.txt")					
			file.append("Results.txt","temp.txt")	
			cat("Relative contribution to total variability of the row element i:\n",file="temp.txt")					
			file.append("Results.txt","temp.txt")					
			write.table(round(CRTi, digits=2),file="temp.txt", sep="\t",dec=",")
			file.append("Results.txt","temp.txt")
				
			cat("\n",file="temp.txt")					
			file.append("Results.txt","temp.txt")	
			cat("Relative contribution to total variability of the column element j:\n",file="temp.txt")					
			file.append("Results.txt","temp.txt")					
			write.table(round(CRTj, digits=2),file="temp.txt", sep="\t",dec=",")
			file.append("Results.txt","temp.txt")

			cat("\n",file="temp.txt")					
			file.append("Results.txt","temp.txt")	
			cat("Relative contribution of the row element i to the q-th factor:\n",file="temp.txt")					
			file.append("Results.txt","temp.txt")					
			write.table(round(CREiFq, digits=2),file="temp.txt", sep="\t",dec=",")
			file.append("Results.txt","temp.txt")

			cat("\n",file="temp.txt")					
			file.append("Results.txt","temp.txt")	
			cat("Relative contribution of the column element j to the q-th factor:\n",file="temp.txt")					
			file.append("Results.txt","temp.txt")					
			write.table(round(CREjFq, digits=2),file="temp.txt", sep="\t",dec=",")
			file.append("Results.txt","temp.txt")

					
			cat("\n",file="temp.txt")					
			file.append("Results.txt","temp.txt")	
			cat("Relative contribution of the q-th factor to row element i:\n",file="temp.txt")					
			file.append("Results.txt","temp.txt")					
			write.table(round(CRFqEi, digits=2),file="temp.txt", sep="\t",dec=",")
			file.append("Results.txt","temp.txt")


			cat("\n",file="temp.txt")					
			file.append("Results.txt","temp.txt")	
			cat("Relative contribution of the q-th factor to column element j:\n",file="temp.txt")					
			file.append("Results.txt","temp.txt")					
			write.table(round(CRFqEj, digits=2),file="temp.txt", sep="\t",dec=",")
			file.append("Results.txt","temp.txt")


			if (tipo != "RCMP"){
			cat("\n",file="temp.txt")					
			file.append("Results.txt","temp.txt")	
			cat("Goodness of fit:  ",file="temp.txt")					
			file.append("Results.txt","temp.txt")					
			cat(round(bonajuste, digits=2),file="temp.txt")
			file.append("Results.txt","temp.txt")
			cat(" %",file="temp.txt")					
			file.append("Results.txt","temp.txt")	

			}


			cat("\n",file="temp.txt")					
			file.append("Results.txt","temp.txt")	
			cat("Quality of approximation for rows:  ",file="temp.txt")					
			file.append("Results.txt","temp.txt")					
			cat(round(calfilas, digits=2),file="temp.txt")
			file.append("Results.txt","temp.txt")
			cat(" %",file="temp.txt")					
			file.append("Results.txt","temp.txt")	



			cat("\n",file="temp.txt")					
			file.append("Results.txt","temp.txt")	
			cat("Quality of approximation for columns:  ",file="temp.txt")					
			file.append("Results.txt","temp.txt")					
			cat(round(calcol, digits=2),file="temp.txt")
			file.append("Results.txt","temp.txt")
			cat(" %",file="temp.txt")					
			file.append("Results.txt","temp.txt")	



			cat("\n",file="temp.txt")					
			file.append("Results.txt","temp.txt")	
			cat("\n",file="temp.txt")					
			file.append("Results.txt","temp.txt")	
			cat("Individual coordinates:\n",file="temp.txt")					
			file.append("Results.txt","temp.txt")					
			write.table(round(coindividuosnam, digits=2),file="temp.txt",sep="\t",dec=",")
			file.append("Results.txt","temp.txt")
			



			cat("\n",file="temp.txt")					
			file.append("Results.txt","temp.txt")	
			cat("Variable coordinates:\n",file="temp.txt")					
			file.append("Results.txt","temp.txt")					
			write.table(round(covariablesnam, digits=2), file="temp.txt", sep="\t", dec=",")
			file.append("Results.txt","temp.txt")
			
      
      		cat("\n",file="temp.txt")					
			file.append("Results.txt","temp.txt")	
			cat("Variable lengths:\n",file="temp.txt")					
			file.append("Results.txt","temp.txt")					
			write.table(round(longitudprin, digits=2), file="temp.txt", sep="\t", dec=",")
			file.append("Results.txt","temp.txt")
			

		  	cat("\n",file="temp.txt")					
			file.append("Results.txt","temp.txt")	
			cat("Angles between variables:\n",file="temp.txt")					
			file.append("Results.txt","temp.txt")					
			write.table(round(ang, digits=2), file="temp.txt", sep="\t", dec=",")
			file.append("Results.txt","temp.txt")
		
    
    			cat("\n",file="temp.txt")					
			file.append("Results.txt","temp.txt")	
			cat("Angles between variables and axes:\n",file="temp.txt")					
			file.append("Results.txt","temp.txt")					
			write.table(round(angax, digits=2), file="temp.txt", sep="\t", dec=",")
			file.append("Results.txt","temp.txt")
		
    
      		cat("\n",file="temp.txt")					
			file.append("Results.txt","temp.txt")	
			cat("Eigenvalues: \n",file="temp.txt")					
			file.append("Results.txt","temp.txt")					
			write.table(round(descom$d, digits=2), file="temp.txt", sep="\t", dec=",")
			file.append("Results.txt","temp.txt")

      		cat("\n",file="temp.txt")
			file.append("Results.txt","temp.txt")
			cat("Proportion of inertia explained by each axis: \n",file="temp.txt")
			file.append("Results.txt","temp.txt")
			write.table(round(inercia, digits=2), file="temp.txt", sep="\t", dec=",")
			file.append("Results.txt","temp.txt")

			


			file.show("Results.txt")
			file.remove("temp.txt")



			datos<-rbind(coindividuos,covariables)
			textos<-datos
			centro<-c(0,0)
			


		xCoords<-textos[,dim1]
    		yCoords<-textos[,dim2]
		labelsVec <- c()
		sizesVec <- c()
		colores<-c()
		simbolos<-c()

     for (k in 1:length(colindividuos))
      {
        colores<-c(colores, get(colindividuos[k], envir = mientorno))
        labelsVec<-c(labelsVec, get(textindividuos[k], envir = mientorno))
        sizesVec<-c(sizesVec, get(cexindividuos[k], envir = mientorno))
        simbolos<-c(simbolos, get(simindividuos[k], envir = mientorno))
     }
     for (k in 1:length(colvariables))
      {
       colores<-c(colores, get(colvariables[k], envir = mientorno))
       labelsVec<-c(labelsVec, get(textvariables[k], envir = mientorno))
       sizesVec<-c(sizesVec, get(cexvariables[k], envir = mientorno))
       simbolos<-c(simbolos, get(simvariables[k], envir = mientorno))
      }
    

	indexLabeled<-c(1:length(xCoords))
	indexLabeledaux<-c()
	labeledPoints <- list()

	
	wgr <- tktoplevel()
	tkwm.title(wgr,"Graph")

	plotFunctiond <- function(screen=TRUE)
	{
    		labelsVec <- c()
		sizesVec <- c()
		colores<-c()
		simbolos<-c()

     for (k in 1:length(colindividuos))
      {
        colores<-c(colores, get(colindividuos[k], envir = mientorno))
        labelsVec<-c(labelsVec, get(textindividuos[k], envir = mientorno))
        sizesVec<-c(sizesVec, get(cexindividuos[k], envir = mientorno))
        simbolos<-c(simbolos, get(simindividuos[k], envir = mientorno))
     }
     for (k in 1:length(colvariables))
      {
       colores<-c(colores, get(colvariables[k], envir = mientorno))
       labelsVec<-c(labelsVec, get(textvariables[k], envir = mientorno))
       sizesVec<-c(sizesVec, get(cexvariables[k], envir = mientorno))
       simbolos<-c(simbolos, get(simvariables[k], envir = mientorno))
      }
    

     
     	xCoords<-textos[,dim1]
	yCoords<-textos[,dim2]
      params <- par(bg="white")
 			plot(datos[,c(dim1,dim2)],main= "Graph",type="n",xlab=paste(round(inerciatot[dim1]*100, digits=2),"%"),ylab=paste(round(inerciatot[dim2]*100,digits=2),"%"))
				
				points(coindividuos[,dim1],coindividuos[,dim2],pch=simbolos[1:length(simindividuos)],col=colores[1:length(colindividuos)])

				arrows(centro[1],centro[2],covariables[,dim1],covariables[,dim2],col=colores[1+length(colindividuos):length(colores)],lty="dotted",length=0.05)

				points(centro[1],centro[2],pch=18,col="black")
				
			
################ Show axes or not
				cbVal <<- as.character(tclvalue(cbValue))
				if (cbVal=="1"){
	
					abline(h=centro[2],v=centro[1],lty="dotted")
				}

		



  		if (length(indexLabeled)>0)
    		for (i in (1:length(indexLabeled)))
    		{
      		indexClosest <- indexLabeled[i]
      		text(xCoords[indexClosest],yCoords[indexClosest],
           		labels=labelsVec[indexClosest], col= colores[indexClosest],cex=as.numeric(sizesVec)[indexClosest])
    		}
  		parPlotSize <<- par("plt")
  		usrCoords   <<- par("usr")
  		par(params)
	}
	
	
	
    
    g3d<-function()
  {
   
    if (nejes>2)
     { 
      zCoords<-datos[,dim3]
      bg3d("white")
      aspect3d("iso")
      lims <- par3d("bbox")
      if (cbVal=="1"){
          axes3d()
          }
    		labelsVec <- c()
		sizesVec <- c()
		colores<-c()
		simbolos<-c()

     for (k in 1:length(colindividuos))
      {
        colores<-c(colores, get(colindividuos[k], envir = mientorno))
        labelsVec<-c(labelsVec, get(textindividuos[k], envir = mientorno))
        sizesVec<-c(sizesVec, get(cexindividuos[k], envir = mientorno))
        simbolos<-c(simbolos, get(simindividuos[k], envir = mientorno))
     }
     for (k in 1:length(colvariables))
      {
       colores<-c(colores, get(colvariables[k], envir = mientorno))
       labelsVec<-c(labelsVec, get(textvariables[k], envir = mientorno))
       sizesVec<-c(sizesVec, get(cexvariables[k], envir = mientorno))
       simbolos<-c(simbolos, get(simvariables[k], envir = mientorno))
      }
    

      
      points3d(xCoords,yCoords,zCoords, color=colores)
      texts3d(xCoords, yCoords, zCoords,labelsVec,color=colores, cex= as.numeric(sizesVec))
  	
      
	     for (i in 1:(dim(covariables)[1]))
	     {
	       linea<-rbind(covariables[i,c(dim1, dim2, dim3)],c(0,0,0))	
	       segments3d(linea[,1],linea[,2], linea[,3],color=colores[i+length(colindividuos)])

	     }
	     rgl.bringtotop()
	     }else{
        msg <- "You have selected less than 3 dimensions. 3D-graph not available"
		    tkmessageBox(message=msg)
       }
	     
  }
  
  
  #############################################################################
### Functions to save the graph
#############################################################################
	  SaveFileJPG <- function() {
        FileName <- tclvalue(tkgetSaveFile(filetypes = "{{Jpeg files} {.jpg .jpeg}} {{All files} *}"))
        if (nchar(FileName)) {
            nn <- nchar(FileName)
            if (nn < 5 || substr(FileName, nn - 3, nn) != ".jpg") 
                FileName <- paste(FileName, ".jpg", sep = "")
            jpeg(FileName, width = 8, height = 8, units = "in", 
                restoreConsole = FALSE, res = 96, quality = 50)
            plotFunctiond(screen = FALSE)
            dev.off()
        }
    }
    SaveFilePDF <- function() {
        FileName <- tclvalue(tkgetSaveFile(filetypes = "{{PDF files} {.pdf}} {{All files} *}"))
        if (nchar(FileName)) {
            nn <- nchar(FileName)
            if (nn < 5 || substr(FileName, nn - 3, nn) != ".pdf") 
                FileName <- paste(FileName, ".pdf", sep = "")
            pdf(FileName, width = 7, height = 7)
            plotFunctiond(screen = FALSE)
            dev.off()
        }
    }
    SaveFileBmp <- function() {
        FileName <- tclvalue(tkgetSaveFile(filetypes = "{{Bitmap files} {.bmp}} {{All files} *}"))
        if (nchar(FileName)) {
            nn <- nchar(FileName)
            if (nn < 5 || substr(FileName, nn - 3, nn) != ".bmp") 
                FileName <- paste(FileName, ".bmp", sep = "")
            bmp(FileName, width = 8, height = 8, units = "in", 
                restoreConsole = FALSE, res = 96)
            plotFunctiond(screen = FALSE)
            dev.off()
        }
    }
    SaveFilePng <- function() {
        FileName <- tclvalue(tkgetSaveFile(filetypes = "{{Png files} {.png}} {{All files} *}"))
        if (nchar(FileName)) {
            nn <- nchar(FileName)
            if (nn < 5 || substr(FileName, nn - 3, nn) != ".png") 
                FileName <- paste(FileName, ".png", sep = "")
            png(FileName, width = 8, height = 8, units = "in", 
                restoreConsole = FALSE, res = 96)
            plotFunctiond(screen = FALSE)
            dev.off()
        }
    }
              topMenugr <- tkmenu(wgr)
              tkconfigure(wgr, menu = topMenugr)
                menuFile <- tkmenu(topMenugr, tearoff = FALSE)
                menuSaveAs <- tkmenu(topMenugr, tearoff = FALSE)
                menu3d <- tkmenu(topMenugr, tearoff = FALSE)
                
                tkadd(menuFile, "command", label = "Copy image",
                  command = function() {
                    tkrreplot(img)
                  })
                tkadd(menuFile, "cascade", label = "Save image",
                  menu = menuSaveAs)
                tkadd(menuSaveAs, "command", label = "PDF file",
                  command = function() {
                    SaveFilePDF()
                  })
                
                tkadd(menuSaveAs, "command", label = "Bmp file",
                  command = function() {
                    SaveFileBmp()
                  })
                tkadd(menuSaveAs, "command", label = "Png file",
                  command = function() {
                    SaveFilePng()
                  })
                tkadd(menuSaveAs, "command", label = "Jpg/Jpeg file",
                  command = function() {
                    SaveFileJPG()
                  })
                tkadd(menuFile, "separator")
                tkadd(menuFile, "command", label = "Exit", command = function() {
                  tkdestroy(wgr)
                })
                tkadd(menu3d, "command", label = "3D", command = function() {
                  g3d()
                })
                tkadd(topMenugr, "cascade", label = "File", menu = menuFile)
                tkadd(menuFile, "separator")
                tkadd(topMenugr, "cascade", label = "3D", menu = menu3d)
                
  
         
                                     
	img <<- tkrplot(wgr,fun=plotFunctiond,hscale=1.5,vscale=1.5)
	
  framedim1<-tkframe(wgr, relief = "flat", borderwidth = 2, 
        background = "whitesmoke")

	 
  
  
  comboBoxdim1 <- tkwidget(framedim1,"ComboBox",editable=FALSE,values=rep(1:nejes),width=10, text= dim1)
  comboBoxdim2 <- tkwidget(framedim1,"ComboBox",editable=FALSE,values=rep(1:nejes),width=10, text= dim2)
  comboBoxdim3 <- tkwidget(framedim1,"ComboBox",editable=FALSE,values=rep(1:nejes),width=10, text= dim3)


	chang.symdim1 <- function()
	{
   	 	dim1 <<-as.numeric(tclvalue(tcl(comboBoxdim1,"getvalue")))+1
		dim2 <<-as.numeric(tclvalue(tcl(comboBoxdim2,"getvalue")))+1
		dim3 <<-as.numeric(tclvalue(tcl(comboBoxdim3,"getvalue")))+1
      tkrreplot(img)


 	}
	Change.symboldim1 <-tkbutton(framedim1,text="Choose",command=chang.symdim1, bg= "lightblue", width=10, foreground = "navyblue")
	
  
	tkpack(tklabel(framedim1, text="Select X, Y and Z axes numbers:"),
    	expand="FALSE", side= "left", fill ="both")
  	tkpack(comboBoxdim1, comboBoxdim2, comboBoxdim3, Change.symboldim1, side="left", expand="FALSE")
  	tkpack(img, side="top", expand="TRUE", fill="both")
  	tkpack(framedim1, side="top", expand="FALSE", fill="both")



labelClosestPointd <- function(xClick,yClick,imgXcoords,imgYcoords)
	{
  		labelsVec <- c()
		sizesVec <- c()
		colores<-c()
		simbolos<-c()

     for (k in 1:length(colindividuos))
      {
        colores<-c(colores, get(colindividuos[k], envir = mientorno))
        labelsVec<-c(labelsVec, get(textindividuos[k], envir = mientorno))
        sizesVec<-c(sizesVec, get(cexindividuos[k], envir = mientorno))
        simbolos<-c(simbolos, get(simindividuos[k], envir = mientorno))
     }
     for (k in 1:length(colvariables))
      {
       colores<-c(colores, get(colvariables[k], envir = mientorno))
       labelsVec<-c(labelsVec, get(textvariables[k], envir = mientorno))
       sizesVec<-c(sizesVec, get(cexvariables[k], envir = mientorno))
       simbolos<-c(simbolos, get(simvariables[k], envir = mientorno))
      }
    

      
      squared.Distance <- (xClick-imgXcoords)^2 + (yClick-imgYcoords)^2
  		indexClosest <<- which.min(squared.Distance)
		  mm<-tktoplevel() 	
		  tkwm.title(mm, labelsVec[indexClosest])	


		framemm1<-tkframe(mm, relief = "groove", borderwidth = 2, 
        	background = "white")

		framemm2<-tkframe(mm, relief = "groove", borderwidth = 2, 
        	background = "white")

		framemm3<-tkframe(mm, relief = "groove", borderwidth = 2, 
        	background = "white")
        	
    		framemm4<-tkframe(mm, relief = "groove", borderwidth = 2, 
        	background = "white")    	

    
    
		colorder <- colores[indexClosest]
		canvasder <- tkcanvas(framemm1,width="120",height="20",bg=colorder)

		ChangeColorder <- function()
		{
      
			colorder <- tclvalue(tcl("tk_chooseColor",initialcolor=colores[indexClosest],title="Choose a color"))
 
			 if (nchar(colorder)>0)
    				{
					tkconfigure(canvasder,bg=colorder)
		 			colores[indexClosest]<-colorder
					
          for(m in 1:length(colindividuos))
					{
          assign(colindividuos[m],colores[m], envir = mientorno)
          }
					for(m in 1:length(colvariables))
					{
          assign(colvariables[m],colores[length(colindividuos)+m], envir = mientorno)
          } 
				}
			tkrreplot(img)
			tkdestroy(mm)
		}

		ChangeColor.buttonder<- tkbutton(framemm1,text="Change Color",command=ChangeColorder)
		tkpack(canvasder,ChangeColor.buttonder,expand = "TRUE", side="left", fill = "both")
		
    Nameder <- labelsVec[indexClosest]
    tclvalue(Nameder) <- labelsVec[indexClosest]
    entry.Nameder <-tkentry(framemm2,width="10",textvariable=Nameder)
		
    NameValder <- Nameder 
		
		OnOKlder <- function()
		{
      		NameValder <- tclvalue(Nameder)
			labelsVec[indexClosest]<-NameValder

			for(m in 1:length(textindividuos))
					{
          assign(textindividuos[m],labelsVec[m], envir = mientorno)
          }
					for(m in 1:length(textvariables))
					{
          assign(textvariables[m],labelsVec[length(textindividuos)+m], envir = mientorno)
          }
          


#####Values of listbox###############################
		
			for (i in 1:dim(Xpon)[1])
			{
				tkdelete(tli,0)
			}

			for (i in 1:(dim(Xpon)[1]))
			{
   				tkinsert(tli,"end",get(textindividuos[i], envir = mientorno))
			}
	
			

			for (i in 1:(dim(Xpon)[2]))
			{
				tkdelete(tlv,0)
			}

			for (i in 1:(dim(Xpon)[2]))
			{
   				tkinsert(tlv,"end",get(textvariables[i], envir = mientorno))
			}



			tkrreplot(img)
			tkdestroy(mm)

	}

	OK.butlder <-tkbutton(framemm2,text=" Change label",command=OnOKlder,width=2)
	tkbind(entry.Nameder, "<Return>",OnOKlder)
	tkpack(entry.Nameder,OK.butlder,expand = "TRUE", side="left", fill = "both")
	 
 
  
    		Cexder <- sizesVec[indexClosest]  
    		tclvalue(Cexder) <- sizesVec[indexClosest]
		entry.Cexder <-tkentry(framemm3,width="10",textvariable=Cexder)
		NameCexder <- Cexder 

		OnOKcder <- function()
		{
			NameCexder <- tclvalue(Cexder)
			sizesVec[indexClosest]<-NameCexder

			for(m in 1:length(cexindividuos))
					{
          assign(cexindividuos[m],sizesVec[m], envir = mientorno)
          }
					for(m in 1:length(cexvariables))
					{
          assign(cexvariables[m],sizesVec[length(cexindividuos)+m], envir = mientorno)
          }
          
		

			tkrreplot(img)
			tkdestroy(mm)

	}

	OK.butcder <-tkbutton(framemm3,text=" Change size",command=OnOKcder,width=2)
	tkbind(entry.Cexder, "<Return>",OnOKcder)
	tkpack(entry.Cexder,OK.butcder, expand = "TRUE", side="left", fill = "both")
	


	comboBox <- tkwidget(framemm4,"ComboBox",editable=FALSE,values=symbols,width=10, text= simbolos[indexClosest])

	chang.symder <- function()
	{
	   simChoice <-symbols[as.numeric(tclvalue(tcl(comboBox,"getvalue")))+1]
		simbolos[indexClosest]<-simChoice
	
		for(m in 1:length(simindividuos))
					{
          assign(simindividuos[m],simbolos[m], envir = mientorno)
          }
					for(m in 1:length(simvariables))
					{
          assign(simvariables[m],simbolos[length(simindividuos)+m], envir = mientorno)
          }
          

		tkrreplot(img)
		tkdestroy(mm)


 	}
	Change.symbolder <-tkbutton(framemm4,text="   Change symbol   ",command=chang.symder,width=6)
	tkpack(comboBox,Change.symbolder,side="left",expand="TRUE", fill="both")


	tkpack(framemm1,framemm2,framemm3, framemm4, expand = "TRUE", side="top", fill = "both")


	}


OnLeftClick.up <- function(x,y)
	{
	  msg <- ("-To change the label press Yes.\n-To remove it press No.\n-If you do not want to do anything press Cancel.")
	  mbval<- tkmessageBox(title="Change of label",
                       message=msg,type="yesnocancel",icon="question")
   if (tclvalue(mbval)=="yes"){  
			indexLabeled <<- c(indexLabeled,indexClosest)
 		}

		if(tclvalue(mbval)=="no"){

 		indexLabeledaux<<-c()
 		for (i in (1:length(indexLabeled)))
    		{
     			if (indexLabeled[i]!=indexClosest)
				indexLabeledaux <<- c(indexLabeledaux,indexLabeled[i])
    		}
		 indexLabeled<<-indexLabeledaux 
		}
		
		if(tclvalue(mbval)=="cancel"){
			textos[indexClosest,dim1] <<- anteriorx
			textos[indexClosest,dim2] <<- anteriory
 		
		}
  
	tkrreplot(img)
   
   }
	
	
  OnLeftClick.move <- function(x,y)
	{
	xClick <- x
  	yClick <- y
  	width  = as.numeric(tclvalue(tkwinfo("reqwidth",img)))
  	height = as.numeric(tclvalue(tkwinfo("reqheight",img)))

  	xMin = parPlotSize[1] * width
  	xMax = parPlotSize[2] * width
	yMin = parPlotSize[3] * height
 	yMax = parPlotSize[4] * height

 	 rangeX = usrCoords[2] - usrCoords[1]
	 rangeY = usrCoords[4] - usrCoords[3]

 	 imgXcoords = (xCoords-usrCoords[1])*(xMax-xMin)/rangeX + xMin
 	 imgYcoords = (yCoords-usrCoords[3])*(yMax-yMin)/rangeY + yMin

	 xClick <- as.numeric(xClick)+0.5
 	 yClick <- as.numeric(yClick)+0.5
 	 yClick <- height - yClick

	  xPlotCoord = usrCoords[1]+(xClick-xMin)*rangeX/(xMax-xMin)
 	  yPlotCoord = usrCoords[3]+(yClick-yMin)*rangeY/(yMax-yMin)
   

		textos[indexClosest,dim1]<<-xPlotCoord
		textos[indexClosest,dim2]<<-yPlotCoord
		
	tkrreplot(img) 
}


OnLeftClick.down <- function(x,y)
	{
	anteriorx <- NULL
	anteriory <- NULL
  	xClick <- x
  	yClick <- y
  	width  = as.numeric(tclvalue(tkwinfo("reqwidth",img)))
  	height = as.numeric(tclvalue(tkwinfo("reqheight",img)))

  	xMin = parPlotSize[1] * width
  	xMax = parPlotSize[2] * width
	yMin = parPlotSize[3] * height
 	yMax = parPlotSize[4] * height

 	 rangeX = usrCoords[2] - usrCoords[1]
	 rangeY = usrCoords[4] - usrCoords[3]

 	 imgXcoords = (xCoords-usrCoords[1])*(xMax-xMin)/rangeX + xMin
 	 imgYcoords = (yCoords-usrCoords[3])*(yMax-yMin)/rangeY + yMin

	 xClick <- as.numeric(xClick)+0.5
 	 yClick <- as.numeric(yClick)+0.5
 	 yClick <- height - yClick

	  xPlotCoord = usrCoords[1]+(xClick-xMin)*rangeX/(xMax-xMin)
 	  yPlotCoord = usrCoords[3]+(yClick-yMin)*rangeY/(yMax-yMin)

	squared.Distance <- (xClick-imgXcoords)^2 + (yClick-imgYcoords)^2
  	indexClosest <<- which.min(squared.Distance) 

			anteriorx <<- textos[indexClosest,dim1]
			anteriory <<- textos[indexClosest,dim2]
	  
  }






OnRightClick <- function(x,y)
	{
  	xClick <- x
  	yClick <- y
  	width  = as.numeric(tclvalue(tkwinfo("reqwidth",img)))
  	height = as.numeric(tclvalue(tkwinfo("reqheight",img)))

  	xMin = parPlotSize[1] * width
  	xMax = parPlotSize[2] * width
	yMin = parPlotSize[3] * height
 	yMax = parPlotSize[4] * height

 	 rangeX = usrCoords[2] - usrCoords[1]
	 rangeY = usrCoords[4] - usrCoords[3]

 	 imgXcoords = (xCoords-usrCoords[1])*(xMax-xMin)/rangeX + xMin
 	 imgYcoords = (yCoords-usrCoords[3])*(yMax-yMin)/rangeY + yMin

	 xClick <- as.numeric(xClick)+0.5
 	 yClick <- as.numeric(yClick)+0.5
 	 yClick <- height - yClick

	  xPlotCoord = usrCoords[1]+(xClick-xMin)*rangeX/(xMax-xMin)
 	  yPlotCoord = usrCoords[3]+(yClick-yMin)*rangeY/(yMax-yMin)

	
 	 labelClosestPointd(xClick,yClick,imgXcoords,imgYcoords)

	}


	tkbind(img, "<B1-Motion>",OnLeftClick.move)
	tkbind(img, "<ButtonPress-1>",OnLeftClick.down)
	tkbind(img, "<ButtonRelease-1>",OnLeftClick.up)
	tkconfigure(img,cursor="pencil")
	
	tkbind(img, "<Button-3>",OnRightClick)
	tkconfigure(img,cursor="pencil")

	  }
	  

bootstrap <- function(Xpon, X, covartotal, iter){
###############################################################################				
####	We standardize the matrices
###############################################################################

			if (tChoice=="Subtract the global mean"){			
      Xstd<-X
      media<-mean(Xstd)
      
			for(j in 1:dim(Xstd)[2])
				{for(i in 1:dim(Xstd)[1])
					{
						Xstd[i,j]<-(Xstd[i,j]-media)		
					}
				}
      }

		if (tChoice=="Column centering"){
      Xstd<-X
      mediav<-colMeans(Xstd)
	  
			for(j in 1:dim(Xstd)[2])
				{for(i in 1:dim(Xstd)[1])
					{
						Xstd[i,j]<-(Xstd[i,j]-mediav[j])	
					}
				}
      }

		if (tChoice=="Standardize columns"){	
      Xstd<-X
      mediav<-colMeans(Xstd)
	desvvar<-sqrt(diag(var(Xstd)))
		
			for(j in 1:dim(Xstd)[2])
				{for(i in 1:dim(Xstd)[1])
					{
						Xstd[i,j]<-(Xstd[i,j]-mediav[j])/desvvar[j]
					}
				}
      }
	

		if (tChoice=="Row centering"){		
		 Xstd<-X     
    		mediav<-rowMeans(Xstd)
     
			for(j in 1:dim(Xstd)[2])
				{for(i in 1:dim(Xstd)[1])
					{
						Xstd[i,j]<-(Xstd[i,j]-mediav[i])
					}
				}	
      }


		if (tChoice=="Standardize rows"){
      Xstd<-X
      
     	mediav<-rowMeans(Xstd)
			desvvar<-mediav
			for (i in 1:dim(Xstd)[1])
			{
				desvvar[i]<-sqrt(var(Xstd[i,]))
			}

			for(j in 1:dim(Xstd)[2])
				{for(i in 1:dim(Xstd)[1])
					{
						Xstd[i,j]<-(Xstd[i,j]-mediav[i])/desvvar[i]
					}
				}
      }
	
		if (tChoice=="Raw data"){
		Xstd<-X
		}
				  matX <- as.matrix(Xstd)
				  assign(muestrasstd[iter], Xstd, envir = mientorno)
				  textvariables <- colnames(X)
			        textindividuos <- rownames(X)
			  	
##############################################################################
####	We create vectors of the colors
##############################################################################

		colvariables <- rep("blue",times = dim(X)[2])		
		colindividuos <- rep("green",times = dim(X)[1])


##############################################################################
####	We create vectors of the symbols
##############################################################################

		simvariables <- rep(" ",times = dim(X)[2])		
		simindividuos <- rep("+",times = dim(X)[1])
	
##############################################################################
#####		Coordinates
##############################################################################
		ejes<-c()
    for (i in 1:nejes)
    {
      ejes<-c(ejes, paste("Axis",i))
    }
    
        descom <- La.svd(matX, nu = nejes, nv = nejes)
		    suma2valprop <- sum((descom$d[1:nejes])^2)
				sumaRvalprop <- sum((La.svd(matX)$d)^2)
				
				
    if (tipo == "RMP"){
				coindividuos<-descom$u%*%diag(descom$d[1:nejes])
				covariables<-t(descom$v)	
        calcol <- (nejes/length(La.svd(matX)$d)) * 100			
    }
    
    if (tipo == "RCMP"){		
				coindividuos <- descom$u %*% diag(descom$d[1:nejes])
				covariables <- t(descom$v) %*% diag(descom$d[1:nejes])
				calcol <- (suma2valprop/sumaRvalprop) * 100
		}
    
    if (tipo == "CMP"){		
				coindividuos <- descom$u
				covariables <- t(descom$v) %*% diag(descom$d[1:nejes])
				calcol <- (suma2valprop/sumaRvalprop) * 100
			
		}
    
    		covartotalb <<-covartotal
        	assign(covarb[iter], procrustes(covartotalb, covariables)$Yrot, envir = mientorno)
        	covartotalb<<-procrustes(get(covarb[1], envir = mientorno), envir = mientorno, covartotalb)$Yrot
        	assign(covar[iter], covariables, envir = mientorno)
        	assign(coind[iter], coindividuos, envir = mientorno)

##############################################################################
#####		Contributions, goodness of fit and qualities of representation
##############################################################################
	
				

				#suma2valprop4 <- sum((descom$d[1:nejes])^4)
				#sumaRvalprop4 <- sum((descom$d)^4)
					
					
					
	
				coindividuosnam <- as.data.frame(coindividuos)
				rownames(coindividuosnam) <- textindividuos
				colnames(coindividuosnam) <- ejes

				covariablesnam <- as.data.frame(covariables)
				rownames(covariablesnam) <- textvariables
				colnames(covariablesnam) <- ejes

				#coindivcuad <- coindividuos^2
				#CRTi <- rowSums(coindivcuad)
				#CRTi <- (CRTi * 1000) / suma2valprop
				#CRTi <- as.data.frame(CRTi)
				#rownames(CRTi) <- textindividuos

				covarcuad <- covariables^2
				CRTj <- rowSums(covarcuad)
				CRTj <- (CRTj * 1000) / suma2valprop
				CRTj <- as.data.frame(CRTj)
				rownames(CRTj) <- textvariables


				#CREiFq <- array(dim = dim(coindividuos))
				#CREiFq[,1] <- ((coindivcuad)[,1] * 1000) / ((descom$d[1])^2)
				#CREiFq[,2] <- ((coindivcuad)[,2] * 1000) / ((descom$d[2])^2)
				#CREiFq[,3] <- ((coindivcuad)[,3] * 1000) / ((descom$d[3])^2)
				#CREiFq <- as.data.frame(CREiFq)
				#rownames(CREiFq) <- textindividuos
				#colnames(CREiFq) <- ejes
					
				CREjFq <- array(dim = dim(covariables))
				sumavar <- rowSums(covarcuad)
								
				CRFqEj <- covarcuad
				
				for (i in 1: nejes)
				{
        			CREjFq[,i] <- ((covarcuad)[,i] * 1000) / ((descom$d[i])^2)
				CRFqEj[,i] <- ((covarcuad)[,i] * 1000) / (sumavar)
				}
        			
				CREjFq <- as.data.frame(CREjFq)
				rownames(CREjFq) <- textvariables
				colnames(CREjFq) <- ejes
        
        			CRFqEj <- as.data.frame(CRFqEj)
				rownames(CRFqEj) <- textvariables
				colnames(CRFqEj) <- ejes

        
				#CRFqEi <- coindivcuad
				#sumaindi <- rowSums(coindivcuad)
				#CRFqEi[,1] <- ((coindivcuad)[,1] * 1000) / (sumaindi)
				#CRFqEi[,2] <- ((coindivcuad)[,2] * 1000) / (sumaindi)
				#CRFqEi[,3] <- ((coindivcuad)[,3] * 1000) / (sumaindi)
				#CRFqEi <- as.data.frame(CRFqEi)
				#rownames(CRFqEi) <- textindividuos
				#colnames(CRFqEi) <- ejes



				#calfilas <- (suma2valprop4 / sumaRvalprop4) * 100

				Xestim <- descom$u %*% diag(descom$d[1:nejes]) %*% descom$v
				
				SCT <- sum(diag(t(matX) %*% matX))
				SCEx <- sum(diag(t(Xestim) %*% Xestim))

				bonajusXstd <- (SCEx / SCT) * 100



    	assign(autovalores[iter], descom$d, envir = mientorno)
    	assign(CRTjv[iter], CRTj, envir = mientorno)
    	assign(CREjFqv[iter], CREjFq, envir = mientorno)
    	assign(CRFqEjv[iter], CRFqEj, envir = mientorno)
    	assign(bondades[iter], bonajusXstd, envir = mientorno)
    	assign(calidadcolumnas[iter], calcol, envir = mientorno)
    
    #######################################
    #####save  .RData
    #######################################
		datos <- rbind(coindividuos, covariables)
		centro <- c(0,0)
			
		
		labelsVec <- c(textindividuos, textvariables)
		colores <- c(colindividuos, colvariables)
		simbolos <- c(simindividuos, simvariables)
		
		
######cálculo de ángulos entre variables

n <- dim(covariables)[1]
o <- c(0,0,0)
ang <- array(dim = c(n,n)) 
 
for (z in 1:n)
  {
    for(j in z:n)
      {
        ang[z,j] <- (atan2(get(covar[iter], envir = mientorno)[z,1], get(covar[iter], envir = mientorno)[z,2]) - atan2(get(covar[iter], envir = mientorno)[j,1], get(covar[iter], envir = mientorno)[j,2])) * 180/pi
       
	  if (ang[z,j] < 0)
		ang[z,j] <- -ang[z,j]
	  	ang[j,z] <- ang[z,j]
    }  
  }
  
  assign(angulo[iter], ang, envir = mientorno)

######cálculo de ángulos entre variables y ejes

n <- dim(covariables)[1]
o <- c(0,0)
axisx <- c(1,0)
axisy <- c(0,1)
angeje <- array(dim = c(n,2)) 
 
for (z in 1:n)
  {
    
        angeje[z,1] <- (atan2(get(covar[iter], envir = mientorno)[z,1], get(covar[iter], envir = mientorno)[z,2]) - atan2(axisx[1], axisx[2])) * 180/pi
        angeje[z,2] <- (atan2(get(covar[iter], envir = mientorno)[z,1], get(covar[iter], envir = mientorno)[z,2]) - atan2(axisy[1], axisy[2])) * 180/pi
    
    
    for(j in 1:2)
      {   
	  if (angeje[z,j] < 0)
		angeje[z,j] <- -angeje[z,j]
    }  
  }
  
  assign(anguloeje[iter], angeje, envir = mientorno)

  
  }

muestras <<- rep("Xsample", niter)
muestrasstd <<- rep("Xsamplestd", niter)
covar <<- rep("covar", niter)
covarb <<- rep("covarb", niter)
coind <<- rep("coind", niter)
angulo <<- rep("ang", niter)
anguloeje <<- rep("angeje", niter)
autovalores <<- rep("eig", niter)
CRTjv <<- rep("CRTj", niter)
CREjFqv <<- rep("CREjFq", niter)
CRFqEjv <<- rep("CRFqEj", niter)
bondades <<- rep("bondadajus", niter)
calidadcolumnas <<- rep("calcol", niter)


for (i in 1:niter)
{
indices <<- sample(1:dim(Xpon)[1], replace = T)
muestras[i] <<- paste("Xsample", i, sep = "")
muestrasstd[i] <<- paste("Xsamplestd", i, sep = "")
covar[i] <<- paste("covar", i, sep = "")
covarb[i] <<- paste("covarb", i, sep = "")
coind[i] <<- paste("coind", i, sep = "")
angulo[i] <<- paste("ang", i, sep = "")
anguloeje[i] <<- paste("angeje", i, sep = "")
autovalores[i] <<- paste("eig", i, sep = "")
CRTjv[i] <<- paste("CRTj", i, sep = "")
CREjFqv[i] <<- paste("CREjFq", i, sep = "")
CRFqEjv[i] <<- paste("CRFqEj", i, sep = "")
bondades[i] <<- paste("bondadajus", i, sep = "")
calidadcolumnas[i] <<- paste("calcol", i, sep = "")



assign(muestras[i], Xpon[indices,], envir = mientorno)
bootstrap(Xpon, Xpon[indices,], covartotal, i)
print(i)
}

######################################################################
############ Resultados bootstrap
######################################################################
####pintar coordenadas procrustes
coloresproc <- c("red","green","blue","yellow","pink","orange", "black",
"violet", "brown", "grey", "navyblue", "darkgreen")

        
coordenadas <- covartotalb
coor <- rep("coord",dim(covartotalb)[1])
for(j in 1:dim(covartotalb)[1])
        {
          coor[j] <- paste("coor", j, sep="")
          assign(coor[j], array(dim=c(niter,nejes)), envir = mientorno)
        }


colproc <- coloresproc [1:dim(covartotalb)[1]]
for(i in 1:length(covarb))
{
  coordenadas <- rbind(coordenadas, get(covarb[i], envir = mientorno))
}



cooraux<-array(dim=c(niter,nejes))

for (j in 1: dim(covartotalb)[1])
{
  for (i in 1:niter+1)
    {
    cooraux[i-1,] <- coordenadas[j+(dim(covartotalb)[1])*(i-1),]
    }
assign(coor[j], cooraux, envir = mientorno)  
}

plot(coordenadas[,1], coordenadas[,2], col = colproc, xlab="", ylab="")
#text(coordenadas[,1], coordenadas[,2], labels =colnames(Xpon), col = colproc)

text(coordenadas[1:dim(covartotal)[1],1], coordenadas[1:dim(covartotal)[1],2], labels =colnames(x), col = colproc)


####crear vectores con cada autovalor

autovaloresalm <- rep("eigenvalue", length(get(autovalores[1], envir = mientorno)))
#icautovaloresalm <- array(dim= c(length(get(autovalores[1], envir = mientorno)),2))
for (i in 1:length(get(autovalores[1], envir = mientorno)))
{
	autovaloresalm[i] <- paste("eigenvalue", i, sep = "")
	autovalor <- c()
	
	for(j in 1:niter)
	{
		autovalor <- c(autovalor, get(autovalores[j], envir = mientorno)[i])
	}
	assign(autovaloresalm[i], autovalor, envir = mientorno)
  
}


####crear vectores con bondades y calidades de representación de columnas

	bondadesalm <- c()
	calidadesalm <- c()
	
	for(i in 1:niter)
	{
		bondadesalm <- c(bondadesalm, get(bondades[i], envir = mientorno))
		calidadesalm <- c(calidadesalm, get(calidadcolumnas[i], envir = mientorno))
	}





###angulos menores de 180º
for (z in 1:niter)
{
	angaux <- get(angulo[z], envir = mientorno)
	for (i in 1:dim(get(angulo[1], envir = mientorno))[1])
	{
		for (j in i:dim(get(angulo[1], envir = mientorno))[1])
		{
			if (angaux[i,j] > 180){
			angaux[i,j] <- -(angaux[i,j] - 360)
			angaux[j,i] <- angaux[i,j]
			}
		}
	}
	assign(angulo[z], angaux, envir = mientorno)
}



####crear vectores con cada angulo

angulosalm <- array(rep("angalm", length(get(angulo[1], envir = mientorno))), dim = c(dim(get(angulo[1], envir = mientorno))[1],dim(get(angulo[1], envir = mientorno))[1]))
for (i in 1:dim(get(angulo[1], envir = mientorno))[1])
{
	for (j in i:dim(get(angulo[1], envir = mientorno))[1])
	{
		angulosalm[i,j] <- paste("angalm", i, j, sep = "")
		angulosalm[j,i] <- paste("angalm", j, i, sep = "")
		angaux<-c()
	
		for (z in 1:niter)
		{
			angaux<-c(angaux, get(angulo[z], envir = mientorno)[i,j])
		}
		assign(angulosalm[i,j], angaux, envir = mientorno)
		assign(angulosalm[j,i], angaux, envir = mientorno)
	}
}

      ###angulos menores de 180º
for (z in 1:niter)
{
	angaux <- get(anguloeje[z], envir = mientorno)

	for (i in 1:dim(get(anguloeje[1], envir = mientorno))[1])
	{
		for (j in 1:2)
		{
			if (angaux[i,j] > 180){
			angaux[i,j] <- -(angaux[i,j] - 360)
      }
      if (angaux[i,j] > 90){
		  angaux[i,j] <- -(angaux[i,j] - 180)
      
      }
      
      
		}
	}
	assign(anguloeje[z], angaux, envir = mientorno)
}



####crear vectores con cada angulo (ejes)

anguloejesalm <- array(rep("angejealm", length(get(anguloeje[1], envir = mientorno))), dim = c(dim(get(angulo[1], envir = mientorno))[1],2))
for (i in 1:dim(get(anguloeje[1], envir = mientorno))[1])
{
	for (j in 1:2)
	{
		anguloejesalm[i,j] <- paste("angejealm", i, j, sep = "")
		angaux<-c()
	
		for (z in 1:niter)
		{
			angaux<-c(angaux, get(anguloeje[z], envir = mientorno)[i,j])
		}
		assign(anguloejesalm[i,j], angaux, envir = mientorno)
	}
}

#### contribuciones de los ejes a las variables

CRFqEjelem <- rep("CRFqEjelem", dim(covartotal)[1])
CRFqEjvar <- rep("CRFqEjvar", dim(covartotal)[1]*nejes)
CRFqEjvar <- array(dim=c(dim(covartotal)[1],nejes))
CRFqEjtable <- rep("CRFqEjtable", dim(covartotal)[1])

for (i in 1:dim(covartotal)[1])
{
  CRFqEjelem[i] <- paste("CRFqEjelem", i, sep = "")
  assign(CRFqEjelem[i], c(), envir = mientorno)
  for (j in 1:nejes)
  {
      CRFqEjvar[i,j] <- paste("CRFqEjvar", i,j, sep = "")
      assign(CRFqEjvar[i,j], c(), envir = mientorno)
  }
  CRFqEjtable[i] <- paste("CRFqEjtable", i, sep = "")
  assign(CRFqEjtable[i], NULL, envir = mientorno)

}


for(i in 1:niter)
{
	for (j in 1:dim(covartotal)[1])
	{
  assign(CRFqEjelem[j], c(get(CRFqEjelem[j], envir = mientorno), colnames(get(CRFqEjv[i], envir = mientorno))[which(get(CRFqEjv[i], envir = mientorno)[j,] >= 600)]), envir = mientorno)
    for (z in 1: nejes)
        {
            assign(CRFqEjvar[j,z], c(get(CRFqEjvar[j,z], envir = mientorno), get(CRFqEjv[i], envir = mientorno)[j,z]), envir = mientorno)
        }
  }
}

for (i in 1:dim(covartotal)[1])
{
  assign(CRFqEjtable[i],table(get(CRFqEjelem[i], envir = mientorno)), envir = mientorno)
}

#### contribuciones de las variables a los ejes

CREjFqaxis <- rep("CREjFqaxis", nejes)
CREjFqax <- rep("CREjFqax", dim(covartotal)[1]*nejes)
CREjFqax <- array(unlist(CREjFqax), dim=c(dim(covartotal)[1],nejes))
CREjFqtable <- rep("CREjFqtable", nejes)

for (i in 1:nejes)
{
  CREjFqaxis[i] <- paste("CREjFqaxis", i, sep = "")
  assign(CREjFqaxis[i], c(), envir = mientorno)
  for (j in 1:dim(covartotal)[1])
  {
      CREjFqax[j,i] <- paste("CREjFqax", j,i, sep = "")
      assign(CREjFqax[j,i], c(), envir = mientorno)
  }
  CREjFqtable[i] <- paste("CREjFqtable", i, sep = "")
  assign(CREjFqtable[i], NULL, envir = mientorno)

}


for(i in 1:niter)
{
	for (j in 1:nejes)
	{
  assign(CREjFqaxis[j], c(get(CREjFqaxis[j], envir = mientorno), rownames(get(CREjFqv[i], envir = mientorno))[which(get(CREjFqv[i], envir = mientorno)[,j] >= 600)]), envir = mientorno)
  for (z in 1: dim(covartotal)[1])
        {
            assign(CREjFqax[z,j], c(get(CREjFqax[z,j], envir = mientorno), get(CREjFqv[i], envir = mientorno)[z,j]), envir = mientorno)
        }
  
  }
}

for (i in 1:nejes)
{
  assign(CREjFqtable[i],table(get(CREjFqaxis[i], envir = mientorno)), envir = mientorno)
}

######################################################################
########## CRTj
######################################################################

crtjalm <- rep("crtj", dim(covartotal)[1])

for (i in 1:dim(covartotal)[1])
{
  crtjalm[i] <- paste("crtjalm", i, sep = "")
  assign(crtjalm[i], c(), envir = mientorno)
}




for (i in 1:niter)
{
  for (j in 1: dim(covartotal)[1])
  {
	assign(crtjalm[j], c(get(crtjalm[j], envir = mientorno),get(CRTjv[i], envir = mientorno)[j,1]), envir = mientorno)
  }
}

######################################################################
########## Longitudes
######################################################################


longitud<-c()

for (i in 1:dim(coordenadas)[1])
{
	lon<-sqrt(coordenadas[i,1]^2 + coordenadas[i,2]^2)
	longitud<-c(longitud,lon)
}


longitudes <- rep("longitudes", dim(covartotal)[1])

for (i in 1:dim(covartotal)[1])
{
  longitudes[i] <- paste("longitudes", i, sep = "")
  assign(longitudes[i], c(), envir = mientorno)
}



for (i in 1:niter+1)
{
  for (j in 1: dim(covartotal)[1])
  {
	assign(longitudes[j], c(get(longitudes[j], envir = mientorno),longitud[j+(dim(covartotal)[1])*(i-1)]), envir = mientorno)
  }
}

#########################################################################
### Guardar resultados
#########################################################################
      
      
      cat("File saved in:    ",file="Resultsbootstrap.txt")
			cat(getwd(),file="temp.txt")					
			file.append("Resultsbootstrap.txt","temp.txt")	
			cat("\n",file="temp.txt")					
			file.append("Resultsbootstrap.txt","temp.txt")
			cat("\n",file="temp.txt")					
			file.append("Resultsbootstrap.txt","temp.txt")		
				                             
			
      alphaic <<- tclvalue(Nalpha)
	alphaic <<- as.numeric(alphaic)
      liminf <- (1 - alphaic*0.01) / 2
	limsup <- 1 - (1 - alphaic*0.01) / 2
	    
     nombresvariables <-c()
     for (i in 1: length(textvariables))
        {
         nombresvariables <-c(nombresvariables, get(textvariables[i], envir = mientorno))
        }
        
        
	    cal.ic <- function (muestra, liminf, limsup, valorobs)
	    {
        c.mean <- mean (muestra)
        se <- sd(muestra)
        sesgo <- c.mean - valorobs
        t.ic <- se * (-qt(liminf,(dim(x)[1]-1)))
        ic.t <- c(c.mean - t.ic, c.mean + t.ic)
        ic.p <- quantile (muestra,c(liminf, limsup))
        return(c(c.mean, se,sesgo,ic.t, ic.p))
      }
      titulo <-c("Obs. Value","Mean","SE","Bias","IC t-boot inf","IC t-boot sup","IC perc inf","IC perc sup")
	    
	    
### Goodness of fit
     
     if (cgfVal=="1")
     {
        if (tipo != "RCMP"){		
		
        calc.cgf <-cal.ic(bondadesalm, liminf, limsup, bonajuste)
        cgf.mean <- calc.cgf[1]
        se.cgf <- calc.cgf[2]
        sesgo.cgf <- calc.cgf[3]
        ic.t.cgf <- calc.cgf[4:5]
        ic.p.cgf <- calc.cgf[6:7]
        
        
        calc.cgf <-array(c(bonajuste, calc.cgf), dim=c(1,8))
        calc.cgf <- as.data.frame(calc.cgf)
        colnames(calc.cgf) <- titulo
        rownames(calc.cgf) <-c("Goodnes of Fit")
        
        
        tiff(paste("Histogram of Goodness of fit", ".tif", sep = ""), height = 500, width = 500)
        par(mfrow=c(1,2))
        hist(bondadesalm,main="Histogram", xlab="Goodness of Fit")
        abline(v=cgf.mean, lwd=2, col="blue")
        abline(v=bonajuste, lty =2, lwd=2, col="red")
        qqnorm(bondadesalm)
        dev.off()
        
        
     
     
			cat("\n",file="temp.txt")					
			file.append("Resultsbootstrap.txt","temp.txt")	
			cat("Goodness of fit:  \n",file="temp.txt")					
			file.append("Resultsbootstrap.txt","temp.txt")					
			write.table(round(calc.cgf, digits=2),file="temp.txt",, sep="\t", dec=",")
			file.append("Resultsbootstrap.txt","temp.txt")
			
      }
     }

### Quality of representation     
    
     if (ccrVal=="1")
     {
        calc.ccr <-cal.ic(calidadesalm, liminf, limsup, calcol)
        ccr.mean <- calc.ccr[1]
        se.ccr <- calc.ccr[2]
        sesgo.ccr <- calc.ccr[3]
        ic.t.ccr <- calc.ccr[4:5]
        ic.p.ccr <- calc.ccr[6:7]
        
        
        calc.ccr <-array(c(calcol,calc.ccr), dim=c(1,8))
        calc.ccr <- as.data.frame(calc.ccr)
        colnames(calc.ccr) <- titulo
        rownames(calc.ccr) <-c("Quality of Approximation")
    
        tiff(paste("Histogram of Quality of Approximation", ".tif", sep = ""), height = 500, width = 500)
        par(mfrow=c(1,2))
        hist(calidadesalm,main="Histogram", xlab="Quality of Approximation")
        abline(v=ccr.mean, lwd=2, col="blue")
        abline(v=calcol, lty =2, lwd=2, col="red")
        qqnorm(calidadesalm)
        dev.off()
     
     
		     	cat("\n",file="temp.txt")					
			file.append("Resultsbootstrap.txt","temp.txt")	
			cat("Quality of approximation for columns:  \n",file="temp.txt")					
			file.append("Resultsbootstrap.txt","temp.txt")					
			write.table(round(calc.ccr, digits=2),file="temp.txt", sep="\t", dec=",")
			file.append("Resultsbootstrap.txt","temp.txt")

     }

### Eigenvalues
     
	   if (ceiVal=="1")
     {
      
      calc.cei <-c()
      cei.mean <-c()
      se.cei <-c()
      sesgo.cei <-c()
      ic.t.ceiinf <-c()
      ic.t.ceisup <-c()
      ic.p.ceiinf <-c()
      ic.p.ceisup <-c()
    
      for (i in 1:length(autovaloresalm))
      { 
      
        calc.cei <-cal.ic(get(autovaloresalm[i], envir = mientorno), liminf, limsup, descom$d[i])
        cei.mean <- c(cei.mean, calc.cei[1])
        se.cei <- c(se.cei,calc.cei[2])
        sesgo.cei <- c(sesgo.cei,calc.cei[3])
        ic.t.ceiinf <- c(ic.t.ceiinf,calc.cei[4])
        ic.t.ceisup <- c(ic.t.ceisup,calc.cei[5])
        ic.p.ceiinf <- c(ic.p.ceiinf,calc.cei[6])
        ic.p.ceisup <- c(ic.p.ceisup,calc.cei[7])
    
        
        
        tiff(paste("Histogram of eigenvalue", i, ".tif", sep = ""), height = 500, width = 500)
        par(mfrow=c(1,2))
        hist(get(autovaloresalm[i], envir = mientorno),main="Histogram", xlab=paste("Eigenvalue", i))
        abline(v=cei.mean[i], lwd=2, col="blue")
        abline(v=descom$d[i], lty =2, lwd=2, col="red")
        qqnorm(get(autovaloresalm[i], envir = mientorno))
        dev.off()
      }
      
      
        calc.cei <-array(cbind(descom$d, cei.mean, se.cei, sesgo.cei, ic.t.ceiinf, ic.t.ceisup, ic.p.ceiinf, ic.p.ceisup),
         dim=c(length(descom$d),8))
        calc.cei <- as.data.frame(calc.cei)
        colnames(calc.cei) <- titulo
        
        nombreseig<-c()
        for (i in 1: length(descom$d))
        {
         nombreseig <-c(nombreseig, paste("Eigenvalue",i, sep=""))
        }
        
        rownames(calc.cei) <- nombreseig
        
      
      		cat("\n",file="temp.txt")					
			file.append("Resultsbootstrap.txt","temp.txt")	
			cat("Eigenvalues: \n",file="temp.txt")					
			file.append("Resultsbootstrap.txt","temp.txt")					
			write.table(round(calc.cei, digits=2), file="temp.txt", sep="\t", dec=",")
			file.append("Resultsbootstrap.txt","temp.txt")

     }

### Angles between variables
     
	   if (cavarVal=="1")
     {
      		cat("\n",file="temp.txt")					
			file.append("Resultsbootstrap.txt","temp.txt")	
			cat("Angles between variables:",file="temp.txt")					
			file.append("Resultsbootstrap.txt","temp.txt")					
			
			
      for (i in 1:(dim(angulosalm)[1]-1))
      { 
        
          calc.cavar <-c()
          cavar.mean <-c()
          se.cavar <-c()
          sesgo.cavar <-c()
          ic.t.cavarinf <-c()
          ic.t.cavarsup <-c()
          ic.p.cavarinf <-c()
          ic.p.cavarsup <-c()
    
    
        for (j in  (i+1) : dim(angulosalm)[2])
        {
          calc.cavar <-cal.ic(get(angulosalm[i,j], envir = mientorno), liminf, limsup, ang[i,j])
          cavar.mean <- c(cavar.mean, calc.cavar[1])
          se.cavar <- c(se.cavar,calc.cavar[2])
          sesgo.cavar <- c(sesgo.cavar,calc.cavar[3])
          ic.t.cavarinf <- c(ic.t.cavarinf,calc.cavar[4])
          ic.t.cavarsup <- c(ic.t.cavarsup,calc.cavar[5])
          ic.p.cavarinf <- c(ic.p.cavarinf,calc.cavar[6])
          ic.p.cavarsup <- c(ic.p.cavarsup,calc.cavar[7])
    
        
        
        tiff(paste("Histogram of angles between ", nombresvariables[i]," and ", nombresvariables[j], ".tif", sep = ""), height = 500, width = 500)
        par(mfrow=c(1,2))
        hist(get(angulosalm[i,j], envir = mientorno),main="Histogram", xlab=paste("Angle between ", nombresvariables[i],"\n and ", nombresvariables[j]))
        abline(v=cavar.mean[length(cavar.mean)], lwd=2, col="blue")
        abline(v=ang[i,j], lty =2, lwd=2, col="red")
        qqnorm(get(angulosalm[i,j], envir = mientorno))
        dev.off()
        
        }
       
        
        calc.cavar <-array(cbind(ang[-(1:i),i], cavar.mean, se.cavar, sesgo.cavar, ic.t.cavarinf, ic.t.cavarsup, ic.p.cavarinf, ic.p.cavarsup),
         dim=c(dim(angulosalm)[1]-i,8))
        calc.cavar <- as.data.frame(calc.cavar)
        colnames(calc.cavar) <- titulo
        
        rownames(calc.cavar) <- nombresvariables[-(1:i)]
      
      
			cat("\n Angles between ", nombresvariables[i]," and ", "\n",file="temp.txt")					
			file.append("Resultsbootstrap.txt","temp.txt")					
			write.table(round(calc.cavar, digits=2), file="temp.txt", sep="\t", dec=",")
			file.append("Resultsbootstrap.txt","temp.txt")     
    
    }
 }
     
### Angles between variables and axes

	   if (cavarjVal=="1")
     {
      		cat("\n",file="temp.txt")					
			file.append("Resultsbootstrap.txt","temp.txt")	
			cat("Angles between variables and axes:",file="temp.txt")					
			file.append("Resultsbootstrap.txt","temp.txt")					
			
			
      for (i in 1:dim(covartotal)[1])
      { 
        
          calc.cavarj <-c()
          cavarj.mean <-c()
          se.cavarj <-c()
          sesgo.cavarj <-c()
          ic.t.cavarjinf <-c()
          ic.t.cavarjsup <-c()
          ic.p.cavarjinf <-c()
          ic.p.cavarjsup <-c()
    
    
        for (j in  1 : 2)
        {
        
          calc.cavarj <-cal.ic(get(anguloejesalm[i,j], envir = mientorno), liminf, limsup, angax[i,j])
          cavarj.mean <- c(cavarj.mean, calc.cavarj[1])
          se.cavarj <- c(se.cavarj,calc.cavarj[2])
          sesgo.cavarj <- c(sesgo.cavarj,calc.cavarj[3])
          ic.t.cavarjinf <- c(ic.t.cavarjinf,calc.cavarj[4])
          ic.t.cavarjsup <- c(ic.t.cavarjsup,calc.cavarj[5])
          ic.p.cavarjinf <- c(ic.p.cavarjinf,calc.cavarj[6])
          ic.p.cavarjsup <- c(ic.p.cavarjsup,calc.cavarj[7])
    
        
        
        tiff(paste("Histogram of angles between ", nombresvariables[i]," and axis ", j, ".tif", sep = ""), height = 500, width = 500)
        par(mfrow=c(1,2))
        hist(get(anguloejesalm[i,j], envir = mientorno),main="Histogram", xlab=paste("Angle between \n", nombresvariables[i]," and axis ", j))
        abline(v=cavarj.mean[length(cavarj.mean)], lwd=2, col="blue")
        abline(v=angax[i,j], lty =2, lwd=2, col="red")
        qqnorm(get(anguloejesalm[i,j], envir = mientorno))
        dev.off()
        
        }
        
        calc.cavarj <-array(cbind(unlist(angax[i,]), cavarj.mean, se.cavarj, sesgo.cavarj, ic.t.cavarjinf, ic.t.cavarjsup, ic.p.cavarjinf, ic.p.cavarjsup),
         dim=c(2,8))
        calc.cavarj <- as.data.frame(calc.cavarj)
        colnames(calc.cavarj) <- titulo
        
        rownames(calc.cavarj) <- ejes[1:2]
      
      
			cat("\n Angles between ", nombresvariables[i]," and ", "\n",file="temp.txt")					
			file.append("Resultsbootstrap.txt","temp.txt")					
			write.table(round(calc.cavarj, digits=2), file="temp.txt", sep="\t", dec=",")
			file.append("Resultsbootstrap.txt","temp.txt")     
    
    }
      
  }
     
### Variable coordinates

     #if (cvcooVal=="1")
     #{
      		#cat("\n",file="temp.txt")					
			#file.append("Resultsbootstrap.txt","temp.txt")	
			#cat("Variable coordinates:",file="temp.txt")					
			#file.append("Resultsbootstrap.txt","temp.txt")					
			
      #for (i in 1:length(coor))
      #{ 
      #  calc.coo <-c()
      #  coo.mean <-c()
      #  se.coo <-c()
      #  sesgo.coo <-c()
      #  ic.t.cooinf <-c()
      #  ic.t.coosup <-c()
      #  ic.p.cooinf <-c()
      #  ic.p.coosup <-c()
      #
      #  for (j in  1:nejes)
      #  {

      #  calc.coo <-cal.ic(get(coor[i],envir=mientorno)[,j], liminf, limsup, coordenadas[i,j])
      #  coo.mean <- c(coo.mean, calc.coo[1])
      #  se.coo <- c(se.coo,calc.coo[2])
      #  sesgo.coo <- c(sesgo.coo,calc.coo[3])
      #  ic.t.cooinf <- c(ic.t.cooinf,calc.coo[4])
      #  ic.t.coosup <- c(ic.t.coosup,calc.coo[5])
      #  ic.p.cooinf <- c(ic.p.cooinf,calc.coo[6])
      #  ic.p.coosup <- c(ic.p.coosup,calc.coo[7])
    
        
        
      #  tiff(paste("Histogram of coordinate of ", nombresvariables[i]," in axis ", j, ".tif", sep = ""), height = 500, width = 500)
      #  par(mfrow=c(1,2))
      #  hist(get(coor[i],envir=mientorno)[,j],main="Histogram", xlab=paste("Coordinate of ", nombresvariables[i]," in axis ", j))
      #  abline(v=coo.mean[j], lwd=2, col="blue")
      #  abline(v=coordenadas[i,j], lty =2, lwd=2, col="red")
      #  qqnorm(get(coor[i],envir=mientorno)[,j])
      #  dev.off()
        
      #  }
      
      #  calc.coo <-array(cbind(coordenadas[i,] ,coo.mean, se.coo, sesgo.coo, ic.t.cooinf, ic.t.coosup, ic.p.cooinf, ic.p.coosup),
      #   dim=c(nejes,8))
      #  calc.coo <- as.data.frame(calc.coo)
      #  colnames(calc.coo) <- titulo
      #  
      #  rownames(calc.coo) <- ejes
      
      
			#cat("\n Coordinates of", nombresvariables[i],file="temp.txt")					
			#file.append("Resultsbootstrap.txt","temp.txt")					
			#write.table(round(calc.coo, digits=2), file="temp.txt", sep="\t", dec=",")
			#file.append("Resultsbootstrap.txt","temp.txt")     
    
    #}
      
     #  }

### Relative contribution to total variability
     
	   if (ccrtjVal=="1")
     {
      calc.crtj <-c()
      ccrtj.mean <-c()
      se.crtj <-c()
      sesgo.crtj <-c()
      ic.t.crtjinf <-c()
      ic.t.crtjsup <-c()
      ic.p.crtjinf <-c()
      ic.p.crtjsup <-c()
    
 
      for (i in 1:length(crtjalm))
        {
          calc.crtj <-cal.ic(get(crtjalm[i], envir = mientorno), liminf, limsup, CRTj[i,1])
          ccrtj.mean <- c(ccrtj.mean, calc.crtj[1])
          se.crtj <- c(se.crtj,calc.crtj[2])
          sesgo.crtj <- c(sesgo.crtj,calc.crtj[3])
          ic.t.crtjinf <- c(ic.t.crtjinf,calc.crtj[4])
          ic.t.crtjsup <- c(ic.t.crtjsup,calc.crtj[5])
          ic.p.crtjinf <- c(ic.p.crtjinf,calc.crtj[6])
          ic.p.crtjsup <- c(ic.p.crtjsup,calc.crtj[7])

          
          tiff(paste("Histogram of contribution to total variability of variable", i, ".tif", sep = ""), height = 500, width = 500)
          par(mfrow=c(1,2))
          hist(get(crtjalm[i], envir = mientorno),main="Histogram", xlab=paste("CRT of variable", i))
          abline(v=ccrtj.mean[i], lwd=2, col="blue")
          abline(v=CRTj[i,1], lty =2, lwd=2, col="red")
          qqnorm(get(crtjalm[i], envir = mientorno))
          dev.off()
        }
        
        
        calc.crtj <-array(cbind(CRTj[,1],ccrtj.mean, se.crtj, sesgo.crtj, ic.t.crtjinf, ic.t.crtjsup, ic.p.crtjinf, ic.p.crtjsup),
        dim=c(dim(CRTj)[1],8))
        calc.crtj <- as.data.frame(calc.crtj)
        colnames(calc.crtj) <- titulo
        
        rownames(calc.crtj) <- nombresvariables
        
      
      		cat("\n",file="temp.txt")					
			file.append("Resultsbootstrap.txt","temp.txt")	
			cat("Relative contribution to total variability of the column element j:\n",file="temp.txt")					
			file.append("Resultsbootstrap.txt","temp.txt")					
			write.table(round(calc.crtj, digits=2),file="temp.txt", sep="\t",dec=",")
			file.append("Resultsbootstrap.txt","temp.txt")
     }


### Relative contribution of element j to factor q
     
     	   if (ccrejfqVal=="1")
     {
			cat("\n",file="temp.txt")
			file.append("Resultsbootstrap.txt","temp.txt")
			cat("Relative contribution of the column element j to the q-th factor:",file="temp.txt")
			file.append("Resultsbootstrap.txt","temp.txt")



      for(i in 1: dim(CREjFqax)[1])
       {
        calc.crejfq <-c()
        crejfq.mean <-c()
        se.crejfq <-c()
        sesgo.crejfq <-c()
        ic.t.crejfqinf <-c()
        ic.t.crejfqsup <-c()
        ic.p.crejfqinf <-c()
        ic.p.crejfqsup <-c()



       for (j in  1:dim(CREjFqax)[2])
        {
        crejfqaux <- get(CREjFqax[i,j], envir = mientorno)
        calc.crejfq <-cal.ic(crejfqaux, liminf, limsup, CREjFq[i,j])
        crejfq.mean <- c(crejfq.mean, calc.crejfq[1])
        se.crejfq <- c(se.crejfq,calc.crejfq[2])
        sesgo.crejfq <- c(sesgo.crejfq,calc.crejfq[3])
        ic.t.crejfqinf <- c(ic.t.crejfqinf,calc.crejfq[4])
        ic.t.crejfqsup <- c(ic.t.crejfqsup,calc.crejfq[5])
        ic.p.crejfqinf <- c(ic.p.crejfqinf,calc.crejfq[6])
        ic.p.crejfqsup <- c(ic.p.crejfqsup,calc.crejfq[7])



        tiff(paste("Histogram of CREjFq of ", nombresvariables[i]," to axis ", j, ".tif", sep = ""), height = 500, width = 500)
        par(mfrow=c(1,2))
        hist(crejfqaux,main="Histogram", xlab=paste("CREjFq of ", nombresvariables[i],"\n to axis ", j))
        abline(v=crejfq.mean[j], lwd=2, col="blue")
        abline(v=CREjFq[i,j], lty =2, lwd=2, col="red")
        qqnorm(crejfqaux)
        dev.off()

        }

        calc.crejfq <-array(cbind(unlist(CREjFq[i,]), crejfq.mean, se.crejfq, sesgo.crejfq, ic.t.crejfqinf, ic.t.crejfqsup, ic.p.crejfqinf, ic.p.crejfqsup),
         dim=c(nejes,8))
        calc.crejfq <- as.data.frame(calc.crejfq)
        colnames(calc.crejfq) <- titulo

        rownames(calc.crejfq) <- ejes



		      cat("\n",file="temp.txt")
			file.append("Resultsbootstrap.txt","temp.txt")
			cat(nombresvariables[i],"\n", file="temp.txt")
			file.append("Resultsbootstrap.txt","temp.txt")
      		write.table(round(calc.crejfq, digits=2),file="temp.txt", sep="\t",dec=",")
			file.append("Resultsbootstrap.txt","temp.txt")

     }
    }

### Relative contribution of  factor q to element j
     
	   if (ccrfqejVal=="1")
     {
      		cat("\n",file="temp.txt")					
			file.append("Resultsbootstrap.txt","temp.txt")	
			cat("Relative contribution of the q-th factor to column element j:",file="temp.txt")					
			file.append("Resultsbootstrap.txt","temp.txt")	


      for(i in 1: dim(CRFqEjvar)[2])
       {
        calc.crfqej <-c()
        crfqej.mean <-c()
        se.crfqej <-c()
        sesgo.crfqej <-c()
        ic.t.crfqejinf <-c()
        ic.t.crfqejsup <-c()
        ic.p.crfqejinf <-c()
        ic.p.crfqejsup <-c()



       for (j in  1:dim(CRFqEjvar)[1])
        {
        crfqejaux <- get(CRFqEjvar[j,i], envir = mientorno)
        calc.crfqej <-cal.ic(crfqejaux, liminf, limsup, CRFqEj[j,i])
        crfqej.mean <- c(crfqej.mean, calc.crfqej[1])
        se.crfqej <- c(se.crfqej, calc.crfqej[2])
        sesgo.crfqej <- c(sesgo.crfqej, calc.crfqej[3])
        ic.t.crfqejinf <- c(ic.t.crfqejinf,calc.crfqej[4])
        ic.t.crfqejsup <- c(ic.t.crfqejsup,calc.crfqej[5])
        ic.p.crfqejinf <- c(ic.p.crfqejinf,calc.crfqej[6])
        ic.p.crfqejsup <- c(ic.p.crfqejsup,calc.crfqej[7])



        tiff(paste("Histogram of CRFqEj of axis ", i, " to variable " , nombresvariables[j], ".tif", sep = ""), height = 500, width = 500)
        par(mfrow=c(1,2))
        hist(crfqejaux,main="Histogram", xlab=paste("CRFqEj of axis", i, "\n to variable ", nombresvariables[j]))
        abline(v=crfqej.mean[j], lwd=2, col="blue")
        abline(v=CRFqEj[j,i], lty =2, lwd=2, col="red")
        qqnorm(crfqejaux)
        dev.off()

        }

        calc.crfqej <-array(cbind(unlist(CRFqEj[,i]), crfqej.mean, se.crfqej, sesgo.crfqej, ic.t.crfqejinf, ic.t.crfqejsup, ic.p.crfqejinf, ic.p.crfqejsup),
         dim=c(length(nombresvariables),8))
        calc.crfqej <- as.data.frame(calc.crfqej)
        colnames(calc.crfqej) <- titulo

        rownames(calc.crfqej) <- nombresvariables



		      cat("\n",file="temp.txt")
			file.append("Resultsbootstrap.txt","temp.txt")
		  	cat(ejes[i],"\n",file="temp.txt")
			file.append("Resultsbootstrap.txt","temp.txt")
      		write.table(round(calc.crfqej, digits=2),file="temp.txt", sep="\t",dec=",")
			file.append("Resultsbootstrap.txt","temp.txt")

     }				
			
     }


### Variable lengths
      
	# 	 if (clvVal=="1")
#     {
 #     calc.clv <-c()
 #     clv.mean <-c()
 #     se.clv <-c()
 #     sesgo.clv <-c()
 #     ic.t.clvinf <-c()
 #     ic.t.clvsup <-c()
 #     ic.p.clvinf <-c()
 #     ic.p.clvsup <-c()
    
           
      
 #     for (i in 1:length(longitudes))
 #       {
 #         calc.clv <-cal.ic(get(longitudes[i], envir = mientorno), liminf, limsup, longitudprin[i,1])
 #         clv.mean <- c(clv.mean, calc.clv[1])
 #         se.clv <- c(se.clv,calc.clv[2])
 #         sesgo.clv <- c(sesgo.clv,calc.clv[3])
 #         ic.t.clvinf <- c(ic.t.clvinf,calc.clv[4])
 #         ic.t.clvsup <- c(ic.t.clvsup,calc.clv[5])
 #         ic.p.clvinf <- c(ic.p.clvinf,calc.clv[6])
 #         ic.p.clvsup <- c(ic.p.clvsup,calc.clv[7])

          
          
 #         tiff(paste("Histogram of length of variable ", nombresvariables[i], ".tif", sep = ""), height = 500, width = 500)
 #         par(mfrow=c(1,2))
 #         hist(get(longitudes[i], envir = mientorno),main="Histogram", xlab=paste("Length of Variable ", nombresvariables[i]))
 #         abline(v=clv.mean[i], lwd=2, col="blue")
 #         abline(v=longitudprin[i,1], lty =2, lwd=2, col="red")
 #         qqnorm(get(longitudes[i], envir = mientorno))
 #         dev.off()
 #       }
     
     
 #       calc.clv <-array(cbind( longitudprin[,1],clv.mean, se.clv, sesgo.clv, ic.t.clvinf, ic.t.clvsup, ic.p.clvinf, ic.p.clvsup),
 #       dim=c(dim(longitudprin)[1],8))
 #       calc.clv <- as.data.frame(calc.clv)
 #       colnames(calc.clv) <- titulo
        
 #       rownames(calc.clv) <- nombresvariables
         
        
 #     		cat("\n",file="temp.txt")					
	#		file.append("Resultsbootstrap.txt","temp.txt")	
#			cat("Variable lengths:\n",file="temp.txt")					
#			file.append("Resultsbootstrap.txt","temp.txt")					
#			write.table(round(calc.clv, digits=2), file="temp.txt", sep="\t", dec=",")
#			file.append("Resultsbootstrap.txt","temp.txt")

 #    }
      
	   	file.show("Resultsbootstrap.txt")
			file.remove("temp.txt")

    }  
    numaxis <- tclVar( 1 )
    enumaxis <-tkentry(barvp,width="50",textvariable=numaxis)
    but.axis <-tkbutton(barvp,text="Choose",command=Onaxis, bg= "lightblue", width=10, foreground = "navyblue")
    tkpack(imgbar, expand="TRUE", fill="both")  
    
    tkpack(tklabel(barvp,text="Select the number of axes:"),
    enumaxis,but.axis,expand="FALSE", side= "left", fill ="both")
 
    tkfocus(barvp)
		}
		graphic.button <-tkbutton(framegraphic,text="    Graph    ",command=Graphics, bg= "lightblue", width=20, foreground = "navyblue")



		tkpack(graphic.button, expand="TRUE", side= "left", fill ="both")





  	
  	tkpack(framecol11,framecol12, side="left", expand = "TRUE", fill="both")
	tkpack(framename11,framename12, side="left", expand = "TRUE", fill="both")
	tkpack(framecex11,framecex12, side="left", expand = "TRUE", fill="both")
  	tkpack(frames11,frames12, side="left", expand = "TRUE", fill="both")
	
  	tkpack(framecol21,framecol22, side="left", expand = "TRUE", fill="both")
	tkpack(framename21,framename22, side="left", expand = "TRUE", fill="both")
	tkpack(framecex21,framecex22, side="left", expand = "TRUE", fill="both")
	tkpack(tklabel(frames2, text="Show axes"),cb, expand = "TRUE", side="left",expand="TRUE", fill = "both")
	#tkpack(frames21,frames22, side="left", expand = "TRUE", fill="both")
	
  
  	tkpack(frametext1,framet1,frameok1,framecol1,framename1,framecex1,frames1,expand = "TRUE", fill="both")
	tkpack(frametext2,framet2,frameok2,framecol2,framename2,framecex2,frames2,expand = "TRUE", fill="both")
	
	tkpack(framett1,framett2, expand = "TRUE",side="left", fill="both")
  	tkpack(framett,framegraphic,expand = "TRUE",side="top", fill="y")

}


	
OK.butinf <-tkbutton(framewigr,text="   OK   ",command=OnOKinf, bg= "lightblue", width=20, foreground = "navyblue")

tkpack(OK.butinf, expand="TRUE", side= "left", fill ="both")
tkpack(framewi21,framewi22, expand = "TRUE",side="left", fill="x")
tkpack(framewi1,framewi2, expand = "TRUE",side="top", fill="both")
tkpack(framewi,framewigr, expand = "TRUE",side="top", fill="y")
  
tkfocus(winfor)

}
