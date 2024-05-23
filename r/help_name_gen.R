
threeLetNames<- c(" Abb Ada Abe Aja	Ace	Ala	Acy	Ali	Ada	Ama	Add	Ami Alf	Amy Ali Ana	Amy Ann Ann Ara Ant 
Ari	Art	Ava	Asa Aya	Avi Bea Axl Bee	Bev Ben	Deb	Bob	Dee	Bud	Dot Cal	Eda Cam	Ela Cap	Ema	Cas	Ena	Che Era	
Con	Eva	Coy	Eve	Dan	Exa	Dax	Fae	Dee Fay	Del	Flo Doc Don	Gia Dow Ica Ean	Icy	Ebb Ida Edd Ila Edw Ilo Eli 
Ima Ell Imo	Ely	Ina Eva	Ira	Pho Hip Lip Ryu Gaz Nin Nan Ant Fay Isa	Pem Pin Lot Lil Bez Biz Bop Zip Zap 
Kho Kit Oop Thi Chu Cha Sha Xem Zam Zig Zag Fed Iva	Fed Foo Fig Gax Gab Gib Mim Mam Ilo Tio Tya Tye Yam 
Yum Pip Pap Pop Lap Fox	Ivy	Foy Iza Gay	Jan Gee Joe Geo Joi Gil Joy	Gus	Kai Guy Kay	Hal Kia	Ham Kim	Hoy 
Kya Lea Lee Ida Leo	Awl Bay Bud Boo Cam Cig Cog Coz Coy Def Del Din Dit Dol Dor Dos Ike Lia	Ira Imp Nut
Liv	Egg Eke Ewe Fab Far Eye Let Mag Mar Irl	Liz	Iva Lou Ivy Lue Jad Luz Jan Lyn Jax Mae Jay Mai	Jeb May	
Jed Meg Jep Mia Jim Mya Job Nan	Joe Nia	Jon Noa	Joy Nya Kai Oda Kay Ola Kem Oma Ken Ona Kim Ora	Kip Osa	 
Kit Ota Koa Ova Kye	Pam Lea	Pat	Lee Rae Lem Ray Len Roy Leo Sky Les Sue Lew Tai Lex Tea Lim Tia Lon Tom	
Lou	Ula	Loy	Una	Luc Ura Lue Val Lum Zoa	Lyn Zoe	Mac Mae Mal Mat Sec Sob Sou Spy Tav Tub Tui Sag Oxo Orc 
Orb Nod Mil Mew Mem Ilk Hep Hic Dog	Max	May	Mel Moe Nat Ned	Nim	Noe	Obe	Oda	Ola	Ole	Ora	Ott	Ova	Pat	Rae	
Ras	Ray	Red	Rex	Rey	Rob	Rod	Roe	Ron	Tet Tot Vim Woz Tux Tor Roy	Sal	Sam	Sid	Sie	Sim	Sol	Son	Tab	Tad	Taj	
Tal	Ted	Tex	Tim	Tod	Tom	Toy	Tre	Tye	Val	Van	Vic	Von	Wes	Yee	Zeb	Zed	Zev Yob Yap Wha Wen Wiz Toy Viz Bom
Lid Sal Mak Nik Tak Ryu San Mim Moo Maz Max Mel Shu Tib Tab Odi Pan Gob Gut Nak Niv Ziz Kod Jok Pil Pop
Mum Dad Boo Erm Wod Wib Web Wac Wul Jaz Hoo Xoo Zaw Tar Uno Oak Ash Vod Ilk Hot Zam Yar Par Pip Mop Map
Fud Fil Fri Sir Lad Gal Boy Bab Cla Nib Nub Ned Nec Nek Ken Kin Kis Kix Vix Vax Caz Cez Kez Old Yug Yig
Yop Yuk Boz Baz Biz Bix Bax Mix Lux Pox Pix Pie Pin Pup Red Rad Rod Ril Rup Rej Wip Wap Wil Win Wix Hid
Hil Hib Hen Hat")

threeLetNames<-str_extract_all(threeLetNames, "[A-Z][a-z][a-z]")

threeLetNames <- as.matrix(unlist(threeLetNames),nrow=1)
threeLetNames <- unique(threeLetNames)

threeLetNames <- tibble(threeLetNames)

alphaNumerics <- c(LETTERS, 1:9)

alphaNumSmall <- c(letters, 1:9)

alphaNumSmall <- alphaNumSmall[alphaNumSmall != "o"]
alphaNumSmall <- alphaNumSmall[alphaNumSmall != "l"]





agentNameGen <- function(listLen) {
  agentNames <- paste0(sprintf("%s",sample(threeLetNames$threeLetNames,listLen,replace = TRUE)),sprintf("%s",sample(threeLetNames$threeLetNames,listLen,replace = TRUE)),sprintf("%s",sample(threeLetNames$threeLetNames,listLen,replace = TRUE)))
  while(any(duplicated(agentNames))) {
    numOfDupes <- sum(duplicated(agentNames))
    agentNames[which(duplicated(agentNames))] <- 
      paste0(sprintf("%s",sample(threeLetNames$threeLetNames,numOfDupes,replace = TRUE)),sprintf("%s",sample(threeLetNames$threeLetNames,numOfDupes,replace = TRUE)),sprintf("%s",sample(threeLetNames$threeLetNames,numOfDupes,replace = TRUE)))
  }      
  return(agentNames) }

agentNameGen2 <- function(listLen) {
  agentNames <- paste0("00",sprintf("%s",sample(threeLetNames$threeLetNames,listLen,replace = TRUE)),sprintf("%s",sample(alphaNumerics,listLen,replace = TRUE)),sprintf("%s",sample(alphaNumerics,listLen,replace = TRUE)),sprintf("%s",sample(alphaNumerics,listLen,replace = TRUE)))
  while(any(duplicated(agentNames))) {
    numOfDupes <- sum(duplicated(agentNames))
    agentNames[which(duplicated(agentNames))] <- 
      paste0("00",sprintf("%s",sample(threeLetNames$threeLetNames,numOfDupes,replace = TRUE)),sprintf("%s",sample(alphaNumerics,numOfDupes,replace = TRUE)),sprintf("%s",sample(alphaNumerics,numOfDupes,replace = TRUE)),sprintf("%s",sample(alphaNumerics,numOfDupes,replace = TRUE)))
  }      
  return(agentNames) }

agentNameGenAlphaNum <- function(listLen) {
  agentNames <- paste0("0",sprintf("%s",sample(alphaNumSmall,listLen,replace = TRUE)),sprintf("%s",sample(alphaNumSmall,listLen,replace = TRUE)),sprintf("%s",sample(alphaNumSmall,listLen,replace = TRUE)),sprintf("%s",sample(alphaNumSmall,listLen,replace = TRUE)),sprintf("%s",sample(alphaNumSmall,listLen,replace = TRUE)))
  while(any(duplicated(agentNames))) {
    numOfDupes <- sum(duplicated(agentNames))
    agentNames[which(duplicated(agentNames))] <- 
      paste0("0",sprintf("%s",sample(alphaNumSmall,numOfDupes,replace = TRUE)),sprintf("%s",sample(alphaNumSmall,numOfDupes,replace = TRUE)),sprintf("%s",sample(alphaNumSmall,numOfDupes,replace = TRUE)),sprintf("%s",sample(alphaNumSmall,numOfDupes,replace = TRUE)),sprintf("%s",sample(alphaNumSmall,numOfDupes,replace = TRUE)))
  }      
  return(agentNames) }

agentNameGenSimple1 <- function(listLen) {
  agentNames <- paste0(sprintf("%s",sample(threeLetNames$threeLetNames,listLen,replace = TRUE)),sprintf("%s",sample(1:9,listLen,replace = TRUE)))
  while(any(duplicated(agentNames))) {
    numOfDupes <- sum(duplicated(agentNames))
    agentNames[which(duplicated(agentNames))] <- 
      paste0(sprintf("%s",sample(threeLetNames$threeLetNames,listLen,replace = TRUE)),sprintf("%s",sample(1:9,listLen,replace = TRUE)))
  }      
  return(agentNames) }

agentNameGenSimple2 <- function(listLen) {
  agentNames <- paste0(sprintf("%s",sample(threeLetNames$threeLetNames,listLen,replace = TRUE)),sprintf("%s",sample(1:9,listLen,replace = TRUE)),sprintf("%s",sample(1:9,listLen,replace = TRUE)),sprintf("%s",sample(1:9,listLen,replace = TRUE)))
  while(any(duplicated(agentNames))) {
    numOfDupes <- sum(duplicated(agentNames))
    agentNames[which(duplicated(agentNames))] <- 
      paste0(sprintf("%s",sample(threeLetNames$threeLetNames,listLen,replace = TRUE)),sprintf("%s",sample(1:9,listLen,replace = TRUE)),sprintf("%s",sample(1:9,listLen,replace = TRUE)),sprintf("%s",sample(1:9,listLen,replace = TRUE)))
  }      
  return(agentNames) }

vowels <- c("a","e","i","o","u","y")
constonants <- letters[! letters %in% vowels]

nameGenX <- function(listLen) {
  CVCV <- paste0(sprintf("%s",sample(constonants,listLen,replace = TRUE)),sprintf("%s",sample(vowels,listLen,replace = TRUE)),sprintf("%s",sample(constonants,listLen,replace = TRUE)),sprintf("%s",sample(vowels,listLen,replace = TRUE)),sprintf("%s",sample(0:9,listLen,replace = TRUE)),sprintf("%s",sample(0:9,listLen,replace = TRUE)),sprintf("%s",sample(0:9,listLen,replace = TRUE)))
  CVVC <- paste0(sprintf("%s",sample(constonants,listLen,replace = TRUE)),sprintf("%s",sample(vowels,listLen,replace = TRUE)),sprintf("%s",sample(vowels,listLen,replace = TRUE)),sprintf("%s",sample(constonants,listLen,replace = TRUE)),sprintf("%s",sample(0:9,listLen,replace = TRUE)),sprintf("%s",sample(0:9,listLen,replace = TRUE)),sprintf("%s",sample(0:9,listLen,replace = TRUE)))
  VCVC <- paste0(sprintf("%s",sample(vowels,listLen,replace = TRUE)),sprintf("%s",sample(constonants,listLen,replace = TRUE)),sprintf("%s",sample(vowels,listLen,replace = TRUE)),sprintf("%s",sample(constonants,listLen,replace = TRUE)),sprintf("%s",sample(0:9,listLen,replace = TRUE)),sprintf("%s",sample(0:9,listLen,replace = TRUE)),sprintf("%s",sample(0:9,listLen,replace = TRUE)))
  VCCV <- paste0(sprintf("%s",sample(vowels,listLen,replace = TRUE)),sprintf("%s",sample(constonants,listLen,replace = TRUE)),sprintf("%s",sample(constonants,listLen,replace = TRUE)),sprintf("%s",sample(vowels,listLen,replace = TRUE)),sprintf("%s",sample(0:9,listLen,replace = TRUE)),sprintf("%s",sample(0:9,listLen,replace = TRUE)),sprintf("%s",sample(0:9,listLen,replace = TRUE)))
  CVCC <- paste0(sprintf("%s",sample(constonants,listLen,replace = TRUE)),sprintf("%s",sample(vowels,listLen,replace = TRUE)),sprintf("%s",sample(constonants,listLen,replace = TRUE)),sprintf("%s",sample(constonants,listLen,replace = TRUE)),sprintf("%s",sample(0:9,listLen,replace = TRUE)),sprintf("%s",sample(0:9,listLen,replace = TRUE)),sprintf("%s",sample(0:9,listLen,replace = TRUE)))
  CCVC <- paste0(sprintf("%s",sample(constonants,listLen,replace = TRUE)),sprintf("%s",sample(constonants,listLen,replace = TRUE)),sprintf("%s",sample(vowels,listLen,replace = TRUE)),sprintf("%s",sample(constonants,listLen,replace = TRUE)),sprintf("%s",sample(0:9,listLen,replace = TRUE)),sprintf("%s",sample(0:9,listLen,replace = TRUE)),sprintf("%s",sample(0:9,listLen,replace = TRUE)))
  agentNames <- sample(c(CVCV,CVVC,VCVC,VCCV,CVCC,CCVC),listLen,replace = TRUE)
  while(any(duplicated(agentNames))) {
    numOfDupes <- sum(duplicated(agentNames))
    agentNames[which(duplicated(agentNames))] <- 
      sample(c(CVCV,CVVC,VCVC,VCCV,CVCC,CCVC),listLen,replace = TRUE)
  }      
  return(agentNames) } 


