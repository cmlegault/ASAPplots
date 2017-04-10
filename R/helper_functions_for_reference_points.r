#--------Reference Point helper functions-------

#-------Spawners per recruit -----------------------------
s.per.recr<-function(nages,fec.age,mat.age,M.age, F.mult, sel.age, spawn.time ) {
  
  spr=0.0
  cum.survive=1.0
  z=0.0
  for (i in 1:(nages-1)  ) {
    z=M.age[i] + F.mult*sel.age[i]
    z.ts=(M.age[i]+F.mult*sel.age[i])*spawn.time
    spr=spr+cum.survive*fec.age[i]*mat.age[i]*exp(-z.ts)
    cum.survive=cum.survive*exp(-z )
    
  }
  
  z= M.age[nages] + F.mult*sel.age[nages]
  z.ts=(M.age[nages]+F.mult*sel.age[nages])*spawn.time
  spr=spr + fec.age[nages]*mat.age[nages]*cum.survive*exp(-z.ts)/( 1- exp(-z ) )
  
  return(spr)
  
}

#-------Yield per recruit -----------------------------
ypr<-function(nages, wgt.age, M.age, F.mult, sel.age ) {
  
  yield=0.0
  cum.survive=1.0
  z=0.0
  
  for (i in 1:(nages-1)  ) {
    z=M.age[i] + F.mult*sel.age[i]
    yield=yield + wgt.age[i]*F.mult*sel.age[i]*(1-exp(-z) )*cum.survive/z
    cum.survive=cum.survive*exp(-z)
  }
  
  z= M.age[nages] + F.mult*sel.age[nages]
  yield=yield + wgt.age[nages]*F.mult*sel.age[nages]*cum.survive/z
  
  return(yield)
  
}

#-------Equilibrium SSB ------------------------
ssb.eq<-function(recr.par, R0.BH, spr, spr0, is.steepness=T ) {
  
  if (is.steepness==T)  alpha.BH <- 4*recr.par/(1-recr.par)
  if (is.steepness==F)  alpha.BH <- recr.par
  sprF=spr/spr0
  
  ssb=spr0*R0.BH*(sprF*alpha.BH - 1.0)/(alpha.BH-1.0)
  
  
  return(ssb)
  
}
#------Beverton-Holt alpha-----------------------  
bev.holt.alpha<-function(S,R0,recr.par,spr0, is.steepness=T){
  
  if (is.steepness==T)  alpha.BH <- 4*recr.par/(1-recr.par)
  if (is.steepness==F)  alpha.BH <- recr.par
  
  
  y=rep(0,length(S))
  y=R0*S*alpha.BH/(R0*spr0 +(alpha.BH-1.0)*S)
  return(y)
  
}  
