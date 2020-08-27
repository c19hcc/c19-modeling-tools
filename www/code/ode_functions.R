##################################################
## Project: C19HCC COVID-19 Modeling Dashboard and Tools
## Purpose: ODE Functions for SEIR model
## Date: June 2020
## Developers: Allison Hill, Brendan Abraham, Kristin Fitzgerald, Kyle Furlong, Dr. Chris Glazner, Eric Neumann
## Copyright 2020, The MITRE Corporation
## Approved for Public Release; Distribution Unlimited. Case Number 20-1521.
##################################################

# Primarily developed by Alison Hill (Harvard University) with parameter tuning and additional analysis by Eric Neumann

#set ODEs
library(dplyr)
library(tidyr)
library(deSolve)
library(reshape2)
m_slope = 4.0
#m_slope = 20
eps = 0.000001

Set_ODEs_SEIR=function(t,y,p){
  S = y[1]
  E = y[2]
  I0 = y[3]
  I1 = y[4]
  I2 = y[5]
  I3 = y[6]
  R = y[7]
  D = y[8]
  
  #  b1_v = y[9]
  #  if(b1_v < b1) {
  #    s = slope_b1
  #  }else if (b1_v > b1) {
  #    s = -slope_b1
  #  }else {
  #    s=0
  #  }
  #  
  #  d_b1= s 
  
  with(as.list(p),{
    
    #    dS.dt = -(b1*I1+b2*I2+b3*I3)*S
    infx = (b0*I0+b1*I1+b2*I2+b3*I3)*S
    dS.dt = -infx
    
    #    dE.dt=(b1*I1+b2*I2+b3*I3)*S-a*E
    dE.dt=infx-(a+a0)*E
    
    dI0.dt=a0*E-g0*I0
    
    dI1.dt=a*E-g1*I1-p1*I1
    
    #### old    dI2.dt=p1*I1-g2*I2-p2*I2
    # from Alison: p2(hc) = p20 + (p2max-p20)/(1+(hc/I2)^m) 
    
    # here for now...
    FracProgressNoCare = 50/100. # % of symptomatic infections that would progress to critical without any hospital care
    FracDieNoCare=50/100. # % of critical infections that would die without ICU care (can't be exactly 100%)
    
    #p2max=10*p2
    p2max=FracProgressNoCare*g2/(1-FracProgressNoCare)
    
    #umax=10*u
    umax=FracDieNoCare*g3/(1-FracDieNoCare)
    
    ##    hc_z = (abs(1-(I2/hc)^m) + eps)
    p2_hc = p2 + (p2max-p2)/(1+(hc/(I2+eps))^m_slope)
    ##    dI2.dt=p1*I1-g2*I2-p2*I2/hc_z # Hospital capacity term: /(hc-I2)   /(abs(hc-I2) + eps)   eps = 0.0001
    #dI2.dt=p1*I1-g2*I2-p2_hc*I2 # Hospital capacity term: /(hc-I2)   /(abs(hc-I2) + eps)   eps = 0.0001
    dI2.dt=p1*I1-g2*I2 - progress_fun(I2, p2, p2max, hc) # Hospital capacity term: /(hc-I2)   /(abs(hc-I2) + eps)   eps = 0.0001
    
    #umax=10*u
    #### old    dI3.dt=p2*I2-g3*I3-u*I3
    
    #vc_z = 1./(abs(1-(I3/vc)^m_slope) + eps) steep cutoff
    vc_z = u + (umax - u)/(1+(vc/(I3+eps))^m_slope)   
    #dI3.dt= progress_fun(I2, p2, p2max, hc) - g3*I3 - u*I3*vc_z # ventilator capacity term: /(vc-I3)   /(abs(vc-I3) + eps)
    dI3.dt= progress_fun(I2, p2, p2max, hc) - g3*I3 - progress_fun(I3, u, umax, vc) # ventilator capacity term: /(vc-I3)   /(abs(vc-I3) + eps)
    # progress_fun(I3, u, umax, vc)
    
    ##    dR.dt=g1*I1+g2*I2+g3*I3
    dR.dt=g0*I0+g1*I1+g2*I2+g3*I3
    
    ##    dD.dt=u*I3
    # dD.dt=u*I3*vc_z
    dD.dt= progress_fun(I3, u, umax, vc)
    
    return(list(c(dS.dt, dE.dt, dI0.dt, dI1.dt, dI2.dt, dI3.dt, dR.dt, dD.dt)))
    ##    return(list(c(dS.dt, dE.dt, dI1.dt, dI2.dt, dI3.dt, dR.dt, dD.dt)))
  })
}

# ----------------------------------------------------------------------------
# progress_fun function:
# -----------------------
# Describes the total progression rate from severe to critical disease (p2(I2)*I2) with capacity. Individuals below capacity threshold are assumed to receive standard of care and progress at rate p2, while individuals above capacity threshold are assumed to not receive standard of care and progress at rate p2max
# INPUT: I2 - current # of ppl with severe infection
#        p2 - progression rate for individuals receiving care
#        p2max - progression rate for individuals not receiving care (e.g. when capacity reached)
#        hcap - hospital capacity
# OUTPUT: effective total rate of progression, replacing p2*I2

progress_fun=function(I,p,pmax,cap){
  #progress=p2*I2/(1+(I2/hcap)^m)+(p2*hcap+p2max*(I2-hcap))*(1-1/(1+(I2/hcap)^m)) #continuous
  #progress = p*I + I*(pmax-p)/(1+(cap/(I+eps))^m_slope)
  progress=ifelse(I<cap,p*I,p*cap+pmax*(I-cap)) #discrete
  return(progress)
}




# ----------------------------------------------------------------------------
# GetSpread_SEIR function:
# --------------------
GetSpread_SEIR = function(pModel,N,Tmax,y0){
  
  #m_slope = 4.0
  # eps = 0.000001
  t = seq(from=0, to=Tmax, by=1)
  # We solve the ODE's for the model in the presence and absence of disclosure 
  # (out and out0, respectively)
  out = ode(y=y0, times=t, func=Set_ODEs_SEIR, parms=pModel)
  #diagnostics(out)
  #print(head(out))
  
  df = as.data.frame(out)
  
  #KRF: these aren't spit out anywhere so do we even need them?
  #p2max=4*as.numeric(pModel["p2"])
  #p2_0=as.numeric(pModel["p2"])
  ##p2_hc_mx = max(p2_0 + (p2max-p2_0)/(1+(pModel["hc"]/(out[,5]+eps))^m_slope))
  #p2_hc_mx = max(as.numeric(pModel["hc"])/(out[,5]+eps)^m_slope)
  #vc_z_mx = max(1./(abs(1-(out[,6]/pModel["vc"])^m_slope) + eps))
  
  return(df)
}


# ----------------------------------------------------------------------------
# GetParams_SEIR function:
# --------------------

GetParams_SEIR = function(pClin){
  
  with(as.list(pClin),{
    
    a=1/IncubPeriod
    a0=a*FracAsympto/100
    a=a*(100-FracAsympto)/100 # normalization of pop rates for asympto distribution
    
    g1=(1/DurMildInf)*FracMild
    p1=(1/DurMildInf)-g1
    
    p2=(1/DurHosp)*(FracCritical/(FracSevere+FracCritical))
    g2=(1/DurHosp)-p2
    
    if(FracCritical==0){
      u=0
    }else{
      u=(1/TimeICUDeath)*(CFR/FracCritical)
      ##u=(1/TimeICUDeath)*(effCFR/FracCritical)
    }
    
    g3=(1/TimeICUDeath)-u
    
    # quick add..
    #a0=a*.2 # 1/5 of pop get it
    #b0=b1 # same contagiousness as mild
    g0=g1  # same recovery as mild
    
    return(c(a=a,a0=a0,g0=g0,g1=g1,g2=g2,g3=g3,p1=p1,p2=p2,u=u)) })
  ##    return(c(a=a,g1=g1,g2=g2,g3=g3,p1=p1,p2=p2,u=u,hc=hc,ic=ic,vc=vc))  })
}

SetHospCapacitySliders=function(input){

  HospBedper = input$HospBedper
  HospBedOcc = input$HospBedOcc
  
  AvailHospBeds=HospBedper*((100-HospBedOcc)/100) #Available hospital beds per 1000 ppl in US based on total beds and occupancy
  AvailICUBeds=input$ICUBedper*((100-input$ICUBedOcc)/100) #Available ICU beds per 1000 ppl in US, based on total beds and occupancy. Only counts adult not neonatal/pediatric beds
  ConvVentCap=input$ConvMVCap #Estimated excess # of patients who could be ventilated in US (per 1000 ppl) using conventional protocols
  ContVentCap=input$ContMVCap #Estimated excess # of patients who could be ventilated in US (per 1000 ppl) using contingency protocols
  CrisisVentCap=input$CrisisMVCap #Estimated excess # of patients who could be ventilated in US (per 1000 ppl) using crisis protocols
  
  capParams=c("AvailHospBeds"=AvailHospBeds,"AvailICUBeds"=AvailICUBeds,"ConvVentCap"=ConvVentCap,"ContVentCap"=ContVentCap,"CrisisVentCap"=CrisisVentCap)
  
  return(capParams)
}

GetModelParams = function(input, hdata){
  
  IncubPeriod=input$IncubPeriod  #Incubation period, days
  FracAsympto=input$FracAsympto # % of asymptomatic patients
  AsymptoCrossSect=input$AsymptoCrossSect # asymptomatic transmission scaling
  DurMildInf=input$DurMildInf #Duration of mild infections, days
  FracSevere=input$FracSevere/100 #Fraction of infections that are severe
  FracCritical=input$FracCritical/100 #Fraction of infections that are critical
  FracMild=1-FracSevere-FracCritical  #Fraction of infections that are mild
  ProbDeath=input$ProbDeath  #Probability of dying given critical infection
  CFR=ProbDeath*FracCritical/100 #Case fatality rate (fraction of infections resulting in death)
  TimeICUDeath=input$TimeICUDeath #Time from ICU admission to death, days
  DurHosp=input$DurHosp #Duration of hospitalization, days
  
  pClin=c(IncubPeriod=IncubPeriod, FracAsympto=FracAsympto, DurMildInf=DurMildInf,FracMild=FracMild, FracSevere=FracSevere,FracCritical=FracCritical,CFR=CFR,TimeICUDeath=TimeICUDeath,DurHosp=DurHosp) # capParams
  
  pModel=GetParams_SEIR(pClin)
  
  N=input$population.size
  
  #print(paste("input$b1 is:", input$b1))
  
  b1=input$b1/N  ## necessary scaling when using abs pops for S,E,I1...
  
  #print(paste("pmodel b1 is:", b1))
  
  #EN self-quarantineing of (self or doctor)-diagnosed clinically sick should reduce b1 but not b0!!!!
  b0=b1 * AsymptoCrossSect # same contagiousness for asymptomatics as for mild... possible difference in "avoidance" dynamic: b10
  ## need b10 slider
  b2=input$b21*b1
  b3=input$b31*b1
  b=c(b1,b2,b3)
  #print (c("c(b1,b2,b3)", b))
  
  ##  effective CFR = [Untreated Fatality Rate] + ([Treated Fatality Rate]-[Untreated Fatality Rate])/(1+[Hospital Strain])
  ## [Untreated Fatality Rate] (1 - 1/(1+[Hospital Strain])) + [Treated Fatality Rate]/(1+[Hospital Strain])
  ## a_hosp_strain = 1/(1+[Hospital Strain])
  ## (1-a)*unCFR + a*tCFR, a = 1/(1+[Hospital Strain])
  
  ##eff_p2 = (1 - a_hosp_strain)*p2 + a_hosp_strain*p2  ##  AvailHospBeds or hdata$HospBedOcc
  ##eff_CFR = (1 - a_icu_strain)*untrt_CFR + a_icu_strain*trt_CFR  ## AvailICUBeds or hdata$ICUBedOcc
  
  if(!is.null(input$HospBedper)) {
    ## EN added: new params for ODE...
    AvailHospBeds=input$HospBedper*((100-input$HospBedOcc)/100) #Available hospital beds per 1000 ppl in US based on total beds and occupancy
    AvailICUBeds=input$ICUBedper*((100-input$ICUBedOcc)/100) #Available ICU beds per 1000 ppl in US, based on total beds and occupancy. Only counts adult not neonatal/pediatric beds
    ConvVentCap=input$ConvMVCap #Estimated excess # of patients who could be ventilated in US (per 1000 ppl) using conventional protocols
    ContVentCap=input$ContMVCap #Estimated excess # of patients who could be ventilated in US (per 1000 ppl) using contingency protocols
    CrisisVentCap=input$CrisisMVCap #Estimated excess # of patients who could be ventilated in US (per 1000 ppl) using crisis protocols
    
    capParams=c("AvailHospBeds"=AvailHospBeds,"AvailICUBeds"=AvailICUBeds,"ConvVentCap"=ConvVentCap,"ContVentCap"=ContVentCap,"CrisisVentCap"=CrisisVentCap)
    #print(capParams)
    
    ## EN capacity code
    hc = AvailHospBeds*N/1000
    ic = AvailICUBeds*N/1000
    vc = (ConvVentCap+ContVentCap+CrisisVentCap)*N/1000
    vc = min(vc, ic) # basic limit equation for ICU
    #    vc = (as.numeric(capParams["ConvVentCap"])+as.numeric(capParams["ContVentCap"])+as.numeric(capParams["CrisisVentCap"]))*N/1000
  } else {
    #print("is.null(AvailHospBeds)... hdata$HospBedper")
    #print(hdata$HospBedper)
    AvailHospBeds=hdata$HospBedper*((100-hdata$HospBedOcc)/100) #Available hospital beds per 1000 ppl in US based on total beds and occupancy
    AvailICUBeds=hdata$ICUBedper*((100-hdata$ICUBedOcc)/100) #Available ICU beds per 1000 ppl in US, based on total beds and occupancy. Only counts adult not neonatal/pediatric beds
    ConvVentCap=hdata$ConvMVCap #Estimated excess # of patients who could be ventilated in US (per 1000 ppl) using conventional protocols
    ContVentCap=hdata$ContMVCap #Estimated excess # of patients who could be ventilated in US (per 1000 ppl) using contingency protocols
    CrisisVentCap=hdata$CrisisMVCap #Estimated excess # of patients who could be ventilated in US (per 1000 ppl) using crisis protocols
    
    #hc = 2.8 *1000
    #ic = 0.259 *1000
    #vc = (0.0619+0.155+0.418) *1000
    hc = AvailHospBeds*N/1000
    ic = AvailICUBeds*N/1000
    vc = (ConvVentCap+ContVentCap+CrisisVentCap)*N/1000
    vc = min(vc, ic) # basic limit equation for ICU
    #print("capParams=SetHospCapacitySliders")
    #vc = (as.numeric(capParams["ConvVentCap"])+as.numeric(capParams["ContVentCap"])+as.numeric(capParams["CrisisVentCap"]))*N/1000
  }
  pModel=c(b0=b0,b=b,pModel,hc=hc,ic=ic,vc=vc)
  #print (c("GetModelParams pModel", pModel))
  
  return(list("N"=N,"pModel"=pModel))
  
}

GetRo_SEIR = function(p,N){
  
  with(as.list(p),{
    
    Ro=N*((b0/g0)+(b1/(p1+g1))+(p1/(p1+g1))*(b2/(p2+g2)+ (p2/(p2+g2))*(b3/(u+g3))))
    #Ro=N*((b1/(p1+g1))+(p1/(p1+g1))*(b2/(p2+g2)+ (p2/(p2+g2))*(b3/(u+g3))))
    
    return(Ro)
  })
  
}


Getr_SEIR = function(out,t1,t2,V){
  
  outV=out[out$variable==V,]
  
  value1=outV$value[which.min(abs(t1-outV$time))]
  value2=outV$value[which.min(abs(t2-outV$time))]
  
  r=(log(value2)-log(value1))/(t2-t1)
  
  DoublingTime=log(2)/r
  
  return(list("r"=r,"DoublingTime"=DoublingTime))
  
}

run_model <- function(input, hops.cap.dat, intervention.df, graph.start.date=NULL, beta1.override=FALSE, interv_y0=NULL){
  hdata <- hops.cap.dat
  ParamStruct=GetModelParams(input, hdata=hdata)
  pModel=ParamStruct$pModel
  N=ParamStruct$N
  Tmax=input$Tmax
  
  # Set initial conditions and time interval
  
  # METHOD1=TRUE is the original way of setting the model; 
  # METHOD1=FALSE is the random way of initializing based on first case date
  method1 <- FALSE
  if (method1){
    I2.val <- 0
    I1.val <- 0
    E0 <- input$initial.infected 
    S0 <- N - E0 - I1.val
  }else{
    I2.val = input$initial.infected
    I1.val = I2.val * 10 #this is completely arbitrary
    E0= I1.val * 5 * input$IncubPeriod
    S0 = N - E0 - I2.val - I1.val
  }
  
  y0 = c(S=S0, E=E0, I0=0, I1=I1.val, I2=I2.val, I3=0, R=0, D=0)
  #    y0 = c(S=S0, E=E0, I1=0, I2=0, I3=0, R=0, D=0)
  
  #get Ro value
  #KF Commented out for now
  
  #Ro=GetRo_SEIR(pModel,N)
  
  out.df=GetSpread_SEIR(pModel,N,Tmax,y0) %>%
    mutate(b1=as.numeric(pModel['b1'])*N)
  out=melt(out.df,id="time")
  
  #get r value
  V="E" #variable to calculate r for
  tpeak=out.df[which.max(select(out.df,"time",V)[,2]),"time"];
  
  t2=tpeak/4
  t1=tpeak/8
  r.out=Getr_SEIR(out,t1,t2,V)
  r=r.out$r
  DoublingTime=r.out$DoublingTime
  
  # INTERVENTION
  # intervention parameters ....now a for loop for multiple interventions
  full.outInt.df <- data.frame()
  blue.interv.list <- list()
  Tend <- 0
  
  intervention.df[nrow(intervention.df), "end_time"] <- ifelse(intervention.df[nrow(intervention.df), "end_time"] < Tmax, Tmax, intervention.df[nrow(intervention.df), "end_time"])
  
  if (nrow(intervention.df)>0 & ncol(intervention.df)>1){
    for (i in 1:nrow(intervention.df)){
      
      pModelInt=pModel
      
      if (beta1.override){
        if (str_detect(string = intervention.df$beta.step[i], pattern = "proj")){
          pModelInt["b1"] = intervention.df[i, "b1"]/N *(1-intervention.df[i, "Prop_Reduction_Mild_Trans"])
        }else{
          pModelInt["b1"] = intervention.df[i, "b1"]/N
        }
      }else{
        pModelInt["b1"]=pModelInt["b1"]*(1-intervention.df[i, "Prop_Reduction_Mild_Trans"])
      }
      
      
      pModelInt["b2"]=pModelInt["b2"]*(1-intervention.df[i, "Prop_Reduction_Severe_Trans"])
      pModelInt["b3"]=pModelInt["b3"]*(1-intervention.df[i, "Prop_Reduction_Death"])
      pModelInt$b0 = pModelInt$b1 * input$AsymptoCrossSect
      
      RoInt=GetRo_SEIR(pModelInt,N)
      
      # start time of intervention
      
      Tint=as.numeric(intervention.df[i, "start_time"])
      Tend=as.numeric(intervention.df[i, "end_time"])
      
      #print(paste0('b1: ', (as.numeric(pModelInt['b1']) *  N), ' | start_time: ', Tint, ' | end_time: ', Tend))
      
      if (i==1){
        # if initial condition vector interv_y0 passed, use that to start. Otherwise,
        # generate initial conditions and time interval from last date of baseline period
        if(!is.null(interv_y0)){
          y0 = interv_y0
        }else{
          # Set initial conditions and time interval
          iInt=which.min(abs(Tint-out$time)) # find nearest time to Tint
          
          S0 = out.df[iInt,"S"]
          E0 = out.df[iInt,"E"]
          I00 = out.df[iInt,"I0"]
          I10 = out.df[iInt,"I1"]
          I20 = out.df[iInt,"I2"]
          I30 = out.df[iInt,"I3"]
          D0 = out.df[iInt,"D"]
          R0 = out.df[iInt,"R"]
          
          y0 = c(S=S0, E=E0, I0=I00, I1=I10, I2=I20, I3=I30, R=R0, D=D0)
        }
      }else{
        #eg another intervention - add to the end
        iInt <- nrow(full.outInt.df)
        S0 = full.outInt.df[iInt,"S"]
        E0 = full.outInt.df[iInt,"E"]
        I00 = full.outInt.df[iInt,"I0"]
        I10 = full.outInt.df[iInt,"I1"]
        I20 = full.outInt.df[iInt,"I2"]
        I30 = full.outInt.df[iInt,"I3"]
        D0 = full.outInt.df[iInt,"D"]
        R0 = full.outInt.df[iInt,"R"]
        
        y0 = c(S=S0, E=E0, I0=I00, I1=I10, I2=I20, I3=I30, R=R0, D=D0)
       
#        y0 = c(S=S0, E=E0, I0=I00, I1=I10, I2=I20, I3=I30, R=R0, D=D0)
        # don't think this solution worked as intended. We want to start the intervention on the last day of the baseline period, 
        # not the first day of the model.
      }
      
      
      
      
      #Run intervention time course until Tend. Up to time Tint, use baseline solution
      Trun=Tend-Tint
      
      outInt.df=GetSpread_SEIR(pModelInt,N,Trun,y0) %>%
        mutate(time = time+Tint,
               b1 = as.numeric(as.numeric(pModelInt['b1']))*N)
      # outInt.df$time=outInt.df$time+Tint
      
      #print(outInt.df)
      
      if (i != nrow(intervention.df)){
        # eg not at end of all active interventions
        if (intervention.df[i, "end_time"] == intervention.df[i+1, "start_time"]){
          # go immediately into next intervention; no need to run with baseline
          full.outInt.df <- rbind.data.frame(full.outInt.df, outInt.df) %>%
            distinct()
        }else{
          # temp return to baseline
          Trun2=intervention.df[i+1, "start_time"]-Tend
          
          #Set initial conditions and time interval
          #Round all numbers to lowest integer, so if less than 1, go to zero
          iEnd=nrow(outInt.df)
          S0 = outInt.df[iEnd,"S"]
          E0 = outInt.df[iEnd,"E"]
          I00 = outInt.df[iEnd,"I0"]
          I10 = outInt.df[iEnd,"I1"]
          I20 = outInt.df[iEnd,"I2"]
          I30 = outInt.df[iEnd,"I3"]
          D0 = outInt.df[iEnd,"D"]
          R0 = outInt.df[iEnd,"R"]
          
          y0 = c(S=S0, E=E0, I0=I00, I1=I10, I2=I20, I3=I30, R=R0, D=D0)
          # y0 = c(S=S0, E=E0, I1=I10, I2=I20, I3=I30, R=R0, D=D0)
          
          #run with parameters back to baseline
          outIntOff.df=GetSpread_SEIR(pModel,N,Trun2,y0)
          outIntOff.df$time=outIntOff.df$time+Tend
          outIntOff.df$b1 = as.numeric(pModel['b1'])*N
          
          #combine vectors
          outInt.df=rbind(outInt.df,outIntOff.df)
          
          full.outInt.df <- rbind.data.frame(full.outInt.df, outInt.df) %>%
            distinct()
          #continue on to next intervention
        }
        
      }else{
        full.outInt.df <- rbind.data.frame(full.outInt.df, outInt.df) %>%
          distinct()
      }
    }
    #after interventions - check to see if at end of simulation time
    #Tend holds on to last intervention's end date from within for-loop
    Trun2=Tmax-Tend
    
    if(Trun2<=0){
    }else{
      #Set initial conditions and time interval
      #Round all numbers to lowest intergar, so if less than 1, go to zero
      iEnd=nrow(full.outInt.df)
      S0 = full.outInt.df[iEnd,"S"]
      E0 = full.outInt.df[iEnd,"E"]
      I00 = full.outInt.df[iEnd,"I0"]
      I10 = full.outInt.df[iEnd,"I1"]
      I20 = full.outInt.df[iEnd,"I2"]
      I30 = full.outInt.df[iEnd,"I3"]
      D0 = full.outInt.df[iEnd,"D"]
      R0 = full.outInt.df[iEnd,"R"]
      
      y0 = c(S=S0, E=E0, I0=I00, I1=I10, I2=I20, I3=I30, R=R0, D=D0)
      # y0 = c(S=S0, E=E0, I1=I10, I2=I20, I3=I30, R=R0, D=D0)
      
      #run with parameters back to baseline
      outIntOff.df=GetSpread_SEIR(pModel,N,Trun2,y0)
      outIntOff.df$time=outIntOff.df$time+Tend
      outIntOff.df$b1 = as.numeric(pModel['b1'])*N
      
      #combine vectors
      full.outInt.df=rbind(full.outInt.df,outIntOff.df)
    }
  }
  return(list('Baseline'=out.df, 'Intervention'=full.outInt.df))
}


get_state_hosp_data <- function(state_name, hdata=NULL){
  if(is.null(hdata)){
    hdata = get_hosp_data()
  }
  if(nchar(state_name) > 2){
    state_name = state_lookup(state_name)
  }
  resources = get_state_resource_stats(HOSPITALS, state_name)
  for(resource in names(resources)){
    hdata[[resource]] = resources[[resource]]
  }
  return(hdata)
}


draw_intervention_plot <- function(input, hops.cap.dat, intervention.df, VarShowCap = c('Deaths'), mod_results=NULL, graph.start.date=NULL, interv.real.overlay=FALSE, plotCounty=F){
  hdata <- hops.cap.dat
  ParamStruct=GetModelParams(input, hdata=hdata)
  pModel=ParamStruct$pModel
  N=ParamStruct$N
  Tmax=input$Tmax
  
  if(!('interv.per.capita' %in% names(input))) input$interv.per.capita=F
  
  if(is.null(graph.start.date)){
    graph.start.date = input$start_date
  }
  
  # if no model results, run model with input params. Otherwise, use pre-existing results.
  if(is.null(mod_results)){
    # Set initial conditions and time interval
    
    # METHOD1=TRUE is the original way of setting the model; 
    # METHOD1=FALSE is the random way of initializing based on first case date
    method1 <- FALSE
    if (method1){
      I2.val <- 0
      I1.val <- 0
      E0 <- input$initial.infected 
      S0 <- N - E0 - I1.val
    }else{
      I2.val = input$initial.infected
      I1.val = I2.val * 10 #this is completely arbitrary
      E0= I1.val * 5 * input$IncubPeriod
      S0 = N - E0 - I2.val - I1.val
    }
    
    y0 = c(S=S0, E=E0, I0=0, I1=I1.val, I2=I2.val, I3=0, R=0, D=0)
    #    y0 = c(S=S0, E=E0, I1=0, I2=0, I3=0, R=0, D=0)
    
    #get Ro value
    #KF Commented out for now
    
    #Ro=GetRo_SEIR(pModel,N)
    
    out.df=GetSpread_SEIR(pModel,N,Tmax,y0)
    out=melt(out.df,id="time")
    
    #get r value
    V="E" #variable to calculate r for
    tpeak=out.df[which.max(select(out.df,"time",V)[,2]),"time"];
    
    t2=tpeak/4
    t1=tpeak/8
    r.out=Getr_SEIR(out,t1,t2,V)
    r=r.out$r
    DoublingTime=r.out$DoublingTime
    
    # INTERVENTION
    # intervention parameters ....now a for loop for multiple interventions
    full.outInt.df <- data.frame()
    blue.interv.list <- list()
    Tend <- 0
    if (nrow(intervention.df)>0 & ncol(intervention.df)>1){
      for (i in 1:nrow(intervention.df)){
        
        pModelInt=pModel
        pModelInt["b1"]=pModelInt["b1"]*(1-intervention.df[i, "Prop_Reduction_Mild_Trans"])
        pModelInt["b2"]=pModelInt["b2"]*(1-intervention.df[i, "Prop_Reduction_Severe_Trans"])
        pModelInt["b3"]=pModelInt["b3"]*(1-intervention.df[i, "Prop_Reduction_Death"])
        pModelInt$b0 = pModelInt["b1"] * input$AsymptoCrossSect
        
        RoInt=GetRo_SEIR(pModelInt,N)
        
        # start time of intervention
        
        Tint=as.numeric(intervention.df[i, "start_time"])
        Tend=as.numeric(intervention.df[i, "end_time"])
        
        if (i==1){
          iInt=which.min(abs(Tint-out$time)) # find nearest time to Tint
          
          # Set initial conditions and time interval
          S0 = out.df[iInt,"S"]
          E0 = out.df[iInt,"E"]
          I00 = out.df[iInt,"I0"]
          I10 = out.df[iInt,"I1"]
          I20 = out.df[iInt,"I2"]
          I30 = out.df[iInt,"I3"]
          D0 = out.df[iInt,"D"]
          R0 = out.df[iInt,"R"]
          
        }else{
          #eg another intervention - add to the end
          iInt <- nrow(full.outInt.df)
          S0 = full.outInt.df[iInt,"S"]
          E0 = full.outInt.df[iInt,"E"]
          I00 = full.outInt.df[iInt,"I0"]
          I10 = full.outInt.df[iInt,"I1"]
          I20 = full.outInt.df[iInt,"I2"]
          I30 = full.outInt.df[iInt,"I3"]
          D0 = full.outInt.df[iInt,"D"]
          R0 = full.outInt.df[iInt,"R"]
          
        }
        
        #    y0 = c(S=S0, E=E0, I1=I10, I2=I20, I3=I30, R=R0, D=D0)
        y0 = c(S=S0, E=E0, I0=I00, I1=I10, I2=I20, I3=I30, R=R0, D=D0)
        
        #Run intervention time course until Tend. Up to time Tint, use baseline solution
        Trun=Tend-Tint
        
        outInt.df=GetSpread_SEIR(pModelInt,N,Trun,y0)
        outInt.df$time=outInt.df$time+Tint
        
        if (i != nrow(intervention.df)){
          # eg not at end of all active interventions
          if (intervention.df[i, "end_time"] == intervention.df[i+1, "start_time"]){
            # go immediately into next intervention; no need to run with baseline
            full.outInt.df <- rbind.data.frame(full.outInt.df, outInt.df)
          }else{
            # temp return to baseline
            Trun2=intervention.df[i+1, "start_time"]-Tend
            
            #Set initial conditions and time interval
            #Round all numbers to lowest integer, so if less than 1, go to zero
            iEnd=nrow(outInt.df)
            S0 = outInt.df[iEnd,"S"]
            E0 = outInt.df[iEnd,"E"]
            I00 = outInt.df[iEnd,"I0"]
            I10 = outInt.df[iEnd,"I1"]
            I20 = outInt.df[iEnd,"I2"]
            I30 = outInt.df[iEnd,"I3"]
            D0 = outInt.df[iEnd,"D"]
            R0 = outInt.df[iEnd,"R"]
            
            y0 = c(S=S0, E=E0, I0=I00, I1=I10, I2=I20, I3=I30, R=R0, D=D0)
            # y0 = c(S=S0, E=E0, I1=I10, I2=I20, I3=I30, R=R0, D=D0)
            
            #run with parameters back to baseline
            outIntOff.df=GetSpread_SEIR(pModel,N,Trun2,y0)
            outIntOff.df$time=outIntOff.df$time+Tend
            
            #combine vectors
            outInt.df=rbind(outInt.df,outIntOff.df)
            
            full.outInt.df <- rbind.data.frame(full.outInt.df, outInt.df)
            #continue on to next intervention
          }
          
        }else{
          full.outInt.df <- rbind.data.frame(full.outInt.df, outInt.df)
        }
      }
      #after interventions - check to see if at end of simulation time
      #Tend holds on to last intervention's end date from within for-loop
      Trun2=Tmax-Tend
      
      if(Trun2==0){
      }else{
        #Set initial conditions and time interval
        #Round all numbers to lowest intergar, so if less than 1, go to zero
        iEnd=nrow(full.outInt.df)
        S0 = full.outInt.df[iEnd,"S"]
        E0 = full.outInt.df[iEnd,"E"]
        I00 = full.outInt.df[iEnd,"I0"]
        I10 = full.outInt.df[iEnd,"I1"]
        I20 = full.outInt.df[iEnd,"I2"]
        I30 = full.outInt.df[iEnd,"I3"]
        D0 = full.outInt.df[iEnd,"D"]
        R0 = full.outInt.df[iEnd,"R"]
        
        y0 = c(S=S0, E=E0, I0=I00, I1=I10, I2=I20, I3=I30, R=R0, D=D0)
        # y0 = c(S=S0, E=E0, I1=I10, I2=I20, I3=I30, R=R0, D=D0)
        
        #run with parameters back to baseline
        outIntOff.df=GetSpread_SEIR(pModel,N,Trun2,y0)
        outIntOff.df$time=outIntOff.df$time+Tend
        
        #combine vectors
        full.outInt.df=rbind(full.outInt.df,outIntOff.df)
      }
    }
    plot_multiple_models=F
  }else{
    if(length(setdiff(names(mod_results), c("Baseline", "Intervention"))) > 0){
      if('ensemble' %in% names(mod_results)){
        mod_results$Intervention = mod_results$ensemble
        mod_results$ensemble = NULL
        model_name = "Ensemble\n"
      }
      plot_multiple_models = T
      Tmax = as.integer(max(mod_results$Intervention$date, na.rm=T) - graph.start.date + 3)
    }else{
      plot_multiple_models=F
      model_name = ""
    }
    out.df = mod_results$Baseline
    full.outInt.df = mod_results$Intervention
  }
  tt.round.digits <- 0
  if('interv.per.capita' %in% names(input)){
    if (input$interv.per.capita){
      out.df$S <- (1000*out.df$S)/input$population.size
      out.df$E <- (1000*out.df$E)/input$population.size
      out.df$I0 <- (1000*out.df$I0)/input$population.size
      out.df$I1 <- (1000*out.df$I1)/input$population.size
      out.df$I2 <- (1000*out.df$I2)/input$population.size
      out.df$I3 <- (1000*out.df$I3)/input$population.size
      out.df$R <- (1000*out.df$R)/input$population.size
      out.df$D <- (1000*out.df$D)/input$population.size
      
      full.outInt.df$S <- (1000*full.outInt.df$S)/input$population.size
      full.outInt.df$E <- (1000*full.outInt.df$E)/input$population.size
      full.outInt.df$I0 <- (1000*full.outInt.df$I0)/input$population.size
      full.outInt.df$I1 <- (1000*full.outInt.df$I1)/input$population.size
      full.outInt.df$I2 <- (1000*full.outInt.df$I2)/input$population.size
      full.outInt.df$I3 <- (1000*full.outInt.df$I3)/input$population.size
      full.outInt.df$R <- (1000*full.outInt.df$R)/input$population.size
      full.outInt.df$D <- (1000*full.outInt.df$D)/input$population.size
      
      tt.round.digits <- 3
    }
  }

  
  #subset the relevant variables and add in a column for capacity
  capParams=SetHospCapacitySliders(input)
  init_date <- graph.start.date
  
  # if chosen region is county, set selected.region accordingly.
  if (plotCounty){
    selected.st.abb <- STATE_NAMES$abbr[STATE_NAMES$full==input$inter_state] 
    selected.region <- paste(input$inter_county, selected.st.abb, sep = ", ")
  }else{
    selected.region <- input$inter_state
  }
  
  # need this plotting function to work offline too!
  if('plt.height' %in% names(input)){
    plt.height = round(input$dimension[2]*0.25)
  }else{
    plt.height = NULL
  }
  
  
  INTERV_COLS <- c("#332288", "#88CCEE", "#117733", "#DDCC77", "#CC6677","#AA4499", "#44AA99",
                   "#969696", "#969696", "#969696", 
                   "#d3d3d3","#d3d3d3",
                   "#DDCC77", "#AA4499")
  names(INTERV_COLS) <- c("Cases-Model Estimate", "Cases Anticipated\nNeeding Hospitalization", 
                          "Critical Infections", "Deaths-Model Estimate",
                          
                          "Cases-Total Confirmed", "Deaths-Confirmed", "Relative Mobility",
                          
                          "Conventional Mechanical\nVentilator Capacity","Contingency Mechanical\nVentilator Capacity",
                          "Crisis Mechanical\nVentilator Capacity",
                          
                          "Available ICU Beds", "Available Hospital Beds",
                          
                          "Daily Deaths\nModel Estimate", "Daily Deaths\nConfirmed")
  
  INTER_LINETYPES <- c("solid", "solid", "solid", "solid", "solid",  "solid", "solid",
                       "dash", "dot", "dashdot",
                       "dash", "dot",
                       
                       "solid", "solid")
  
  MOD_NAME_MAP = list('npi'='Policy Model\n',
                      'mobility'='Mobility Model\n',
                      'ppe' = 'Trendline Model\n', 
                      'ensemble'='Ensemble Model\n')
  
  SHOWCAP_NAME_MAP = list('DeltaD'="Daily Deaths",
                          'Deaths'='Deaths',
                          'I3'='Critical Cases',
                          'I2_I3'='Hospitalizations',
                          'CasesCap'='Cumulative Cases')
  
  ret.plt <- plot_ly(colors = INTERV_COLS, 
                     linetypes = INTER_LINETYPES,
                     height = plt.height, 
                     legendgroup='group1')
  all.plt.dat <- data.frame()
  
  ##=========LOGIC TO PLOT MULTIPLE MODELS=================#
  if(plot_multiple_models){
    models = intersect(names(MOD_NAME_MAP), names(mod_results))
    # get common column names across all model data
    common_cols = setdiff(colnames(mod_results[[models[[1]]]]), c('period', 'b1'))
    for(model in models){common_cols = intersect(common_cols, names(mod_results[[model]]))}
    #model_colors <- colorRampPalette(c("#F0F0F0", "#BDBDBD"))(length(models))
    model_linetypes = c("dash", "dot", "dashdot","longdash", "longdashdot")
    # bind model data together
    for(i in c(1:length(models))){
      model = models[[i]]
      all_model_data = mod_results[[model]][,common_cols] 
      for(var_name in VarShowCap){
        add_value_trace = T
        if(var_name %in% c('I3')){
          all_model_data$value =  all_model_data[,'I3']
        }else if(var_name == 'I2_I3'){
          all_model_data$value = rowSums(all_model_data[,c('I2','I3')])
        }else if(var_name == 'CasesCap'){
          if(model == 'ppe'){
            all_model_data$value = rowSums(all_model_data[,c("I1", "I2","I3")], na.rm=T) # # only sum cases for PPE model
          }else{
            all_model_data$value = rowSums(all_model_data[,c("I1", "I2","I3", "D", "R")], na.rm=T) # sum across all groups for SEIR models
          }
        }else if(var_name == 'Daily_Deaths'){
          all_model_data = all_model_data %>% distinct() %>%
            mutate(value = D - lag(D, n = 1, default = 0)) %>% ungroup()
        }else if(var_name == 'Deaths'){
          all_model_data$value = all_model_data[,'D']
        }else{
          add_value_trace = F
        }
        if(add_value_trace){
          all_model_data <- all_model_data %>% mutate('model_name' = paste0(SHOWCAP_NAME_MAP[[var_name]],'-', MOD_NAME_MAP[[model]], ' Estimate'))
          ret.plt = ret.plt %>% add_trace(data=all_model_data, x=~date, y=~value, name=~model_name, 
                                          hovertext = ~paste0(model_name,": ", round(value, tt.round.digits)), hoverinfo="text",
                                          line = list(color="#D6D6D6", width=3, dash=model_linetypes[[i]]),type='scatter', mode='lines', 
                                          legendgroup='group2')# %>%
           # layout(xaxis=list(range=c(0, max(all_model_data$value, na.rm=T)*1.05)))
        }
      }
      
    }
  }
  
  if ("I3" %in% VarShowCap){
    out.df$value=out.df[,"I3"] # create observed variable
    out.df$Intervention="Critical Infections\n(w/o NPIs)" # add intervention column
    
    if (nrow(full.outInt.df)>0){
      full.outInt.df$value=full.outInt.df[,"I3"]
      full.outInt.df$Intervention="Critical Infections"
      common_cols = intersect(colnames(out.df), colnames(full.outInt.df))
      outAll.df=rbind(out.df[,common_cols],full.outInt.df[,common_cols]) #combine baseline and intervention
    }else{
      outAll.df <- out.df
    }
    
    outAll.sub=subset(outAll.df, select=c("time","value","Intervention")) # choose only case column
    outAll.sub$Intervention=factor(outAll.sub$Intervention) # set intervention as factor
    combData=outAll.sub[with(outAll.sub,order(Intervention,time)),]
    
    combData <- combData %>%
      mutate(date = as.Date(init_date) + ddays(time))
    
    combData.pre.inter <- combData %>%
      filter(time < min(time[Intervention=="Critical Infections"], na.rm = TRUE)) %>%
      filter(Intervention == "Critical Infections\n(w/o NPIs)") %>%
      mutate(Intervention = "Critical Infections")
    
    #pre intervention time
    combData <- combData %>%
      union_all(combData.pre.inter) %>%
      arrange(date)
    if (nrow(intervention.df)>0 & ncol(intervention.df)>0){
      ret.plt <- ret.plt %>%
        add_trace(data = subset(combData, Intervention == "Critical Infections"), x=~date, y=~value, 
                  hovertext = ~paste("Critical Infections:", round(value, tt.round.digits)), hoverinfo="text",
                  color=~Intervention, linetype=~Intervention,
                  type='scatter', mode='lines', line=list(color='#117733',dash='solid',width = 4))
      all.plt.dat <- all.plt.dat %>%
        union_all(subset(combData, Intervention == "Critical Infections"))
    }else{
      ret.plt <- ret.plt %>%
        add_trace(data = subset(combData, Intervention == "Critical Infections\n(w/o NPIs)"), x=~date, y=~value, 
                  color=~Intervention, linetype=~Intervention,
                  hovertext = ~paste("Critical Infections (w/o NPIs):", round(value, tt.round.digits) ), hoverinfo="text",
                  type='scatter', mode='lines', line=list(color='#117733',dash='solid',width = 4))
      all.plt.dat <- all.plt.dat %>%
        union_all(subset(combData, Intervention == "Critical Infections\n(w/o NPIs)"))
    }
    
    # if confidence intervals provided, plot them.
    if(any(grepl('upper', colnames(full.outInt.df)))){
      ci_df = subset(full.outInt.df, grepl('forecast', period)) %>%
        mutate(lower = I3_lower,
               upper = I3_upper)
      #ci_df = subset(combData, Intervention == "Deaths-Model Estimate (w/o NPIs)") %>%
      ret.plt = ret.plt %>%
        add_ribbons(data = ci_df, x=~date,ymin=~lower, ymax=~upper,
                    line = list(color = 'rgba(201,201,201, 0.4)'),
                    fillcolor = 'rgba(201,201,201, 0.4)',showlegend=F)
    }
    
  }
  
  if ("I3mv" %in% VarShowCap){
    out.df$value=out.df[,"I3"] # create observed variable
    out.df$Intervention="Do Nothing" # add intervention column
    
    if (nrow(full.outInt.df)>0){
      full.outInt.df$value=full.outInt.df[,"I3"]
      full.outInt.df$Intervention="Critical Infections"
      common_cols = intersect(colnames(out.df), colnames(full.outInt.df))
      outAll.df=rbind(out.df[,common_cols],full.outInt.df[,common_cols]) #combine baseline and intervention
    }else{
      outAll.df <- out.df
    }
    
    outAll.sub=subset(outAll.df, select=c("time","value","Intervention")) # choose only case column
    outAll.sub$Intervention=factor(outAll.sub$Intervention) # set intervention as factor
    outAll.sub=outAll.sub[with(outAll.sub,order(Intervention,time)),]
    
    cap.times <- sort(unique(outAll.sub$time))
    
    if (!input$interv.per.capita){
      capData=data.frame("time"=cap.times,"value"=as.numeric(capParams["ConvVentCap"]*(N/1000)), "Intervention"="Conventional Mechanical\nVentilator Capacity")
      combData=rbind(outAll.sub,capData)
      capData=data.frame("time"=cap.times,"value"=as.numeric(capParams["ContVentCap"]*(N/1000)), "Intervention"="Contingency Mechanical\nVentilator Capacity")
      combData=rbind(combData,capData)
      capData=data.frame("time"=cap.times,"value"=as.numeric(capParams["CrisisVentCap"]*(N/1000)), "Intervention"="Crisis Mechanical\nVentilator Capacity")
    }else{
      capData=data.frame("time"=cap.times,"value"=as.numeric(capParams["ConvVentCap"]), "Intervention"="Conventional Mechanical\nVentilator Capacity")
      combData=rbind(outAll.sub,capData)
      capData=data.frame("time"=cap.times,"value"=as.numeric(capParams["ContVentCap"]), "Intervention"="Contingency Mechanical\nVentilator Capacity")
      combData=rbind(combData,capData)
      capData=data.frame("time"=cap.times,"value"=as.numeric(capParams["CrisisVentCap"]), "Intervention"="Crisis Mechanical\nVentilator Capacity")
      
    }
    
    combData <- combData %>%
      union_all(capData) %>%
      mutate(date = as.Date(init_date) + ddays(time))
    
    ret.plt <- ret.plt %>%
      add_trace(data = subset(combData, Intervention == "Conventional Mechanical\nVentilator Capacity"), x=~date, y=~value, color=~Intervention, 
                hovertext = ~paste("Conventional Capacity:", round(value, tt.round.digits)), hoverinfo="text",
                linetype=~Intervention, type='scatter', mode='lines', 
                line=list(color='#969696', dash="dash")) %>%
      add_trace(data = subset(combData, Intervention == "Contingency Mechanical\nVentilator Capacity"), x=~date, y=~value, color=~Intervention, 
                hovertext = ~paste("Contingency Capacity:", round(value, tt.round.digits)), hoverinfo="text",
                linetype=~Intervention, type='scatter', mode='lines', 
                line=list(color='#969696', dash="dashdot")) %>%
      add_trace(data = subset(combData, Intervention == "Crisis Mechanical\nVentilator Capacity"), x=~date, y=~value, color=~Intervention, 
                hovertext = ~paste("Crisis Capacity:", round(value, tt.round.digits)), hoverinfo="text",
                linetype=~Intervention, type='scatter', mode='lines', 
                line=list(color='#969696', dash="dot"))
    
    all.plt.dat <- all.plt.dat %>%
      union_all(subset(combData, Intervention == "Crisis Mechanical\nVentilator Capacity"))
    
    # if confidence intervals provided, plot them.
    if(any(grepl('upper', colnames(full.outInt.df)))){
      ci_df = subset(full.outInt.df, grepl('forecast', period)) %>%
        mutate(lower = I3_lower,
               upper = I3_upper)
      #ci_df = subset(combData, Intervention == "Deaths-Model Estimate (w/o NPIs)") %>%
      ret.plt = ret.plt %>%
        add_ribbons(data = ci_df, x=~date,ymin=~lower, ymax=~upper,
                    line = list(color = 'rgba(201,201,201, 0.4)'),
                    fillcolor = 'rgba(201,201,201, 0.4)',showlegend=F)
    }
  }
  
  if ("I3bed" %in% VarShowCap){
    out.df$value=out.df[,"I3"] # create observed variable
    out.df$Intervention="Do Nothing" # add intervention column
    
    if (nrow(full.outInt.df)>0){
      full.outInt.df$value=full.outInt.df[,"I3"]
      full.outInt.df$Intervention="Critical Infections"
      common_cols = intersect(colnames(out.df), colnames(full.outInt.df))
      outAll.df=rbind(out.df[,common_cols],full.outInt.df[,common_cols]) #combine baseline and intervention
    }else{
      outAll.df <- out.df
    }
    
    outAll.sub=subset(outAll.df, select=c("time","value","Intervention")) # choose only case column
    outAll.sub$Intervention=factor(outAll.sub$Intervention) # set intervention as factor
    outAll.sub=outAll.sub[with(outAll.sub,order(Intervention,time)),]
    
    cap.times <- sort(unique(outAll.sub$time))
    
    if (!input$interv.per.capita){
      capData=data.frame("time"=cap.times,"value"=as.numeric(capParams["AvailICUBeds"]*(N/1000)), "Intervention"="Available ICU Beds")
    }else{
      capData=data.frame("time"=cap.times,"value"=as.numeric(capParams["AvailICUBeds"]), "Intervention"="Available ICU Beds")
    }
    
    combData <- capData %>%
      mutate(date = as.Date(init_date) + ddays(time))
    
    ret.plt <- ret.plt %>%
      add_trace(data = subset(combData, Intervention == "Available ICU Beds"), x=~date, y=~value, color=~Intervention, 
                hovertext = ~paste("ICU Beds:", round(value, tt.round.digits)), hoverinfo="text",
                linetype=~Intervention, type='scatter', mode='lines', 
                line=list(color='#d3d3d3', dash="dash"))
    
    all.plt.dat <- all.plt.dat %>%
      union_all(subset(combData, Intervention == "Available ICU Beds"))
    
    # if confidence intervals provided, plot them.
    if(any(grepl('upper', colnames(full.outInt.df))) && !("I2_I3" %in% VarShowCap)){
      ci_df = subset(full.outInt.df, grepl('forecast', period)) %>%
        mutate(lower = I3_lower,
               upper = I3_upper)
      #ci_df = subset(combData, Intervention == "Deaths-Model Estimate (w/o NPIs)") %>%
      ret.plt = ret.plt %>%
        add_ribbons(data = ci_df, x=~date,ymin=~lower, ymax=~upper,
                    line = list(color = 'rgba(201,201,201, 0.4)'),
                    fillcolor = 'rgba(201,201,201, 0.4)',showlegend=F)
    }
  }
  
  if ("I2_I3" %in% VarShowCap){
    
    out.df$value=rowSums(out.df[,c("I2","I3")]) # create observed variable
    out.df$Intervention="Cases Anticipated Needing\nHospitalization (w/o NPIs)" # add intervention column
    
    if (nrow(full.outInt.df)>0){
      full.outInt.df$value=rowSums(full.outInt.df[,c("I2","I3")])
      full.outInt.df$Intervention="Cases Anticipated\nNeeding Hospitalization"
      common_cols = intersect(colnames(out.df), colnames(full.outInt.df))
      outAll.df=rbind(out.df[,common_cols],full.outInt.df[,common_cols]) #combine baseline and intervention
    }else{
      outAll.df <- out.df
    }
    
    outAll.sub=subset(outAll.df, select=c("time","value","Intervention")) # choose only case column
    outAll.sub$Intervention=factor(outAll.sub$Intervention) # set intervention as factor
    outAll.sub=outAll.sub[with(outAll.sub,order(Intervention,time)),]
    
    combData <- outAll.sub %>%
      mutate(date = as.Date(init_date) + ddays(time))
    
    combData.pre.inter <- combData %>%
      filter(time < min(time[Intervention=="Cases Anticipated\nNeeding Hospitalization"], na.rm = TRUE)) %>%
      filter(Intervention == "Cases Anticipated Needing\nHospitalization (w/o NPIs)") %>%
      mutate(Intervention = "Cases Anticipated\nNeeding Hospitalization")
    
    #pre intervention time
    combData <- combData %>%
      union_all(combData.pre.inter) %>%
      arrange(date)
    
    if (nrow(intervention.df)>0 & ncol(intervention.df)>0){
      ret.plt <- ret.plt %>%
        add_trace(data = subset(combData, Intervention == "Cases Anticipated\nNeeding Hospitalization"), 
                  x=~date, y=~value, color=~Intervention, linetype=~Intervention,
                  hovertext = ~paste("Cases Anticipated Needing Hospitalization:", round(value, tt.round.digits)), hoverinfo="text",
                  type='scatter', mode='lines', line=list(color='#88CCEE',dash='solid',width = 4))
      all.plt.dat <- all.plt.dat %>%
        union_all(subset(combData, Intervention == "Cases Anticipated\nNeeding Hospitalization"))
    }else{
      ret.plt <- ret.plt %>%
        add_trace(data = subset(combData, Intervention == "Cases Anticipated Needing\nHospitalization (w/o NPIs)"), 
                  x=~date, y=~value, color=~Intervention, linetype=~Intervention,
                  hovertext = ~paste("Cases Anticipated Needing Hospitalization (w/o NPIs):", round(value, tt.round.digits)), hoverinfo="text",
                  type='scatter', mode='lines', line=list(color='#88CCEE',dash='solid',width = 4))
      all.plt.dat <- all.plt.dat %>%
        union_all(subset(combData, Intervention == "Cases Anticipated Needing\nHospitalization (w/o NPIs)"))
    }
    # if confidence intervals provided, plot them.
    if(any(grepl('upper', colnames(full.outInt.df)))){
      ci_df = subset(full.outInt.df, grepl('forecast', period)) %>%
        mutate(lower = I2_lower + I3_lower,
               upper = I2_upper + I3_upper)
      #ci_df = subset(combData, Intervention == "Deaths-Model Estimate (w/o NPIs)") %>%
      ret.plt = ret.plt %>%
        add_ribbons(data = ci_df, x=~date,ymin=~lower, ymax=~upper,
                    line = list(color = 'rgba(201,201,201, 0.4)'),
                    fillcolor = 'rgba(201,201,201, 0.4)',showlegend=F)
    }
  }
  
  if ("Hosp" %in% VarShowCap){
    out.df$value=out.df[,"I3"] # create observed variable
    out.df$Intervention="Do Nothing" # add intervention column
    
    if (nrow(full.outInt.df)>0){
      full.outInt.df$value=full.outInt.df[,"I3"]
      full.outInt.df$Intervention="Critical Infections"
      common_cols = intersect(colnames(out.df), colnames(full.outInt.df))
      outAll.df=rbind(out.df[,common_cols],full.outInt.df[,common_cols]) #combine baseline and intervention
    }else{
      outAll.df <- out.df
    }
    
    outAll.sub=subset(outAll.df, select=c("time","value","Intervention")) # choose only case column
    outAll.sub$Intervention=factor(outAll.sub$Intervention) # set intervention as factor
    outAll.sub=outAll.sub[with(outAll.sub,order(Intervention,time)),]
    
    cap.times <- sort(unique(outAll.sub$time))
    
    if (!input$interv.per.capita){
      capData=data.frame("time"=cap.times,"value"=as.numeric(capParams["AvailHospBeds"]*(N/1000)), "Intervention"="Available Hospital Beds")
    }else{
      capData=data.frame("time"=cap.times,"value"=as.numeric(capParams["AvailHospBeds"]), "Intervention"="Available Hospital Beds")
    }
    
    combData <- capData %>%
      mutate(date = as.Date(init_date) + ddays(time))
    
    ret.plt <- ret.plt %>%
      add_trace(data = subset(combData, Intervention == "Available Hospital Beds"), x=~date, y=~value, color=~Intervention, 
                hovertext = ~paste("Hospital Beds:", round(value, tt.round.digits)), hoverinfo="text",
                linetype=~Intervention, type='scatter', mode='lines', 
                line=list(color='#d3d3d3', dash="dot"))
    all.plt.dat <- all.plt.dat %>%
      union_all(subset(combData, Intervention == "Available Hospital Beds"))
    
    # if confidence intervals provided, plot them.
    if(any(grepl('upper', colnames(full.outInt.df)))){
      ci_df = subset(full.outInt.df, grepl('forecast', period)) %>%
        mutate(lower = I3_lower,
               upper = I3_upper)
      #ci_df = subset(combData, Intervention == "Deaths-Model Estimate (w/o NPIs)") %>%
      ret.plt = ret.plt %>%
        add_ribbons(data = ci_df, x=~date,ymin=~lower, ymax=~upper,
                    line = list(color = 'rgba(201,201,201, 0.4)'),
                    fillcolor = 'rgba(201,201,201, 0.4)',showlegend=F)
    }
  }
  
  if ("CasesCap" %in% VarShowCap){
    
    out.df$value=rowSums(out.df[,c("I1", "I2","I3", "D", "R")], na.rm=T) # create observed variable
    out.df$Intervention="Cases-Model Estimate (w/o NPIs)" # add intervention column
    
    if (nrow(full.outInt.df)>0){
      full.outInt.df$value=rowSums(full.outInt.df[,c("I1","I2","I3", "D", "R")], na.rm=T)
      full.outInt.df$Intervention="Cases-Model Estimate"
      common_cols = intersect(colnames(out.df), colnames(full.outInt.df))
      outAll.df=rbind(out.df[,common_cols],full.outInt.df[,common_cols]) #combine baseline and intervention
    }else{
      outAll.df <- out.df
    }
    
    outAll.sub=subset(outAll.df, select=c("time","value","Intervention")) # choose only case column
    outAll.sub$Intervention=factor(outAll.sub$Intervention) # set intervention as factor
    outAll.sub=outAll.sub[with(outAll.sub,order(Intervention,time)),]
    
    combData <- outAll.sub %>%
      mutate(date = as.Date(init_date) + ddays(time))
    
    combData.pre.inter <- combData %>%
      filter(time < min(time[Intervention=="Cases-Model Estimate"], na.rm = TRUE)) %>%
      filter(Intervention == "Cases-Model Estimate (w/o NPIs)") %>%
      mutate(Intervention = "Cases-Model Estimate")
    
    #pre intervention time
    combData <- combData %>%
      union_all(combData.pre.inter) %>%
      arrange(date)
    
    if (nrow(intervention.df)>0 & ncol(intervention.df)>0){
      ret.plt <- ret.plt %>%
        add_trace(data = subset(combData, Intervention == "Cases-Model Estimate"), 
                  x=~date, y=~value, color=~Intervention, linetype=~Intervention,
                  hovertext = ~paste("Cases-Model Estimate:", round(value, tt.round.digits)), hoverinfo="text",
                  type='scatter', mode='lines', line=list(color='#332288',dash='solid',width = 4))
      all.plt.dat <- all.plt.dat %>%
        union_all(subset(combData, Intervention == "Cases-Model Estimate"))
    }else{
      ret.plt <- ret.plt %>%
        add_trace(data = subset(combData, Intervention == "Cases-Model Estimate (w/o NPIs)"), 
                  x=~date, y=~value, color=~Intervention, linetype=~Intervention,
                  hovertext = ~paste("Cases-Model Estimate (w/o NPIs):", round(value, tt.round.digits)), hoverinfo="text",
                  type='scatter', mode='lines', line=list(color='#332288',dash='solid',width = 4))
      all.plt.dat <- all.plt.dat %>%
        union_all(subset(combData, Intervention == "Cases-Model Estimate (w/o NPIs)"))
    }
    # if confidence intervals provided, plot them.
    if(any(grepl('upper', colnames(full.outInt.df)))){
      ci_df = subset(full.outInt.df, grepl('forecast', period)) %>%
        mutate(lower = I1_lower + I2_lower + I3_lower + R_lower + D_lower,
               upper = I1_upper + I2_upper + I3_upper + R_lower + D_lower)
      #ci_df = subset(combData, Intervention == "Deaths-Model Estimate (w/o NPIs)") %>%
      ret.plt = ret.plt %>%
        add_ribbons(data = ci_df, x=~date,ymin=~lower, ymax=~upper,
                    line = list(color = 'rgba(201,201,201, 0.4)'),
                    fillcolor = 'rgba(201,201,201, 0.4)',showlegend=F)
    }
  }
  
  if ("Deaths" %in% VarShowCap){
    
    out.df$value=out.df[,"D"] # create observed variable
    out.df$Intervention="Deaths-Model Estimate (w/o NPIs)" # add intervention column
    
    if (nrow(full.outInt.df)>0){
      full.outInt.df$value=full.outInt.df[,"D"]
      full.outInt.df$Intervention=paste0("Deaths-",model_name," Model Estimate")
      common_cols = intersect(colnames(out.df), colnames(full.outInt.df))
      outAll.df=rbind(out.df[,common_cols],full.outInt.df[,common_cols]) #combine baseline and intervention
    }else{
      outAll.df <- out.df
    }
    
    outAll.sub=subset(outAll.df, select=c("time","value","Intervention")) # choose only case column
    outAll.sub$Intervention=factor(outAll.sub$Intervention) # set intervention as factor
    outAll.sub=outAll.sub[with(outAll.sub,order(Intervention,time)),]
    
    combData <- outAll.sub %>%
      mutate(date = as.Date(init_date) + ddays(time))
    
    combData.pre.inter <- combData %>%
      filter(time < min(time[Intervention==paste0("Deaths-",model_name," Model Estimate")], na.rm = TRUE)) %>%
      filter(Intervention == "Deaths-Model Estimate (w/o NPIs)") %>%
      mutate(Intervention = paste0("Deaths-",model_name," Model Estimate"))
    
    #pre intervention time
    combData <- combData %>%
      union_all(combData.pre.inter) %>%
      arrange(date)
    
    if (nrow(intervention.df)>0 & ncol(intervention.df)>0){
      ret.plt <- ret.plt %>%
        add_trace(data = subset(combData, Intervention == paste0("Deaths-",model_name," Model Estimate")), x=~date, y=~value, color=~Intervention, linetype=~Intervention,
                  hovertext = ~paste(paste0("Deaths-",model_name," Model Estimate:"), round(value, tt.round.digits)), hoverinfo="text",
                  type='scatter', mode='lines', line=list(color='#DDCC77',dash='solid',width = 4, legendgroup='group1'))
      all.plt.dat <- all.plt.dat %>%
        union_all(subset(combData, Intervention == paste0("Deaths-",model_name," Model Estimate")))
    }else{
      ret.plt <- ret.plt %>%
        add_trace(data = subset(combData, Intervention == "Deaths-Model Estimate (w/o NPIs)"), 
                  x=~date, y=~value, color=~Intervention, linetype=~Intervention, legendgroup='group1',
                  hovertext = ~paste("Deaths-Model Estimate (w/o NPIs):", round(value, tt.round.digits)), hoverinfo="text",
                  type='scatter', mode='lines', line=list(color='#DDCC77',dash='solid',width = 4))
      all.plt.dat <- all.plt.dat %>%
        union_all(subset(combData, Intervention == "Deaths-Model Estimate (w/o NPIs)"))
    }
    # if confidence intervals provided, plot them.
    if(any(grepl('upper', colnames(full.outInt.df)))){
      ci_df = subset(full.outInt.df, grepl('forecast', period))
      #ci_df = subset(combData, Intervention == "Deaths-Model Estimate (w/o NPIs)") %>%
      ret.plt = ret.plt %>%
        add_ribbons(data = ci_df, x=~date,ymin=~D_lower, ymax=~D_upper,
                    line = list(color = 'rgba(201,201,201, 0.4)'),
                    fillcolor = 'rgba(201,201,201, 0.4)',showlegend=F)
    }
  }
  
  if ("Daily_Deaths" %in% VarShowCap){
    
    out.df$value=out.df[,"D"] # create observed variable
    out.df$Intervention="Daily Deaths\nModel Estimate (w/o NPIs)" # add intervention column
    
    if (nrow(full.outInt.df)>0){
      full.outInt.df$value=full.outInt.df[,"D"]
      full.outInt.df$Intervention="Daily Deaths\nModel Estimate"
      common_cols = intersect(colnames(out.df), colnames(full.outInt.df))
      outAll.df=rbind(out.df[,common_cols],full.outInt.df[,common_cols]) #combine baseline and intervention
      }else{
      outAll.df <- out.df
    }
    
    outAll.sub=subset(outAll.df, select=c("time","value","Intervention")) %>% # choose only case column
      distinct()
    outAll.sub$Intervention=factor(outAll.sub$Intervention) # set intervention as factor
    outAll.sub=outAll.sub[with(outAll.sub,order(Intervention,time)),]
    
    combData <- outAll.sub %>%
      mutate(date = as.Date(init_date) + ddays(time))
    
    combData.pre.inter <- combData %>%
      filter(time < min(time[Intervention=="Daily Deaths\nModel Estimate"], na.rm = TRUE)) %>%
      filter(Intervention == "Daily Deaths\nModel Estimate (w/o NPIs)") %>%
      mutate(Intervention = "Daily Deaths\nModel Estimate")
    
    #pre intervention time
    combData <- combData %>%
      union_all(combData.pre.inter) %>%
      arrange(date) %>%
      distinct() %>%
      group_by(Intervention) %>%
      mutate(value = value - lag(value, n = 1, default = 0)) %>%
      ungroup()
    
    if (nrow(intervention.df)>0 & ncol(intervention.df)>0){
      ret.plt <- ret.plt %>%
        add_trace(data = subset(combData, Intervention == "Daily Deaths\nModel Estimate"), x=~date, y=~value, color=~Intervention, linetype=~Intervention,
                  hovertext = ~paste("Daily Deaths-Estimate:", round(value, tt.round.digits)), hoverinfo="text",
                  type='scatter', mode='lines', line=list(color='#DDCC77',dash='solid',width = 4))
      all.plt.dat <- all.plt.dat %>%
        union_all(subset(combData, Intervention == "Daily Deaths\nModel Estimate"))
    }else{
      ret.plt <- ret.plt %>%
        add_trace(data = subset(combData, Intervention == "Daily Deaths\nModel Estimate (w/o NPIs)"), 
                  x=~date, y=~value, color=~Intervention, linetype=~Intervention,
                  hovertext = ~paste("Daily Deaths-Estimate (w/o NPIs):", round(value, tt.round.digits)), hoverinfo="text",
                  type='scatter', mode='lines', line=list(color='#DDCC77',dash='solid',width = 4))
      all.plt.dat <- all.plt.dat %>%
        union_all(subset(combData, Intervention == "Daily Deaths\nModel Estimate (w/o NPIs)"))
    }
    
  
    # if confidence intervals provided, plot them.
    if(any(grepl('upper', colnames(full.outInt.df)))){
      ci_df = subset(full.outInt.df, grepl('forecast', period)) %>%
        select(date, D, D_lower, D_upper) %>%
        mutate(D = D - lag(D, n=1, default=0), 
               D_lower = D_lower - lag(D_lower, n=1, default=0), 
               D_upper = D_upper - lag(D_upper, n=1, default=0), 
               unique = (D + D_lower + D_upper > 0)) %>%
        subset(unique==T)
      #ci_df = subset(combData, Intervention == "Deaths-Model Estimate (w/o NPIs)") %>%
      ret.plt = ret.plt %>%
        add_ribbons(data = ci_df, x=~date,ymin=~D_lower, ymax=~D_upper,
                    line = list(color = 'rgba(201,201,201, 0.4)'),
                    fillcolor = 'rgba(201,201,201, 0.4)',showlegend=F)
    }
  }
  
  max.real <- 0
  if (interv.real.overlay){
    
    temp.real.dat.all <- STATE_INF_STATS %>%
      select(-State) %>%
      union_all(COUNTY_INF_STATS) %>%
      # filter(str_detect(string = mState.Providence, pattern = selected.region),  too broad of a pattern! 
      filter(mState.Providence == selected.region,
             date >= as.Date(firstCaseDate)-days(1)) %>%
      mutate(date = as.Date(date)) %>%
      arrange(date) 
    
    if ("Deaths" %in% VarShowCap){
      temp.real.dat = temp.real.dat.all %>%
        mutate(value = death_count,
               Intervention = "Deaths-Confirmed") %>%
        select(date, value, Intervention)
      if (input$interv.per.capita){
        temp.real.dat$value <- 1000*temp.real.dat$value/input$population.size
      }
      
      ret.plt <- ret.plt %>%
        add_trace(data = temp.real.dat, x=~date, y=~value, color=~Intervention, legendgroup='group1',
                  hovertext = ~paste("Deaths-Confirmed:", round(value, tt.round.digits)), hoverinfo="text",
                  type='scatter', mode='markers', marker=list(color='#AA4499'))
      max.real <- max(max(temp.real.dat$value,na.rm = TRUE), max.real, na.rm = TRUE)
    }
    
    if ("Daily_Deaths" %in% VarShowCap){
      temp.real.dat <- temp.real.dat.all %>%
        mutate(value = death_count - lag(death_count, n=1, default = 0),
               Intervention = "Daily Deaths\nConfirmed"
        ) %>%
        select(date, value, Intervention) 
      
      if (input$interv.per.capita){
        temp.real.dat$value <- 1000*temp.real.dat$value/input$population.size
      }
      
      ret.plt <- ret.plt %>%
        add_trace(data = temp.real.dat, x=~date, y=~value, color=~Intervention, #linetype=~Intervention,
                  hovertext = ~paste("Daily Deaths-Confirmed:", round(value, tt.round.digits)), hoverinfo="text",
                  type='scatter', mode='markers', marker=list(color='#AA4499'))
      max.real <- max(max(temp.real.dat$value,na.rm = TRUE), max.real, na.rm = TRUE)
    }
    
    if (any(c("CasesCap", "I2_I3", "I3") %in% VarShowCap)){
      temp.real.dat <- temp.real.dat.all %>%
        mutate(value = case_count,
               Intervention = "Cases-Total Confirmed"
        ) %>%
        select(date, value, Intervention)
      
      if (input$interv.per.capita){
        temp.real.dat$value <- 1000*temp.real.dat$value/input$population.size
      }
      
      ret.plt <- ret.plt %>%
        add_trace(data = temp.real.dat, x=~date, y=~value, color=~Intervention, #linetype=~Intervention,
                  hovertext = ~paste("Cases-Total Confirmed:", round(value, tt.round.digits)), hoverinfo="text",
                  type='scatter', mode='markers', marker=list(color='#CC6677'))
      max.real <- max(max(temp.real.dat$value,na.rm = TRUE), max.real, na.rm = TRUE)
    }
  }
  
  yaxis2.opts <- NULL
  
  current.day <- Sys.Date()
  
  
  plt.opts <- c("I3bed" = "ICU Beds", 
                "I3" = "Critical Infections",
                "I3mv" = "Ventilator Capacity", 
                "I2_I3" = "Cases Anticipated Needing Hospitalization", 
                "Hosp" = "Hospital Beds",
                "CasesCap" = "Cumulative Cases", 
                "Deaths" = "Deaths",
                "Daily_Deaths" = "Daily Deaths")
  
  if (length(VarShowCap)==0){
    plot.title <- ""
    y.axis.title <- ""
  }else if (length(VarShowCap)==1){
    if (input$interv.per.capita){
      plot.title <- paste0(selected.region, " COVID-19 Modeled ", plt.opts[VarShowCap[1]], " per capita")
      y.axis.title <- paste(plt.opts[VarShowCap[1]], "per 1000 people")
    }else{
      plot.title <- paste(selected.region, "COVID-19 Modeled", plt.opts[VarShowCap[1]])
      y.axis.title <- plt.opts[VarShowCap[1]]
    }
  }else{
    if (input$interv.per.capita){
      plot.title <- paste(selected.region, "COVID-19 Modeled Populations per capita")
      y.axis.title <- "Population per 1000 people"
    }else{
      plot.title <- paste(selected.region, "COVID-19 Modeled Populations")
      y.axis.title <- "Population Size"
    }
  }
  
  date.line <- all.plt.dat %>%
    select(date) %>%
    distinct() %>%
    mutate(value=0,
           date.ft = format(as.Date(date), "%b %d, %Y"))
  
  if (nrow(date.line)>0){
    ret.plt <- ret.plt %>%
      add_trace(data = date.line, x=~date, y=~value, 
                hovertext = ~date.ft, hoverinfo="text",
                type="scatter", mode="lines", line=list(color="transparent"),
                showlegend = FALSE)
  }
  
  if (nrow(all.plt.dat)>0){
    if (any(!is.na(intervention.df$start_time)) & any(!is.na(intervention.df$end_time))){
      
      max.y.intervention <- all.plt.dat %>%
        filter(time <= 150) %>%
        summarise(val = max(value, na.rm=TRUE)) %>%
        .$val
      
      max.y.intervention <- max(max.y.intervention, max.real, na.rm = TRUE)
      print(Tmax)
      ret.plt=layout(ret.plt,
                     legend=list(y=0.85),
                     font = list(family = "Arial"),
                     title = list(text=plot.title,x=0,xref="paper"),
                     hovermode = "x unified",
                     xaxis=list(title="", range = c(as.Date(init_date), as.Date(init_date)+ddays(Tmax))), 
                     yaxis=list(title=y.axis.title, range = c(0, max.y.intervention*1.05)),
                     margin=list(l=55, r=200, autoexpand=FALSE),
                     yaxis2=yaxis2.opts,
                     shapes = #blue.interv.list
                       list(list(type = "rect",
                                 fillcolor = "blue", line = list(color = "blue"), opacity = 0.1,
                                 x0 = (as.Date(init_date) + ddays(min(intervention.df$start_time,na.rm = TRUE)) ), 
                                 x1 = (as.Date(init_date)+ ddays(max(intervention.df$end_time,na.rm = TRUE))), xref = "x",
                                 y0 = 0, y1 = 1, yref = "paper"),
                            list(type="line",
                                 x0=current.day,
                                 x1=current.day,
                                 xref="x",
                                 yref= 'paper', y0= 0, y1= 1,
                                 line = list(color="red")
                            )
                       ),
                     annotations = list(x = current.day,
                                        y = 1,
                                        text = "<i>Today</i>",
                                        font = list(color="red"),
                                        xref = "x",
                                        yref = "paper",
                                        showarrow = FALSE,
                                        xanchor="left")
                     
      )
    }else{
      ret.plt=layout(ret.plt,
                     legend=list(y=0.85),
                     title = list(text=plot.title,x=0,xref="paper"),
                     font = list(family = "Arial"),
                     hovermode = "x unified",
                     xaxis=list(title="", range = c(as.Date(init_date), as.Date(init_date)+ddays(Tmax))), 
                     yaxis=list(title=y.axis.title),
                     margin=list(l=55, r=200, autoexpand=FALSE),
                     yaxis2=yaxis2.opts,
                     shapes = list(list(type="line",
                                        x0=current.day,
                                        x1=current.day,
                                        xref="x",
                                        yref= 'paper', y0= 0, y1= 1,
                                        line = list(color="red")
                     )),
                     annotations = list(x = current.day,
                                        y = 1,
                                        text = "<i>Today</i>",
                                        font = list(color="red"),
                                        xref = "x",
                                        yref = "paper",
                                        showarrow = FALSE,
                                        xanchor="left")
                     
      )
    }
  }
  
  ret.plt = ret.plt %>% layout(yaxis=list(range=c(0, max(all.plt.dat$value, na.rm=T) * 1.25)))
  #if(plot_multiple_models) ret.plt = ret.plt %>% layout(xaxis=list(range=c(0, max(all.plt.dat$date, na.rm=T))))
  return(ret.plt)
}


get_mitre_seir_df <- function(model.data, selected.pop, graph.start.date = NULL, selected.st){

  init_date <- graph.start.date

  if (selected.pop == "C"){
    ret.dat <- model.data %>%
      filter(Intervention == "NPI Strategy") %>%
      select(time, I2, I3) %>%
      mutate(date = as.Date(init_date) + ddays(time)) %>%
      mutate(point0 = I2+I3,
             point = cumsum(point0),
             fc.name = "mitre_seir",
             quant_0.25 = NA,
             quant_0.75 = NA,
             forecast_date = min(STATE_TUNING_PARAMS$tuning_date[STATE_TUNING_PARAMS$state==selected.st], na.rm = TRUE),
             forecast_date = as.Date(forecast_date)
             ) %>%
      select(forecast_date, target_date=date, fc.name, point, quant_0.25, quant_0.75) %>%
      distinct()
  }else if (selected.pop == "D"){
    ret.dat <- model.data %>%
      filter(Intervention == "NPI Strategy") %>%
      select(time, D) %>%
      mutate(date = as.Date(init_date) + ddays(time)) %>%
      mutate(point = D,
             fc.name = "mitre_seir",
             quant_0.25 = NA,
             quant_0.75 = NA,
             forecast_date = min(STATE_TUNING_PARAMS$tuning_date[STATE_TUNING_PARAMS$state==selected.st], na.rm = TRUE),
             forecast_date = as.Date(forecast_date)
      ) %>%
      select(forecast_date, target_date=date, fc.name, point, quant_0.25, quant_0.75) %>%
      distinct()
  }else if (selected.pop == "DeltaD"){
    ret.dat <- model.data %>%
      filter(Intervention == "NPI Strategy") %>%
      select(time, D) %>%
      mutate(date = as.Date(init_date) + ddays(time)) %>%
      arrange(date) %>%
      mutate(point = D - lag(D, n=1, default = 0),
             fc.name = "mitre_seir",
             quant_0.25 = NA,
             quant_0.75 = NA,
             forecast_date = min(STATE_TUNING_PARAMS$tuning_date[STATE_TUNING_PARAMS$state==selected.st], na.rm = TRUE),
             forecast_date = as.Date(forecast_date)
      ) %>%
      select(forecast_date, target_date=date, fc.name, point, quant_0.25, quant_0.75) %>%
      distinct()
  }else if (selected.pop == "DeltaI2_I3"){
    ret.dat <- model.data %>%
      filter(Intervention == "NPI Strategy") %>%
      select(time, I2, I3) %>%
      mutate(date = as.Date(init_date) + ddays(time)) %>%
      arrange(date) %>%
      mutate(point0 = I2+I3,
             point01 = cumsum(point0),
             point = point01 - lag(point01, n=1, default = 0),
             fc.name = "mitre_seir",
             quant_0.25 = NA,
             quant_0.75 = NA,
             forecast_date = min(STATE_TUNING_PARAMS$tuning_date[STATE_TUNING_PARAMS$state==selected.st], na.rm = TRUE),
             forecast_date = as.Date(forecast_date)
      ) %>%
      select(forecast_date, target_date=date, fc.name, point, quant_0.25, quant_0.75) %>%
      distinct()
  }else{
    warning("Unknown selected.pop in model.comp plotting")
  }

  return(ret.dat)

}

get_mitre_ens_df <- function(model.data, selected.pop, graph.start.date = NULL, selected.st){
  
  init_date <- graph.start.date
  
  if (selected.pop == "C"){
    ret.dat <- model.data %>%
      select(time, I2, I3) %>%
      distinct() %>%
      mutate(date = as.Date(init_date) + ddays(time)) %>%
      mutate(point0 = I2+I3,
             point = cumsum(point0),
             fc.name = "mitre_ens",
             quant_0.25 = NA,
             quant_0.75 = NA,
             forecast_date = Sys.Date()-days(7),
             forecast_date = as.Date(forecast_date)
      ) %>%
      select(forecast_date, target_date=date, fc.name, point, quant_0.25, quant_0.75) %>%
      distinct()
  }else if (selected.pop == "D"){
    ret.dat <- model.data %>%
      select(time, D) %>%
      distinct() %>%
      mutate(date = as.Date(init_date) + ddays(time)) %>%
      mutate(point = D,
             fc.name = "mitre_ens",
             quant_0.25 = NA,
             quant_0.75 = NA,
             forecast_date = Sys.Date()-days(7),
             forecast_date = as.Date(forecast_date)
      ) %>%
      select(forecast_date, target_date=date, fc.name, point, quant_0.25, quant_0.75) %>%
      distinct()
  }else if (selected.pop == "DeltaD"){
    ret.dat <- model.data %>%
      select(time, D) %>%
      distinct() %>%
      mutate(date = as.Date(init_date) + ddays(time)) %>%
      arrange(date) %>%
      mutate(point = D - lag(D, n=1, default = 0),
             fc.name = "mitre_ens",
             quant_0.25 = NA,
             quant_0.75 = NA,
             forecast_date = Sys.Date()-days(7),
             forecast_date = as.Date(forecast_date)
      ) %>%
      select(forecast_date, target_date=date, fc.name, point, quant_0.25, quant_0.75) %>%
      distinct()
  }else if (selected.pop == "DeltaI2_I3"){
    ret.dat <- model.data %>%
      select(time, I2, I3) %>%
      distinct() %>%
      mutate(date = as.Date(init_date) + ddays(time)) %>%
      arrange(date) %>%
      mutate(point0 = I2+I3,
             point01 = cumsum(point0),
             point = point01 - lag(point01, n=1, default = 0),
             fc.name = "mitre_ens",
             quant_0.25 = NA,
             quant_0.75 = NA,
             forecast_date = Sys.Date()-days(7),
             forecast_date = as.Date(forecast_date)
      ) %>%
      select(forecast_date, target_date=date, fc.name, point, quant_0.25, quant_0.75) %>%
      distinct()
  }else{
    warning("Unknown selected.pop in model.comp plotting")
  }
  
  return(ret.dat)
  
}

# draw_new_intervention_plotly function removed 4/29
# NOTE - CHANGED TIME OFFSET FROM -1 TO 0 WHEN CONVERTING TO DATES SO MODEL DOESN'T START BEFORE START DATE

get_mitre_seir_df_old <- function(input, hops.cap.dat, intervention.df, graph.start.date){
  hdata <- hops.cap.dat
  ParamStruct=GetModelParams(input, hdata=hdata)
  pModel=ParamStruct$pModel
  N=ParamStruct$N
  Tmax=input$Tmax
  
  # Set initial conditions and time interval
  
  # METHOD1=TRUE is the original way of setting the model; 
  # METHOD1=FALSE is the random way of initializing based on first case date
  method1 <- FALSE
  if (method1){
    I2.val <- 0
    I1.val <- 0
    E0 <- input$initial.infected
    S0 <- N - E0 - I1.val
  }else{
    I2.val = input$initial.infected
    I1.val = I2.val * 10 #this is completely arbitrary
    E0= I1.val * 5 * input$IncubPeriod
    S0 = N - E0 - I2.val - I1.val
  }
  
  y0 = c(S=S0, E=E0, I0=0, I1=I1.val, I2=I2.val, I3=0, R=0, D=0)
  #    y0 = c(S=S0, E=E0, I1=0, I2=0, I3=0, R=0, D=0)
  
  #get Ro value
  #KF Commented out for now
  
  #Ro=GetRo_SEIR(pModel,N)
  
  out.df=GetSpread_SEIR(pModel,N,Tmax,y0)
  out=melt(out.df,id="time")
  
  #get r value
  V="E" #variable to calculate r for
  tpeak=out.df[which.max(select(out.df,"time",V)[,2]),"time"];
  
  t2=tpeak/4
  t1=tpeak/8
  r.out=Getr_SEIR(out,t1,t2,V)
  r=r.out$r
  DoublingTime=r.out$DoublingTime
  
  # INTERVENTION
  # intervention parameters ....now a for loop for multiple interventions
  full.outInt.df <- data.frame()
  blue.interv.list <- list()
  Tend <- 0
  if (nrow(intervention.df)>0 & ncol(intervention.df)>1){
    for (i in 1:nrow(intervention.df)){
      
      pModelInt=pModel
      pModelInt["b1"]=pModelInt["b1"]*(1-intervention.df[i, "Prop_Reduction_Mild_Trans"])
      pModelInt["b2"]=pModelInt["b2"]*(1-intervention.df[i, "Prop_Reduction_Severe_Trans"])
      pModelInt["b3"]=pModelInt["b3"]*(1-intervention.df[i, "Prop_Reduction_Death"])
      
      RoInt=GetRo_SEIR(pModelInt,N)
      
      # start time of intervention
      
      Tint=as.numeric(intervention.df[i, "start_time"])
      Tend=as.numeric(intervention.df[i, "end_time"])
      
      if (i==1){
        iInt=which.min(abs(Tint-out$time)) # find nearest time to Tint
        
        # Set initial conditions and time interval
        S0 = out.df[iInt,"S"]
        E0 = out.df[iInt,"E"]
        I00 = out.df[iInt,"I0"]
        I10 = out.df[iInt,"I1"]
        I20 = out.df[iInt,"I2"]
        I30 = out.df[iInt,"I3"]
        D0 = out.df[iInt,"D"]
        R0 = out.df[iInt,"R"]
        
      }else{
        #eg another intervention - add to the end
        iInt <- nrow(full.outInt.df)
        S0 = full.outInt.df[iInt,"S"]
        E0 = full.outInt.df[iInt,"E"]
        I00 = full.outInt.df[iInt,"I0"]
        I10 = full.outInt.df[iInt,"I1"]
        I20 = full.outInt.df[iInt,"I2"]
        I30 = full.outInt.df[iInt,"I3"]
        D0 = full.outInt.df[iInt,"D"]
        R0 = full.outInt.df[iInt,"R"]
        
      }
      
      #    y0 = c(S=S0, E=E0, I1=I10, I2=I20, I3=I30, R=R0, D=D0)
      y0 = c(S=S0, E=E0, I0=I00, I1=I10, I2=I20, I3=I30, R=R0, D=D0)
      
      #Run intervention time course until Tend. Up to time Tint, use baseline solution
      Trun=Tend-Tint
      
      outInt.df=GetSpread_SEIR(pModelInt,N,Trun,y0)
      outInt.df$time=outInt.df$time+Tint
      
      if (i != nrow(intervention.df)){
        # eg not at end of all active interventions
        if (intervention.df[i, "end_time"] == intervention.df[i+1, "start_time"]){
          # go immediately into next intervention; no need to run with baseline
          full.outInt.df <- rbind.data.frame(full.outInt.df, outInt.df)
        }else{
          # temp return to baseline
          Trun2=intervention.df[i+1, "start_time"]-Tend
          
          #Set initial conditions and time interval
          #Round all numbers to lowest intergar, so if less than 1, go to zero
          iEnd=nrow(outInt.df)
          S0 = outInt.df[iEnd,"S"]
          E0 = outInt.df[iEnd,"E"]
          I00 = outInt.df[iEnd,"I0"]
          I10 = outInt.df[iEnd,"I1"]
          I20 = outInt.df[iEnd,"I2"]
          I30 = outInt.df[iEnd,"I3"]
          D0 = outInt.df[iEnd,"D"]
          R0 = outInt.df[iEnd,"R"]
          
          y0 = c(S=S0, E=E0, I0=I00, I1=I10, I2=I20, I3=I30, R=R0, D=D0)
          # y0 = c(S=S0, E=E0, I1=I10, I2=I20, I3=I30, R=R0, D=D0)
          
          #run with parameters back to baseline
          outIntOff.df=GetSpread_SEIR(pModel,N,Trun2,y0)
          outIntOff.df$time=outIntOff.df$time+Tend
          
          #combine vectors
          outInt.df=rbind(outInt.df,outIntOff.df)
          
          full.outInt.df <- rbind.data.frame(full.outInt.df, outInt.df)
          #continue on to next intervention
        }
        
      }else{
        full.outInt.df <- rbind.data.frame(full.outInt.df, outInt.df)
      }
    }
    #after interventions - check to see if at end of simulation time
    #Tend holds on to last intervention's end date from within for-loop
    Trun2=Tmax-Tend
    
    if(Trun2<=0){
    }else{
      #Set initial conditions and time interval
      #Round all numbers to lowest intergar, so if less than 1, go to zero
      iEnd=nrow(full.outInt.df)
      S0 = full.outInt.df[iEnd,"S"]
      E0 = full.outInt.df[iEnd,"E"]
      I00 = full.outInt.df[iEnd,"I0"]
      I10 = full.outInt.df[iEnd,"I1"]
      I20 = full.outInt.df[iEnd,"I2"]
      I30 = full.outInt.df[iEnd,"I3"]
      D0 = full.outInt.df[iEnd,"D"]
      R0 = full.outInt.df[iEnd,"R"]
      
      y0 = c(S=S0, E=E0, I0=I00, I1=I10, I2=I20, I3=I30, R=R0, D=D0)
      # y0 = c(S=S0, E=E0, I1=I10, I2=I20, I3=I30, R=R0, D=D0)
      
      #run with parameters back to baseline
      outIntOff.df=GetSpread_SEIR(pModel,N,Trun2,y0)
      outIntOff.df$time=outIntOff.df$time+Tend
      
      #combine vectors
      full.outInt.df=rbind(full.outInt.df,outIntOff.df)
    }
  }
  
  #subset the relevant variables and add in a column for capacity
  capParams=SetHospCapacitySliders(input)
  init_date <- graph.start.date
  
  if(input$model.comp.population.type=="I3"){
    
    out.df$value=out.df[,"I3"] # create observed variable
    out.df$Intervention="Do Nothing" # add intervention column
    
    if (nrow(full.outInt.df)>0){
      full.outInt.df$value=full.outInt.df[,"I3"]
      full.outInt.df$Intervention="NPI Strategy"
      outAll.df=rbind(out.df,full.outInt.df) #combine baseline and intervention
    }else{
      outAll.df <- out.df
    }
    
    outAll.sub=subset(outAll.df, select=c("time","value","Intervention")) # choose only case column
    outAll.sub$Intervention=factor(outAll.sub$Intervention) # set intervention as factor
    outAll.sub=outAll.sub[with(outAll.sub,order(Intervention,time)),]
    
    combData <- outAll.sub %>%
      #union_all(capData) %>%
      mutate(date = as.Date(init_date) + ddays(time))
    
    combData.pre.inter <- combData %>%
      filter(time < min(time[Intervention=="NPI Strategy"], na.rm = TRUE)) %>%
      filter(Intervention == "Do Nothing") %>%
      mutate(Intervention = "NPI Strategy")
    
    #pre intervention time
    combData <- combData %>%
      union_all(combData.pre.inter)
    
  }else if(input$model.comp.population.type=="I2_I3"){
    
    out.df$value=rowSums(out.df[,c("I2","I3")]) # create observed variable
    out.df$Intervention="Do Nothing" # add intervention column
    
    if (nrow(full.outInt.df)>0){
      full.outInt.df$value=rowSums(full.outInt.df[,c("I2","I3")])
      full.outInt.df$Intervention="NPI Strategy"
      outAll.df=rbind(out.df,full.outInt.df) #combine baseline and intervention
    }else{
      outAll.df <- out.df
    }
    
    outAll.sub=subset(outAll.df, select=c("time","value","Intervention")) # choose only case column
    outAll.sub$Intervention=factor(outAll.sub$Intervention) # set intervention as factor
    outAll.sub=outAll.sub[with(outAll.sub,order(Intervention,time)),]
    
    combData <- outAll.sub %>%
      mutate(date = as.Date(init_date) + ddays(time))
    
    combData.pre.inter <- combData %>%
      filter(time < min(time[Intervention=="NPI Strategy"], na.rm = TRUE)) %>%
      filter(Intervention == "Do Nothing") %>%
      mutate(Intervention = "NPI Strategy")
    
    #pre intervention time
    combData <- combData %>%
      union_all(combData.pre.inter)
    
    
  }else if(input$model.comp.population.type=="D"){
    
    out.df$value=out.df[,"D"] # create observed variable
    out.df$Intervention="Do Nothing" # add intervention column
    
    if (nrow(full.outInt.df)>0){
      full.outInt.df$value=full.outInt.df[,"D"]
      full.outInt.df$Intervention="NPI Strategy"
      outAll.df=rbind(out.df,full.outInt.df) #combine baseline and intervention
    }else{
      outAll.df <- out.df
    }
    
    outAll.sub=subset(outAll.df, select=c("time","value","Intervention")) # choose only case column
    outAll.sub$Intervention=factor(outAll.sub$Intervention) # set intervention as factor
    outAll.sub=outAll.sub[with(outAll.sub,order(Intervention,time)),]
    
    capData=data.frame("time"=seq(0, Tmax, length.out = 1e3),
                       "value"=rep(1,1e3)*capParams["AvailICUBeds"]*(N/1000), 
                       "Intervention"="Available ICU Beds")
    
    combData <- outAll.sub %>%
      #union_all(capData) %>% KRF removing
      mutate(date = as.Date(init_date) + ddays(time))
    
    combData.pre.inter <- combData %>%
      filter(time < min(time[Intervention=="NPI Strategy"], na.rm = TRUE)) %>%
      filter(Intervention == "Do Nothing") %>%
      mutate(Intervention = "NPI Strategy")
    
    #pre intervention time
    combData <- combData %>%
      union_all(combData.pre.inter)
    
  }else if (input$model.comp.population.type=="I1_I2_I3"){
    out.df$value=rowSums(out.df[,c("I1", "I2","I3")]) # create observed variable
    out.df$Intervention="Do Nothing" # add intervention column
    
    if (nrow(full.outInt.df)>0){
      full.outInt.df$value=rowSums(full.outInt.df[,c("I1","I2","I3")])
      full.outInt.df$Intervention="NPI Strategy"
      outAll.df=rbind(out.df,full.outInt.df) #combine baseline and intervention
    }else{
      outAll.df <- out.df
    }
    
    outAll.sub=subset(outAll.df, select=c("time","value","Intervention")) # choose only case column
    outAll.sub$Intervention=factor(outAll.sub$Intervention) # set intervention as factor
    outAll.sub=outAll.sub[with(outAll.sub,order(Intervention,time)),]
    
    combData <- outAll.sub %>%
      mutate(date = as.Date(init_date) + ddays(time))
    
    combData.pre.inter <- combData %>%
      filter(time < min(time[Intervention=="NPI Strategy"], na.rm = TRUE)) %>%
      filter(Intervention == "Do Nothing") %>%
      mutate(Intervention = "NPI Strategy")
    
    #pre intervention time
    combData <- combData %>%
      union_all(combData.pre.inter)
  }
  
  ret.dat <- combData %>%
    filter(Intervention == "NPI Strategy") %>%
    mutate(Intervention = "MITRE SEIR") %>%
    arrange(date)
  
  return(ret.dat)
}

draw_model_comparison_plot <- function(dat, selected.y, selected.state){
  
  if (nrow(dat)>0){
    model.opts <- c("MITRE SEIR", "C19HCC Short-term", "C19HCC Ensemble", "Columbia University (40% Contact Reduction)", "Columbia University (30% Contact Reduction)",
                    "Columbia University (20% Contact Reduction)", "IHME", "UT Austin", "University of Geneva",
                    "Northeastern (MOBS)", "Youyang Gu (YYG)")
    model.opts.short <- c("MITRE SEIR", "C19HCC Short-term","C19HCC Ensemble", "Columbia (40%)","Columbia (30%)","Columbia (20%)", "IHME", "UT-Austin", "Geneva", "MOBS", "YYG")
    names(model.opts.short) <- model.opts
    col.pal <- c(viridis(length(model.opts), end = 0.85), "#000000")
    names(col.pal) <- c(model.opts, "Observed Data")
    
    current.day <- Sys.Date()
    
    plt.opts <- c("C"="Cumulative Hospitalizations", "D"= "Cumulative Deaths",
                  "DeltaD"= "New Daily Deaths", "DeltaI2_I3" = "New Hospitalizations")
    
    plot.title <- paste(selected.state, "COVID-19 Modeled", plt.opts[selected.y])
    
    real.plt.dat <- STATE_INF_STATS %>% filter(Country.Region=='United States')
    if(selected.state=='United States'){
      real.plt.dat = real.plt.dat %>%
        group_by(date) %>%
        summarise(
          case_count = sum(case_count, na.rm=T),
          death_count = sum(death_count, na.rm=T),
          recovered_count = sum(recovered_count, na.rm=T)
        ) %>%
        mutate(Country.Region='United States',
               mState.Providence='United States', 
               firstCaseDate = min(select(filter(real.plt.dat, case_count > 0), date)[[1]]),
               firstDeathDate = min(select(filter(real.plt.dat, death_count > 0), date)[[1]])) %>%
        select(mState.Providence, Country.Region, date, C=case_count, D=death_count)
      # real.plt.dat$firstCaseDate = min(select(filter(real.plt.dat, case_count > 0), date)[[1]])
      # real.plt.dat$firstDeathDate = min(select(filter(real.plt.dat, death_count > 0), date)[[1]])
    }else{
      real.plt.dat = real.plt.dat %>%
      filter(mState.Providence == selected.state,
             date >= as.Date(firstCaseDate)-days(1)) %>%
      arrange(date) %>%
      select(mState.Providence, Country.Region, date, C=case_count, D=death_count) %>%
      distinct() 
    }
    real.plt.dat = real.plt.dat %>%
      mutate(
        D = ifelse(D > lead(D,n=1,default = Inf), floor((lag(D,n=1,default = 0)+lead(D,n=1,default = NA))/2) , D), #data issue; take avg
        DeltaD=D-lag(D, n=1, default=0),
        DeltaI2_I3 = C-lag(C,n=1,default = 0),
        trace.text = paste0("<b>Observed Data: </b>", get(selected.y)),
        full.model.name="Observed Data")
    
    date.line <- data.frame(date = c(real.plt.dat$date, dat$target_date)) %>%
      mutate(date = as.Date(date),
             date.ft = format(date, "%b %d, %Y"))
    
    dat <- dat %>%
      mutate(
        model.short = sapply(full.model.name, function(x){return(model.opts.short[x])}),
        trace.text = ifelse(!is.na(quant_0.25),
                            paste0("<b>", model.short, ":</b> ", round(point, 1), "(50% CI: ", round(quant_0.25,1),"-",round(quant_0.75,1),")"),
                            paste0("<b>", model.short, ":</b> ", round(point, 1))),
        ribbon.fill = sapply(full.model.name, function(x){return(col.pal[x])}),
      )
    
    
    xmin <- min(as.Date(dat$forecast_date), na.rm = TRUE) - dweeks(1)
    xmax <- max(as.Date(dat$target_date), na.rm=TRUE)
    
    plt <- plot_ly(colors = col.pal, type='scatter', mode='lines', height = 600) %>%
      
      add_ribbons(data = dat, x=~as.Date(target_date), y=~point, ymin = ~quant_0.25,ymax = ~quant_0.75,
                  line = list(color = 'transparent'),  hoverinfo="skip", fillcolor = dat$ribbon.fill,
                  opacity=0.1, showlegend=FALSE) %>%
      add_trace(
        data = dat, x = ~target_date, y = ~point, color = ~full.model.name, hovertext=~trace.text, hoverinfo="text"
      ) %>%
      add_trace(data=date.line, line = list(color="transparent"), hovertext = ~date.ft, hoverinfo = "text",
                x = ~date, y = 0, showlegend=FALSE) %>%
      add_markers(type="scatter", mode="markers",
                  data = real.plt.dat, x=~date, y = as.formula(paste0("~", selected.y)), color = ~full.model.name,
                  hovertext=~trace.text, hoverinfo="text") %>%
      layout(
        title = list(text=plot.title, x = 0, xref="paper"),
        font = list(family = "Arial"),
        legend=list(y=0.85),
        hovermode = "x",
        xaxis=list(title="",
                   range = c(xmin, xmax)),
        yaxis = list(title=plt.opts[selected.y]),
        shapes = list(list(type="line",
                           x0=current.day,x1=current.day,
                           xref="x",
                           yref= 'paper', y0= 0, y1= 1,
                           line = list(color="red"))),
        annotations = list(x = current.day,
                           y = 1,
                           text = "<i>Today</i>",
                           font = list(color="red"),
                           xref = "x",
                           yref = "paper",
                           showarrow = FALSE,
                           xanchor="left"),
        margin = list(t=42,b=57, pad=3)
      )
  }else{
    plt <- plot_ly()
  }

  return(plt)
  
}

get_mitre_gaussian_comp_df <- function(input){
  gp.dat <- GAUSSIAN_PROJ %>% 
    filter(full == input$model.comp.state) %>%
    mutate(date = as.Date(date))
  
  if (input$model.comp.population.type == "D" | ncol(GAUSSIAN_PROJ) == 7){
    gp.dat <- gp.dat %>%
      arrange(date) %>%
      mutate(value = cumsum(deaths_mean))
  }else{
    gp.dat <- gp.dat %>%
      arrange(date) %>%
      mutate(value = cumsum(cases_mean))
  }
  
  ret.dat <- gp.dat %>%
    mutate(model.type = "MITRE Gaussian Process") %>%
    select(date, model.type, value)
  
  return(ret.dat)
}




