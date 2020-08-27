##################################################
## Project: C19HCC COVID-19 Modeling Dashboard and Tools
## Purpose: 
## Date: June 2020
## Developers: Brendan Abraham, Kristin Fitzgerald, Kyle Furlong, Dr. Chris Glazner
## Copyright 2020, The MITRE Corporation
## Approved for Public Release; Distribution Unlimited. Case Number 20-1521.
##################################################


###### WINDOWS
## pass array of differences to each function
## this is computed BEFORE the values are squared and summed !

## Rectangular Window
idWindow <- function (v) {
  v
}

## Sine Window
gSineW = NULL ## global calculated  window array for optimization
sineWindow <- function(v) {
  len = length(v)
  if(is.null(gSineW) | len!=length(gSineW)) {
    print("Using Sine Window...")
    SineW = sin(seq(0,pi,length.out = len))
    assign("gSineW", SineW, envir = .GlobalEnv)
  }
  v*gSineW
}

## Triangular Window
gTriW = NULL  ## global calculated  window array for optimization
triWindow <- function(v) {
  len = length(v)
  if(is.null(gTriW) | len!=length(gTriW)) {
    print("Using Tri Window...")
    TriW = c(seq(0,1-1/(len/2),length.out = len/2), seq(1,0,length.out = len-len/2))
    assign("gTriW", TriW, envir = .GlobalEnv)
  }
  v*gTriW
}

## Parzen Window
gParzenW = NULL  ## global calculated  window array for optimization
parzenWindow <- function(v) {
  len = length(v)
  if(is.null(gParzenW) | len!=length(gParzenW)) {
    print("Using Parzen Window...")
    N = len
    L = N+1
    
    w0 <- function(x) {
      if(abs(x) < L/4) {
        z = 1 - 6*(x/(L/2))^2 * (1 - abs(x)/(L/2))
      }else {
        z = 2*(1 - abs(x)/(L/2))^3
      }
      z
    }
    n = seq(0,N-1) - N/2
    ParzenW = sapply(n,FUN = w0) # w0(n - N/2)
    assign("gParzenW", ParzenW, envir = .GlobalEnv)
  }
  v*gParzenW
}

## Hann Window
gHannW = NULL  ## global calculated  window array for optimization
hannWindow <- function(v) {
  len = length(v)
  if(is.null(gHannW) | len!=length(gHannW)) {
    print("Using Hann Window...")
    HannW = sin(seq(0,pi,length.out = len))^2
    assign("gHannW", HannW, envir = .GlobalEnv)
  }
  v*gHannW
}

window_func <- function (v) {
  hannWindow(v)
}