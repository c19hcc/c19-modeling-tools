---
title: "SEIR"
output:
  html_document:
    df_print: paged
editor_options:
  chunk_output_type: inline
---
## SEIR Model Description


![Model flowchart](images/model_diagram_small.png)

We use a compartmental epidemiological model, based on the classic SEIR model, to describe the spread and clinical progression of COVID-19. A nice primer to this sort of model is available on [Wikipedia](https://en.wikipedia.org/wiki/Compartmental_models_in_epidemiology).  It is important to track the different clinical outcomes of infection, since they require different level of healthcare resources to care for and may be tested and isolated at different rates. Susceptible ($S$) individuals who become infected start out in an exposed class $E$, where they are asymptomatic and do not transmit infection. The rate of progressing from the exposed stage to the infected stage $I$, where the individual is symptomatic and infectious, occurs at rate $a$. The clinical descriptions of the different stages of infection are given below. Infected individuals begin with *mild* infection ($I_1$), from which they either recover, at rate $\gamma_1$, or progress to *severe* infection ($I_2$), at rate $p_1$. Severe infection resolves at rate $\gamma_2$ or progresses to a critical stage ($I_3$) at rate $p_2$. $p_2$ is further adjusted in response to hospital bed capacity being reached. Individuals with critical infection recover at rate $\gamma_3$ and die at rate $\mu$. $\mu$ is further adjusted in response to hospital ventilator capacity being reached. Recovered individuals are tracked by class $R$ and are assumed to be protected from re-infection for life. Individuals may transmit the infection at any stage, though with different rates. The  transmission rate in stage $i$ is described by $\beta_i$ . A new subpopulation for *asymptomatic* infections  ($I_0$) has been added, that only has a recovery rate ($\gamma_0$) and no progression, but can infect anyone via transmission rate $\beta_0$. This will allow modeling of a recently identified group that may explaqin why COVID-19 seems so contagious (see https://www.sciencemag.org/news/2020/03/cellphone-tracking-could-help-stem-spread-coronavirus-privacy-price).
 
### Equations

\begin{equation}
\dot{S} = -\beta_1 I_1 S -\beta_2 I_2 S - \beta_3 I_3 S
\end{equation}

\begin{equation}
\dot{E} =\beta_1 I_1 S +\beta_2 I_2 S + \beta_3 I_3 S - (a + a_0) E \\
\end{equation}

\begin{equation}
\dot{I_0} = a_0 E - \gamma_0 I_0 \\
\end{equation}

\begin{equation}
\dot{I_1} = a E - \gamma_1 I_1 - p_1 I_1 \\
\end{equation}

\begin{equation}
p _ \texttt{2\_hc} = p_2 + \dfrac{pmax_2 - p_2}{1 + (\frac{hc}{I_2 + \epsilon}) ^ m  } \\
\end{equation}

\begin{equation}
\dot{I_2} = p_1 I_1 -\gamma_2 I_2 - p _ \texttt{2\_hc} I_2 \\
\end{equation}

\begin{equation}
\mu_{vc} = \dfrac{\mu}{abs(1 - (\frac{I_3}{vc}) ^ m) + \epsilon} \\
\end{equation}

\begin{equation}
\dot{I_3} = p _ \texttt{2\_hc} I_2 - \gamma_3 I_3 - \mu _ {vc} I_3 \\
\end{equation}

\begin{equation}
\dot{R} = \gamma_1 I_1 + \gamma_2 I_2 + \gamma_3 I_3 \\
\end{equation}

\begin{equation}
\dot{D}  = \mu_{vc} I_3 \\
\end{equation}



### Variables
* $S$: Susceptible individuals
* $E$: Exposed individuals - infected but not yet infectious or symptomatic
* $I_i$: Infected individuals in severity class $i$. Severity increaes with $i$ and we assume individuals must pass through all previous classes
  * $I_0$: Asymptomatic infection 
  * $I_1$: Mild infection 
  * $I_2$: Severe infection 
  * $I_3$: Critical infection 
* $R$: individuals who have recovered from disease and are now immune
* $D$: Dead individuals
* $N=S+E+I_1+I_2+I_3+R+D$ Total population size (constant)

### Rate Parameters
* $\beta_i$ rate at which infected individuals in class $I_i$ contact susceptibles and infect them
* $\beta_0 = \beta_1 * AsymptoCrossSect$ infection rate for asymptomatics scaled relative to $\beta_1$
* $a$ rate of progression from the exposed to infected class
* $a_0$ rate of progression from the exposed to asymptomatic (hidden) infected class
* $\gamma_i$ rate at which infected individuals in class $I_i$ recover from disease and become immune
* $p_i$ rate at which infected individuals in class $I_i$ progress to class $I_{i+1}$
* $p _ \texttt{2\_hc}$ adjusted $p_2$ by hospital bed capacity
* $\mu$ death rate for individuals in the most severe stage of disease
* $\mu _ {vc}$ adjusted $\mu$ by ventilator capacity
* $m$ cutoff steepness for saturatible hospital capacities

### Hospital Parameters
* $hc$ number of hospital beds per 1000 ppl
* $ic$ number of ICU beds per 1000 ppl
* $vc$ number of (combined) ventilators per 1000 ppl

### Clinical stages

* Mild infection - These individuals have symptoms like fever and cough and may have mild pneumonia.  Hospitalization is not required (though in many countries such individuals are also hospitalized)
* Severe infection - These individuals have more severe pneumonia that leads to dyspnea, respiratory frequency <30/min, blood oxygen saturation <93%, partial pressure of arterial oxygen to fraction of inspired oxygen ratio <300, and/or lung infiltrates >50% within 24 to 48 hours. Hospitalization and supplemental oxygen are generally required.
* Critical infection - These individuals experience respiratory failure, septic shock, and/or multiple organ dysfunction or failure. Treatment in an ICU, often with mechanical ventilation, is required.


### Relating clinical observations to model parameters

To determine the model parameters consistent with current clinical data, we collect the following values from the slider values chosen by the user, and then use the formulas below to relate them to the rate parameters in the model. Note that the slider inputs for time intervals are averages durations. 

* IncubPeriod:  Average incubation period, days
* FracAsympto:  % of asymptomatic patients out of first-stage infected-cases
* AsymptoCrossSect: effective scaling of infection rate for asymptomatics relative to $\beta_1$
* DurMildInf: Average duration of mild infections, days
* FracMild: Average fraction of (symptomatic) infections that are mild
* FracSevere: Average fraction of (symptomatic) infections that are severe
* FracCritical: Average fraction of (symptomatic) infections that are critical
* CFR: Case fatality rate (fraction of infections that eventually result in death)
* DurHosp: Average duration of hospitalization for individuals with severe/critical infection, days
* TimeICUDeath: Average time from ICU admission to death, days

\begin{equation}
  a = \frac{1}{IncubPeriod}
\end{equation}

<!-- #g1=(1/DurMildInf)*FracMild -->
\begin{equation}
  \gamma_1 = \frac{1}{DurMildInf} * FracMild
\end{equation}

<!-- #p1=(1/DurMildInf)-g1 -->
\begin{equation}
  p_1 = \frac{1}{DurMildInf} - \gamma_1
\end{equation}

<!-- #p2=(1/DurHosp)*(FracCritical/(FracSevere+FracCritical)) -->
\begin{equation}
  p_2 = \frac{1}{DurHosp} * \frac{FracCritical}{(FracSevere+FracCritical)}
\end{equation}

<!-- #g2=(1/DurHosp)-p2 -->
\begin{equation}
  \gamma_2 = \frac{1}{DurHosp} - p_2
\end{equation}

<!-- #u=(1/TimeICUDeath)*(CFR/FracCritical) -->
\begin{equation}
  u = \frac{1}{TimeICUDeath} * \frac{CFR}{FracCritical}
\end{equation}

<!-- #g3=(1/TimeICUDeath)-u -->
\begin{equation}
  \gamma_3 = \frac{1}{TimeICUDeath} - u
\end{equation}



### Basic reproductive ratio

Idea: $R_0$ is the sum of 
1. the average number of secondary infections generated from an individual in stage $I_1$
2. the probability that an infected individual progresses to $I_2$ multiplied by the average number of secondary infections generated from an individual in stage $I_2$
3.  the probability that an infected individual progresses to $I_3$ multiplied by the average number of secondary infections generated from an individual in stage $I_3$

\begin{equation}
R_0  = N\frac{\beta_1}{p_1+\gamma_1} + N\frac{\beta_0}{\gamma_0} + \frac{p_1}{p_1 + \gamma_1} \left( \frac{N \beta_2}{p _ \texttt{2\_hc}+\gamma_2} + \frac{p _ \texttt{2\_hc}}{p _ \texttt{2\_hc} + \gamma_2} \frac{N \beta_3}{\mu _ {vc} + \gamma_3}\right)
\end{equation}

\begin{equation}
 = N\left( \frac{\beta_0}{\gamma_0} + \frac{\beta_1}{p_1+\gamma_1} \left(1 + \frac{p_1}{p _ \texttt{2\_hc} + \gamma_2}\frac{\beta_2}{\beta_1} \left( 1 + \frac{p _ \texttt{2\_hc}}{\mu _ {vc} + \gamma_3} \frac{\beta_3}{\beta_2} \right) \right) \right)
\end{equation}

Calculations using the next generation matrix give the same results. 

### Early epidemic growth rate

Early in the epidemic, before susceptibles are depleted, the epidemic grows at an exponential rate $r$, which can also be described with doubling time T$_2$=ln(2)$/r$. During this phase all infected classes grow at the same rate.

### Assumptions

* This model is formulated as a system of differential equations and the output therefore represents the expected values of each quantity. It does not take into account stochastic events or report the expected variance in the variables, which can be large. 
* Individuals must pass through a mild stage before reaching a severe or critical stage
* Individuals must pass through a severe stage before reaching a critical stage
* Only individuals in a critical stage die
* Asymptomatic individuals do not progress further and can only be identified using PCR or antibody testing
