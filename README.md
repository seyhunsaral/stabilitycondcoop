# The Stability of Conditional Cooperation (Andreozzi, Ploner, Saral) 

This repository contains the files for the study: the data, R code, and z-Tree files. 
It also includes a zBrac compatible treatment file and the language template for easy translation. (To find more information about zBrac [click here](https://www.github.com/seyhunsaral/zbrac))


## Glossary for z-Tree Files
| Variable        | Table    | Type      | Description                                                     | 
|-----------------|----------|-----------|-----------------------------------------------------------------| 
| IE              | globals  | constant  | Initial Endowment                                               | 
| IEg             | globals  | constant  | Initial Endowment for the beliefs                               | 
| cL              | globals  | constant  | Low contribution amount                                         | 
| cM              | globals  | constant  | Medium contribution amount                                      | 
| cH              | globals  | constant  | High contribution amount                                        | 
| PeriodSelected  | globals  | constant  | Selected period for payments                                    | 
| RegularTimeOut  | globals  | constant  | Timeout for stages                                              | 
| Debug           | globals  | debugging | Debug mode on (1) off (0)                                       | 
| DebugTimeOut    | globals  | debugging | Timeout when debug mode is on                                   | 
| dbgFocusOnStage | globals  | debugging | Stage to go directly when debug mode is on                      | 
| dbgShouldLeave  | globals  | debugging | Condition to leave stage when debug mode is on                  | 
| dbgCurrentStage | globals  | debugging | Stage number to be used for skipping to a certain stage         | 
| FirstPlayer     | subjects | game      | 1 if the player is selected to be the first player, 0 otherwise | 
| uC              | subjects | game      | choice as the first player                                      | 
| ccL             | subjects | game      | conditional choice if the other player chooses L (0)            | 
| ccM             | subjects | game      | conditional choice if the other player chooses M  (50)          | 
| ccH             | subjects | game      | conditional choice if the other player chooses H  (100)         | 
| ouc             | subjects | game      | opponent's choice of the opponent                               | 
| occL            | subjects | game      | opponent's conditional choice for L (0)                         | 
| occM            | subjects | game      | opponent's conditional choice for M (50)                        | 
| occH            | subjects | game      | opponent's conditional choice for H (100)                       |
