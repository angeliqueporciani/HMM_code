# Notes générales sur ce qu'il me faut en automatisation.

La première étape consiste à regarder s'il y a une différence entre les groupes en terme de nombre d'état correspondant au meilleur modèle. 
Pour ça, il faut que je recupère les AIC de chaque modèle de 2 à 5 état par individu (200X5) avec les info de status et Food associée à chaque individu ça serait mieux.

Ensuite, si on voit qu'on a pas de différence marquée entre groupe, on part sur 4 états pour tous le monde et là il faut faire la selection de covariables, ce sont principalement des covariables temporelles. 
Pour cette étape on vas ajuster plusieurs modèles par ID en ajoutant/changeant les covar. Et en sortie il faut que j'ai l'AIC de chaque modèle, puis que je conserve le modèle (resultats du modèle momentuHMM) qui a eu le plus petit AIC pour pouvoir faire les graphiques sur ces "best" modeles par ID par la suite. 

Enfin, pour la représentation des données, il faudrait que j'ai tous les modèles selectionnée par ID dans une liste afin de pouvoir extraire : les probabilité stationnaire, les lambda et les proba de transition de chqe ID. Je sais pas encore comment stocker tout ça... 

