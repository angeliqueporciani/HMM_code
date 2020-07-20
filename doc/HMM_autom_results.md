# Resultats HMM en automatique

## 1 Nombre d'états

Une de mes hypothèses était que selon le groupe (status physio * regime) on pouvait avoir ds nombre d'états detectés différent, du fait d'une différence d'activité. Pour vérifier cela j'ai ajusté des modèles de 2 à 5 états sans covariable pour chaque individu. 
J'ai récuperé l'AIC minimum pour chaque ID et le nombre d'état correspondant, les résultats sont présentés dans la table 1. 

|                 |  2|  3|  4|  5| Sum|
|:----------------|--:|--:|--:|--:|---:|
|InseminatedBlood |  8|  4|  8| 15|  35|
|InseminatedSugar |  5|  4| 11| 18|  38|
|VirginBlood      |  7|  5| 25| 13|  50|
|VirginSugar      | 11|  1| 16| 22|  50|
|Sum              | 31| 14| 60| 68| 173|
Table 1: Effectifs des groupes en fonction du nombre d'état. 

Il semble que les modèles qui s'ajuste le mieux sont des modèles à 5 états toute catégorie confondues.
Cependant, les modèles à 4 états sont aussi fréquement retrouvé et si on calcule la médianne pour chaque groupe on obtiens 4. Les modèles à 5 états sont peut être ceux qui s'ajuste le mieux, cependant, la significiation biologique des 5 états n'est pas évidente. En effet, dans la plupart des modèles à 5 états, les 2 premiers états detectés sont compris entre 0 et 1, d'un point de vue biologique il n'est pas possible de les différencier et il semble plus logique de les regrouper dans une même catégorie, correspondant à l'état de repos. 
