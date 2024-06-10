#Projet
install.packages("expm")
install.packages('pracma')
install.packages("ggplot2")

# librairie pour plot les courbes
library(ggplot2)

# librairie pour mettre à la puissance une matrice "%^%"
library(expm)

#
library('pracma')

# librairies ACP
library("FactoMineR")
library("factoextra")

# librairie pour la corrélation
library("corrplot")
library("ade4")
library(dplyr)

#Exercice 1
exercice1 <- function(){
  
  # Initialisation de la matrice  construite à partir des trois suites u,v et w
  A = rbind(c(1, 0, 2), c(0, -1, 0), c(2, 0, 1))
  print(A)
  # Calcul des vecteurs et valeurs propres
  valeur_propre = round(eigen(A)$values)
  vecteur_propre = round(eigen(A)$vectors)
  multiplicitee_vp = table(valeur_propre)
  
  print(valeur_propre)
  print(vecteur_propre)
  print(multiplicitee_vp)
  
  # Initialisation des matrices et P, P-1 et D
  P = round(eigen(A)$vectors)
  D = diag(valeur_propre)
  P_inv = solve(P)
  
  # vérification de la diagonalisation
  all.equal(A, P%*%D%*%P_inv)
  
  
  u0=1
  v0=2
  w0=3
  # Initialisation du vecteur représentant la suite
  X0=c(u0, v0, w0)
  
  n <- readline(prompt="Entrer un entier n positif: ")
  n = as.integer(n)
  X_n =P %*% (D %^% n)  %*% P_inv%*% X0
  #Afficher les résultats
  print(paste0("U_",n," = ",X_n[1,]))
  print(paste0("V_",n," = ",X_n[2,]))
  print(paste0("W_",n," = ",X_n[3,]))  
}

#Exercice 2
exercice2 <- function(){
  
  choose_degree <- function(x, y, max_degree) {
    degrees <- 1:max_degree
    aics <- rep(0, max_degree)
    
    for (degree in degrees) {
      model <- lm(unlist(y) ~ poly(x, degree, raw = TRUE))
      aics[degree] <- AIC(model)
    }
    
    best_degree <- which.min(aics)
    
    return(best_degree)
  }
  
  # nouvelle fonction regarder a 5 * epsilon les variations sur y
  choose_degree_epsilon <- function(x, y, epsilon, sensitivity = 5) {
    
    #on considere que l'on peut considerer un changement de direction significatif s'il y a un ecart de 5 fois epsilon
    acceptable_deviation = sensitivity * epsilon
    
    y = unlist(y)
    
    #init iteration sur y
    y_init = y[1]
    
    #init degré du polynome
    poly_degree = 0
    
    #-1 or +1
    last_direction = 0
    
    for(i in y)  {
      #definition d'un treshold max et min selon la derniere valeur de y considérée
      treshold_max = y_init + acceptable_deviation
      treshold_min = y_init - acceptable_deviation
      
      #
      if (i>=treshold_max){
        if (last_direction == 1){
        }
        
        else{
          poly_degree = poly_degree + 1
        }
        
        last_direction = 1
        y_init = i
      }
      else if (i<=treshold_min){
        if (last_direction == -1){
        }
        
        else{
          poly_degree = poly_degree + 1
        }
        
        last_direction = -1
        y_init = i
      }
      
      else {
      }
    }
    
    if (poly_degree == 0){
      poly_degree = 1
    }
    else{
    }
    return(poly_degree)
  }
  
  
  determine_polynome <- function(x, y, degree) {
    
    y = unlist(y)  # Degré du polynôme d'approximation choisi
    
    model <- lm(y ~ poly(x, degree, raw = TRUE))
    
    
    # Tracé du polynôme d'approximation
    plot(x, y, pch = 16, xlim = range(x), ylim = range(y), xlab = "x", ylab = "y", main = "Polynôme d'approximation")
    curve(predict(model, newdata = data.frame(x = x)), add = TRUE, col = "red", lwd = 5)
    
    return(model)
  }
  
  n <- readline(prompt="Entrer le nombre de point : ")
  a <- readline(prompt="Entrer la plus petite valeur de l'abscisse : ")
  b <- readline(prompt="Entrer la plus grande valeur de l'abscisse : ")
  c <- readline(prompt="Entrer la plus petite valeur de l'ordonnée : ")
  d <- readline(prompt="Entrer la plus grande valeur de l'ordonnée : ")
  epsilon <- readline(prompt="Entrer la valeur de epsilon : ")
  #Pour augmenter la sensibilité du degré du polynome il faut baisser le chiffre
  sensibilite <- readline(prompt="Entrer la sensibilité du degré du polynome : ")
  
  n = as.integer(n)
  a = as.integer(a)
  b = as.integer(b)
  
  c = as.integer(c)
  d = as.integer(d)
  
  epsilon = as.double(epsilon)
  sensibilite = as.integer(sensibilite)
  #for x equally spaced use linspace
  #X = linspace(a, b, n)
  
  #for x randomly spaced, use runif
  X = runif(n, a, b)
  
  X = sort(X)
  #init loop
  Y = list()
  i = 0
  
  #creating random rumber between c and d for first number
  y_previous = runif(1, c, d)
  while (i<n){
    Y_new = runif(1, y_previous - epsilon, y_previous + epsilon)
    
    if (Y_new>d){
      
      Y_new<-d
      
    } else if (Y_new<c){
      
      Y_new<-c
      
    }else {
      
      #Y_new is between c and d
    }
    
    
    Y = append(Y, Y_new)
    y_previous = Y_new
    i = i + 1
  }
  plot(x = X, y = Y)
  points(X, Y, pch = 16, col = "red")
  #print(X)
  #print(unlist(Y))
  
  best_degree = choose_degree_epsilon(X, Y, epsilon, sensibilite)
  
  model = determine_polynome(X, Y, best_degree)
  
  print(paste('Le polynome conseillé pour une sensibilitée de ', sensibilite, ' est de : ', best_degree, sep=""))
  
  print(model)
  
  #summary(model)  # Afficher un résumé des résultats de la régression
  
  #Question 4
  determine_epsilon <- function(x, y) {
    
    difference_precedente = 0
    for(i in 2:length(y))  {
      
      value_y_prec = y[i-1]
      value_y_next = y[i]
      
      difference = abs(value_y_next - value_y_prec)
      
      if (difference > difference_precedente){
        difference_precedente = difference
      }
      else{
      }
    }
    
    return(difference_precedente)
    
  }
  
  viz_exo4_get_epsilon <- function(x,y) {
    epsilon = determine_epsilon(x, y)
    print(paste('Le Epsilon pour ce nuage de point est de  ', epsilon, sep=""))
    
    plot(x = x, y = y)
    points(x, y, pch = 16, col = "red")
    
    return(epsilon)
  }
  
  x <- c(0, 1, 2, 3, 4, 6, 7, 9, 11, 12, 15, 16, 17, 18, 20)
  y <- c(2, 1, 0, -1, -3, -1, 0, 2, 4, 5, 7, 10, 8, -3, -10)
  
  epsilon = viz_exo4_get_epsilon(x, y)
  
  sensibilite = 0.2
  best_degree = choose_degree_epsilon(x, y, epsilon, sensibilite)
  
  print(paste('Le Degré pour ce nuage de point est de  ', best_degree, sep=""))
  
  fit_poly_and_predictions <- function(x,y, degre = best_degree) {
    #prenons un polynome de degré 3 pour approcher ce nuage de points
    
    model = determine_polynome(x, y, degre)
    
    round_param = lapply(model[1],round,2)
    
    print(round_param)
    
    # Polynôme d'interpolation
    interpolation_poly <- predict(model, newdata = data.frame(x = x))
    
    # Prévisions
    pred_1 = 22
    pred_2 = 25
    pred_3 = 50
    
    predictions <- predict(model, newdata = data.frame(x = c(pred_1, pred_2, pred_3)))
    
    predictions = round(predictions, digits = 2)
    
    print(paste('La prévision pour x = ', pred_1, ' est de ', predictions[1], sep=""))
    print(paste('La prévision pour x = ', pred_2, ' est de ', predictions[2], sep=""))
    print(paste('La prévision pour x = ', pred_3, ' est de ', predictions[3], sep=""))
    
  }
  
  #degre <- readline(prompt="Entrer le degre du polynome : ")
  
  fit_poly_and_predictions(x, y)
}





#Exercice 3 et 4

modele_additif <- function(data, T) {
  data <- as.vector(t(data)) # Transpose la matrice avant de la convertir en vecteur
  size_data = length(data)
  size = length(data) + T
  t <- seq(1, size)
  
  ventes_df <- data.frame(t = t[1:size], Xt = data[1:size])
  
  # Question 1 et 2
  # Les valeurs les plus élevées et les plus faibles pour chaque période
  indices <- seq(1, size_data, by = T)  # Indices de début de chaque période de 4 éléments
  max_values <- vector()
  min_values <- vector()
  period_indices_max <- vector()
  period_indices_min <- vector()
  
  for (i in indices) {
    period <- data[i:(i+T-1)]
    max_value <- max(period)
    min_value <- min(period)
    
    max_values <- c(max_values, max_value)
    min_values <- c(min_values, min_value)
    period_indices_max <- c(period_indices_max, which.max(period) + i - 1)
    period_indices_min <- c(period_indices_min, which.min(period) + i - 1)
  }
  
  highest <- data.frame(max_value = max_values, t = period_indices_max)
  lowest <- data.frame(min_value = min_values, t = period_indices_min)
  
  # Ajustement des modèles linéaires
  model_high <- lm(max_value ~ t, data = highest)
  model_low <- lm(min_value ~ t, data = lowest)
  
  # Création d'un nouveau dataframe pour l'enveloppe haute et basse
  new_data <- data.frame(t = seq(1, size_data, by = 1))
  
  # Prédiction pour les nouveaux points de données
  new_data$high_pred <- predict(model_high, newdata = new_data)
  new_data$low_pred <- predict(model_low, newdata = new_data)
  
  # Droite d'ajustement affine
  modele <- lm(Xt ~ t, data = ventes_df[1:size_data,])
  coeff <- coef(modele)
  a <- coeff["t"]
  b <- coeff["(Intercept)"]
  cat("Tt : y =", a, "x +", b, "\n")
  
  # Création du graphique
  print(ggplot(ventes_df[1:size_data,], aes(x = t, y = Xt, group = 1))  +
          geom_line() +
          geom_point() +
          geom_line(data = new_data, aes(x = t, y = high_pred), color = "blue") +
          geom_line(data = new_data, aes(x = t, y = low_pred), color = "blue") +
          geom_abline(intercept = b, slope = a, color = "blue", linetype = "dashed") +
          labs(x = "Trimestre", y = "Ventes")) 
  
  
  # Question 3
  
  # Moyenne mobile d'ordre 4
  calc_moyenne_mobile <- function(x) {
    n <- length(x)
    moyennes <- rep(NA, n)
    for (t in 3:(n-2)) {
      moyennes[t] <- round((1/8)*(x[t-2] + 2*x[t-1] + 2*x[t] + 2*x[t+1] + x[t+2]),2)
    }
    return(moyennes)
  }
  
  ventes_df$Mt <- calc_moyenne_mobile(ventes_df$Xt)
  print(ventes_df)
  
  # Tendance estimée en fonction des moyennes mobiles
  modele_Tt <- lm(Mt ~ t, data = ventes_df, na.action = na.exclude)  # on exclut les NA pour le calcul de la régression
  coeff_Tt <- coef(modele_Tt)
  a_Tt <- coeff_Tt["t"]
  b_Tt <- coeff_Tt["(Intercept)"]
  cat("Tt (en fonction des Mt) : y =", a_Tt, "x +", b_Tt, "\n")
  ventes_df$Tt <- round(a_Tt * ventes_df$t + b_Tt,2)
  
  # Différences saisonnières en utilisant les moyennes mobiles
  ventes_df$St_chapeau <- round(ventes_df$Xt - ventes_df$Mt,2)
  print(ventes_df)
  
  # Question 4
  
  # Composante saisonnière
  
  calc_St <- function(df) {
    # Calcul des indices des trimestres
    trimestre <- rep(1:T, times = size_data/T + 1)
    
    # Calcul des St_chapeau moyens pour chaque trimestre (applique la fonction mean à chaque groupe de valeurs dans St_chapeau, où les groupes sont définis par le vecteur trimestre)
    St <- round(tapply(df$St_chapeau, trimestre, mean, na.rm = TRUE),2)
    
    return(St)
  }
  
  ventes_df$St <- calc_St(ventes_df)
  print(ventes_df)
  
  # Coefficients saisonniers corrigés
  ventes_df$St_prime <- round(ventes_df$St - mean(ventes_df$St, na.rm = TRUE),2)
  
  # Calcul de la série désaisonnalisée
  ventes_df$Xt_prime <- round(ventes_df$Xt - ventes_df$St_prime,2)
  
  print(ventes_df)
  
  # Question 5
  
  # Prévision pour le quatrième trimestre de 2023
  ventes_df$Xt_prev <- round(ventes_df$Tt + ventes_df$St_prime,2)
  
  # Calcul d'erreur entre l'estimation et les données réelles
  ventes_df$erreur <- round(abs(ventes_df$Xt_prev - ventes_df$Xt),2)
  print(paste0("Erreur moyenne entre l'estimation et les données réelles pour le modèle additif: ", round(sum(ventes_df$erreur[1:size_data])/size_data,2)))
  
  print(ventes_df)
  
  print(ggplot(ventes_df, aes(x = t, y = Xt, group = 1)) +
    geom_point() +
    geom_line(aes(y = Xt), color = "black") +
    geom_line(aes(y = Xt_prev), color = "blue"))
}


modele_multiplicatif <- function(data, T) {
  data <- as.vector(t(data)) # Transpose la matrice avant de la convertir en vecteur
  size_data = length(data)
  size = length(data) + T
  t <- seq(1, size)
  
  ventes_df <- data.frame(t = t[1:size], Xt = data[1:size])
  
  # Question 1 et 2
  # Les valeurs les plus élevées et les plus faibles pour chaque période
  indices <- seq(1, size_data, by = T)  # Indices de début de chaque période de 4 éléments
  max_values <- vector()
  min_values <- vector()
  period_indices_max <- vector()
  period_indices_min <- vector()
  
  for (i in indices) {
    period <- data[i:(i+T-1)]
    max_value <- max(period)
    min_value <- min(period)
    
    max_values <- c(max_values, max_value)
    min_values <- c(min_values, min_value)
    period_indices_max <- c(period_indices_max, which.max(period) + i - 1)
    period_indices_min <- c(period_indices_min, which.min(period) + i - 1)
  }
  
  highest <- data.frame(max_value = max_values, t = period_indices_max)
  lowest <- data.frame(min_value = min_values, t = period_indices_min)
  
  # Ajustement des modèles linéaires
  model_high <- lm(max_value ~ t, data = highest)
  model_low <- lm(min_value ~ t, data = lowest)
  
  # Création d'un nouveau dataframe pour l'enveloppe haute et basse
  new_data <- data.frame(t = seq(1, size_data, by = 1))
  
  # Prédiction pour les nouveaux points de données
  new_data$high_pred <- predict(model_high, newdata = new_data)
  new_data$low_pred <- predict(model_low, newdata = new_data)
  
  # Droite d'ajustement affine
  modele <- lm(Xt ~ t, data = ventes_df[1:size_data,])
  coeff <- coef(modele)
  a <- coeff["t"]
  b <- coeff["(Intercept)"]
  cat("Tt : y =", a, "x +", b, "\n")
  
  # Création du graphique
  print(ggplot(ventes_df[1:size_data,], aes(x = t, y = Xt, group = 1))  +
          geom_line() +
          geom_point() +
          geom_line(data = new_data, aes(x = t, y = high_pred), color = "blue") +
          geom_line(data = new_data, aes(x = t, y = low_pred), color = "blue") +
          geom_abline(intercept = b, slope = a, color = "blue", linetype = "dashed") +
          labs(x = "Trimestre", y = "Ventes")) 
  
  # Question 3
  
  # Moyenne mobile d'ordre 4
  calc_moyenne_mobile <- function(x) {
    n <- length(x)
    moyennes <- rep(NA, n)
    for (t in 3:(n-2)) {
      moyennes[t] <- round((1/8)*(x[t-2] + 2*x[t-1] + 2*x[t] + 2*x[t+1] + x[t+2]),2)
    }
    return(moyennes)
  }
  
  ventes_df$Mt <- calc_moyenne_mobile(ventes_df$Xt)
  print(ventes_df)
  
  # Tendance estimée en fonction des moyennes mobiles
  modele_Tt <- lm(Mt ~ t, data = ventes_df, na.action = na.exclude)  # on exclut les NA pour le calcul de la régression
  coeff_Tt <- coef(modele_Tt)
  a_Tt <- coeff_Tt["t"]
  b_Tt <- coeff_Tt["(Intercept)"]
  cat("Tt (en fonction des Mt) : y =", a_Tt, "x +", b_Tt, "\n")
  ventes_df$Tt <- round(a_Tt * ventes_df$t + b_Tt,2)
  
  # Différences saisonnières en utilisant les moyennes mobiles
  ventes_df$St_chapeau <- round(ventes_df$Xt / ventes_df$Mt,2)
  print(ventes_df)
  
  # Question 4
  
  # Composante saisonnière
  calc_St <- function(df) {
    # Calcul des indices des trimestres
    trimestre <- rep(1:T, times = size_data/T + 1)
    # Calcul des St_chapeau moyens pour chaque trimestre (applique la fonction mean à chaque groupe de valeurs dans St_chapeau, où les groupes sont définis par le vecteur trimestre)
    St <- round(tapply(df$St_chapeau, trimestre, mean, na.rm = TRUE),2)
    return(St)
  }
  ventes_df$St <- calc_St(ventes_df)
  print(ventes_df)
  
  # Coefficients saisonniers corrigés
  ventes_df$St_prime <- round(ventes_df$St / mean(ventes_df$St, na.rm = TRUE),2)
  
  # Calcul de la série désaisonnalisée
  ventes_df$Xt_prime <- round(ventes_df$Xt / ventes_df$St_prime,2)
  
  print(ventes_df)
  
  # Question 5
  
  # Prévision pour l'année 2023
  ventes_df$Xt_prev <- round(ventes_df$Tt * ventes_df$St_prime,2)
  
  # Calcul d'erreur entre l'estimation et les données réelles
  ventes_df$erreur <- round(abs(ventes_df$Xt_prev - ventes_df$Xt),2)
  print(paste0("Erreur moyenne entre l'estimation et les données réelles pour le modèle multiplicatif: ", round(sum(ventes_df$erreur[1:size_data])/size_data,2)))
  
  print(ventes_df)
  
  print(ggplot(ventes_df, aes(x = t, y = Xt, group = 1)) +
    geom_point() +
    geom_line(aes(y = Xt), color = "black") +
    geom_line(aes(y = Xt_prev), color = "blue"))
}

#Exercice 3
exercice3 <- function(){
  data <- cbind(c(4, 3, 2, 3), c(3, 4, 4, 4), c(5, 6, 6, 7), c(22, 21, 24, 26))
  modele_additif(data, 4)
  modele_multiplicatif(data, 4)
}

#Exercice 4
exercice4 <- function(){
  data <- rbind(c(172, 298, 611, 122), c(254, 414, 795, 198), c(300, 472, 903, 265), c(466, 568, 1115, 290), c(352, 624, 1274, 303))
  modele_multiplicatif(data, 4)
}

#Exercice5
exercice5 <- function(features){
  data <- read.table("D:/Projects/ESGI/MathR/Projet/Données ACP_ pluviométrie.csv",sep = ";", header = TRUE, row.names="VILLE")
  # transforme des catégories string en int
  data$Geographie <- as.numeric(factor(data$Geographie))
  # Filtre les colonnes
  data = data[,features]
  # Matrice de corrélation
  M = cor(data)
  # Plot la matrice de correlation
  corrplot(M, method = "number")

  # PCA cercle à analyser pour trouver les corrélations
  # +
  # Graph à analyser pour trouver les composantes principales
  res.pca <- PCA(data, graph = TRUE)
  print(res.pca$eig)
  #On récupère les composantes principales
  res.pc <- prcomp(data, center = TRUE,scale. = TRUE)
  summary(res.pc)
  
  Inertie <- res.pca$eig[,2][1] + res.pca$eig[,2][2]
  print(round(Inertie,2))
  
  # Plot contribution des variables
  print(fviz_contrib(res.pca, choice="var", axes = 1))
  print(fviz_contrib(res.pca, choice="var", axes = 2))
  print(fviz_contrib(res.pca, choice="var", axes = 1:2))
  
  # Plot contribution des variables
  print(fviz_contrib(res.pca, choice="ind", axes = 1, top = 10))
  print(fviz_contrib(res.pca, choice="ind", axes = 2, top = 10))
}

features_monthly_nb_j_pl = c("JANVIERnb.j.pl","FEVRIERnb.j.pl","MARSnb.j.pl","AVRILnb.j.pl","MAInb.j.pl","JUINnb.j.pl","JUILLETnb.j.pl",
                             "AOUTnb.j.pl","SEPTEMBREnb.j.pl","OCTOBREnb.j.pl","NOVEMBREnb.j.pl","DECEMBREnb.j.pl")

features_montly_precipitation = c("JANVIERp","FEVRIERp","MARSp","AVRILp","MAIp","JUINp","JUILLETp",
                                     "AOUTp","SEPTEMBREp","OCTOBREp","NOVEMBREp","DECEMBREp")

features_annuels = c("Nombre.annuel.de.jours.de.pluie",
                     "Temperature.moyenne.annuelle","Amplitude.annuelle.des.temperatures",
                     "Insolation.annuelle","Latitude","Longitude")


#choose exercice
#exercice1()
#exercice2()
#exercice3()
#exercice4()
#exercice5(features_monthly_nb_j_pl)
#exercice5(features_montly_precipitation)
#exercice5(features_annuels)

