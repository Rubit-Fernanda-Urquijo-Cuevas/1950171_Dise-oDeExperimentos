ANÁLISIS FACTORIAL, Rubit Fernanda Urquijo Cuevas, 1950171
================

# IMPORTAR LA BASE DE DATOS EN FORMATO EXCEL

``` r
library(readxl)
datos<- read_excel("C:/Users/INDEX/Documents/Diseño de Experimentos/Temas Examen Final/af.xlsx")
```

# TIPIFICACIÓN O ESTANDARIZACIÓN DE VARIABLES

La tipificación permite que todas las variables métricas gocen de una
misma unidad de medida estadística.

``` r
datost<- datos #crear una nueva base de datos o data frame
datost<- scale(datost, center = T, scale = T)
datost<- as.data.frame(datost)
```

# NORMALIDAD MULTIVARIANTE

H0: Normalidad multivariante  
H1: No normalidad multivariante  
Confianza= 95%  
Alfa= 5% = 0,05  
P value &gt; alfa: no se rechaza la H0 (Normalidad)  
P value &lt; alfa: se rechaza la H0 (No normalidad)

``` r
library(MVN)
```

    ## Registered S3 method overwritten by 'GGally':
    ##   method from   
    ##   +.gg   ggplot2

    ## sROC 0.1-2 loaded

``` r
mvn(datost[2:7])
```

    ## $multivariateNormality
    ##              Test          Statistic           p value Result
    ## 1 Mardia Skewness   49.8154934105718 0.706693268492982    YES
    ## 2 Mardia Kurtosis -0.956936970395602  0.33859906940057    YES
    ## 3             MVN               <NA>              <NA>    YES
    ## 
    ## $univariateNormality
    ##           Test       Variable Statistic   p value Normality
    ## 1 Shapiro-Wilk   Nucleo_Fam      0.9092    0.0142    NO    
    ## 2 Shapiro-Wilk   Estrato_s       0.9336    0.0612    YES   
    ## 3 Shapiro-Wilk N_Habitaciones    0.8988    0.0079    NO    
    ## 4 Shapiro-Wilk    Gana_mes       0.9604    0.3169    YES   
    ## 5 Shapiro-Wilk    N_Carros       0.8868    0.0040    NO    
    ## 6 Shapiro-Wilk  Hrs_internet     0.8026    0.0001    NO    
    ## 
    ## $Descriptives
    ##                 n          Mean Std.Dev      Median       Min      Max
    ## Nucleo_Fam     30  9.068899e-17       1 -0.03014169 -1.386518 2.230485
    ## Estrato_s      30 -6.275181e-17       1  0.02514402 -1.483497 2.288105
    ## N_Habitaciones 30  1.200133e-16       1 -0.03745664 -2.284855 2.209941
    ## Gana_mes       30  2.183222e-16       1 -0.08839197 -1.570257 2.485374
    ## N_Carros       30 -1.703137e-16       1  0.36282385 -1.311748 2.037395
    ## Hrs_internet   30 -1.431942e-16       1  0.04121756 -1.195309 1.277744
    ##                      25th      75th        Skew   Kurtosis
    ## Nucleo_Fam     -0.4822670 0.4219836  0.21447435 -0.8944493
    ## Estrato_s      -0.7291764 0.7794645  0.31615132 -0.7379243
    ## N_Habitaciones -0.8802309 1.0862424 -0.06138032 -0.5010337
    ## Gana_mes       -0.6343424 0.5745478  0.53712470 -0.4355010
    ## N_Carros       -1.1024263 0.3628239  0.02382692 -1.2070186
    ## Hrs_internet   -1.1953091 1.2777442  0.05657937 -1.5198989

Como el p value &gt; alfa: no se rechaza la H0, por lo tanto, existe
normalidad multivariante.

# MATRIZ DE CORRELACIONES

H0: Correlación = 0 (no hay correlación)  
H1: Correlación diferente de 0 (si hay correlación)

Cuando no se rechaza H0, no se aplica AFE.  
Se rechace HO, si para aplicar AFE.

``` r
library(psych)
corr.test(datost[,2:7])
```

    ## Call:corr.test(x = datost[, 2:7])
    ## Correlation matrix 
    ##                Nucleo_Fam Estrato_s N_Habitaciones Gana_mes N_Carros
    ## Nucleo_Fam           1.00      0.04           0.68     0.35     0.05
    ## Estrato_s            0.04      1.00           0.21     0.44     0.62
    ## N_Habitaciones       0.68      0.21           1.00     0.42     0.31
    ## Gana_mes             0.35      0.44           0.42     1.00     0.54
    ## N_Carros             0.05      0.62           0.31     0.54     1.00
    ## Hrs_internet         0.02      0.77           0.15     0.63     0.66
    ##                Hrs_internet
    ## Nucleo_Fam             0.02
    ## Estrato_s              0.77
    ## N_Habitaciones         0.15
    ## Gana_mes               0.63
    ## N_Carros               0.66
    ## Hrs_internet           1.00
    ## Sample Size 
    ## [1] 30
    ## Probability values (Entries above the diagonal are adjusted for multiple tests.) 
    ##                Nucleo_Fam Estrato_s N_Habitaciones Gana_mes N_Carros
    ## Nucleo_Fam           0.00      1.00           0.00     0.38     1.00
    ## Estrato_s            0.85      0.00           1.00     0.13     0.00
    ## N_Habitaciones       0.00      0.28           0.00     0.16     0.60
    ## Gana_mes             0.05      0.01           0.02     0.00     0.02
    ## N_Carros             0.79      0.00           0.10     0.00     0.00
    ## Hrs_internet         0.91      0.00           0.44     0.00     0.00
    ##                Hrs_internet
    ## Nucleo_Fam                1
    ## Estrato_s                 0
    ## N_Habitaciones            1
    ## Gana_mes                  0
    ## N_Carros                  0
    ## Hrs_internet              0
    ## 
    ##  To see confidence intervals of the correlations, print with the short=FALSE option

``` r
correlaciones<- corr.test(datost[,2:7]) #se crea la matriz de correlaciones
correlaciones$r #matriz de correlaciones
```

    ##                Nucleo_Fam  Estrato_s N_Habitaciones  Gana_mes   N_Carros
    ## Nucleo_Fam     1.00000000 0.03606478      0.6820755 0.3547334 0.05047441
    ## Estrato_s      0.03606478 1.00000000      0.2055744 0.4425222 0.62214439
    ## N_Habitaciones 0.68207546 0.20557443      1.0000000 0.4226895 0.30604898
    ## Gana_mes       0.35473337 0.44252223      0.4226895 1.0000000 0.54433649
    ## N_Carros       0.05047441 0.62214439      0.3060490 0.5443365 1.00000000
    ## Hrs_internet   0.02056331 0.77084847      0.1453368 0.6256427 0.66284681
    ##                Hrs_internet
    ## Nucleo_Fam       0.02056331
    ## Estrato_s        0.77084847
    ## N_Habitaciones   0.14533682
    ## Gana_mes         0.62564274
    ## N_Carros         0.66284681
    ## Hrs_internet     1.00000000

``` r
r<- as.matrix(correlaciones$r)
```

Alfa= 0,05  
P value &gt; alfa: no se rechaza H0  
P value &lt; alfa: se rechaza H0, estamos en esta situación, por lo
tanto, si es aplicable el análisis factorial exploratorio

# INDICADORES DE APLICABILIDAD DEL AFE (BONDAD DEL AJUSTE)

## CONTRASTE DE ESFERICIDAD DE BARTLETT

H0: Las correlaciones teóricas entre cada par de variables es nulo  
H1: Las correlaciones teóricas entre cada par de variables no es nulo

P value &gt;alfa: no se aplica el AFE (no se rechaza H0)  
P value &gt;alfa: si se aplica el AFE (se rechaza H0)

``` r
dim(datost)  #tamaño de la muestra= 30 personas
```

    ## [1] 30  7

``` r
cortest.bartlett(r, n= 30)
```

    ## $chisq
    ## [1] 82.33476
    ## 
    ## $p.value
    ## [1] 2.606944e-11
    ## 
    ## $df
    ## [1] 15

Como el p value es menor a alfa, se rechaza la H0, por lo tanto, las
correlaciones teóricas entre cada par de variables es nulo, es decir, si
es aplicable el análisis factorial exploratorio (AFE).

## MEDIDA DE ADECUACION MUESTRAL DE KAISER, MEYER Y OKLIN (KMO)

Estudia variable por variable, si son o no aceptadas en el modelo para
hacer AFE. (Que variables elimino o mantengo) Se mantiene una variable
en el modelo, si el KMO es igual o mayor a 0,7. Se elimina una variable
del modelo, si el KMO es menor a 0,7.

``` r
KMO(r)
```

    ## Kaiser-Meyer-Olkin factor adequacy
    ## Call: KMO(r = r)
    ## Overall MSA =  0.69
    ## MSA for each item = 
    ##     Nucleo_Fam      Estrato_s N_Habitaciones       Gana_mes       N_Carros 
    ##           0.53           0.72           0.59           0.76           0.82 
    ##   Hrs_internet 
    ##           0.67

KMO= 0,69 El modelo es middling (regular), si es adecuado para realizar
análisis factorial. KMO Nucleo\_Fam= 0,53 (Miserable) KMO Estrato\_s=
0,72 (Middling) KMO N\_Habitaciones= 0,59 (Mediocre) KMO Gana\_mes= 0,76
(Middling) KMO N\_Carros= 0,82 (Meritorious) KMO Hrs\_internet= 0,67
(Mediocre)

# DETERMINACION DEL NUMERO DE FACTORES A EXTRAER

## Método de las componentes principales iteradas (Ejes principales)

Este método de las Ejes principales es de naturaleza no paramétrica, es
decir, que se ocupa, cuando no hay normalidad multivariante; pero,
también es válido para modelos paramétricos (normalidad multivariante)

``` r
fa.parallel(r, fm= "pa", n.obs= 30, ylabel= "Eigenvalues")
```

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fac(r = r, nfactors = nfactors, n.obs = n.obs, rotate = rotate, : An
    ## ultra-Heywood case was detected. Examine the results carefully

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fac(r = r, nfactors = nfactors, n.obs = n.obs, rotate = rotate, : An
    ## ultra-Heywood case was detected. Examine the results carefully

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fac(r = r, nfactors = nfactors, n.obs = n.obs, rotate = rotate, : An
    ## ultra-Heywood case was detected. Examine the results carefully

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fac(r = r, nfactors = nfactors, n.obs = n.obs, rotate = rotate, : An
    ## ultra-Heywood case was detected. Examine the results carefully

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fac(r = r, nfactors = nfactors, n.obs = n.obs, rotate = rotate, : An
    ## ultra-Heywood case was detected. Examine the results carefully

![](DiseñoEF_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

    ## Parallel analysis suggests that the number of factors =  2  and the number of components =  2

Con el método de los ejes principales se extraería solo 2 factores.

## Método de las componentes principales

Método paramétrico, sirve solo para modelos con normalidad multivariante

``` r
fa.parallel(r, fm= "pc", n.obs= 30, ylabel= "Eigenvalues")
```

    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fac(r = r, nfactors = nfactors, n.obs = n.obs, rotate = rotate, : An
    ## ultra-Heywood case was detected. Examine the results carefully

    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## An ultra-Heywood case was detected. Examine the results carefully

    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## An ultra-Heywood case was detected. Examine the results carefully

    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## An ultra-Heywood case was detected. Examine the results carefully

    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## An ultra-Heywood case was detected. Examine the results carefully

    ## factor method not specified correctly, minimum residual (unweighted least squares  used

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## An ultra-Heywood case was detected. Examine the results carefully

    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## An ultra-Heywood case was detected. Examine the results carefully

    ## factor method not specified correctly, minimum residual (unweighted least squares  used

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## An ultra-Heywood case was detected. Examine the results carefully

![](DiseñoEF_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

    ## Parallel analysis suggests that the number of factors =  2  and the number of components =  2

con el método de las componentes principales se recomienda extraer 2
factores

## Método de la máxima verosimilitud

Método paramétrico, sirve solo para modelos con normalidad multivariante

``` r
fa.parallel(r, fm= "ml", n.obs= 30, ylabel= "Eigenvalues")
```

![](DiseñoEF_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

    ## Parallel analysis suggests that the number of factors =  2  and the number of components =  2

Con el método de la máxima verosimilitud se recomienda extraer 2
factores.

## Método paralelo con iteraciones

Método paramétrico, sirve solo para modelos con normalidad multivariante

``` r
library(paran)
```

    ## Loading required package: MASS

``` r
paran(r, iterations = 1000, graph = T)
```

    ## 
    ## Using eigendecomposition of correlation matrix.
    ## Computing: 10%  20%  30%  40%  50%  60%  70%  80%  90%  100%
    ## 
    ## 
    ## Results of Horn's Parallel Analysis for component retention
    ## 1000 iterations, using the mean estimate
    ## 
    ## -------------------------------------------------- 
    ## Component   Adjusted    Unadjusted    Estimated 
    ##             Eigenvalue  Eigenvalue    Bias 
    ## -------------------------------------------------- 
    ## 1           2.795197    4.542408      1.747210
    ## -------------------------------------------------- 
    ## 
    ## Adjusted eigenvalues > 1 indicate dimensions to retain.
    ## (1 components retained)

![](DiseñoEF_files/figure-gfm/unnamed-chunk-10-1.png)<!-- --> Con el
método de Horn\`s (método paralelo con iteraciones) se recomienda
extraer 1 factor

Resumen: Ejes principales= 2 factor Componentes principales= 2 factor
Máxima verosimilitud= 2 factor Método paralelo con iteraciones
(Horn\`s)= 1 factor

Conclusión: vamos a extraer 1 factor

# METODOS DE EXTRACCION DE FACTORES

## METODO DE ANALISIS DE LOS COMPONENTES PRINCIPALES (ACP)

``` r
acp<- principal(r, nfactors= 1, rotate= "none")
acp
```

    ## Principal Components Analysis
    ## Call: principal(r = r, nfactors = 1, rotate = "none")
    ## Standardized loadings (pattern matrix) based upon correlation matrix
    ##                 PC1   h2   u2 com
    ## Nucleo_Fam     0.36 0.13 0.87   1
    ## Estrato_s      0.79 0.63 0.37   1
    ## N_Habitaciones 0.54 0.29 0.71   1
    ## Gana_mes       0.81 0.66 0.34   1
    ## N_Carros       0.81 0.66 0.34   1
    ## Hrs_internet   0.84 0.71 0.29   1
    ## 
    ##                 PC1
    ## SS loadings    3.07
    ## Proportion Var 0.51
    ## 
    ## Mean item complexity =  1
    ## Test of the hypothesis that 1 component is sufficient.
    ## 
    ## The root mean square of the residuals (RMSR) is  0.21 
    ## 
    ## Fit based upon off diagonal values = 0.79

PC1: Cargas factoriales de cada variable h2: Comunalidad (varianza común
explicada). Nucleo\_Fam es explicada en un 13% por el factor extraído.
Estrato\_s es explicada en un 63% por el factor extraído,
N\_Habitaciones es explicada en un 29% por el factor extraído, Gana\_mes
es explicada en un 66% por el factor extraído, N\_Carros es explicada en
un 66% por el factor extraído y Hrs\_internet es explicada en un 71% por
el factor extraído.

Mientras más alta sea h2 es mejor el modelo. 0;1

u2: Especificidad (varianza residual o varianza no explicada). El
Nucleo\_Fam no es explicado en un 87%, Estrato\_s pierde 37%,
N\_Habitaciones pierde 71%, Gana\_mes pierde 34%, N\_Carros pierde 34% y
Hrs\_internet pierde 29%

Mientras más pequeño sea u2 es mejor el modelo. 0;1

h2 + u2 = 1  
Comunalidad + Especificidad = 1 Varianza explicada + varianza no
explicada= 1

SS loadings 3.07 (Es la varianza explicada en valores absolutos, o la
suma de los h2). Proportion Var 0.51 = 51% (El % que la varianza
explicada representa del total)

lo “ideal” es que proportion var sea lo más cercano a 1.

RMSR= 0.21 (Raíz cuadrada media de los residuos) Teóricamente un modelo
presenta una solución adecuada cuando el RSMR es menor o igual a 0,21.

## METODO DE LOS EJES PRINCIPALES O COMPONENTES PRINCIPALES ITERADAS (CPI)

``` r
cpi<- fa(r, nfactors= 1, fm="pa", rotate= "none", n.obs= 30)
cpi
```

    ## Factor Analysis using method =  pa
    ## Call: fa(r = r, nfactors = 1, n.obs = 30, rotate = "none", fm = "pa")
    ## Standardized loadings (pattern matrix) based upon correlation matrix
    ##                 PA1    h2   u2 com
    ## Nucleo_Fam     0.24 0.058 0.94   1
    ## Estrato_s      0.75 0.562 0.44   1
    ## N_Habitaciones 0.40 0.161 0.84   1
    ## Gana_mes       0.74 0.543 0.46   1
    ## N_Carros       0.77 0.589 0.41   1
    ## Hrs_internet   0.84 0.708 0.29   1
    ## 
    ##                 PA1
    ## SS loadings    2.62
    ## Proportion Var 0.44
    ## 
    ## Mean item complexity =  1
    ## Test of the hypothesis that 1 factor is sufficient.
    ## 
    ## The degrees of freedom for the null model are  15  and the objective function was  3.15 with Chi Square of  82.33
    ## The degrees of freedom for the model are 9  and the objective function was  1.15 
    ## 
    ## The root mean square of the residuals (RMSR) is  0.19 
    ## The df corrected root mean square of the residuals is  0.25 
    ## 
    ## The harmonic number of observations is  30 with the empirical chi square  32.58  with prob <  0.00016 
    ## The total number of observations was  30  with Likelihood Chi Square =  29.28  with prob <  0.00058 
    ## 
    ## Tucker Lewis Index of factoring reliability =  0.482
    ## RMSEA index =  0.272  and the 90 % confidence intervals are  0.17 0.394
    ## BIC =  -1.33
    ## Fit based upon off diagonal values = 0.83
    ## Measures of factor score adequacy             
    ##                                                    PA1
    ## Correlation of (regression) scores with factors   0.93
    ## Multiple R square of scores with factors          0.87
    ## Minimum correlation of possible factor scores     0.73

Proportion Var= 44% RMSR= 0.19

## METODO DE MAXIMA VEROSIMILITUD

``` r
mve<- fa(r, nfactors= 1, fm="ml", rotate= "none", n.obs= 30)
mve
```

    ## Factor Analysis using method =  ml
    ## Call: fa(r = r, nfactors = 1, n.obs = 30, rotate = "none", fm = "ml")
    ## Standardized loadings (pattern matrix) based upon correlation matrix
    ##                 ML1     h2   u2 com
    ## Nucleo_Fam     0.10 0.0093 0.99   1
    ## Estrato_s      0.82 0.6718 0.33   1
    ## N_Habitaciones 0.26 0.0676 0.93   1
    ## Gana_mes       0.67 0.4447 0.56   1
    ## N_Carros       0.75 0.5550 0.44   1
    ## Hrs_internet   0.92 0.8402 0.16   1
    ## 
    ##                 ML1
    ## SS loadings    2.59
    ## Proportion Var 0.43
    ## 
    ## Mean item complexity =  1
    ## Test of the hypothesis that 1 factor is sufficient.
    ## 
    ## The degrees of freedom for the null model are  15  and the objective function was  3.15 with Chi Square of  82.33
    ## The degrees of freedom for the model are 9  and the objective function was  1.07 
    ## 
    ## The root mean square of the residuals (RMSR) is  0.2 
    ## The df corrected root mean square of the residuals is  0.26 
    ## 
    ## The harmonic number of observations is  30 with the empirical chi square  37.24  with prob <  2.4e-05 
    ## The total number of observations was  30  with Likelihood Chi Square =  27.19  with prob <  0.0013 
    ## 
    ## Tucker Lewis Index of factoring reliability =  0.535
    ## RMSEA index =  0.257  and the 90 % confidence intervals are  0.154 0.381
    ## BIC =  -3.42
    ## Fit based upon off diagonal values = 0.81
    ## Measures of factor score adequacy             
    ##                                                    ML1
    ## Correlation of (regression) scores with factors   0.95
    ## Multiple R square of scores with factors          0.90
    ## Minimum correlation of possible factor scores     0.81

Proportion Var= 43% RMSR= 0,2

### RESUMEN

ACP: var= 51% RMSR= 0,21 CPI: Var= 44% RMSR= 0,19  
MVE: Var= 43% RMSR= 0,2

¿Con cuál nos quedamos? Aquel modelo que tenga la proportion var más
alta y el RMSR más pequeño.

# REPRESENTACION GRAFICA DE LOS FACTORES EXTRAIDOS

## \#\# Método de análisis de las componentes principales (ACP)

## \#\# Método de las componentes principales iteradas (CPI)

## \#\# Método de la máxima verosimilitud (MVE)

# OBTENCION DE LAS PUNTUACIONES FACTORIALES

## \#\# METODO DE ANALISIS DE LAS COMPONENTES PRINCIPALES ITERADAS (ACP)

## \#\# METODO DE LAS COMPONENTES PRINCIPALES ITERADAS (CPI)

## \#\# METODO DE LA MAXIMA VEROSIMILITUD

# OBTENCION DE LOS FACTORES EXTRAIDOS

## Aquí se trabaja con el método que el investigador decidió (ACP, CPI, MVE)

Z1= (PC1)Nº del articulo + (PC1)Cantidad de prod. + (PC1)Precio Unidad +
(PC1)Subtotal + (PC1)IVA

## \# AGREGAR FACTOR EXTRAIDO (PUNTUACIONES FACTORIALES) EN EL DATA FRAME ORIGINAL

## \# GUARDAR EL DATA FRAME “DATOS\_PUNTUACIONES”
