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

KMO= , adecuado para realizar análisis factorial. KMO Nº del articulo=
KMO Cantidad de prod.= KMO Precio Unidad= KMO Subtotal= KMO IVA=

# DETERMINACION DEL NUMERO DE FACTORES A EXTRAER

## Método de las componentes principales iteradas (Ejes principales)

## Este método de las Ejes principales es de naturaleza no paramétrica, es decir, que se ocupa, cuando no hay normalidad multivariante; pero, también es válido para modelos paramétricos (normalidad multivariante)

Con el método de los ejes principales se extraería solo n factor

## Método de las componentes principales

## Método paramétrico, sirve solo para modelos con normalidad multivariante

con el método de las componentes principales se recomienda extraer n
factor

## Método de la máxima verosimilitud

## Método paramétrico, sirve solo para modelos con normalidad multivariante

Con el método de la máxima verosimilitud se recomienda extraer factor

## Método paralelo con iteraciones

## Método paramétrico, sirve solo para modelos con normalidad multivariante

Con el método de Horn\`s (método paralelo con iteraciones) se recomienda
extraer n factor

Resumen: Ejes principales= 1 factor Componentes principales= 1 factor
Máxima verosimilitud= 1 factor Método paralelo con iteraciones
(Horn\`s)= 1 factor

Conclusión: vamos a extraer 1 factor

# METODOS DE EXTRACCION DE FACTORES

## \#\# METODO DE ANALISIS DE LOS COMPONENTES PRINCIPALES (ACP)

PC1: Cargas factoriales de cada variable h2: Comunalidad (varianza común
explicada). Articulo es explicada en un (h2)% por el factor extraído.
Cantidad es explicada en un (h2)% por el factor extraído, Precio U es
explicada en un (h2)% por el factor extraído, Subtotal es explicada en
un (h2)% por el factor extraído, IVA es explicada en un (h2)% por el
factor extraído y Total es explicada en un (h2)% por el factor extraído

Mientras más alta sea h2 es mejor el modelo. 0;1

u2: Especificidad (varianza residual o varianza no explicada). El
articulo no es explicado en un (u2)%, Cantidad pierde (u2)%, precio
pierde (u2)%, Subtotal pierde (u2)%, IVA pierde (u2)% y Total pierde
(u2)%

Mientras más pequeño sea u2 es mejor el modelo. 0;1

h2 + u2 = 1  
Comunalidad + Especificidad = 1 Varianza explicada + varianza no
explicada= 1

SS loadings 3.77 (Es la varianza explicada en valores absolutos, o la
suma de los h2). Proportion Var 0.75 = 75% (El % que la varianza
explicada representa del total)

lo “ideal” es que proportion var sea lo más cercano a 1.

RMSR= 0.16 (Raíz cuadrada media de los residuos) Teóricamente un modelo
presenta una solución adecuada cuando el RSMR es menor o igual a 0,16.

## \#\# METODO DE LOS EJES PRINCIPALES O COMPONENTES PRINCIPALES ITERADAS (CPI)

Proportion Var= 71% RMSR= 0.13

## \#\# METODO DE MAXIMA VEROSIMILITUD

Proportion Var= % RMSR=

### RESUMEN

ACP: var= % RMSR= CPI: Var= % RMSR=  
MVE: Var= % RMSR=

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
