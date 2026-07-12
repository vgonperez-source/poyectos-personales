libname f"C:\\Users\victo\OneDrive\TERCERO\aprendizaje no supervisado";
run;
/* ================================================================
   PRÁCTICA 1 – Análisis de Componentes Principales (ACP)
   Preparación y depuración del conjunto de datos – Fórmula 1
   ================================================================ */


/* 1. Importación de los archivos CSV   */

proc import datafile="C:\Users\victo\OneDrive\TERCERO\aprendizaje no supervisado\results.csv"
    out=results_raw dbms=csv replace;
    guessingrows=max; getnames=yes;
run;

proc import datafile="C:\Users\victo\OneDrive\TERCERO\aprendizaje no supervisado\qualifying.csv"
    out=qualifying_raw dbms=csv replace;
    guessingrows=max; getnames=yes;
run;


/* 2. Limpieza de los datos: sustitución de '\N' por vacío*/

data results_clean;
    set results_raw;
    array ch _character_;
    do over ch;
        if ch = '\N' then ch = '';
    end;
run;

data qualifying_clean;
    set qualifying_raw;
    array ch _character_;
    do over ch;
        if ch = '\N' then ch = '';
    end;
run;


/* 3. Procesamiento del dataset RESULTS   */

data results;
    set results_clean;

    /* Conversión de variables carácter a numéricas */
    number        = input(number, best32.);
    grid          = input(grid, best32.);
    positionOrder = input(positionOrder, best32.);
    points        = input(points, best32.);
    laps          = input(laps, best32.);
    milliseconds  = input(milliseconds, best32.);
    fastestLap    = input(fastestLap, best32.);
    rank          = input(rank, best32.);
    fastestLapSpeed = input(fastestLapSpeed, best32.);

    /* Conversión del tiempo (m:ss.xxx) a milisegundos */
    length fastestLapTime_ms 8.;
    if not missing(fastestLapTime) then do;
        minutos = input(scan(fastestLapTime,1,':'), best32.);
        segundos = input(scan(fastestLapTime,2,':'), best32.);
        fastestLapTime_ms = (coalesce(minutos,0)*60 + coalesce(segundos,0))*1000;
    end;

    /* Variables derivadas */
    positionGain = grid - positionOrder;
    efficiency   = divide(points, laps);

    keep raceId driverId constructorId number grid positionOrder points laps 
         milliseconds fastestLap rank fastestLapSpeed fastestLapTime_ms 
         positionGain efficiency;
run;


/* 4. Procesamiento del dataset QUALIFYING   */

data qualifying;
    set qualifying_clean;

    qualPos = input(position, best32.);

    /* Conversión de tiempos Q1, Q2, Q3 (m:ss.xxx) a milisegundos */
    length q1_ms q2_ms q3_ms 8.;
    array qs{3} $ q1 q2 q3;
    do i=1 to 3;
        if not missing(qs{i}) then do;
            minutos = input(scan(qs{i},1,':'), best32.);
            segundos = input(scan(qs{i},2,':'), best32.);
            select(i);
                when(1) q1_ms = (coalesce(minutos,0)*60 + coalesce(segundos,0))*1000;
                when(2) q2_ms = (coalesce(minutos,0)*60 + coalesce(segundos,0))*1000;
                when(3) q3_ms = (coalesce(minutos,0)*60 + coalesce(segundos,0))*1000;
            end;
        end;
    end;

    drop i minutos segundos q1 q2 q3 position;
    keep raceId driverId qualPos q1_ms q2_ms q3_ms;
run;


/* 5. Eliminación de duplicados y unión de ambos conjuntos        */

proc sort data=results nodupkey; by raceId driverId; run;
proc sort data=qualifying nodupkey; by raceId driverId; run;

data f1_unida;
    merge results(in=a) qualifying(in=b);
    by raceId driverId;
    if a;
run;

/* ===================================================== */
/* 6. DEPURACIÓN: eliminación de valores perdidos y atípicos */
/* ===================================================== */
/* ================================================================ */
/* 6. DEPURACIÓN: limpieza básica del dataset                        */
/* ================================================================ */

/* 6.0 Conversión final a variables numéricas */
data f1_base_num;
    set f1_unida;
    milliseconds_n    = input(milliseconds, best32.);
    fastestLap_n      = input(fastestLap, best32.);
    rank_n            = input(rank, best32.);
    fastestLapSpeed_n = input(fastestLapSpeed, best32.);
    drop milliseconds fastestLap rank fastestLapSpeed;
    rename
        milliseconds_n    = milliseconds
        fastestLap_n      = fastestLap
        rank_n            = rank
        fastestLapSpeed_n = fastestLapSpeed;
run;

/* 6.1 Eliminación de observaciones con valores perdidos */
data f1_depurada;
    set f1_base_num;
    if cmiss(of _numeric_) = 0;
run;

/* 6.2 Estadísticos descriptivos tras la limpieza */
proc means data=f1_depurada n mean std min max;
    title "6.2 - Estadísticos descriptivos tras eliminar missings";
run;

/* 6.3 Dataset sin outliers (criterio ya definido previamente) */
/* Se asume que f1_depurada_no_outliers ya ha sido generado */
data f1_sin_outliers;
    set f1_depurada_no_outliers;
run;

/* 6.4 Estadísticos descriptivos sin outliers */
proc means data=f1_sin_outliers n mean std min max;
    title "6.4 - Estadísticos descriptivos sin outliers";
run;




/* ================================================================ */
/* 7. COMPARACIÓN DEL ACP CON Y SIN OUTLIERS                          */
/* ================================================================ */

/* 7.1 ACP con outliers */
proc princomp data=f1_depurada plots=all;
    var grid positionOrder points laps milliseconds fastestLap rank
        fastestLapSpeed fastestLapTime_ms qualPos q1_ms q2_ms q3_ms
        positionGain efficiency;
    title "7.1 - ACP con outliers";
run;

/* 7.2 ACP sin outliers */
proc princomp data=f1_sin_outliers plots=all;
    var grid positionOrder points laps milliseconds fastestLap rank
        fastestLapSpeed fastestLapTime_ms qualPos q1_ms q2_ms q3_ms
        positionGain efficiency;
    title "7.2 - ACP sin outliers";
run;

/* ================================================================ */
/* 8. DATASET FINAL Y ESTANDARIZACIÓN DEFINITIVA                    */
/* ================================================================ */

/* 8.1 Definición del dataset final */
/* Se decide mantener el dataset con outliers */
data f1_final_raw;
    set f1_depurada;
run;

/* 8.2 Comprobación de ausencia de valores perdidos */
proc means data=f1_final_raw n nmiss;
    title "8.2 - Comprobación de valores perdidos en el dataset final";
run;

/* 8.3 Estadísticos descriptivos previos a la estandarización */
proc means data=f1_final_raw n mean std min max;
    title "8.3 - Estadísticos descriptivos antes de estandarizar";
run;

/* 8.4 ESTANDARIZACIÓN DEFINITIVA DE TODAS LAS VARIABLES */
proc standard data=f1_final_raw mean=0 std=1 out=f1_final;
    var grid positionOrder points laps milliseconds fastestLap rank
        fastestLapSpeed fastestLapTime_ms qualPos q1_ms q2_ms q3_ms
        positionGain efficiency;
run;

/* 8.5 Verificación de la estandarización */
proc means data=f1_final mean std;
    title "8.5 - Verificación de estandarización del dataset final";
run;





/* 4. ANÁLISIS DE COMPONENTES PRINCIPALES SOBRE LA MATRIZ DE CORRELACIONES  */

/*4.1*/
proc corr data=f1_final nosimple plots=matrix(histogram); 
var grid positionOrder points laps milliseconds fastestLap rank 
fastestLapSpeed fastestLapTime_ms qualPos q1_ms q2_ms q3_ms 
positionGain efficiency; 
title "Matriz de Correlaciones - Fórmula 1"; 
run; 

/* 4.2 */
proc princomp data=f1_final out=acp_f1 plots=all; 
var grid positionOrder points laps milliseconds fastestLap rank 
fastestLapSpeed fastestLapTime_ms qualPos q1_ms q2_ms q3_ms 
positionGain efficiency; 
title "Análisis de Componentes Principales - Fórmula 1 (matriz de 
correlaciones)"; 
run; 

/* 4.3, (solo necesito el código para los apartados a y b) */

/* a */
proc princomp data=f1_final n=3 plots=all outstat=stats out=acp_f1; 
var grid positionOrder points laps milliseconds fastestLap rank 
fastestLapSpeed fastestLapTime_ms qualPos q1_ms q2_ms q3_ms 
positionGain efficiency; 
title "Análisis de Componentes Principales - 3 Componentes Retenidas"; 
run; 

/* b */
proc princomp data=f1_final n=3; 
var grid positionOrder points laps milliseconds fastestLap rank 
fastestLapSpeed fastestLapTime_ms qualPos q1_ms q2_ms q3_ms 
positionGain efficiency; 
ods output Eigenvalues=autovalores Eigenvectors=autovectores; 
run; 
data correlaciones_f1 (drop=label); 
set autovectores; 
cor1 = Prin1*sqrt(4.79176827); 
cor2 = Prin2*sqrt(4.09207373); 
cor3 = Prin3*sqrt(1.71927795); 
run; 
proc print data=correlaciones_f1; 
var Variable cor1--cor3; 
title "4.3b - Correlaciones de las variables con las tres primeras 
componentes (Loadings)"; 
run; 






/* 5. ANÁLISIS FACTORIAL SOBRE LA MATRIZ DE CORRELACIONES  */

/* PARTE CON ROTACIIÓN*/


/* Variables numéricas no colineales relacionadas con rendimiento,  */
/* posición y eficiencia del piloto                                 */

%let vars = grid positionOrder points laps milliseconds fastestLap rank 
fastestLapSpeed fastestLapTime_ms qualPos q1_ms q2_ms q3_ms;



/* 5.1 ¿Cuánto vale el índice KMO? ¿Es un valor adecuado?           */


proc factor data=f1_final method=principal msa;
    var &vars;
    title "5.1 - Índice KMO y Prueba de Esfericidad de Bartlett";
run;



/* 5.2 ¿Qué variable eliminarías del modelo para mejorar el KMO? */
/* Se elimina fastestLapSpeed por presentar un MSA muy bajo y tras volver a
comprobar elimino tambien miliseconds */

proc factor data=f1_final 
    method=principal 
    msa;
    var grid positionOrder points laps  fastestLap rank
        fastestLapTime_ms qualPos q1_ms q2_ms q3_ms;
    title "5.2 - KMO tras eliminar fastestLapSpeed";
run;




/* 5.3 Análisis factorial sobre la matriz de correlaciones   */


/* MÉTODO DE COMPONENTES PRINCIPALES */

proc factor data=f1_final corr
    method=principal
    heywood
    nfact=3
    outstat=AfEst1 out=AfMetodo1
    residuals msa scree plot=all;
    var grid positionOrder points laps  fastestLap rank
        fastestLapTime_ms qualPos q1_ms q2_ms q3_ms;
    title "5.3 - Método de Componentes Principales";
run;



/* MÉTODO DEL FACTOR PRINCIPAL (priors=SMC)  */

proc factor data=f1_final corr
    method=prinit
    priors=smc
    heywood
    nfact=3
    maxiter=500
    outstat=AfEst2 out=AfMetodo2
    residuals msa scree plot=all;
    var grid positionOrder points laps  fastestLap rank
        fastestLapTime_ms qualPos q1_ms q2_ms q3_ms;
    title "5.3 - Método del Factor Principal (priors=SMC)";
run;

 

/* COMPROBACIÓN DE NORMALIDAD MULTIVARIANTE */

%include "C:\Users\victo\OneDrive\TERCERO\aprendizaje no supervisado\bloque 1\MACRO_NORMALIDAD.sas";

%NORMAL_MULT(data=f1_final,  
var=grid positionOrder points laps  fastestLap rank
        fastestLapTime_ms qualPos q1_ms q2_ms q3_ms); 

/* MÉTODO 3: MÁXIMA VEROSIMILITUD */ 

proc factor data=f1_final
    method=ml
    nfact=3
    simple
    msa;
var grid positionOrder points laps  fastestLap rank
        fastestLapTime_ms qualPos q1_ms q2_ms q3_ms;
title "5.3.c - Análisis Factorial: Método de Máxima Verosimilitud";
run;



/* CONCLUSIÓN: ELECCIÓN DEL MÉTODO /

/* Según la comparación de comunalidades y RMSR, se elige Componentes Principales */



/* 5.3.a Porcentaje de la varianza explicada  */

proc factor data=f1_final 
    method=principal 
    nfact=3 
    plots=scree;
var grid positionOrder points laps  fastestLap rank
        fastestLapTime_ms qualPos q1_ms q2_ms q3_ms;
title "5.3.a - Varianza explicada por los factores (Componentes Principales)";
run;




/* 5.3.b Path Diagram */

proc factor data=f1_final corr
method=principal
nfact=3
plot=all;
var grid positionOrder points laps  fastestLap rank
        fastestLapTime_ms qualPos q1_ms q2_ms q3_ms;
title "5.3.b - Path Diagram (Componentes Principales)";
run;




/* 5.3.c Gráficos de planos factoriales */

proc factor data=f1_final corr
method=principal
nfact=3
plot=all;
var grid positionOrder points laps  fastestLap rank
        fastestLapTime_ms qualPos q1_ms q2_ms q3_ms;
title "5.3.c - Gráficos de las variables en los planos factoriales (Componentes Principales)";
run;




/* 5.3.d Comunalidades finales y ajuste del modelo                  */
proc factor data=f1_final corr 
    method=principal 
    nfact=3 
    residuals msa plot=none; 
var grid positionOrder points laps  fastestLap rank
        fastestLapTime_ms qualPos q1_ms q2_ms q3_ms;
title "5.3.d - Comunalidades finales (Componentes Principales)";
run;



/* 6. ANÁLISIS FACTORIAL CON ROTACIÓN*/

/* ROTACIÓN 1: VARIMAX */


proc factor data=f1_final simple msa
method=principal priors=one reorder residual nfact=3
rotate=varimax out=factores_varimax outstat=estad_varimax plots=all;
var grid positionOrder points laps  fastestLap rank
        fastestLapTime_ms qualPos q1_ms q2_ms q3_ms;
title "5.2 - Rotación VARIMAX (3 factores)";
run;



/* ROTACIÓN 2: QUARTIMAX */
proc factor data=f1_final simple msa
method=principal priors=one reorder residual nfact=3
rotate=quartimax out=factores_quartimax outstat=estad_quartimax plots=all;
var grid positionOrder points laps  fastestLap rank
        fastestLapTime_ms qualPos q1_ms q2_ms q3_ms;
title "5.2 - Rotación QUARTIMAX (3 factores)";
run;




/* .a - COMPARACIÓN DEL PATHDIAGRAM */

proc factor data=f1_final
    method=principal
    priors=one
    nfact=3
    rotate=varimax
    reorder
    residual
    msa
    plots=pathdiagram;
    
    var grid positionOrder points laps  fastestLap rank
        fastestLapTime_ms qualPos q1_ms q2_ms q3_ms;
    
    title "5.4.a - Path Diagram con rotación VARIMAX (3 factores)";
run;


/* b - Gráficos de los planos factoriales */


proc factor data=f1_final
    method=principal
    priors=one
    nfact=3
    rotate=varimax
    reorder
    residual msa
    plots=all;
    var grid positionOrder points laps  fastestLap rank
        fastestLapTime_ms qualPos q1_ms q2_ms q3_ms;
    title "4.b - Gráficos de los planos factoriales (Rotación VARIMAX)";
run;



/* c - Cargas factoriales rotadas  */
proc factor data=f1_final 
    method=principal 
    priors=one 
    nfact=3 
    rotate=varimax 
    reorder 
	residual msa 
    plots=none; 
var grid positionOrder points laps  fastestLap rank
        fastestLapTime_ms qualPos q1_ms q2_ms q3_ms; 
title "4.c - Cargas factoriales rotadas (Rotated Factor Pattern)";
run;



/* d - Evaluación de los residuos y RMSR */


proc factor data=f1_final
    method=principal
    priors=one
    nfact=3
    rotate=varimax
    residual msa 
    plot=none;
var grid positionOrder points laps  fastestLap rank
        fastestLapTime_ms qualPos q1_ms q2_ms q3_ms;
title "4.d - Raíz de la media de los cuadrados de los residuales (RMSR)";
run;


/* e - Coeficientes de puntuación factorial  */

proc factor data=f1_final
    method=principal
    priors=one
    nfact=3
    rotate=varimax
    score
    plot=none;
var grid positionOrder points laps  fastestLap rank
        fastestLapTime_ms qualPos q1_ms q2_ms q3_ms;
title "4.e - Standardized Scoring Coefficients (Rotación VARIMAX)";
run;



/* 6. ANÁLISIS DE CORRESPONDENCIAS SIMPLES (ACS) */

proc univariate data=f1_final noprint;
   var grid points;
   output out=percentiles
      pctlpts = 25 50 75
      pctlpre = grid_ points_
      pctlname = P25 P50 P75;
run;

proc print data=percentiles;
   title "Percentiles (Q1, Q2, Q3) para Grid y Points";
run;

data _null_;
   set Percentiles;
   call symputx('grid_P25', grid_P25);
   call symputx('grid_P50', grid_P50);
   call symputx('grid_P75', grid_P75);

   call symputx('points_P25', points_P25);
   call symputx('points_P50', points_P50);
   call symputx('points_P75', points_P75);
run;

data correspondencias;
   set f1_final;

   /* Eliminamos valores perdidos */
   if missing(grid) or missing(points) then delete;

   /* Categorización de GRID */
   if grid <= &grid_P25 then grid_cat = 1;
   else if grid <= &grid_P50 then grid_cat = 2;
   else if grid <= &grid_P75 then grid_cat = 3;
   else grid_cat = 4;

   /* Categorización de POINTS */
   if points <= &points_P25 then points_cat = 1;
   else if points <= &points_P50 then points_cat = 2;
   else if points <= &points_P75 then points_cat = 3;
   else points_cat = 4;
run;


proc format;
   value grid_cat
      1 = 'Salida delantera (Q1)'
      2 = 'Salida media-delantera (Q1–Q2)'
      3 = 'Salida media-trasera (Q2–Q3)'
      4 = 'Salida trasera (>Q3)';

   value points_cat
      1 = 'Pocos puntos (Q1)'
      2 = 'Puntos bajos (Q1–Q2)'
      3 = 'Puntos medios (Q2–Q3)'
      4 = 'Muchos puntos (>Q3)';
run;

data correspondencias;
   set correspondencias;
   format grid_cat grid_cat. points_cat points_cat.;
run;

/* 6.3.b Matriz indicadora (tabla disyuntiva completa) */

data matriz_indicadora;
    set correspondencias;

    /* Grid */
    grid_Q1 = (grid_cat = 1);
    grid_Q2 = (grid_cat = 2);
    grid_Q3 = (grid_cat = 3);
    grid_Q4 = (grid_cat = 4);

    /* Points */
    points_Q1 = (points_cat = 1);
    points_Q2 = (points_cat = 2);
    points_Q3 = (points_cat = 3);
    points_Q4 = (points_cat = 4);
run;

/* Mostrar solo las primeras observaciones */
proc print data=matriz_indicadora (obs=10);
    var grid_Q1 grid_Q2 grid_Q3 grid_Q4
        points_Q1 points_Q2 points_Q3 points_Q4;
    title "6.3.b - Matriz indicadora (tabla disyuntiva completa)";
run;

proc freq data=correspondencias;
   tables grid_cat * points_cat / chisq plots=freqplot(scale=percent);
   title "Tabla de contingencia Grid × Points (categorizadas por cuartiles)";
run;


proc corresp data=correspondencias
             all
             chi2p
             outc=mapa_ac
             profile=row;
   tables grid_cat, points_cat;
   title "Análisis de Correspondencias Simples (Grid × Points)";
run;
