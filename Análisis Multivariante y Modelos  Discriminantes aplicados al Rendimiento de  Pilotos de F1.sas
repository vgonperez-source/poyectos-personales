/* El primer paso es Importar los cinco ficheros necesarios para el proyecto */

proc import datafile="C:\Users\victo\OneDrive\TERCERO\aprendizaje no supervisado\results.csv"
    out=results dbms=csv replace;
    getnames=yes;
run;

proc import datafile="C:\Users\victo\OneDrive\TERCERO\aprendizaje no supervisado\drivers.csv"
    out=drivers dbms=csv replace;
    getnames=yes;
run;

proc import datafile="C:\Users\victo\OneDrive\TERCERO\aprendizaje no supervisado\lap_times.csv"
    out=lap_times dbms=csv replace;
    getnames=yes;
run;

proc import datafile="C:\Users\victo\OneDrive\TERCERO\aprendizaje no supervisado\qualifying.csv"
    out=qualifying dbms=csv replace;
    getnames=yes;
run;

proc import datafile="C:\Users\victo\OneDrive\TERCERO\aprendizaje no supervisado\status.csv"
    out=status dbms=csv replace;
    getnames=yes;
run;



/* ========================================================= */
/* DEPURACIÓN, AGREGACIÓN Y CONSTRUCCIÓN DEL DATASET MAESTRO */
/* ========================================================= */

/* ----------------------------- */
/* Tabla A: resultados de carrera */
/* ----------------------------- */
data results_clean;
    set results;
    if driverId ne .;

    /* Indicador correcto de carrera terminada (Finished) */
    finished = (statusId = 1);
run;

/* Agregación por piloto */
proc means data=results_clean noprint;
    class driverId;
    var positionOrder finished;
    output out=agg_results
        mean(positionOrder)=pos_final_media
        mean(finished)=pct_finish;
run;


/* ----------------------------- */
/* Tabla B: clasificación */
/* ----------------------------- */
data qualifying_clean;
    set qualifying;
    if driverId ne .;
    if position ne .;
run;

proc means data=qualifying_clean noprint;
    class driverId;
    var position;
    output out=agg_quali
        mean(position)=pos_quali_media;
run;


/* ----------------------------- */
/* Tabla C: ritmo de carrera */
/* ----------------------------- */
data laps_clean;
    set lap_times;
    if driverId ne .;
    if milliseconds ne .;
run;

proc means data=laps_clean noprint;
    class driverId;
    var milliseconds;
    output out=agg_laps
        mean(milliseconds)=lap_mean
        std(milliseconds)=lap_std;
run;


/* ----------------------------- */
/* Unión de tablas agregadas */
/* ----------------------------- */
proc sort data=agg_results; by driverId; run;
proc sort data=agg_quali;   by driverId; run;
proc sort data=agg_laps;    by driverId; run;

/* Nos quedamos con pilotos con información suficiente */
data master;
    merge agg_results(in=a)
          agg_quali
          agg_laps(in=b);
    by driverId;

    if a and b;
run;


/* ----------------------------- */
/* Añadimos información del piloto */
/* ----------------------------- */
proc sort data=drivers; by driverId; run;

data master_final;
    merge master drivers(keep=driverId forename surname);
    by driverId;
run;



/* ahora vamos a seleccionar las variables*/

data master_vars;
    set master_final;
    drop _type_ _freq_;
run;

proc corr data=master_vars;
    var pos_final_media pct_finish pos_quali_media lap_mean lap_std;
run;






/* DEPURACIÓN DEL DATASET FINAL */
data master_dep;
    set master_vars;

    /* Eliminamos observaciones con valores perdidos */
    if cmiss(pos_final_media,
             pct_finish,
             pos_quali_media,
             lap_mean,
             lap_std) = 0;
run;

/* Comprobación de ausentes tras la depuración */
proc means data=master_dep mean nmiss;
    var pos_final_media
        pct_finish
        pos_quali_media
        lap_mean
        lap_std;
run;


/* ============================ */
/* DETECCIÓN DE ATÍPICOS */
/* ============================ */

/* Cargamos la macro de atípicos */
%include "C:\Users\victo\OneDrive\TERCERO\aprendizaje no supervisado\MACRO_ATIPICOS (1).sas";

/* Aplicamos la macro de outliers sobre el dataset depurado */
%outliers_mult(
    data=master_dep,
    var= pos_final_media
         pct_finish
         pos_quali_media
         lap_mean
         lap_std,
    id=driverId
);


/* Visualización de atípicos multivariantes */
proc print data=outliers;
    where conclusion ne "caso logico";
    title "Observaciones atípicas multivariantes";
run;


/* Visualización de atípicos univariantes */
proc print data=Univariante;
    where llamada = "PROBLEMAS";
    title "Observaciones con problemas univariantes";
run;


/* ============================ */
/* ELIMINACIÓN DEL ATÍPICO EXTREMO */
/* ============================ */
/* Sustituir driverId por el detectado por la macro */

data master_dep;
    set master_dep;
    if driverId ne 28;
run;





/* ahora realizamos la estandarización de las variables para el análisis cluster */
proc standard data=master_dep mean=0 std=1 out=cluster_std;
    var pos_final_media
        pct_finish
        pos_quali_media
        lap_mean
        lap_std;
run;

proc means data=cluster_std mean std min max;
    var pos_final_media
        pct_finish
        pos_quali_media
        lap_mean
        lap_std;
run;



/*ahora vamos con la elección del método de distancia y del número de clusters*/
 
/*enlace promedio*/
proc cluster data=cluster_std
    method=average
    nonorm
    pseudo rsquare
    plots=den(vertical)
    outtree=tree_avg
    print=15;
    var pos_final_media pct_finish pos_quali_media lap_mean lap_std;
    id driverId;
run;



/*distancia entre centroides*/
proc cluster data=cluster_std
    method=centroid
    std nonorm
    pseudo rsquare
    plots=den(vertical)
    outtree=tree_centroid
    print=15;
    var pos_final_media pct_finish pos_quali_media lap_mean lap_std;
    id driverId;
run;

/* Distancia Ward  */
proc cluster data=cluster_std
    method=ward
    std nonorm
    pseudo rsquare
    plots=den(vertical)
    outtree=tree_ward
    print=15;
    var pos_final_media pct_finish pos_quali_media lap_mean lap_std;
    id driverId;
run;

/* Enlace Simple  */
proc cluster data=cluster_std
    method=single
    std nonorm
    pseudo rsquare
    plots=den(vertical)
    outtree=tree_single
    print=15;
    var pos_final_media pct_finish pos_quali_media lap_mean lap_std;
    id driverId;
run;


/* Enlace Completo  */
proc cluster data=cluster_std
    method=complete
    std nonorm
    pseudo rsquare
    plots=den(vertical)
    outtree=tree_complete
    print=15;
    var pos_final_media pct_finish pos_quali_media lap_mean lap_std;
    id driverId;
run;




/* Obtener clusters finales con Ward (n = 4) */
proc tree data=tree_ward n=4 out=clasif_Ward;
    copy pos_final_media pct_finish pos_quali_media lap_mean lap_std;
    id driverId;
run;


/* Ordenar la clasificación por cluster */
proc sort data=clasif_Ward; 
    by cluster; 
run;

/* Visualizar composición de clusters */
proc print data=clasif_Ward;
    title 'Composición de Clusters (Ward, n=4)';
run;


/* Frecuencia de observaciones por cluster */
proc freq data=clasif_Ward;
    tables cluster;
run;


/* Representación gráfica de los clusters finales */
proc sgplot data=clasif_Ward;
    scatter x=driverId y=pos_final_media / group=cluster;
    keylegend / title="Cluster Membership";
    title "Representación de Clusters según pos_final_media";
run;





/* analisis no jerárquico */ 



/* 5.1 CENTROIDES INICIALES (Ward, k = 4)  */


proc sort data=clasif_Ward;
    by cluster;
run;

/* Calculamos los centroides (medias) de cada cluster */
proc means data=clasif_Ward noprint;
    by cluster;
    var pos_final_media
        pct_finish
        pos_quali_media
        lap_mean
        lap_std;
    output out=centroides_ward
        mean=pos_final_media
             pct_finish
             pos_quali_media
             lap_mean
             lap_std;
run;

/* Mostrar la tabla de centroides */
proc print data=centroides_ward;
    title "Centroides iniciales obtenidos con Ward (k = 4)";
run;

/* 5.2 K-MEANS CON CENTROIDES INICIALES     */


proc fastclus data=cluster_std
              seed=centroides_ward
              maxclusters=4
              maxiter=100
              replace=full
              out=km_con_centroides
              outstat=stat_km_centroides;
    var pos_final_media
        pct_finish
        pos_quali_media
        lap_mean
        lap_std;
    id driverId;
run;

/* Distribución de observaciones */
proc freq data=km_con_centroides;
    tables cluster;
    title "Distribución de clusters (k-means con centroides Ward)";
run;


/*5.3 graficos de apoyo */

proc sgplot data=km_con_centroides;
    vbox lap_mean / category=cluster;
    title "Distribución del ritmo medio de carrera por cluster (k-means)";
run;

proc sgplot data=km_con_centroides;
    vbox pos_final_media / category=cluster;
    title "Distribución de la posición final media por cluster (k-means)";
run;




/*6) ANÁLISIS DISCRIMINANTE */


/* 6.1 Variables que mejor discriminan los clusters */
proc stepdisc data=clasif_kmeans
              method=stepwise
              sle=0.05
              sls=0.10;
    class cluster;
    var pos_final_media
        pct_finish
        pos_quali_media
        lap_mean
        lap_std;
run;


/* 6.2 Comprobación de normalidad multivariante */
%include "C:\Users\victo\OneDrive\TERCERO\aprendizaje no supervisado\MACRO_NORMALIDAD.sas";

/
%NORMAL_MULT(
    DATA=clasif_kmeans,
    VAR=pos_final_media
        pct_finish
        pos_quali_media
        lap_mean
        lap_std
);



/* 6.3 Análisis discriminante  */
data activo test;
    set clasif_kmeans;
    if _N_ <= 99 then output activo;   /* ~70% de 141 */
    else output test;
run;


proc discrim data=activo
             pool=yes
             testdata=test
             testlisterr
             outstat=salida_disc;
    class cluster;
    var pos_final_media
        pct_finish
        pos_quali_media
        lap_mean
        lap_std;
    priors proportional;
run;

/* funciones discriminantes */
proc print data=salida_disc;
    where _TYPE_ = 'LINEAR';
run;
