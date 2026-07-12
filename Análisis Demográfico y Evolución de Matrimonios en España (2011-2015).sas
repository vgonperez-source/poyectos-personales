/* ========================================================================== */
/* 1. DEFINICIÓN DE FORMATOS Y CARGA DEL FICHERO DE MATRIMONIOS               */
/* ========================================================================== */

/* Creación de los catálogos de formatos para las variables categóricas */
proc format;
    value prov
        1 = "Álava" 2 = "Albacete" 3 = "Alicante" 4 = "Almería" 5 = "Ávila"
        6 = "Badajoz" 7 = "Baleares" 8 = "Barcelona" 9 = "Burgos" 10 = "Cáceres"
        11 = "Cádiz" 12 = "Castellón" 13 = "Ciudad Real" 14 = "Córdoba" 15 = "A Coruña"
        16 = "Cuenca" 17 = "Girona" 18 = "Granada" 19 = "Guadalajara" 20 = "Guipúzcoa"
        21 = "Huelva" 22 = "Huesca" 23 = "Jaén" 24 = "León" 25 = "Lleida"
        26 = "La Rioja" 27 = "Lugo" 28 = "Madrid" 29 = "Málaga" 30 = "Murcia"
        31 = "Navarra" 32 = "Ourense" 33 = "Asturias" 34 = "Palencia" 35 = "Las Palmas"
        36 = "Pontevedra" 37 = "Salamanca" 38 = "Santa Cruz de Tenerife" 39 = "Cantabria"
        40 = "Segovia" 41 = "Sevilla" 42 = "Soria" 43 = "Tarragona" 44 = "Teruel"
        45 = "Toledo" 46 = "Valencia" 47 = "Valladolid" 48 = "Bizkaia" 49 = "Zamora"
        50 = "Zaragoza" 51 = "Ceuta" 52 = "Melilla";
run;

proc format;
    value celebracion
        1 = "Católico"
        2 = "Otra religión"
        3 = "Exclusivamente civil";
run;

proc format;
    value hisp
        0 = "No informado"
        1 = "Sí informado"
        . = "Blanco o no consta";   
run;
  
proc format;
    value nacionalidad
        343 = "Colombia" 345 = "Ecuador" 228 = "Marruecos" 128 = "Rumanía"
        351 = "Venezuela" 342 = "Brasil" 326 = "Rep. Dominicana" 340 = "Argentina"
        341 = "Bolivia" 348 = "Perú" 347 = "Paraguay" 154 = "Rusia"
        315 = "Cuba" 110 = "Francia" 321 = "Honduras" 126 = "Alemania"
        135 = "Ucrania" 125 = "Reino Unido" 303 = "México"
        low -< 110 = "Resto Europa"
        111 -< 125 = "Resto Europa"
        127 = "Resto Europa"
        129 -< 135 = "Resto Europa"
        136 -< 154 = "Resto Europa"
        155 -< 200 = "Resto Europa"
        200 -< 228 = "Resto África"
        229 -< 300 = "Resto África"
        300 -< 303 = "Resto América"
        303 -< 315 = "Resto América"
        316 -< 321 = "Resto América"
        322 -< 326 = "Resto América"
        327 -< 340 = "Resto América"
        344 = "Resto América"
        346  = "Resto América"
        349 -< 351 = "Resto América"
        352 -< 400 = "Resto América"
        400 -< 500 = "Resto Asia"
        500 -< 600 = "Oceanía";
run;
  
proc format;
    value origenNac
        1 = "De nacimiento"
        2 = "Posteriormente"
        . = "No consta";
run;

proc format;
    value sexo
        1 = "Varon"
        6 = "Mujer";
run;

proc format;
    value  estado
        1 = "Soltero/a"
        3 = "Viudo/a"
        4 = "Divorciado/a";
run;

proc format;
    value estudios
        00  = "No consta"
        01  = "Analfabeto"
        02  = "Solo leer y escribir"
        03  = "Escuela sin completar Primaria"
        04  = "EGB o ESO completa"
        05  = "Bachiller Superior"
        06  = "FP grado medio"
        07  = "FP superior"
        08  = "Diplomatura"
        09  = "Licenciado"
        10 = "Doctorado";
run;

proc format;
    value profesion
        00  = "No consta"
        01  = "Fuerzas armadas"
        02  = "Dirección empresas"
        03  = "Técnicos y profesionales"
        04  = "Técnicos y profesionales de apoyo"
        05  = "Empleados administrativos"
        06  = "Trabajadores servicios restauración"
        07  = "Trabajadores cualificados agricultura/pesca"
        08  = "Artesanos"
        09  = "Operadores instalaciones y maquinaria"
        10 = "Trabajadores no cualificados"
        11 = "Estudiantes"
        12 = "Tareas del hogar"
        13 = "Pensionistas"
        14 = "Parados";
run;

proc format;
    value tamano
        1 = "<10.000 habitantes"
        2 = "De 10.001 a 20.000"
        3 = "De 20.001 a 50.000"
        4 = "De 50.001 a 100.000"
        5 = "Mayor de 100.000"
        6 = "Capital de provincia";
run;

/* Carga del fichero plano y lectura por posiciones fijas de columnas */
data matrimonios;
    infile "C:\Users\victo\OneDrive\sas\matrimonios.txt" ;
    input 
        provincia 1-2 mes 6-7 year 8-11 tipo_celebracion 12 provincia_res 13-14 
        mes_nac1 21-22 year_nac1 23-26 hispano1 27 extranjero1 28 nacionalidad1 29-31 
        nac_nacimiento1 32 pais_nac1 38-40 sexo 41 estadocivil1 42 year_fallec1 45-48
        orden_viudo1 49 year_divorcio1 52-55 orden_divorcio1 56 estudios1 65-66 
        profesion1 67-68 mes_nac2 69-70 year_nac2 71-74 hispano2 75 extranjero2 76 
        nacionalidad2 77-79 nac_nacimiento2 80 pais_nac2 86-88 sexo2 89 estadocivil2 90 
        year_fallec2 93-96 orden_viudo2 97 year_divorcio2 100-103 orden_divorcio2 104 
        estudios2 113-114 profesion2 115-116 tama_municipio_ins 117 tama_municipio_res 118 
        edad1 129-130 edad2 131-132 nac_esp1 140 nac_ext1 141 nac_esp2 142  nac_ext2 143;

    /* Asignación de los formatos creados a las variables del dataset */
    format provincia provincia_res prov.;
    format tipo_celebracion celebracion.;
    format hispano1 extranjero1 hispano2 extranjero2 nac_esp1 nac_ext1 nac_esp2 nac_ext2  hisp.;
    format nacionalidad1 pais_nac1 nacionalidad2 pais_nac2 nacionalidad.;
    format nac_nacimiento nac_nacimiento2 origenNac.;
    format sexo1 sexo2 sexo.;
    format estadocivil1 estadocivil2 estado.;
    format estudios1 estudios2 estudios.;
    format profesion1 profesion2 profesion.;
    format tama_municipio_ins tama_municipio_res tamano.;
run;

/* Comprobación inicial del dataset importado */
proc print data=matrimonios (obs=10); 
run;


/* ========================================================================== */
/* 2. CONTEO Y EXPANSIÓN DE CÓNYUGES POR GRUPOS DE EDAD                       */
/* ========================================================================== */

/* Generación de una estructura base que contiene el cruce de todas las 
   combinaciones demográficas teóricamente posibles (9.360 observaciones) */
data combinaciones;
    do codigo_provincial = 1 to 52;
        do year = 2011 to 2015; 
            do hispano = 0, 1;
                do sexo = 1, 6;
                    do i = 1 to 9;
                        select (i);
                            when (1) grupo_edad = "15-19";
                            when (2) grupo_edad = "20-24";
                            when (3) grupo_edad = "25-29";
                            when (4) grupo_edad = "30-34";
                            when (5) grupo_edad = "35-39";
                            when (6) grupo_edad = "40-44";
                            when (7) grupo_edad = "45-49";
                            when (8) grupo_edad = "50-54";
                            when (9) grupo_edad = ">=55";
                        end;
                        output;
                    end;
                end;
            end;
        end;
    end;
run;

/* Separación de cada registro matrimonial en dos observaciones individuales 
   (una por cónyuge) y categorización de la edad en su grupo correspondiente */
data conyuges_expandido;
    set matrimonios;

    /* Transformación y recodificación del Cónyuge 1 */
    codigo_provincial = provincia_res;
    hispano = hispano1;
    sexo = sexo1;
    edad = edad1;
    year = year;

    if 15 <= edad <= 19 then grupo_edad = "15-19";
    else if 20 <= edad <= 24 then grupo_edad = "20-24";
    else if 25 <= edad <= 29 then grupo_edad = "25-29";
    else if 30 <= edad <= 34 then grupo_edad = "30-34";
    else if 35 <= edad <= 39 then grupo_edad = "35-39";
    else if 40 <= edad <= 44 then grupo_edad = "40-44";
    else if 45 <= edad <= 49 then grupo_edad = "45-49";
    else if 50 <= edad <= 54 then grupo_edad = "50-54";
    else if edad >= 55 then grupo_edad = ">=55";
    else grupo_edad = "";

    /* Retención únicamente de los individuos dentro de los rangos de edad del estudio */
    if grupo_edad ne "" then output; 

    /* Transformación y recodificación del Cónyuge 2 */
    hispano = hispano2;
    sexo = sexo2;
    edad = edad2;

    if 15 <= edad <= 19 then grupo_edad = "15-19";
    else if 20 <= edad <= 24 then grupo_edad = "20-24";
    else if 25 <= edad <= 29 then grupo_edad = "25-29";
    else if 30 <= edad <= 34 then grupo_edad = "30-34";
    else if 35 <= edad <= 39 then grupo_edad = "35-39";
    else if 40 <= edad <= 44 then grupo_edad = "40-44";
    else if 45 <= edad <= 49 then grupo_edad = "45-49";
    else if 50 <= edad <= 54 then grupo_edad = "50-54";
    else if edad >= 55 then grupo_edad = ">=55";
    else grupo_edad = "";

    if grupo_edad ne "" then output;
run;

/* Agregación de datos: Cálculo del total real de individuos por segmento demográfico */
proc sort data=conyuges_expandido;
    by codigo_provincial year hispano sexo grupo_edad;
run;

data conteos;
    set conyuges_expandido;
    by codigo_provincial year hispano sexo grupo_edad;

    retain total_conyuges;
    if first.grupo_edad then total_conyuges = 0;
    total_conyuges + 1;
    if last.grupo_edad then output;
    
    keep codigo_provincial year hispano sexo grupo_edad total_conyuges;
run;

/* Fusión de la matriz de combinaciones teóricas con los recuentos empíricos.
   Garantiza que los estratos sin observaciones consten explícitamente con valor 0 */
proc sort data=conteos; by codigo_provincial year hispano sexo grupo_edad; run;
proc sort data=combinaciones; by codigo_provincial year hispano sexo grupo_edad; run;

data conyuges_provincia;
    merge combinaciones (in=a) conteos (in=b);
    by codigo_provincial year hispano sexo grupo_edad;
    if a; /* Preservación de la estructura base (Left Join) */
    if total_conyuges = . then total_conyuges = 0;
run;

proc print data=conyuges_provincia (obs=10); 
run;


/* ========================================================================== */
/* 3. DISTRIBUCIÓN GRÁFICA DE MATRIMONIOS POR EDAD Y SEXO                     */
/* ========================================================================== */

/* Segmentación del dataset principal según la variable sexo */
data varones mujeres;
    set conyuges_provincia;
    if sexo = 1 then output varones; 
    else if sexo = 6 then output mujeres;
run;

proc sort data=varones; by grupo_edad; run;
proc sort data=mujeres; by grupo_edad; run;

/* Representación en diagrama de sectores para la población masculina */
title h=3 'Distribución de matrimonios por edad - Varones';
proc gchart data=varones;
    pie grupo_edad / sumvar=total_conyuges
                     slice=arrow
                     percent=inside
                     value=outside;
run;
quit;

/* Representación en diagrama de sectores para la población femenina */
title h=3 'Distribución de matrimonios por edad - Mujeres';
proc gchart data=mujeres;
    pie grupo_edad / sumvar=total_conyuges
                     slice=arrow
                     percent=inside
                     value=outside;
run;
quit;


/* ========================================================================== */
/* 4. ANÁLISIS DE LA EVOLUCIÓN TEMPORAL POR GRUPOS DE EDAD                    */
/* ========================================================================== */

/* Recategorización de edades en el dataset original para su explotación gráfica */
data matrimonios_grupo;
    set matrimonios;

    /* Categorización estructural del Cónyuge 1 */
    select;
        when (15 <= edad1 <= 19) grupo_edad1 = '15-19';
        when (20 <= edad1 <= 24) grupo_edad1 = '20-24';
        when (25 <= edad1 <= 29) grupo_edad1 = '25-29';
        when (30 <= edad1 <= 34) grupo_edad1 = '30-34';
        when (35 <= edad1 <= 39) grupo_edad1 = '35-39';
        when (40 <= edad1 <= 44) grupo_edad1 = '40-44';
        when (45 <= edad1 <= 49) grupo_edad1 = '45-49';
        when (50 <= edad1 <= 54) grupo_edad1 = '50-54';
        when (edad1 >= 55) grupo_edad1 = '>=55';
        otherwise;
    end;

    /* Categorización estructural del Cónyuge 2 */
    select;
        when (15 <= edad2 <= 19) grupo_edad2 = '15-19';
        when (20 <= edad2 <= 24) grupo_edad2 = '20-24';
        when (25 <= edad2 <= 29) grupo_edad2 = '25-29';
        when (30 <= edad2 <= 34) grupo_edad2 = '30-34';
        when (35 <= edad2 <= 39) grupo_edad2 = '35-39';
        when (40 <= edad2 <= 44) grupo_edad2 = '40-44';
        when (45 <= edad2 <= 49) grupo_edad2 = '45-49';
        when (50 <= edad2 <= 54) grupo_edad2 = '50-54';
        when (edad2 >= 55) grupo_edad2 = '>=55';
        otherwise;
    end;
run;

/* Reestructuración a formato vertical para unificar a ambos cónyuges */
data todos_conyuges;
    set matrimonios_grupo;
    sexo = sexo1;
    grupo_edad = grupo_edad1;
    output;
    
    sexo = sexo2;
    grupo_edad = grupo_edad2;
    output;
run;

proc sort data=todos_conyuges;
    by grupo_edad sexo year;
run;

/* Resumen del número de individuos involucrados por año, edad y sexo */
proc means data=todos_conyuges nway noprint;
    class grupo_edad sexo year;
    output out=matrimonios_agrupados (drop=_type_ _freq_) n=numero_conyuges;
run;

proc sort data=matrimonios_agrupados;
    by grupo_edad;
run;

/* Configuración global de ejes y líneas para los gráficos temporales */
axis1 label=(angle=90 'Número de cónyuges') order=(0 to 1200 by 100);
axis2 label=('Año') minor;

symbol1 i=join v=dot c=blue l=1 w=2; /* Línea para Varones (sexo 1) */
symbol2 i=join v=dot c=red  l=1 w=2; /* Línea para Mujeres (sexo 6) */

/* Renderizado dinámico de la evolución temporal (genera un gráfico por cohorte) */
title "Evolución Matrimonios por Grupo de Edad";
proc gplot data=matrimonios_agrupados;
    by grupo_edad; 
    plot numero_conyuges*year=sexo / vaxis=axis1 haxis=axis2;
run;
quit;


/* ========================================================================== */
/* 5. EVOLUCIÓN TEMPORAL RESTRINGIDA A NACIONALES ESPAÑOLES                   */
/* ========================================================================== */

/* Aislamiento de las observaciones pertenecientes exclusivamente a ciudadanos españoles */
data todos_conyuges_esp;
    set matrimonios_grupo;
    
    if hispano1 = 1 then do;
        sexo = sexo1;
        grupo_edad = grupo_edad1;
        output;
    end;
    
    if hispano2 = 1 then do;
        sexo = sexo2;
        grupo_edad = grupo_edad2;
        output;
    end;
run;

proc sort data=todos_conyuges_esp;
    by grupo_edad sexo year;
run;

proc means data=todos_conyuges_esp nway noprint;
    class grupo_edad sexo year;
    output out=matrimonios_esp_agrupados (drop=_type_ _freq_) n=numero_conyuges;
run;

proc sort data=matrimonios_esp_agrupados;
    by grupo_edad;
run;

/* Configuración de ejes y visualización del subgrupo nacional */
axis1 label=(angle=90 'Número de cónyuges (Nacionales Españoles)') order=(0 to 1200 by 100);

proc gplot data=matrimonios_esp_agrupados;
    by grupo_edad;
    plot numero_conyuges*year=sexo / vaxis=axis1 haxis=axis2;
    title "Evolución Matrimonios Españoles por Grupo de Edad";
run;
quit;


/* ========================================================================== */
/* 6. EVOLUCIÓN TEMPORAL RESTRINGIDA A POBLACIÓN EXTRANJERA                   */
/* ========================================================================== */

/* Aislamiento de las observaciones pertenecientes a ciudadanos no españoles */
data todos_conyuges_ext;
    set matrimonios_grupo;
    
    if hispano1 = 0 then do;
        sexo = sexo1;
        grupo_edad = grupo_edad1;
        output;
    end;
    
    if hispano2 = 0 then do;
        sexo = sexo2;
        grupo_edad = grupo_edad2;
        output;
    end;
run;

proc sort data=todos_conyuges_ext;
    by grupo_edad sexo year;
run;

proc means data=todos_conyuges_ext nway noprint;
    class grupo_edad sexo year;
    output out=matrimonios_ext_agrupados (drop=_type_ _freq_) n=numero_conyuges;
run;

proc sort data=matrimonios_ext_agrupados;
    by grupo_edad;
run;

/* Configuración de ejes y visualización del subgrupo extranjero */
axis1 label=(angle=90 'Número de cónyuges (Extranjeros)') order=(0 to 1200 by 100);

proc gplot data=matrimonios_ext_agrupados;
    by grupo_edad;
    plot numero_conyuges*year=sexo / vaxis=axis1 haxis=axis2;
    title "Evolución Matrimonios Extranjeros por Grupo de Edad";
run;
quit;


/* ========================================================================== */
/* 7. IMPORTACIÓN Y ESTRUCTURACIÓN DEL CENSO DEMOGRÁFICO POBLACIONAL          */
/* ========================================================================== */

/* Construcción de la matriz base teórica para la demografía general */
data combinaciones;
    length grupo_edad $10;
    do codigo_provincial = 1 to 52;
        do year = 2011 to 2015;
            do hispanos = 0, 1;  
                do sexo = 1, 2;  
                    do i = 1 to 9;
                        if i = 1 then grupo_edad = "15-19";
                        else if i = 2 then grupo_edad = "20-24";
                        else if i = 3 then grupo_edad = "25-29";
                        else if i = 4 then grupo_edad = "30-34";
                        else if i = 5 then grupo_edad = "35-39";
                        else if i = 6 then grupo_edad = "40-44";
                        else if i = 7 then grupo_edad = "45-49";
                        else if i = 8 then grupo_edad = "50-54";
                        else if i = 9 then grupo_edad = ">=55";
                        output;
                    end; 
                end; 
            end; 
        end; 
    end;
run;

/* Algoritmo de lectura secuencial para extraer metadatos del reporte en formato texto */
data demografia_expandida;
    infile 'C:\Users\victo\OneDrive\sas\demografia.txt';
    input @1 linea $200.;
    retain codigo_provincial year hispanos;

    /* Identificación del bloque provincial a través de los prefijos numéricos */
    if substr(linea,1,2) in ('01','02','03','04','05','06','07','08','09','10',
                             '11','12','13','14','15','16','17','18','19','20',
                             '21','22','23','24','25','26','27','28','29','30',
                             '31','32','33','34','35','36','37','38','39','40',
                             '41','42','43','44','45','46','47','48','49','50',
                             '51','52') then do;
        codigo_provincial = input(substr(linea,1,2), 2.);
    end;

    /* Extracción del identificador temporal (Año) */
    else if input(substr(linea,1,4),4.) >= 2011 and input(substr(linea,1,4),4.) <= 2015 then do;
        year = input(substr(linea,1,4),4.);
    end;

    /* Determinación de la desagregación por origen/nacionalidad */
    else if substr(linea,1,5) = 'Total' then hispanos = 0;
    else if substr(linea,1,8) = 'Española' then hispanos = 1;

    /* Procesamiento de las series numéricas correspondientes a los grupos poblacionales */
    else do;
        /* Parsing de las primeras 9 columnas correspondientes a la población masculina */
        do i = 1 to 9;
            sexo = 1;
            if i = 1 then grupo_edad = "15-19";
            else if i = 2 then grupo_edad = "20-24";
            else if i = 3 then grupo_edad = "25-29";
            else if i = 4 then grupo_edad = "30-34";
            else if i = 5 then grupo_edad = "35-39";
            else if i = 6 then grupo_edad = "40-44";
            else if i = 7 then grupo_edad = "45-49";
            else if i = 8 then grupo_edad = "50-54";
            else if i = 9 then grupo_edad = ">=55";

            poblacion = input(scan(linea, i, ' '), 8.);
            if poblacion ne . then output;
        end;

        /* Parsing de las últimas 9 columnas correspondientes a la población femenina */
        do i = 1 to 9;
            sexo = 2;
            if i = 1 then grupo_edad = "15-19";
            else if i = 2 then grupo_edad = "20-24";
            else if i = 3 then grupo_edad = "25-29";
            else if i = 4 then grupo_edad = "30-34";
            else if i = 5 then grupo_edad = "35-39";
            else if i = 6 then grupo_edad = "40-44";
            else if i = 7 then grupo_edad = "45-49";
            else if i = 8 then grupo_edad = "50-54";
            else if i = 9 then grupo_edad = ">=55";

            poblacion = input(scan(linea, i+9, ' '), 8.);
            if poblacion ne . then output;
        end; 
    end;

    keep codigo_provincial year hispanos sexo grupo_edad poblacion;
run;

/* Agrupación y totalización de los extractos demográficos */
proc sort data=demografia_expandida;
    by codigo_provincial year hispanos sexo grupo_edad;
run;

data poblacion_agrupada;
    set demografia_expandida;
    by codigo_provincial year hispanos sexo grupo_edad;
    retain total_poblacion;

    if first.grupo_edad then total_poblacion = 0;
    total_poblacion + poblacion;

    if last.grupo_edad then output;

    keep codigo_provincial year hispanos sexo grupo_edad total_poblacion;
run;

/* Fase de ensamblaje: Cruce de la demografía real con la matriz de 9.360 iteraciones */
proc sort data=combinaciones;
    by codigo_provincial year hispanos sexo grupo_edad;
run;

proc sort data=poblacion_agrupada;
    by codigo_provincial year hispanos sexo grupo_edad;
run;

data demografia_final;
    merge combinaciones (in=a) poblacion_agrupada (in=b);
    by codigo_provincial year hispanos sexo grupo_edad;
    
    if a; /* Fuerza la preservación de toda la matriz dimensional teórica */
    if total_poblacion = . then total_poblacion = 0;
run;

/* Validación del dataset maestro final */
proc print data=demografia_final (obs=30); 
run;
