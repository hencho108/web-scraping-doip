Web-Scraping My University's Job Board
================
[Hendrik Mischo](https://github.com/hendrik-mischo)
30 January 2019

I am a student at the University of the Balearic Islands. The Department of Professional Orientation ([DOIP](https://fueib.org/ca/doip/319/oferta_doip)) at my university has a job board where new internship and job opportunities are posted regularily.

When looking for a suitable position there are several issues with this site:

-   You can't filter for jobs relevant to your study programme.
-   You can't view more than 10 offers at a time.
-   For details you have to click on each offer.

Therefore, I decided to make a web-scraping tool to get all the relevant information at the push of a button. This allows to export the data to a CSV-file with which you can do whatever filtering you require in Excel, get notified about new offers, further analyze the data and quickly find relevant offers.

Let's start by importing the relevant libraries.

``` r
library(tidyverse)  # General-purpose data wrangling
library(rvest)      # Parsing of HTML/XML files  
library(stringr)    # String manipulation

# Get main page
url_doip = "https://fueib.org/es/doip/319/oferta_doip/1/pagina"
```

Now we define the necessary functions to scrape just the links to all job postings from the main page.

``` r
# Get links to all postings on a given page
get_offer_urls = function(url){
  html = read_html(url)
  html %>% 
    html_nodes("#fue-ofertas-list > li> div.media-body > div > h3 > a") %>% 
    html_attr("href") 
}

# Get last page number
get_last_page = function(html){
  pages_data = html %>% 
    html_nodes("#fue-sidebar-affix-wrapper > div.col-lg-9.col-md-8.col-sm-12.col-xs-12 > 
               section > div:nth-child(1) > div > nav > ul > li > a") %>% 
    html_attr("href")
  
  pages_data[(length(pages_data))] %>%
    strsplit("/") %>%
    unlist() %>%
    nth(6) %>%
    as.numeric()
}

# Get list of all links to all postings
# by applying get_offer_urls() to all pages
get_all_offer_urls = function(url){
  
  # Read first page
  first_page = read_html(url)
  
  # Extract the number of pages that have to be queried
  latest_page_number = get_last_page(first_page)
  
  # Generate the target URLs
  list_of_pages = str_c("https://fueib.org/es/doip/319/oferta_doip/", 1:latest_page_number, "/pagina")
  
  # Apply get_offer_urls() too all links
  list_of_offers = list_of_pages %>% 
    map(get_offer_urls) %>%
    unlist()
  
  # Return list of offers
  list_of_offers
}
```

Using these functions, let's get a list of all the postings currently available on DOIP.

``` r
list_of_offers = get_all_offer_urls(url_doip)
head(list_of_offers)
```

    [1] "https://wwws.fueib.es/fueib/doip/cercaOfertes/detallOferta-51381.html?locale=es"
    [2] "https://wwws.fueib.es/fueib/doip/cercaOfertes/detallOferta-51378.html?locale=es"
    [3] "https://wwws.fueib.es/fueib/doip/cercaOfertes/detallOferta-51375.html?locale=es"
    [4] "https://wwws.fueib.es/fueib/doip/cercaOfertes/detallOferta-51374.html?locale=es"
    [5] "https://wwws.fueib.es/fueib/doip/cercaOfertes/detallOferta-51372.html?locale=es"
    [6] "https://wwws.fueib.es/fueib/doip/cercaOfertes/detallOferta-51371.html?locale=es"

Next we need to define another function that returns NA in case the information we want to scrape is missing. This is crucial to correctly propagate our data table.

``` r
# Function that returns NA if the text we want to scrape is missing
html_text_na = function(x, ...) {
  txt = try(html_text(x, ...))
  if (inherits(txt, "try-error") | 
     (length(txt)==0)) { return(NA) }
  return(txt)
}
```

Now that we have the links to all postings, we can define several functions to scrape each piece of information that is relevant to us from these postings.

``` r
# Get title of offer
get_offer_title = function(html){
  html %>% 
    html_nodes(xpath='//*[@id="oferta.dadesLloc.nomOferta"]') %>%
    html_text_na() 
}

# Get reference number
get_offer_ref = function(html){
  html %>% 
    html_nodes(xpath='//*[@id="oferta.idOferta"]') %>%
    html_text_na() 
}

# Get requirements
get_requirements = function(html){
  html %>% 
    html_nodes(xpath='//td[@class="llista"]/span') %>%
    html_text_na() %>%
    toString()
}

# Get number of hours
get_hours = function(html){
  html %>% 
    html_nodes(xpath='//*[@id="oferta.dadesLloc.doip.N.practiques.S.numHoresDiaries"]') %>%
    html_text_na() 
}

# Get tasks
get_tasks = function(html){
  html %>% 
    html_nodes(xpath='//*[@id="oferta.dadesLloc.doip.N.desTreball"]') %>%
    html_text_na() 
}

# Get activity
get_activity = function(html){
  html %>% 
    html_nodes(xpath='//*[@id="oferta.empresa.activitat"]') %>%
    html_text_na() 
}

# Get Location
get_location = function(html){
  html %>% 
    html_nodes(xpath='//*[@id="oferta.estatEspanyol.E.nomMunicipi"]') %>%
    html_text_na() 
}

# Get salary
get_salary = function(html){
  html %>% 
    html_nodes(xpath='//*[@id="oferta.dadesLloc.doip.N.sou"]') %>%
    html_text_na() 
}

# Get offer type
get_offer_type = function(html){
  html %>% 
    html_nodes(xpath='//*[@id="oferta.dadesLloc.doip.N.nomTipusOferta"]') %>%
    html_text_na() 
}

# Get offer period
get_offer_period = function(html){
  start = html %>%
    html_nodes(xpath='//*[@id="oferta.dataInici"]') %>%
    html_text_na() %>%
    str_trim()
  end = html %>%
    html_nodes(xpath='//*[@id="oferta.dataFinal"]') %>%
    html_text_na() %>%
    str_trim()
  paste(start, end, sep = " - ")
}
```

Let's define one more function that extracts all the relevant information at once for a given posting by executing the previously defined functions.

``` r
# Get all information
get_all_info = function(html){
  
  url = read_html(html)
  
  list("title" = get_offer_title(url), 
       "reference" = get_offer_ref(url),
       "requirements" = get_requirements(url),
       "hours" = get_hours(url),
       "tasks" = get_tasks(url),
       "activities" = get_activity(url),
       "location" = get_location(url),
       "salary" = get_salary(url),
       "type" = get_offer_type(url),
       "period" = get_offer_period(url),
       "link" = toString(html)
       )
}
```

Now all that is left to do is applying `get_all_info()` to the list of links to the postings `list_of_offers` to get all information from all postings and gather it all in a data frame. This may take several minutes.

``` r
# Get list of all info
all_info = list_of_offers %>% map(get_all_info) 

# Create data frame from list
data = do.call(rbind.data.frame, all_info)

# Convert all columns to character
data[] = lapply(data, as.character)

# Let's also save it as a CSV
write.table(data, file = "doip_scraped.csv", na = "", fileEncoding = "UTF-8", sep = ";")

# Check head of data
head(data)
```

                                           title reference
    2                    Controller Financiero/a     51381
    298                   Consultor/a en energ??a     51378
    3       T??cnico/a Superior Riesgos Laborales     51375
    4              Asistente marketing tur??stico     51374
    5   T??cnico/a de apoyo al responsable de PRL     51372
    6                 Asistente Marketing Online     51371
                                                                                                                                                                                                                                                                                                                                                                                              requirements
    2                                                                                                                                                                                                                                                                                                                               Datos de inter??s, Grado de Administraci??n de Empresas, Microsoft Excel
    298                              Datos de inter??s, Ingenier??a industrial, Grado de Ingenier??a Electr??nica Industrial y Autom??tica, Ingenier??a en telecomunicaci??n, Grado de F??sica, Licenciatura en f??sica, Ingenier??a electr??nica, Grado de Ingenier??a Electr??nica Industrial, Rob??tica y Mecatr??nica, Doble titulaci??n: grado de matem??ticas y ingenier??a telem??tica, Grado de Ingenier??a Telem??tica
    3   Datos de inter??s, Grado de Ingenier??a de Edificaci??n, Arquitectura t??cnica, Salud Laboral, Salud Laboral. Ergonom??a y Psicosociolog??a Aplicada, Salud Laboral. Higiene Industrial, Salud Laboral. Seguridad en el Trabajo, M??ster Universitario en Salud Laboral (Prevenci??n de Riesgos Laborales), Ingl??s, T??cnico/a superior en prevenci??n de riesgos laborales, Microsoft Excel, Microsoft Word
    4                                                                                                                                                                                                                                                                                                                                                           Datos de inter??s, Alem??n, Grado de Turismo
    5                                                                                                                                                                                                                                  Datos de inter??s, Ingenier??a t??cnica en obras p??blicas, Grado de Ingenier??a de Edificaci??n, M??ster Universitario en Salud Laboral (Prevenci??n de Riesgos Laborales)
    6                                                                                                                                                                                                                                                                                                                                        Datos de inter??s, Alem??n, Grado de Administraci??n de Empresas
        hours
    2    <NA>
    298  <NA>
    3    <NA>
    4       5
    5    <NA>
    6       5
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      tasks
    2                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              * Cuentas a cobrar: emisi??n y seguimiento de facturas.* Elaboraci??n y seguimiento de presupuestos. * Control de desviaciones y de previsiones de gastos.* Definir indicadores financieros claves.* Realizar informes financieros* Definir reglas contables* Mejora de procesos contables
    298                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    Puesto de trabajo en proyecto Redes Inteligentes en control y seguimiento de proyectos nacionales e internacionales ligados a coches el??ctricos y otros ??mbitos de Smart Cities.
    3                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  Sectores construcci??n, hosteler??a y comercio, otros.
    4                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     *Soporte en gesti??n de contenidos para redes sociales : Facebook, Instagram. *Gesti??n de reputaci??n online de hoteles y restaurantes en los principales portales tur??sticos : Holiday Check, Tripadvisor, Google?*Soporte en implantaci??n de contenidos en nueva app desarrollada por la empresa.
    5   En estos momentos necesitamos incorporar en BALEARES para importante empresa de energ??a un:T??CNICO/A DE APOYO AL RESPONSABLE PRLImprescindible ser residente en Baleares.Buscamos Ingenieros/as T??cnicos/as, en posesi??n del m??ster de T??cnico Superior en Prevenci??n de Riesgos Laborales, a ser posible en sus tres especialidades y se valorar?? que posean el curso de Coordinador de Seguridad y Salud (200h.), ISO 9001, ISO 14001, OSHAS 18001, as?? como alta capacidad de organizaci??n, gesti??n y planificaci??n.Sus funciones en el lugar de trabajo ser??n las de apoyo al t??cnico responsable de Prevenci??n de Riesgos Laborales y Medioambiente en la coordinaci??n de los sistemas de gesti??n de calidad interno, inspecciones en campo y redacci??n de informes entre otras funciones.Ofrecemos estabilidad, formaci??n y posibilidad de crecimiento dentro de la compa????a. Contrato estable 6M+6M+Indefinido, en jornada parcial de 25-30 horas semanales.
    6                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      *Soporte en gesti??n de contenidos para redes sociales : Facebook, Instagram. *Gesti??n de reputaci??n online de hoteles y restaurantes en los principales portales tur??sticos : Holiday Check, Tripadvisor, Google*Soporte en implantaci??n de contenidos en nueva app desarrollada por la empresa.
                                       activities location     salary
    2                                                Palma     18.000
    298               Consultoria y proyectos TIC    Palma      1.500
    3                         Servei de prevenci??    Palma      21000
    4             MARKETING ONLINE Y COMUNICACI??N    Palma        450
    5   Servicios de PRL en obras de construcci??n    Palma A CONVENIR
    6             MARKETING ONLINE Y COMUNICACI??N    Palma        450
                                                               type
    2                                                Oferta laboral
    298                                              Oferta laboral
    3                                                Oferta laboral
    4   Pr??cticas para estudiantes universitarios grado y postgrado
    5                                                Oferta laboral
    6   Pr??cticas para estudiantes universitarios grado y postgrado
                         period
    2             18/02/2019 - 
    298 03/07/2019 - 31/12/2019
    3             25/02/2019 - 
    4   01/04/2019 - 30/09/2019
    5             29/01/2019 - 
    6   01/04/2019 - 30/09/2019
                                                                                   link
    2   https://wwws.fueib.es/fueib/doip/cercaOfertes/detallOferta-51381.html?locale=es
    298 https://wwws.fueib.es/fueib/doip/cercaOfertes/detallOferta-51378.html?locale=es
    3   https://wwws.fueib.es/fueib/doip/cercaOfertes/detallOferta-51375.html?locale=es
    4   https://wwws.fueib.es/fueib/doip/cercaOfertes/detallOferta-51374.html?locale=es
    5   https://wwws.fueib.es/fueib/doip/cercaOfertes/detallOferta-51372.html?locale=es
    6   https://wwws.fueib.es/fueib/doip/cercaOfertes/detallOferta-51371.html?locale=es

Now let's just select the ones that are relevant to my study programme.

``` r
offers_madm = data %>%
  select(everything()) %>%
  filter(str_detect(tolower(requirements), "datos masivos"))

write.table(offers_madm, file = "offers_madm.csv", na = "", fileEncoding = "UTF-8", sep = ";")

offers_madm
```

                                            title reference
    1                  Programador/a Estad??stic/a     51307
    2  Budgeting & Forecasting intern - Hotelbeds     51005
    3 Pr??ctiques Business Intelligence i Big Data     50541
    4               Business Intelligence Analyst     51232
    5                       Responsable BI Hotels     50993
    6                           Software engineer     50590
    7                          Beca Sales Analyst     50832
                                                                                                                                                                                                                                                                                                                                                                                                  requirements
    1                                                                                                                                                                                                                                                 Datos de inter??s, Ingl??s, Grado de Matem??ticas, Grado de Ingenier??a Inform??tica, M??ster Universitario de An??lisis de Datos Masivos en Econom??a y Empresa
    2                                             Datos de inter??s, Ingl??s, Grado de Ingenier??a Electr??nica Industrial y Autom??tica, Grado de Matem??ticas, Grado de Econom??a, Grado de Administraci??n de Empresas, M??ster Universitario de An??lisis de Datos Masivos en Econom??a y Empresa, M??ster Universitario en Ingener??a Industrial, Revenue Management, M??ster Universitario en Contabilidad y Auditoria
    3                                                                                                                                                                                                                                                                                          Datos de inter??s, Grado de Matem??ticas, M??ster Universitario de An??lisis de Datos Masivos en Econom??a y Empresa
    4 Datos de inter??s, Ingl??s, Microsoft Excel, Grado de Ingenier??a Inform??tica, Licenciatura en administraci??n y direcci??n de empresas, Ingenier??a en inform??tica, Grado de Matem??ticas, Licenciatura en matem??ticas, Doble titulaci??n: grado de administraci??n de empresas y grado de turismo, Grado de Administraci??n de Empresas, M??ster Universitario de An??lisis de Datos Masivos en Econom??a y Empresa
    5                                                                                                                    Datos de inter??s, Ingenier??a en inform??tica, Grado de Ingenier??a Inform??tica, Ingenier??a en telecomunicaci??n, Grado de Ingenier??a Telem??tica, Doble titulaci??n: grado de matem??ticas y ingenier??a telem??tica, M??ster Universitario de An??lisis de Datos Masivos en Econom??a y Empresa
    6                                                Datos de inter??s, Grado de Ingenier??a Inform??tica, Grado de Ingenier??a Telem??tica, Grado de Matem??ticas, Grado de Ingenier??a Electr??nica Industrial y Autom??tica, Doble titulaci??n: grado de matem??ticas y ingenier??a telem??tica, M??ster Universitario de An??lisis de Datos Masivos en Econom??a y Empresa, M??ster Universitario en Ingenier??a Inform??tica
    7                                                                                            Datos de inter??s, Ingl??s, Grado de Administraci??n de Empresas, Grado de Econom??a, Grado de Turismo, Doble titulaci??n: grado de administraci??n de empresas y grado de turismo, Doble titulaci??n: grado de economia y grado de turismo, M??ster Universitario de An??lisis de Datos Masivos en Econom??a y Empresa
      hours
    1     5
    2     5
    3     5
    4  <NA>
    5  <NA>
    6     5
    7     5
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  tasks
    1                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         Funciones detalladas: - Creaci??n de documentos estad??sticos.- Realizaci??n de actividades de programaci??n para producir resultados estad??sticos, es decir, mesas, listas y figuras. - Participaci??n en las actividades de gesti??n de datos, limpieza de datos, revisi??n de datos, gesti??n de queries y database lock. - Aplicaci??n de los est??ndares de la empresa, y participaci??n activa en la mejora y estandarizaci??n de los procesos.
    2                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         The Budgeting intern role is to support during the commercial planning process, track the key drivers impacting the bedbank business ambition and communicate to Management the results, providing greater visibility and understanding of trading.Functions:- Support the development of planning models (e.g. next fiscal year, 5 Year Plan) based on the business growth drivers- Assist in tracking the organization's performance against the established ambition- Support the elaboration of ad-hoc reports following requests from other teams, both within and outside Commercial Strategy function- Assist in providing objective analysis that might be challenged by other business functions within the organization- Being able to deal with tight deadlines to give answers regarding performance of specific drivers (e.g. new clients, new hotels)Candidate Profile:- Degree in Business Administration / Economics / Engineering / Mathematics or similar- Excellent analytical, numerical and presentation skills; able to interpret, summarize and present complex data sets from multiple sources clearly and succinctly; strong attention to detail- Methodical and organized mindset- Proactive attitude- Team spirit and attitude, comfortable working in multiple initiatives simultaneously- Very good knowledge of Excel- Very good English skills, both written and spoken (Spanish would be a distinct advantage)Candidate Profile:- Degree in Business Administration / Economics / Engineering / Mathematics or similar- Excellent analytical, numerical and presentation skills; able to interpret, summarize and present complex data sets from multiple sources clearly and succinctly; strong attention to detail- Methodical and organized mindset- Proactive attitude- Team spirit and attitude, comfortable working in multiple initiatives simultaneously- Very good knowledge of Excel- Very good English skills, both written and spoken (Spanish would be a distinct advantage)
    3                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        - Tareas en proyectos de Business Intelligence y Big Data.- Manejo de herramientas actuales del mercado de an??lisis y gesti??n de datos.- Manejo de Office a nivel usuario.
    4 For our Business Intelligence team in Palma Head Quarters we are seeking a Business Analyst. Our mission is to provide accurate, business valuable and action driven information to all the employees of the organization.In today?s complex business environment, an organization?s adaptability, agility, and ability to manage constant change through innovation can be keys to success. That?s where business analysis comes in. Corporations achieve goals through projects that translate customer needs into new products, services, and profits. Business analysts can make it all happen.The business analyst's primary objective is helping businesses implement analytical technology solutions by understanding the needs of business to translate business requirements into technical requirements.GENERAL DESCRIPTIONAs a BI Analyst, you will be responsible of taking care of our worldwide business community and giving them full support by:Effectively manage all requests received by the business community, distributing them to each responsible teammate, following up and making sure they are completed.Elaborate advanced and call-to-action reports to support all kind of business decisions using the set of tools available like Excel, Power BI and SSRS.Solve any questions or incidences that could arise with regards data analysis or usage of the BI platform.Manage with precise language all communications channels available by:Creating, managing and sending all communications and newsletters required to keep updated the business community about news on our platform.Generating content and fully managing our departmental section in the Corporate Intranet.Give support to all training activities and to all new product releases.Collaborate with colleagues within all MTS Shared Service Center to make sure all tasks and projects assigned are accomplished.RequirementsDegree in Information Technologies and/or Business Administration or related careers.Advanced knowledge of Microsoft Excel is a must.High-level of written and spoken English is a must.Experience working with SQL Server would be a plus.Knowledge of advanced data visualization tools like Power BI would be plus.Knowledge of Sharepoint would be a plus.Attention to detail with analytical mindset.Excellent communications skills.High capacity of multitasking.Results-oriented and self-organized working style.Experienced working in international environments.
    5                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            La persona seleccionada liderar?? o formar?? parte de un equipo encargado de mejorar la toma de decisiones a la compa????a mediante datos.Tareas:-Definir los dashboards y paneles de control de datos necesarios en cada departamento-Dise??ar la arquitectura de datos necesaria (c??mo viajan los datos desde las fuentes hasta los dashboards)-Ejecutar esta arquitectura y el reporting
    6                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          Desarrollar aplicaciones e integraciones del sector tur??stico.Integrarse en un grupo coordinado de gesti??n de proyectos.
    7                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              Buscamos a l@s mejores profesionales que compartan la ilusi??n por seguir creciendo hacia un horizonte muy prometedor. Ofrecemos formar parte de una compa????a l??der dedicada a la automoci??n y turismo en Baleares. Somos una compa????a joven y din??mica en pleno crecimiento, donde podr??s desarrollar tus capacidades profesionales y personales.Queremos contar con el mejor equipo, por eso valoramos tu ilusi??n y compromiso hacia la compa????a. Llega donde quieras en OK Group. ??Los l??mites los pones t??!Las funciones que desarrollar?? la persona que se incorpore ser??n las siguientes: - Confecci??n de los partes diarios de ventas y env??o a las bases.- Gesti??n de facturas de los diferentes acuerdos comerciales.- Extracci??n de datos de sistema para la elaboraci??n de informes.- Creaci??n y actualizaci??n de Bases de Datos.- Carga de datos en sistema por oficina.- Gesti??n y resoluci??n incidencias de tarifas con las diferentes oficinas.- Confecci??n mensual de los incentivos de los departamentos de Contrataci??n y Check In.
                                                     activities location
    1                                    Serveis d'investigaci??    Palma
    2                                                   turismo    Palma
    3 Consultoria Informatica y venta de productos relacionados    Palma
    4                                                   Turismo    Palma
    5                                Desenvolupament tecnol??gic    Palma
    6     Programaci??, comercialitzaci?? i m??rqueting inform??tic    Palma
    7                             Alquiler y venta de veh??culos    Palma
            salary                                                        type
    1          450 Pr??cticas para estudiantes universitarios grado y postgrado
    2          550 Pr??cticas para estudiantes universitarios grado y postgrado
    3          500 Pr??cticas para estudiantes universitarios grado y postgrado
    4 Seg??n perfil                                              Oferta laboral
    5        35000                                              Oferta laboral
    6          650 Pr??cticas para estudiantes universitarios grado y postgrado
    7          450 Pr??cticas para estudiantes universitarios grado y postgrado
                       period
    1 28/01/2019 - 31/05/2019
    2 01/02/2019 - 21/06/2019
    3 22/01/2019 - 28/06/2019
    4           04/02/2019 - 
    5           02/01/2019 - 
    6 17/12/2018 - 27/06/2019
    7 22/10/2018 - 18/02/2019
                                                                                 link
    1 https://wwws.fueib.es/fueib/doip/cercaOfertes/detallOferta-51307.html?locale=es
    2 https://wwws.fueib.es/fueib/doip/cercaOfertes/detallOferta-51005.html?locale=es
    3 https://wwws.fueib.es/fueib/doip/cercaOfertes/detallOferta-50541.html?locale=es
    4 https://wwws.fueib.es/fueib/doip/cercaOfertes/detallOferta-51232.html?locale=es
    5 https://wwws.fueib.es/fueib/doip/cercaOfertes/detallOferta-50993.html?locale=es
    6 https://wwws.fueib.es/fueib/doip/cercaOfertes/detallOferta-50590.html?locale=es
    7 https://wwws.fueib.es/fueib/doip/cercaOfertes/detallOferta-50832.html?locale=es
