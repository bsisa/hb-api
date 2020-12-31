#Procédure

Pour le point de départ voir hb-api/conf/routes :

    GET /api/melfin/create/prestation/:referenceYear/:createYear/:owner ch.bsisa.hyperbird.controllers.DbMigration.newPrestations(referenceYear, createYear, owner)


Ensuite tu verras que la logique est dans:

    /hb-api/app/ch/bsisa/hyperbird/db/evolution/YearlyPrestationsCreation.scala

Qui utilise entre autre: 

    get_PRESTATION_list_for_year.xq


## Exécution

1. S'identifier dans l'application comme d'habitude
1. Ouvrir un nouvel onglet dans le navigateur
1. Coller l'adresse de commande (Voir historique pour des exemples)

#Historique

## 2021
### Test
    http://vdn138.vdn.ne.ch:9999/api/melfin/create/prestation/2020/2021/FMPA
    http://vdn138.vdn.ne.ch:9999/api/melfin/create/prestation/2020/2021/CDP
    http://vdn138.vdn.ne.ch:9999/api/melfin/create/prestation/2020/2021/NE

### Prod
    http://vdn132.vdn.ne.ch:9999/api/melfin/create/prestation/2020/2021/FMPA
    http://vdn132.vdn.ne.ch:9999/api/melfin/create/prestation/2020/2021/CDP
    http://vdn132.vdn.ne.ch:9999/api/melfin/create/prestation/2020/2021/NE



## 2020 
Important: En 2020, seules les prestations du groupe Investissement sont clonées
### Test
    http://vdn138.vdn.ne.ch:9999/api/melfin/create/prestation/2019/2020/FMPA
    http://vdn138.vdn.ne.ch:9999/api/melfin/create/prestation/2019/2020/CDP
    http://vdn138.vdn.ne.ch:9999/api/melfin/create/prestation/2019/2020/NE
    
### Prod
    http://vdn132.vdn.ne.ch:9999/api/melfin/create/prestation/2019/2020/FMPA
    http://vdn132.vdn.ne.ch:9999/api/melfin/create/prestation/2019/2020/CDP
    http://vdn132.vdn.ne.ch:9999/api/melfin/create/prestation/2019/2020/NE


## 2019 
### Test
    http://vdn138.vdn.ne.ch:9999/api/melfin/create/prestation/2018/2019/FMPA
    http://vdn138.vdn.ne.ch:9999/api/melfin/create/prestation/2018/2019/CDP
    http://vdn138.vdn.ne.ch:9999/api/melfin/create/prestation/2018/2019/NE
    
### Prod
    http://vdn132.vdn.ne.ch:9999/api/melfin/create/prestation/2018/2019/FMPA
    http://vdn132.vdn.ne.ch:9999/api/melfin/create/prestation/2018/2019/CDP
    http://vdn132.vdn.ne.ch:9999/api/melfin/create/prestation/2018/2019/NE

## 2018 
### Test
    http://vdn138.vdn.ne.ch:9999/api/melfin/create/prestation/2017/2018/FMPA
    http://vdn138.vdn.ne.ch:9999/api/melfin/create/prestation/2017/2018/CDP
    http://vdn138.vdn.ne.ch:9999/api/melfin/create/prestation/2017/2018/NE
    
### Prod
    http://vdn132.vdn.ne.ch:9999/api/melfin/create/prestation/2017/2018/FMPA
    http://vdn132.vdn.ne.ch:9999/api/melfin/create/prestation/2017/2018/CDP
    http://vdn132.vdn.ne.ch:9999/api/melfin/create/prestation/2017/2018/NE



## Request `get_PRESTATION_list_for_year`

```
xquery version "3.0";
import module namespace request="http://exist-db.org/xquery/request";

(:The PRESTATION list returned is ordered by IMMEUBLE/IDENTIFIANT/OBJECTIF in ascending order :)
(:and for each IMMEUBLE the selected PRESTATION are ordered in IDENTIFIANT/OBJECTIF descending order:)
(:allowing easy determination of the highest PRESTATION/IDENTIFIANT/OBJECTFIF for each IMMEUBLE group :)
(:of PRESTATIONs:)

let $yearOfReference := request:get-parameter("refYear", "2017")
let $owner := request:get-parameter("owner", "NE")
(:let $owner := request:get-parameter("owner", "FMPA"):)
(:let $owner := request:get-parameter("owner", "CDP"):)

(: Restrictions 
 : 1) Active: Buildings without year end "IDENTIFIANT/A" are active 
 : 2) Owner:  Only 'NE' owner is selected (Ville de Neuchâtel). ETAT is archived. DOMAIN is managed by its own departement.
 : WARN: FMPA and CDP are to be created as well as NE
 : :)
let $activeBuildings := collection("/db/hb4/G20040930101030005")//ELFIN[@CLASSE='IMMEUBLE' and (string-length(IDENTIFIANT/A/string())) = 0 and PARTENAIRE/PROPRIETAIRE/@NOM=$owner]

(:let $prestations := collection("/db/hb4/G20081113902512302")//ELFIN[@CLASSE='PRESTATION' and IDENTIFIANT/DE=$yearOfReference and PARTENAIRE/PROPRIETAIRE/@NOM=$owner]:)

(:PRESTATION creation is limited to provided $owner in server side Scala function. :)
(:Here we need to go across all owners to get the highest index due to existing building with duplicate ids:)
(:It is MANDATORY that the $yearOfReference is the latest year for which PRESTATION are defined otherwise the highest index :)
(:determination will of course fail:)
let $prestations := collection("/db/hb4/G20081113902512302")//ELFIN[@CLASSE='PRESTATION' and IDENTIFIANT/DE=$yearOfReference]


let $referencePrestations := 
    for $activeBuilding in $activeBuildings 
        let $source := concat($activeBuilding/@ID_G, "/", $activeBuilding/@CLASSE, "/", $activeBuilding/@Id) 
        let $referenceYearPrestations := 
            for $refPrestation in $prestations[@SOURCE=concat($activeBuilding/@ID_G, "/", $activeBuilding/@CLASSE, "/", $activeBuilding/@Id) ]
            order by $refPrestation/IDENTIFIANT/OBJECTIF descending
            return $refPrestation
(:    IMPORTANT: In the current data model design PRESTATION cannot be different for two buildings (IMMEUBLE) with the same IDENTIFIANT/OBJECTIF. Thus they are shared.:)
(:    To avoid duplicating identical PRESTATION for identical IDENTIFIANT/OBJECTIF (No SAI) used by more than a single IMMEUBLE, we need to perform a `group by` close. :)
    group by $source
    order by $source ascending
    return $referenceYearPrestations

for $refP in $referencePrestations
order by $refP/IDENTIFIANT/OBJECTIF descending
return $refP


```