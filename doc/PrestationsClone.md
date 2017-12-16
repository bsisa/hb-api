#Procédure

Pour le point de départ voir hb-api/conf/routes :

    GET /api/melfin/create/prestation/:referenceYear/:createYear/:owner ch.bsisa.hyperbird.controllers.DbMigration.newPrestations(referenceYear, createYear, owner)


Ensuite tu verras que la logique est dans:

    /hb-api/app/ch/bsisa/hyperbird/db/evolution/YearlyPrestationsCreation.scala

Qui utilise entre autre: 

    get_PRESTATION_list_for_year.xq
