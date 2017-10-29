# Contracts and reports

## Excel queries

Contracts and reports configuration works as follows :

### Trigger

The UI calls an XLS report with a dedicated URI in the form of 
`/api/melfin/spreadsheet/<NAME_OF_THE_REPORT>.xls?PARAM_1=VALUE&PARAM_2=VALUE&...`

### Report building

The report builder retrieves the report file from the folder `/db/hb4/queries` 


