# all i want is a
# room somewhere / with clawfoot tub
# and a frigidaire
nychvs_csv_import <-
	function( this_url ){
		
		tf <- tempfile()
		
		download.file( this_url , tf , mode = 'wb' )
		
		this_df <- read.csv( tf )
		
		names( this_df ) <- tolower( names( this_df ) )
		
		this_df
	}
all_units_df <- nychvs_csv_import( "https://www2.census.gov/programs-surveys/nychvs/datasets/2021/microdata/allunits_puf_21.csv" )

occupied_units_df <- nychvs_csv_import( "https://www2.census.gov/programs-surveys/nychvs/datasets/2021/microdata/occupied_puf_21.csv" )

person_df <- nychvs_csv_import( "https://www2.census.gov/programs-surveys/nychvs/datasets/2021/microdata/person_puf_21.csv" )

vacant_units_df <- nychvs_csv_import( "https://www2.census.gov/programs-surveys/nychvs/datasets/2021/microdata/vacant_puf_21.csv" )

stopifnot( nrow( all_units_df ) == nrow( occupied_units_df ) + nrow( vacant_units_df ) )
before_nrow <- nrow( occupied_units_df )

occupied_units_df <- merge( all_units_df , occupied_units_df )

stopifnot( nrow( occupied_units_df ) == before_nrow )

before_nrow <- nrow( vacant_units_df )

vacant_units_df <- merge( all_units_df , vacant_units_df )

stopifnot( nrow( vacant_units_df ) == before_nrow )

stopifnot( nrow( nychvs_df ) == nrow( nychvs_df_person ) )

nychvs_df[ , 'one' ] <- 1
# nychvs_fn <- file.path( path.expand( "~" ) , "NYCHVS" , "this_file.rds" )
# saveRDS( nychvs_df , file = nychvs_fn , compress = FALSE )
# nychvs_df <- readRDS( nychvs_fn )
library(survey)

nychvs_design <-
	svrepdesign(
		weight = ~fw ,
		repweights = 'fw[0-9]+' ,
		scale = 4 / 80 ,
		rscales = rep( 1 , 80 ) ,
		mse = TRUE ,
		type = 'JK1' ,
		data = nychvs_df
	)
nychvs_design <- 
	update( 
		nychvs_design , 
		
		one = 1 ,
		
		home_owners = as.numeric( sc115 == 1 ) ,

		yearly_household_income = ifelse( uf42 == 9999999 , 0 , as.numeric( uf42 ) ) ,
		
		gross_monthly_rent = ifelse( uf17 == 99999 , NA , as.numeric( uf17 ) ) ,
		
		borough =
			factor( boro , levels = 1:5 , labels =
				c( 'Bronx' , 'Brooklyn' , 'Manhattan' , 
				'Queens' , 'Staten Island' )
			) ,
			
		householder_sex = factor( hhr2 , labels = c( 'male' , 'female' ) )
			
	)
sum( weights( nychvs_design , "sampling" ) != 0 )

svyby( ~ one , ~ borough , nychvs_design , unwtd.count )
svytotal( ~ one , nychvs_design )

svyby( ~ one , ~ borough , nychvs_design , svytotal )
svymean( ~ yearly_household_income , nychvs_design , na.rm = TRUE )

svyby( ~ yearly_household_income , ~ borough , nychvs_design , svymean , na.rm = TRUE )
svymean( ~ householder_sex , nychvs_design )

svyby( ~ householder_sex , ~ borough , nychvs_design , svymean )
svytotal( ~ yearly_household_income , nychvs_design , na.rm = TRUE )

svyby( ~ yearly_household_income , ~ borough , nychvs_design , svytotal , na.rm = TRUE )
svytotal( ~ householder_sex , nychvs_design )

svyby( ~ householder_sex , ~ borough , nychvs_design , svytotal )
svyquantile( ~ yearly_household_income , nychvs_design , 0.5 , na.rm = TRUE )

svyby( 
	~ yearly_household_income , 
	~ borough , 
	nychvs_design , 
	svyquantile , 
	0.5 ,
	ci = TRUE , na.rm = TRUE
)
svyratio( 
	numerator = ~ gross_monthly_rent , 
	denominator = ~ yearly_household_income , 
	nychvs_design ,
	na.rm = TRUE
)
sub_nychvs_design <- subset( nychvs_design , boro == 3 )
svymean( ~ yearly_household_income , sub_nychvs_design , na.rm = TRUE )
this_result <- svymean( ~ yearly_household_income , nychvs_design , na.rm = TRUE )

coef( this_result )
SE( this_result )
confint( this_result )
cv( this_result )

grouped_result <-
	svyby( 
		~ yearly_household_income , 
		~ borough , 
		nychvs_design , 
		svymean ,
		na.rm = TRUE 
	)
	
coef( grouped_result )
SE( grouped_result )
confint( grouped_result )
cv( grouped_result )
degf( nychvs_design )
svyvar( ~ yearly_household_income , nychvs_design , na.rm = TRUE )
# SRS without replacement
svymean( ~ yearly_household_income , nychvs_design , na.rm = TRUE , deff = TRUE )

# SRS with replacement
svymean( ~ yearly_household_income , nychvs_design , na.rm = TRUE , deff = "replace" )
svyciprop( ~ home_owners , nychvs_design ,
	method = "likelihood" )
svyttest( yearly_household_income ~ home_owners , nychvs_design )
svychisq( 
	~ home_owners + householder_sex , 
	nychvs_design 
)
glm_result <- 
	svyglm( 
		yearly_household_income ~ home_owners + householder_sex , 
		nychvs_design 
	)

summary( glm_result )

library(srvyr)
nychvs_srvyr_design <- as_survey( nychvs_design )
nychvs_srvyr_design %>%
	summarize( mean = survey_mean( yearly_household_income , na.rm = TRUE ) )

nychvs_srvyr_design %>%
	group_by( borough ) %>%
	summarize( mean = survey_mean( yearly_household_income , na.rm = TRUE ) )
