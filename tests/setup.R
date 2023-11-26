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
datasets_url <- "https://www2.census.gov/programs-surveys/nychvs/datasets/2021/microdata/"

all_units_df <-
	nychvs_csv_import( paste0( datasets_url , "allunits_puf_21.csv" ) )

occupied_units_df <-
	nychvs_csv_import( paste0( datasets_url , "occupied_puf_21.csv" ) )

person_df <-
	nychvs_csv_import( paste0( datasets_url , "person_puf_21.csv" ) )

vacant_units_df <-
	nychvs_csv_import( paste0( datasets_url , "vacant_puf_21.csv" ) )

stopifnot( nrow( all_units_df ) == nrow( occupied_units_df ) + nrow( vacant_units_df ) )
before_nrow <- nrow( occupied_units_df )

occupied_units_df <- merge( all_units_df , occupied_units_df )

stopifnot( nrow( occupied_units_df ) == before_nrow )

before_nrow <- nrow( vacant_units_df )

vacant_units_df <- merge( all_units_df , vacant_units_df )

stopifnot( nrow( vacant_units_df ) == before_nrow )

before_nrow <- nrow( person_df )

weighting_variables <- grep( "^fw([0-9]+)?$" , names( occupied_units_df ) , value = TRUE )

person_df <-
	merge(
		occupied_units_df[ setdiff( names( occupied_units_df ) , weighting_variables ) ] ,
		person_df
	)

stopifnot( nrow( person_df ) == before_nrow )

all_units_df[ , 'one' ] <- occupied_units_df[ , 'one' ] <-
	vacant_units_df[ , 'one' ] <- person_df[ , 'one' ] <- 1
# nychvs_fn <- file.path( path.expand( "~" ) , "NYCHVS" , "this_file.rds" )
# saveRDS( nychvs_df , file = nychvs_fn , compress = FALSE )
# nychvs_df <- readRDS( nychvs_fn )
library(survey)

all_units_design <-
	svrepdesign(
		weight = ~fw ,
		repweights = 'fw[0-9]+' ,
		scale = 4 / 80 ,
		rscales = rep( 1 , 80 ) ,
		mse = TRUE ,
		type = 'JK1' ,
		data = all_units_df
	)
	
occupied_units_design <-
	svrepdesign(
		weight = ~fw ,
		repweights = 'fw[0-9]+' ,
		scale = 4 / 80 ,
		rscales = rep( 1 , 80 ) ,
		mse = TRUE ,
		type = 'JK1' ,
		data = occupied_units_df
	)
	
vacant_units_design <-
	svrepdesign(
		weight = ~fw ,
		repweights = 'fw[0-9]+' ,
		scale = 4 / 80 ,
		rscales = rep( 1 , 80 ) ,
		mse = TRUE ,
		type = 'JK1' ,
		data = vacant_units_df
	)
	
person_design <-
	svrepdesign(
		weight = ~pw ,
		repweights = 'pw[0-9]+' ,
		scale = 4 / 80 ,
		rscales = rep( 1 , 80 ) ,
		mse = TRUE ,
		type = 'JK1' ,
		data = person_df
	)

nychvs_design <-
	occupied_units_design
nychvs_design <- 
	update( 
		nychvs_design , 
		
		one = 1 ,
		
		home_owners = as.numeric( tenure == 2 ) ,

		yearly_household_income = hhinc_rec1 ,
		
		rent_amount = ifelse( rent_amount == -2 , NA , rent_amount ) ,
		
		borough =
			factor( boro , levels = 1:5 , labels =
				c( 'Bronx' , 'Brooklyn' , 'Manhattan' , 
				'Queens' , 'Staten Island' )
			) ,
			
		food_insecurity = 
			factor( 
				foodinsecure , 
				levels = 1:3 , 
				labels = 
					c( 'not insecure' , 'insecure' , 'very insecure' )
			)
			
	)
sum( weights( nychvs_design , "sampling" ) != 0 )

svyby( ~ one , ~ borough , nychvs_design , unwtd.count )
svytotal( ~ one , nychvs_design )

svyby( ~ one , ~ borough , nychvs_design , svytotal )
svymean( ~ hhinc_rec1 , nychvs_design , na.rm = TRUE )

svyby( ~ hhinc_rec1 , ~ borough , nychvs_design , svymean , na.rm = TRUE )
svymean( ~ food_insecurity , nychvs_design , na.rm = TRUE )

svyby( ~ food_insecurity , ~ borough , nychvs_design , svymean , na.rm = TRUE )
svytotal( ~ hhinc_rec1 , nychvs_design , na.rm = TRUE )

svyby( ~ hhinc_rec1 , ~ borough , nychvs_design , svytotal , na.rm = TRUE )
svytotal( ~ food_insecurity , nychvs_design , na.rm = TRUE )

svyby( ~ food_insecurity , ~ borough , nychvs_design , svytotal , na.rm = TRUE )
svyquantile( ~ hhinc_rec1 , nychvs_design , 0.5 , na.rm = TRUE )

svyby( 
	~ hhinc_rec1 , 
	~ borough , 
	nychvs_design , 
	svyquantile , 
	0.5 ,
	ci = TRUE , na.rm = TRUE
)
svyratio( 
	numerator = ~ rent_amount , 
	denominator = ~ hhinc_rec1 , 
	nychvs_design ,
	na.rm = TRUE
)
sub_nychvs_design <- subset( nychvs_design , rentburden_cat %in% 1:2 )
svymean( ~ hhinc_rec1 , sub_nychvs_design , na.rm = TRUE )
this_result <- svymean( ~ hhinc_rec1 , nychvs_design , na.rm = TRUE )

coef( this_result )
SE( this_result )
confint( this_result )
cv( this_result )

grouped_result <-
	svyby( 
		~ hhinc_rec1 , 
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
svyvar( ~ hhinc_rec1 , nychvs_design , na.rm = TRUE )
# SRS without replacement
svymean( ~ hhinc_rec1 , nychvs_design , na.rm = TRUE , deff = TRUE )

# SRS with replacement
svymean( ~ hhinc_rec1 , nychvs_design , na.rm = TRUE , deff = "replace" )
svyciprop( ~ home_owners , nychvs_design ,
	method = "likelihood" )
svyttest( hhinc_rec1 ~ home_owners , nychvs_design )
svychisq( 
	~ home_owners + food_insecurity , 
	nychvs_design 
)
glm_result <- 
	svyglm( 
		hhinc_rec1 ~ home_owners + food_insecurity , 
		nychvs_design 
	)

summary( glm_result )
result <- svytotal( ~ one , nychvs_design )
stopifnot( round( coef( result ) , 0 ) == 3157105 )
stopifnot( round( SE( result ) , 0 ) == 13439 )
library(srvyr)
nychvs_srvyr_design <- as_survey( nychvs_design )
nychvs_srvyr_design %>%
	summarize( mean = survey_mean( hhinc_rec1 , na.rm = TRUE ) )

nychvs_srvyr_design %>%
	group_by( borough ) %>%
	summarize( mean = survey_mean( hhinc_rec1 , na.rm = TRUE ) )
