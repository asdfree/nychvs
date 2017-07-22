if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )

library(lodown)
# examine all available NYCHVS microdata files
nychvs_cat <-
	get_catalog( "nychvs" ,
		output_dir = file.path( getwd() ) )

# 2014 only
nychvs_cat <- subset( nychvs_cat , year == 2014 )
# download the microdata to your local computer
stopifnot( nrow( nychvs_cat ) > 0 )

options( survey.lonely.psu = "adjust" )

library(survey)

# load the occupied units table
nychvs_df <- readRDS( file.path( getwd() , "2014/occ.rds" ) )

nychvs_design <- 
	svydesign( ~ 1 , data = nychvs_df , weights = ~ fw )
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
	ci = TRUE ,
	keep.var = TRUE ,
	na.rm = TRUE
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

