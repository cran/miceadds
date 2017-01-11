
mice.impute.2l.pmm <- function(y, ry, x, type, intercept=TRUE,
                                  groupcenter.slope=FALSE, draw.fixed=TRUE,
                                  random.effects.shrinkage=1E-6,
                                  glmer.warnings=TRUE, donors = 5 ,
								  match_sampled_pars = TRUE ,
								  blme_use = FALSE , blme_args = NULL , 
                                  ...){

	imp <- mice.impute.2l.lmer(y=y, ry=ry, x=x, type=type, 
				intercept=intercept, groupcenter.slope=groupcenter.slope, 
				draw.fixed=draw.fixed,
				random.effects.shrinkage=random.effects.shrinkage,
                glmer.warnings=glmer.warnings, 
				model = "pmm" , donors = donors , 
				match_sampled_pars = match_sampled_pars , 
				blme_use = FALSE , blme_args = NULL , 				
				...)
	base::return(imp)
}