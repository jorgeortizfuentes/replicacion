rm(list = ls())

require(lme4)
require(parallel)

load(file="data//ess_rescaled.Rda") # load data

# paralelizing function

eff_mc = function(calls, mc.cores) {
require(parallel)
m.list = mclapply(calls, function(i) eval(parse(text=i)), 
                  mc.cores = mc.cores, mc.preschedule = FALSE)
return(m.list)
print(parse(text=i))
}

##########
# Models #
##########


controls2 <- c("gender", "agea", "eduyrs", "domicil", "unemployed", "rideol", "union", "prtdgcl_num", "region2")
controls <- c("gender", "agea", "eduyrs", "domicil", "unemployed", "rideol", "union", "prtdgcl_num", "region")
agr.ctrl <- c("gdp_wb_ppp", "wb_vae", "gov_genlr")
end <- "1 + (1 + rideol|countryyear) + (1|cntry), data=data, control=glmerControl(optimizer='bobyqa', optCtrl = list(maxfun = 1e9)), family=binomial(link='logit'))"
end2 <- "1 + (1 + rideol|countryyear) + (1|cntry), data=subset(data, cntry %in% c('BE', 'CH', 'CZ', 'DE', 'DK', 'EE', 'ES', 'FI', 'FR', 'GB', 'HU', 'IE', 'NL', 'NO', 'PL', 'PT', 'SE', 'SI')), control=glmerControl(optimizer='bobyqa', optCtrl = list(maxfun = 1e9)), family=binomial(link='logit'))"

models = c(paste0("glmer(protest ~ ", paste(c(controls2, agr.ctrl, end), collapse="+")),
paste0("glmer(protest ~ region2*rideol+", paste(c(controls2, agr.ctrl, end), collapse="+")),
paste0("glmer(protest ~ region2*rideol*year_num + year_num+", paste(c(controls2, agr.ctrl, end), collapse="+")),
paste0("glmer(protest ~ soc_pop_eleches+", paste(c(controls2, agr.ctrl, end), collapse="+")),
paste0("glmer(protest ~ soc_pop_eleches*region2+", paste(c(controls2, agr.ctrl, end), collapse="+")),
paste0("glmer(protest ~ region2*rideol*soc_pop_eleches+", paste(c(controls2, agr.ctrl, end), collapse="+")),
paste0("glmer(protest ~ region2*prtdgcl_num+", paste(c(controls2, agr.ctrl, end), collapse="+")),
paste0("glmer(protest ~ region2*rideol*prtdgcl_num+", paste(c(controls2, agr.ctrl, end), collapse="+")),
paste0("glmer(protest ~ gov_dist +", paste(c(controls2, agr.ctrl, end), collapse="+")),
paste0("glmer(protest ~ region2*gov_dist+gov_dist+", paste(c(controls2, agr.ctrl, end), collapse="+")),
paste0("glmer(protest ~ region2*gov_genlr*gov_dist+gov_dist+", paste(c(controls2, agr.ctrl, end), collapse="+")),
paste0("glmer(protest ~ ", paste(c(controls, agr.ctrl, end2), collapse="+")),
paste0("glmer(protest ~ region*rideol+", paste(c(controls, agr.ctrl, end2), collapse="+")),
paste0("glmer(protest ~ region*rideol*year_num + year_num+", paste(c(controls, agr.ctrl, end2), collapse="+")),
paste0("glmer(protest ~ soc_pop_eleches+", paste(c(controls, agr.ctrl, end2), collapse="+")),
paste0("glmer(protest ~ soc_pop_eleches*region+", paste(c(controls, agr.ctrl, end2), collapse="+")),
paste0("glmer(protest ~ region*rideol*soc_pop_eleches+", paste(c(controls, agr.ctrl, end2), collapse="+")),
paste0("glmer(protest ~ region*prtdgcl_num+", paste(c(controls, agr.ctrl, end2), collapse="+")),
paste0("glmer(protest ~ region*rideol*prtdgcl_num+", paste(c(controls, agr.ctrl, end2), collapse="+")),
paste0("glmer(protest ~ gov_dist + ", paste(c(controls, agr.ctrl, end2), collapse="+")),
paste0("glmer(protest ~ region*gov_dist+gov_dist+", paste(c(controls, agr.ctrl, end2), collapse="+")),
paste0("glmer(protest ~ region*gov_genlr*gov_dist+gov_dist+", paste(c(controls, agr.ctrl, end2), collapse="+")))

m.list = eff_mc(models, 24)

m.1 <- c(m.list[1:2])
m.2 <- c(m.list[3:4])
m.3 <- c(m.list[5:6])
m.4 <- c(m.list[7:8])
m.5 <- c(m.list[9:10])
m.6 <- c(m.list[11:12])
m.7 <- c(m.list[13:14])
m.8 <- c(m.list[15:16])
m.9 <- c(m.list[17:18])
m.10 <- c(m.list[19:20])
m.11 <- c(m.list[21:22])


save(m.1, data, file='m_1.RData')
save(m.2, data, file='m_2.RData')
save(m.3, data, file='m_3.RData')
save(m.4, data, file='m_4.RData')
save(m.5, data, file='m_5.RData')
save(m.6, data, file='m_6.RData')
save(m.7, data, file='m_7.RData')
save(m.8, data, file='m_8.RData')
save(m.9, data, file='m_9.RData')
save(m.10, data, file='m_10.RData')
save(m.11, data, file='m_11.RData')

