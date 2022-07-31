##########################################
#######      Amex Metrix           #####
##########################################

# Adapted from https://www.kaggle.com/code/inversion/amex-competition-metric-python

#The evaluation metric, 𝑀, for this competition is the mean of two measures of rank ordering: Normalized Gini Coefficient, 𝐺, and default rate captured at 4%, 𝐷.
#𝑀=0.5⋅(𝐺+𝐷)
#The default rate captured at 4% is the percentage of the positive labels (defaults) captured within the highest-ranked 4% of the predictions, and represents a Sensitivity/Recall statistic.
#For both of the sub-metrics 𝐺 and 𝐷, the negative labels are given a weight of 20 to adjust for downsampling.
#This metric has a maximum value of 1.0.

amex_metric = function(target, pred){
  
  df = data.frame(target, pred)
  
    # Default rate captured at 4%
    top_four_percent_captured = function(target, pred){
      
    df = data.frame(target, pred)
    
    # Descending predictions (pred == 1 first)
    df = df  %>% arrange(-pred)
    # define weight (negative labels are given a weight of 20 to adjust for down sampling)
    df[,'weight'] = ifelse(df[,'target'] == 0, 20, 1)
    # get rows under 4% cutoff
    pctCut = as.integer(sum(0.04*df[,'weight']))
    pctCut = df[cumsum(df[,'weight']) <= pctCut,]
    
    # return
    sum(pctCut['target'] == 1)/sum(df['target'] == 1)
  }
  
  weighted_gini = function(target, pred){
    
    df = data.frame(target, pred)
    df = df  %>% arrange(-pred)
    # define weight (negative labels are given a weight of 20 to adjust for down sampling)
    df[,'weight'] = ifelse(df[,'target'] == 0, 20, 1)
    df[,'random'] = cumsum(df[,'weight'] / sum(df[,'weight']))
    total_pos = sum(df[,'target'] * df[,'weight'])
    df[,'cum_pos_found'] = cumsum(df[,'target'] * df[,'weight'])
    #Define lorentz and Gini variables
    df[,'lorentz'] = df[,'cum_pos_found'] / total_pos
    df[,'gini'] = df[,'weight'] * (df[,'lorentz'] - df[,'random'])
    
    # return
    sum(df[,'gini'])
  }
  
  normalized_weighted_gini = function(target, pred){
    weighted_gini(target, pred) / weighted_gini(target, target)
  }
  
  G = normalized_weighted_gini(target, pred)
  D = top_four_percent_captured(target, pred)
  
  # Return val
  0.5 * (G + D)
  
}