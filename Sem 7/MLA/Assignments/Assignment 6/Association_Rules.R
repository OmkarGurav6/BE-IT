
    library(arules)
    library(arulesViz)
    library(datasets)
    
    data("Groceries")
    
    inspect(Groceries[1:10])
    
    summary(Groceries)
    
    par(mfrow = c(1,1))
    # Frequency plot of top 10 items
    itemFrequencyPlot(Groceries, type = 'absolute', topN = 10)
    #itemFrequencyPlot(Groceries, type = 'relative', topN = 10)

    
      # 1.
      
      # Getting rules
      rules <- apriori(Groceries, parameter = list(supp = 0.01, conf = 0.3))
      
      summary(rules)
      
      inspect(rules[1:10])
      
      # Checking if there are any reduntant rules
      rules[is.redundant(rules)]
      inspect(rules[is.redundant(rules)])
      
      # Removing reduntant rules
      rules <- rules[!is.redundant(rules)]
      
      # Sorting rules by confidence
      rules <- sort(rules, by = 'confidence')
      inspect(rules[1:10])
      
      plot(rules, method = 'graph',measure = "confidence", shading = "support",
           engine = "htmlwidget", control = list(max = 50))
      
      plot(rules, method = 'paracoord')
    
      
      # 2.
      
      # Getting rules
      rules_1 <- apriori(Groceries, parameter = list(supp = 0.03, conf = 0.3))
      
      summary(rules_1)
      
      inspect(rules_1)
      
      # Checking if there are any reduntant rules
      rules_1[is.redundant(rules_1)]
      
      # Sorting rules by confidence
      rules_1 <- sort(rules_1, by = 'confidence')
      inspect(rules_1)
      
      plot(rules_1, method = 'graph',measure = "support", shading = "confidence",
           engine = "htmlwidget")
      plot(rules_1, method = 'paracoord')
      
      
      # 3.
      
      # Getting rules
      rules_2 <- apriori(Groceries, parameter = list(supp = 0.04, conf = 0.4))
      
      summary(rules_2)
      
      inspect(rules_2)
      
      # Checking if there are any reduntant rules
      rules_2[is.redundant(rules_2)]
      
      # Sorting rules by lift
      rules_2 <- sort(rules_2, by = 'lift')
      inspect(rules_2)
      
      plot(rules_2, method = 'graph',measure = "lift", shading = "support",
           engine = "htmlwidget")
      
      plot(rules_2, method = 'paracoord')

      