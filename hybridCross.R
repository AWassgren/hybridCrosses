monoHybrid = function(parent1 = "Aa",parent2 = "Aa",
                      dom = "A",rec = "a",
                      fullList = FALSE){
  
  library(tidyverse)
  
  parent1 = unlist(strsplit(as.character(parent1),split = ""))
  parent2 = unlist(strsplit(as.character(parent2),split = ""))
  
  parent1 = case_when(
    parent1 == dom ~ 2,
    parent1 == rec ~ 1,
  )
  
  parent2 = case_when(
    parent2 == dom ~ 2,
    parent2 == rec ~ 1,
  )
  # convert dom = 2, rec = 1
  
  parent1 = rep(parent1,times = 2, each = 1)
  parent2 = rep(parent2,times = 1, each = 2)
  F1 = parent1 + parent2
  # A a A a
  #+
  # A A a a
  
  F1 = case_when(
    F1 == 4 ~ as.character(paste(dom,dom,sep = "")),
    # AA
    F1 == 3 ~ as.character(paste(dom,rec,sep = "")),
    # Aa
    F1 == 2 ~ as.character(paste(rec,rec,sep = ""))
    # aa
  )
  
  
  if (fullList == FALSE){
    return(c(length(F1[F1 == paste(dom,dom,sep = "")])+length(F1[F1 == paste(dom,rec,sep = "")]),
             length(F1[F1 == paste(rec,rec,sep = "")])))
  }else{
    return(F1)
  }
  
}

diHybrid = function(parent1 = "Aa Bb",parent2 = "Aa Bb",
                    dom1 = "A",rec1 = "a",
                    dom2 = "B",rec2 = "b",
                    fullList = FALSE){
  
  library(tidyverse)
  
  parent1 = unlist(str_split(as.character(parent1), pattern = " ",n = 2))
  parent2 = unlist(str_split(as.character(parent2), pattern = " ",n = 2))
  # split parents in half
  # then put both halves through a monoHybrid cross against the corresponding halves of parent2
  
  F1AA = monoHybrid(parent1 = parent1[1],parent2 = parent2[1], dom = dom1,rec = rec1, fullList = TRUE)
  F1BB = monoHybrid(parent1 = parent1[2],parent2 = parent2[2],dom = dom2,rec = rec2, fullList = TRUE)
  
  F1AA = rep(F1AA,times = 4, each = 1)
  F1BB = rep(F1BB,times = 1, each = 4)
  
  F1AA = case_when(
    F1AA == paste(dom1,dom1,sep = "") ~ 4,
    # AA = 4
    F1AA == paste(dom1,rec1,sep = "") ~ 3,
    # Aa = 3
    F1AA == paste(rec1,rec1,sep = "") ~ 2
    # aa = 2
  )
  
  F1BB = case_when(
    F1BB == paste(dom2,dom2,sep = "") ~ 400,
    # BB = 400
    F1BB == paste(dom2,rec2,sep = "") ~ 300,
    # Bb = 300
    F1BB == paste(rec2,rec2,sep = "") ~ 200
    # bb = 200
  )
  # convert dom1 = 2, rec1 = 1
  # dom2 = 200, rec2 = 100
  
  F1 = F1AA + F1BB
  # AA, Aa, Aa, aa
  # +BB
  
  # AA, Aa, Aa, aa
  # +Bb
  
  # AA, Aa, Aa, aa
  # +Bb
  
  # AA, Aa, Aa, aa
  # +bb
  
  
  # dominant = 4
  # hetero = 3
  # recessive = 2
  F1 = case_when(
    F1 == 404 ~ as.character(paste(dom1,dom1,dom2,dom2,sep = "")),
    # AABB
    F1 == 403 ~ as.character(paste(dom1,rec1,dom2,dom2,sep = "")),
    # AaBB
    F1 == 402 ~ as.character(paste(rec1,rec1,dom2,dom2,sep = "")),
    # aaBB
    
    F1 == 304 ~ as.character(paste(dom1,dom1,dom2,rec2,sep = "")),
    # AABb
    F1 == 303 ~ as.character(paste(dom1,rec1,dom2,rec2,sep = "")),
    # AaBb
    F1 == 302 ~ as.character(paste(rec1,rec1,dom2,rec2,sep = "")),
    # aaBb
    
    F1 == 204 ~ as.character(paste(dom1,dom1,rec2,rec2,sep = "")),
    # AAbb
    F1 == 203 ~ as.character(paste(dom1,rec1,rec2,rec2,sep = "")),
    # Aabb
    F1 == 202 ~ as.character(paste(rec1,rec1,rec2,rec2,sep = ""))
    # aabb
  )

  if (fullList == FALSE){
    return(c(length(F1[F1 == paste(dom1,dom1,dom2,dom2,sep = "")]) + length(F1[F1 == paste(dom1,rec1,dom2,dom2,sep = "")]) + length(F1[F1 == paste(dom1,dom1,dom2,rec2,sep = "")]) + length(F1[F1 == paste(dom1,rec1,dom2,rec2,sep = "")]),
             length(F1[F1 == paste(dom1,dom1,rec2,rec2,sep = "")]) + length(F1[F1 == paste(dom1,rec1,rec2,rec2,sep = "")]),	
             length(F1[F1 == paste(rec1,rec1,dom2,rec2,sep = "")]) + length(F1[F1 == paste(rec1,rec1,dom2,dom2,sep = "")]),
             length(F1[F1 == paste(rec1,rec1,rec2,rec2,sep = "")])
           ))
  }else{
    return(F1)
  }
  
}

triHybrid = function(parent1 = "Aa Bb Cc",parent2 = "Aa Bb Cc",
                    dom1 = "A",rec1 = "a",
                    dom2 = "B",rec2 = "b",
                    dom3 = "C",rec3 = "c",
                    fullList = FALSE){
  library(tidyverse)
  
  parent1 = unlist(str_split(as.character(parent1), pattern = " ",n = 3))
  parent2 = unlist(str_split(as.character(parent2), pattern = " ",n = 3))
  # split parents in half
  # then put both halves through a monoHybrid cross against the corresponding halves of parent2
  
  F1AA = monoHybrid(parent1 = parent1[1],parent2 = parent2[1], dom = dom1,rec = rec1, fullList = TRUE)
  F1BB = monoHybrid(parent1 = parent1[2],parent2 = parent2[2],dom = dom2,rec = rec2, fullList = TRUE)
  F1CC = monoHybrid(parent1 = parent1[3],parent2 = parent2[3],dom = dom3,rec = rec3, fullList = TRUE)
  
  F1AA = rep(F1AA,times = 16, each = 1)
  F1BB = rep(F1BB,times = 4, each = 4)
  F1CC = rep(F1CC,times = 1, each = 16)
  
  F1AA = case_when(
    F1AA == paste(dom1,dom1,sep = "") ~ 4,
    # AA = 4
    F1AA == paste(dom1,rec1,sep = "") ~ 3,
    # Aa = 3
    F1AA == paste(rec1,rec1,sep = "") ~ 2
    # aa = 2
  )
  
  F1BB = case_when(
    F1BB == paste(dom2,dom2,sep = "") ~ 400,
    # BB = 400
    F1BB == paste(dom2,rec2,sep = "") ~ 300,
    # Bb = 300
    F1BB == paste(rec2,rec2,sep = "") ~ 200
    # bb = 200
  )
  
  F1CC = case_when(
    F1CC == paste(dom3,dom3,sep = "") ~ 4000,
    # BB = 400
    F1CC == paste(dom3,rec3,sep = "") ~ 3000,
    # Bb = 300
    F1CC == paste(rec3,rec3,sep = "") ~ 2000
    # bb = 200
  )
  # convert dom1 = 2, rec1 = 1
  # dom2 = 200, rec2 = 100
  
  F1 = F1AA + F1BB + F1CC
  # (AA, Aa, Aa, aa
  # +BB
  
  # AA, Aa, Aa, aa
  # +Bb
  
  # AA, Aa, Aa, aa
  # +Bb
  
  # AA, Aa, Aa, aa
  # +bb)
  
  # + (CC,Cc,cc)
  
  
  # dominant = 4
  # hetero = 3
  # recessive = 2
  
  F1 = case_when(
    # HomoDom CC
    F1 == 4404 ~ as.character(paste(dom1,dom1,dom2,dom2,dom3,dom3,sep = "")),
    # AABBCC
    F1 == 4403 ~ as.character(paste(dom1,rec1,dom2,dom2,dom3,dom3,sep = "")),
    # AaBBCC
    F1 == 4402 ~ as.character(paste(rec1,rec1,dom2,dom2,dom3,dom3,sep = "")),
    # aaBBCC
    
    F1 == 4304 ~ as.character(paste(dom1,dom1,dom2,rec2,dom3,dom3,sep = "")),
    # AABbCC
    F1 == 4303 ~ as.character(paste(dom1,rec1,dom2,rec2,dom3,dom3,sep = "")),
    # AaBbCC
    F1 == 4302 ~ as.character(paste(rec1,rec1,dom2,rec2,dom3,dom3,sep = "")),
    # aaBbCC
    
    F1 == 4204 ~ as.character(paste(dom1,dom1,rec2,rec2,dom3,dom3,sep = "")),
    # AAbbCC
    F1 == 4203 ~ as.character(paste(dom1,rec1,rec2,rec2,dom3,dom3,sep = "")),
    # AabbCC
    F1 == 4202 ~ as.character(paste(rec1,rec1,rec2,rec2,dom3,dom3,sep = "")),
    # aabbCC
    
    # Hetero Cc
    F1 == 3404 ~ as.character(paste(dom1,dom1,dom2,dom2,dom3,rec3,sep = "")),
    # AABBCc
    F1 == 3403 ~ as.character(paste(dom1,rec1,dom2,dom2,dom3,rec3,sep = "")),
    # AaBBCc
    F1 == 3402 ~ as.character(paste(rec1,rec1,dom2,dom2,dom3,rec3,sep = "")),
    # aaBBCc
    
    F1 == 3304 ~ as.character(paste(dom1,dom1,dom2,rec2,dom3,rec3,sep = "")),
    # AABbCc
    F1 == 3303 ~ as.character(paste(dom1,rec1,dom2,rec2,dom3,rec3,sep = "")),
    # AaBbCc
    F1 == 3302 ~ as.character(paste(rec1,rec1,dom2,rec2,dom3,rec3,sep = "")),
    # aaBbCc
    
    F1 == 3204 ~ as.character(paste(dom1,dom1,rec2,rec2,dom3,rec3,sep = "")),
    # AAbbCc
    F1 == 3203 ~ as.character(paste(dom1,rec1,rec2,rec2,dom3,rec3,sep = "")),
    # AabbCc
    F1 == 3202 ~ as.character(paste(rec1,rec1,rec2,rec2,dom3,rec3,sep = "")),
    # aabbCc
    
    # HomoRec cc
    F1 == 2404 ~ as.character(paste(dom1,dom1,dom2,dom2,rec3,rec3,sep = "")),
    # AABBcc
    F1 == 2403 ~ as.character(paste(dom1,rec1,dom2,dom2,rec3,rec3,sep = "")),
    # AaBBcc
    F1 == 2402 ~ as.character(paste(rec1,rec1,dom2,dom2,rec3,rec3,sep = "")),
    # aaBBcc
    
    F1 == 2304 ~ as.character(paste(dom1,dom1,dom2,rec2,rec3,rec3,sep = "")),
    # AABbcc
    F1 == 2303 ~ as.character(paste(dom1,rec1,dom2,rec2,rec3,rec3,sep = "")),
    # AaBbcc
    F1 == 2302 ~ as.character(paste(rec1,rec1,dom2,rec2,rec3,rec3,sep = "")),
    # aaBbcc
    
    F1 == 2204 ~ as.character(paste(dom1,dom1,rec2,rec2,rec3,rec3,sep = "")),
    # AAbbcc
    F1 == 2203 ~ as.character(paste(dom1,rec1,rec2,rec2,rec3,rec3,sep = "")),
    # Aabbcc
    F1 == 2202 ~ as.character(paste(rec1,rec1,rec2,rec2,rec3,rec3,sep = ""))
    # aabbcc
  )
  
  if (fullList == FALSE){
    return(c
          ((length(F1[F1 == paste(dom1,dom1,dom2,dom2,dom3,dom3,sep = "")]) 
            + length(F1[F1 == paste(dom1,rec1,dom2,dom2,dom3,dom3,sep = "")])
            + length(F1[F1 == paste(dom1,dom1,dom2,rec2,dom3,dom3,sep = "")])
            + length(F1[F1 == paste(dom1,rec1,dom2,rec2,dom3,dom3,sep = "")])
            + length(F1[F1 == paste(dom1,dom1,dom2,dom2,dom3,rec3,sep = "")])
            + length(F1[F1 == paste(dom1,rec1,dom2,dom2,dom3,rec3,sep = "")])
            + length(F1[F1 == paste(dom1,dom1,dom2,rec2,dom3,rec3,sep = "")])
            + length(F1[F1 == paste(dom1,rec1,dom2,rec2,dom3,rec3,sep = "")])
          ),
            # 1 - dom A, dom B, dom C
            
            (length(F1[F1 == paste(dom1,dom1,dom2,dom2,rec3,rec3,sep = "")]) 
             + length(F1[F1 == paste(dom1,rec1,dom2,dom2,rec3,rec3,sep = "")])
             + length(F1[F1 == paste(dom1,dom1,dom2,rec2,rec3,rec3,sep = "")])
             + length(F1[F1 == paste(dom1,rec1,dom2,rec2,rec3,rec3,sep = "")])
            ),  
            # 2 - dom A, dom B, rec C
            
            (length(F1[F1 == paste(dom1,dom1,rec2,rec2,dom3,dom3,sep = "")]) 
             + length(F1[F1 == paste(dom1,rec1,rec2,rec2,dom3,dom3,sep = "")])
             + length(F1[F1 == paste(dom1,dom1,rec2,rec2,dom3,rec3,sep = "")])
             + length(F1[F1 == paste(dom1,rec1,rec2,rec2,dom3,rec3,sep = "")])
            ),         
            # 3 - dom A, rec B, dom C
            
            (length(F1[F1 == paste(rec1,rec1,dom2,dom2,dom3,dom3,sep = "")]) 
             + length(F1[F1 == paste(rec1,rec1,dom2,rec2,dom3,dom3,sep = "")])
             + length(F1[F1 == paste(rec1,rec1,dom2,dom2,dom3,rec3,sep = "")])
             + length(F1[F1 == paste(rec1,rec1,dom2,rec2,dom3,rec3,sep = "")])
            ),         
            # 4 - rec A, dom B, dom C 
            
            (length(F1[F1 == paste(dom1,dom1,rec2,rec2,rec3,rec3,sep = "")]) 
             + length(F1[F1 == paste(dom1,rec1,rec2,rec2,rec3,rec3,sep = "")])
            ),         
            # 5 - dom A, rec B, rec C
            
            (length(F1[F1 == paste(rec1,rec1,dom2,dom2,rec3,rec3,sep = "")]) 
             + length(F1[F1 == paste(rec1,rec1,dom2,rec2,rec3,rec3,sep = "")])
            ),         
            # 6 - rec A, dom B, rec C
            
            (length(F1[F1 == paste(rec1,rec1,rec2,rec2,dom3,dom3,sep = "")]) 
             + length(F1[F1 == paste(rec1,rec1,rec2,rec2,dom3,rec3,sep = "")])
            ),         
            # 7 - rec A, rec B, dom C
            
            (length(F1[F1 == paste(rec1,rec1,rec2,rec2,rec3,rec3,sep = "")]))         
            # 8 - rec A, rec B, rec C
          ))
  }else{
    return(F1)
  }
}