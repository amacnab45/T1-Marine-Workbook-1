# Indentifying problems to important R learning
# First step is understand each variable and observation actuallly mean
# Second step is look at the untidy data with two problems often occuring - One variable across mulitple columns and one observation is scattered across multiple rows
# Using Pivot to fix the data pivot_longer() lengthens the data while pivot_wider() widens it
#Grouping function facet_wrap()
#%>% acts as a reader from right to left filter the data and summarizes (Pipe line)
#Helps look for packages
find("%>%")



                                                           
                                                           
                                                           
                                                           
                                                           ( # ❌ Error 1: Extra opening bracket. Remove this.
                                                             
                                                             Max_body_mass <- penguins %>% # ✅ This part is fine
                                                               
                                                               drop_na(body_mass_g) %>% # ✅ Remove rows with missing body mass
                                                               
                                                               group_by(species, island) %>% # ✅ Group by species and island
                                                               
                                                               summarise(), # ❌ Error 2: summarise() is empty
                                                             # ❌ Error 3: comma should not be here
                                                             
                                                             maximum_body_mass = max(body_mass_g) %>% # ❌ Error 4:
                                                               # This calculation must be
                                                               # INSIDE summarise()
                                                               
                                                               pivot_wider( # ❌ Error 5:
                                                                 # pivot_wider() should come after summarise()
                                                                 # has produced a data frame
                                                                 
                                                                 names_from = island,
                                                                 
                                                                 values_from = max_body_mass
                                                                 
                                                               ))
                                                           
                                                           # ❌ Error 6: Extra closing brackets because of the extra
                                                           # opening bracket at the top. 