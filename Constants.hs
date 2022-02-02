
module Constants where

--Change Environment Variable (Amount of Turns)
change_environment::Int
change_environment = 10


--Board Size
--Width
n::Int
n = 10
--Height
m::Int
m = 10

corral_width::Int
corral_width = 5

corral_heigth::Int
corral_heigth = 5

number_kids::Int
number_kids = 25

obstacles::Int
obstacles = 5

--ROBOTS
reactive_robots_dirt::Int
reactive_robots_dirt = 0

--0 Value Not Permited
reactive_robots_kids::Int
reactive_robots_kids = 4

deductive_robots::Int
deductive_robots = 4


--Do not change
inf::Int
inf =  n * m

title_change_environment::String
title_change_environment = "-------------------------------\n"++
                            "ENVIRONMENT CHANGES \n" ++
                            "-------------------------------\n"
                            
title_robot_turn::String
title_robot_turn = "-------------------------------\n"++
                    "ROBOT TURN \n" ++
                    "-------------------------------\n"
                            
title_environment_turn::String
title_environment_turn = "-------------------------------\n"++
                         "ENVIRONMENT TURN \n" ++
                         "-------------------------------\n"

title_finish::String
title_finish = "-------------------------------\n"++
                "FINISH \n" ++
                "-------------------------------\n"

title_start::String
title_start = "-------------------------------\n"++
              "START \n" ++
              "-------------------------------\n"

title_complete::String
title_complete = "-------------------------------\n"++
                 "COMPLETE    \n" ++
                 "-------------------------------\n" 

title_percent::Int -> String
title_percent p = "-------------------------------\n"++
                 "Percent Clean     " ++ show p ++ 
                 "%\n-------------------------------\n" 

title_min_percent::Int -> String
title_min_percent p = "-------------------------------\n"++
                      "Min Percent Clean     " ++ show p ++ 
                      "%\n-------------------------------\n"

title_steps::Int -> String
title_steps p = "-------------------------------\n"++
                "Total Steps     " ++ show p ++ 
                "\n-------------------------------\n"
