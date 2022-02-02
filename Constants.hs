
module Constants where

--Change Environment Variable (Amount of Turns)
change_environment::Int
change_environment = 10

--Actual Turn
turns::Int
turns = 1

--Board Size
--Width
n::Int
n = 7
--Height
m::Int
m = 7

corral_width::Int
corral_width = 4

corral_heigth::Int
corral_heigth = 3

number_kids::Int
number_kids = 12

obstacles::Int
obstacles = 10

--ROBOTS
reactive_robots_dirt::Int
reactive_robots_dirt = 2

--0 Value Not Permited
reactive_robots_kids::Int
reactive_robots_kids = 2

deductive_robots::Int
deductive_robots = 2


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