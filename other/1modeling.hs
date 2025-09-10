data Weather = Sunny | Rainy | Cloudy | Snowy deriving (Show,Eq)

isGoodWeather :: Weather -> Bool
isGoodWeather Sunny = True
isGoodWeather _ = False

data TrafficLight = Red | Yellow | Green deriving (Show, Eq)
canGo :: TrafficLight -> Bool
canGo Green = True
canGo _ = False

data Animal = Dog String Int | Cat String Int deriving(Show,Eq)
animalAge :: Animal -> Int
animalAge x = 