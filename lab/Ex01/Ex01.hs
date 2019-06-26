module Ex01 where

-- needed to display the picture in the playground
import Codec.Picture
import Data.List

-- our line graphics programming interface
import ShapeGraphics

-- Part 1
-- picture of a house
housePic :: Picture
housePic = [door, house]
  where
    house :: PictureObject
    house = Path points green Solid
        where
            points::[Point]
            points = map pairToPoint houseCOs
    door :: PictureObject
    door  = Path points2 red Solid
        where
            points2 :: [Point]
            points2 = map pairToPoint doorCOs
-- these are the coordinates - convert them to a list of Point
houseCOs :: [(Float, Float)]
houseCOs = [(300, 750), (300, 450), (270, 450), (500, 200),
         (730, 450), (700, 450), (700, 750)]

doorCOs :: [(Float, Float)]
doorCOs = [(420, 750), (420, 550), (580, 550), (580, 750)]

grey :: Colour
grey = Colour 255 255 255 128

smoke :: PictureObject
smoke = Path smokePoint grey Solid
    where
        smokePoint :: [Point]
        smokePoint = map pairToPoint smokeCOs

chimneyHouse :: Picture
chimneyHouse = [chimney, smoke, door]
    where
        chimney::PictureObject
        chimney = Path chimneyPoints green Solid
            where
                chimneyPoints = map pairToPoint chimneyCOs

        door::PictureObject
        door = Path points2 red Solid
            where
                points2 :: [Point]
                points2 = map pairToPoint doorCOs

-- my own things
pairToPoint::(Float, Float) -> Point
pairToPoint (x,y) = Point x y

smokeCOs::[(Float, Float)]
smokeCOs = [(635, 240), (625, 230), (635, 220), (625, 210)]

chimneyCOs::[(Float, Float)]
chimneyCOs =[(300, 750), (300, 450), (270, 450), (500, 200),(615, 325), (615, 250), (650, 250), (650, 363),
         (730, 450), (700, 450), (700, 750)]

-- Part 2
movePoint :: Point -> Vector -> Point
movePoint (Point x y) (Vector xv yv)
  = Point (x + xv) (y + yv)

--my tasks
movePictureObject :: Vector -> PictureObject -> PictureObject
-- for path
movePictureObject vec (Path points colour lineStyle) =
    Path newPoints colour lineStyle
        where
            newPoints:: [Point]
            newPoints = map ($ vec) $ map movePoint points

--for circle
movePictureObject vec (Circle point radius colour lineStyle fillStyle) =
     Circle newPoint radius colour lineStyle fillStyle
        where
            newPoint:: Point
            newPoint = movePoint point vec
-- Ellipse
movePictureObject vec (Ellipse point width height rotation colour lineStyle fillStyle) =
     Ellipse newPoint width height rotation colour lineStyle fillStyle
        where
            newPoint:: Point
            newPoint = movePoint point vec
--Polygon
movePictureObject vec (Polygon points colour lineStyle fillStyle) =
    Polygon newPoints colour lineStyle fillStyle
        where
            newPoints:: [Point]
            newPoints = map ($ vec) $ map movePoint points


---helper things
myRed = red { opacityC = 180 }
xy = (Point 400 400)
circ = Circle xy 20 myRed Solid SolidFill
v = (Vector 0 0)

path = Path points2 myRed Solid
    where
        points2 :: [Point]
        points2 = map pairToPoint doorCOs

polygon = Polygon points2 myRed Solid NoFill
    where
        points2 :: [Point]
        points2 = map pairToPoint doorCOs

elpse = Ellipse xy 200 50 0 myRed Solid SolidFill
-- add other cases



-- Part 3


-- generate the picture consisting of circles:
-- [Circle (Point 400 400) (400/n) col Solid SolidFill,
--  Circle (Point 400 400) 2 * (400/n) col Solid SolidFill,
--  ....
--  Circle (Point 400 400) 400 col Solid SolidFill]
simpleCirclePic :: Colour -> Float -> Picture
simpleCirclePic col n = map makeShape radius
    where
        radius :: [Float]
        radius = [1 * (400/n), 2 * (400/n) .. n * (400/n)]

        makeShape:: Float -> PictureObject
        makeShape r =
            Circle (Point 400 400) r col Solid SolidFill

myCol:: Colour
myCol = Colour 153 0 153 100

-- use 'writeToFile' to write a picture to file "ex01.png" to test your
-- program if you are not using Haskell for Mac
-- e.g., call
-- writeToFile [house, door]

writeToFile pic
  = writePng "ex01.png" (drawPicture 3 pic)



--TODO: think all the error case
