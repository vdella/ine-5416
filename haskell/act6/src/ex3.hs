data Point = Point2D Float Float | Point3D Float Float Float

distance :: Point -> Point -> Float
distance (Point2D x1 y1) (Point2D x2 y2) = sqrt (diffModulum x1 x2 + diffModulum y1 y2)
                                           where diffModulum a b = (a - b)^2  
distance (Point3D x1 y1 z1) (Point3D x2 y2 z2) = sqrt (diffModulum x1 x2 + diffModulum y1 y2 + diffModulum z1 z2)
                                                 where diffModulum a b = (a - b)^2

main = do
    print (distance (Point2D 1 1) (Point2D 2 2))
    print (distance (Point3D 2 0 1) (Point3D 6 7 9))