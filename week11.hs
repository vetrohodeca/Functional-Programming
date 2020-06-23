main::IO()
main = do

data Point = Point2D Double Double | Point3D Double Double Double deriving (Show,Eq,Ord)

distance:: Point -> Point -> Double
distance (Point2D _ _)  (Point3D _ _)    = error "Not compatible"
distance (Point3D _ _)  (Point2D _ _)    = error "Not compatible"
distance (Point2D a1 b1) (Point3D a2 b2) = sqrt((a1 - a2) ** 2 + (b1 - b2) ** 2)
distance (Point3D a1 b1 c1) (Point3D a2 b2 c2) = sqrt((a1 - a2) ** 2 + (b1 - b2) ** 2+ (c1 - c2) ** 2)

getClosestPoint :: [Point] -> Point -> Point
getClosestPoint ps p = minimum[distance p k | k <- ps]x`