module Vector 
let origin=(0.0f, 0.0f, 0.0f)

let translate (x:float32, y:float32, z:float32) (x1,y1,z1) = (x+x1, y+y1, z+z1)

let scale (magnitude:float32) (x,y,z)  = (x*magnitude, y*magnitude, z*magnitude)

let scaleComponents (mx:float32,my:float32,mz:float32) (x:float32,y:float32,z:float32) = (mx*x, my*y, mz*z)

let shrinkComponents (mx:float32, my:float32, mz:float32) (x:float32,y:float32,z:float32) = (x/mx, y/my, z/mz)

let toRadians (angle:float32) = angle * (float32 System.Math.PI)/180.0f

let length (x:float32,y,z) = sqrt (x*x + y*y + z*z)

let rotateX degrees (x:float32,y:float32,z:float32) = 
    let angle = toRadians degrees
    (x, y*(cos angle)-z*(sin angle), z*(cos angle)+y*(sin angle))

let rotateY degrees (x:float32,y:float32,z:float32) = 
    let angle = -(toRadians degrees)
    (x*(cos angle)-z*(sin angle), y,z*(cos angle)+x*(sin angle))

let rotateZ (degrees:float32) (x:float32,y:float32,z:float32) = 
    let angle = toRadians degrees
    (x*(cos angle)-y*(sin angle), y*(cos angle)+x*(sin angle),z)
 
let lengthSquared (x,y,z) = (x*x)+(y*y)+(z*z)
let subtract (x,y,z) (x1, y1, z1) = (x-x1, y-y1, z-z1)

let horizontalDistaneSquare (x,y,z) (x1,y1,z1) = subtract (x,0.0f,z) (x1,0.0f,z1) |> lengthSquared

let inUnitCube (x,y,z) = 
    x>=(-0.5f) && x<=0.5f &&
    y>=(-0.5f) && y<=0.5f &&
    z>=(-0.5f) && z<=0.5f 
